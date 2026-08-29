# ============================================================
# 03_convert_generic.R — TIMSS 四年級與 PIRLS 的轉檔
#
# IEA 系列的檔名規則一致，只有前綴與量尺代碼不同：
#   TIMSS 八年級  b + sa/sg/cg + <國碼> + m8     推估值 BSMMAT01–05、BSSSCI01–05
#   TIMSS 四年級  a + sa/sg/cg + <國碼> + m8     推估值 ASMMAT01–05、ASSSCI01–05
#   PIRLS 2021    a + sa/sg/cg + <國碼> + p2     推估值 ASRREA01–05（閱讀總分）
#
# 三者的抽樣設計相同：TOTWGT 權重、JKZONE／JKREP 折刀分區，5 個推估值，
# 故 lib_timss.R 的 JK2 估計核心可以共用，不需另寫。
# ============================================================
suppressPackageStartupMessages({library(data.table); library(haven); library(arrow)})

strip_labels <- function(d)
  as.data.table(haven::zap_labels(haven::zap_missing(as.data.frame(d))))

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# study: "timss_g4" 或 "pirls"
convert_iea <- function(study, sdir, outdir, suffix) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pat_sa <- sprintf("^asa[a-z]{3}%s\\.rdata$", suffix)
  pat_sg <- sprintf("^asg[a-z]{3}%s\\.rdata$", suffix)
  pat_cg <- sprintf("^acg[a-z]{3}%s\\.rdata$", suffix)

  ld <- function(f) { e <- new.env(); load(file.path(sdir, f), envir = e)
                      strip_labels(get(ls(e)[1], envir = e)) }
  cnt_of <- function(f) toupper(substr(f, 4, 6))

  keep_sa <- function(nm) unique(c(
    grep("^(IDCNTRY|IDSCHOOL|IDSTUD|IDCLASS|JKZONE|JKREP|TOTWGT|HOUWGT|SENWGT)$", nm, value = TRUE),
    grep("^AS[A-Z]{4}[0-9]{2}$", nm, value = TRUE),   # 全部量尺的推估值
    grep("^ITSEX$", nm, value = TRUE)))

  fs <- list.files(sdir, pattern = pat_sa)
  log_msg(study, " 學生成就檔 ", length(fs), " 國")
  sa <- rbindlist(lapply(fs, function(f) {
    d <- ld(f); keep <- intersect(keep_sa(names(d)), names(d))
    d <- d[, ..keep]; d[, CNT := cnt_of(f)]; d
  }), fill = TRUE)
  log_msg("  ", nrow(sa), " 列 × ", ncol(sa), " 欄")
  write_parquet(sa, file.path(outdir, "SA.parquet"), compression = "zstd")

  gs <- list.files(sdir, pattern = pat_sg)
  if (length(gs)) {
    sg <- rbindlist(lapply(gs, function(f) {
      d <- ld(f)
      keep <- intersect(c(grep("^(IDCNTRY|IDSCHOOL|IDSTUD|ITSEX)$", names(d), value = TRUE),
                          grep("^AS[BD]G[A-Z]{2,4}$", names(d), value = TRUE),
                          grep("^ASBG(01|03|04|05|06|07)", names(d), value = TRUE)), names(d))
      d <- d[, ..keep]; d[, CNT := cnt_of(f)]; d
    }), fill = TRUE)
    log_msg("  學生背景 ", nrow(sg), " 列 × ", ncol(sg), " 欄")
    write_parquet(sg, file.path(outdir, "SG.parquet"), compression = "zstd")
  }

  cs <- list.files(sdir, pattern = pat_cg)
  if (length(cs)) {
    cg <- rbindlist(lapply(cs, function(f) { d <- ld(f); d[, CNT := cnt_of(f)]; d }), fill = TRUE)
    log_msg("  學校 ", nrow(cg), " 列 × ", ncol(cg), " 欄")
    write_parquet(cg, file.path(outdir, "CG.parquet"), compression = "zstd")
  }
  invisible(TRUE)
}
