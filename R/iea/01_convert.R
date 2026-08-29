# ============================================================
# 01_convert.R — 把 TIMSS 的 per-country .rdata 轉成單一 Parquet
#
# TIMSS 的國際資料庫是每個國家一個檔，與 PISA 的單一大檔不同。
# 檔名規則：<年級><檔別><國碼>m<年級>.rdata
#   b = 八年級（a = 四年級）
#   sa 學生成就（含推估值）  sg 學生背景  bcg 學校  btm/bts 數學／科學教師
# ============================================================
suppressPackageStartupMessages({
  library(data.table); library(haven); library(arrow)
})
TIMSS_ROOT <- path.expand("~/TIMSS")
SDIR <- file.path(TIMSS_ROOT, "staging", "R Data")
PDIR <- file.path(TIMSS_ROOT, "parquet")
dir.create(PDIR, recursive = TRUE, showWarnings = FALSE)

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# rdata 欄位帶 haven 標籤類別，算術會失敗，一律轉為純數值
strip_labels <- function(d)
  as.data.table(haven::zap_labels(haven::zap_missing(as.data.frame(d))))

load_one <- function(f) {
  e <- new.env(); load(file.path(SDIR, f), envir = e)
  strip_labels(get(ls(e)[1], envir = e))
}

# 只保留分析需要的欄位，避免 1,145 欄全進 Parquet
KEEP_SA <- function(nm) unique(c(
  grep("^(IDCNTRY|IDSCHOOL|IDSTUD|IDCLASS|JKZONE|JKREP|TOTWGT|HOUWGT|SENWGT)$", nm, value = TRUE),
  grep("^BS[MS][A-Z]{3}[0-9]{2}$", nm, value = TRUE),   # 全部量尺的推估值
  grep("^ITSEX$|^ITBIRTH", nm, value = TRUE)))

# 合成量表一律全留：BSBGxxx 為連續分數（SCL）、BSDGxxx 為切點分類（IDX），
# 兩者成對出現。早期版本只列舉了四個連續量表，導致「學生被霸凌」「重視數學」
# 「喜歡自然」等量表只有分類版可用，無法做加權平均與梯度分析。
KEEP_SG <- function(nm) unique(c(
  grep("^(IDCNTRY|IDSCHOOL|IDSTUD)$", nm, value = TRUE),
  grep("^BSBG(01|03|04|05|06|07|08|10|11|12)", nm, value = TRUE),  # 性別、出生地、家庭資源等
  grep("^BS[BD]G[A-Z]{2,4}$", nm, value = TRUE)))

convert_grade8 <- function() {
  files <- list.files(SDIR, pattern = "^bsa[a-z]{3}m8\\.rdata$")
  log_msg("學生成就檔 ", length(files), " 國")
  sa <- rbindlist(lapply(files, function(f) {
    d <- load_one(f)
    keep <- intersect(KEEP_SA(names(d)), names(d))
    d <- d[, ..keep]; d[, CNT := toupper(substr(f, 4, 6))]; d
  }), fill = TRUE)
  log_msg("  ", nrow(sa), " 列 × ", ncol(sa), " 欄")
  write_parquet(sa, file.path(PDIR, "BSA_2023.parquet"), compression = "zstd")

  gfiles <- list.files(SDIR, pattern = "^bsg[a-z]{3}m8\\.rdata$")
  log_msg("學生背景檔 ", length(gfiles), " 國")
  sg <- rbindlist(lapply(gfiles, function(f) {
    d <- load_one(f)
    keep <- intersect(KEEP_SG(names(d)), names(d))
    d <- d[, ..keep]; d[, CNT := toupper(substr(f, 4, 6))]; d
  }), fill = TRUE)
  log_msg("  ", nrow(sg), " 列 × ", ncol(sg), " 欄")
  write_parquet(sg, file.path(PDIR, "BSG_2023.parquet"), compression = "zstd")

  cfiles <- list.files(SDIR, pattern = "^bcg[a-z]{3}m8\\.rdata$")
  log_msg("學校檔 ", length(cfiles), " 國")
  cg <- rbindlist(lapply(cfiles, function(f) {
    d <- load_one(f); d[, CNT := toupper(substr(f, 4, 6))]; d
  }), fill = TRUE)
  log_msg("  ", nrow(cg), " 列 × ", ncol(cg), " 欄")
  write_parquet(cg, file.path(PDIR, "BCG_2023.parquet"), compression = "zstd")
  invisible(TRUE)
}
