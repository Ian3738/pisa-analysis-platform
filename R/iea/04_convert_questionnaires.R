# ============================================================
# 04_convert_questionnaires.R — 學生／教師／校長／家長問卷的轉檔
#
# 成就檔（xSA）已由 01/03 轉好，此處補的是問卷檔。IEA 系列的檔名規則：
#   <前綴><國碼><輪次碼>.rdata   例如 btmtwnm8.rdata、asgtwnr5.rdata
#
# 問卷檔的合成量表以 IRT 量尺化，國際平均 10、標準差 2（TIMSS／PIRLS）
# 或平均 50、標準差 10（ICCS）。B 版為連續分數，D 版為切點分類，
# 兩者成對出現，本平台一律採 B 版做估計、D 版做交叉表。
#
# 轉檔時同步抽出變數字典（變數名 → 標籤），因為 zap_labels 之後標籤就沒了，
# 而量表的「測量不變性」等重要資訊正是寫在標籤裡（見 TALIS 的教訓）。
# ============================================================
suppressPackageStartupMessages({library(data.table); library(haven); library(arrow)})

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

strip_labels <- function(d)
  as.data.table(haven::zap_labels(haven::zap_missing(as.data.frame(d))))

# 讀一個檔，回傳原始（含標籤）的 data.frame
read_one <- function(path) {
  if (grepl("[.]sav$", path, ignore.case = TRUE)) return(haven::read_sav(path))
  e <- new.env(); load(path, envir = e); get(ls(e)[1], envir = e)
}

# 從含標籤的資料抽變數字典
var_dict <- function(d) {
  data.table(v = names(d), label = vapply(names(d), function(x) {
    l <- attr(d[[x]], "label"); if (is.null(l)) "" else paste(as.character(l), collapse = " | ")
  }, character(1)))
}

#' @param sdir     來源目錄
#' @param prefix   檔名前綴，如 "btm"
#' @param cycle    輪次碼，如 "m8"、"r5"、"c4"、"c3"
#' @param keep     要保留的欄位 regex 向量；NULL 表示全留
#' @param outfile  輸出的 parquet 路徑
convert_q <- function(sdir, prefix, cycle, keep, outfile, dictfile = NULL) {
  pat <- sprintf("^%s[a-z]{3}%s\\.(rdata|sav)$", prefix, cycle)
  fs  <- list.files(sdir, pattern = pat, ignore.case = TRUE)
  if (!length(fs)) { log_msg("  ", prefix, " 找不到檔案（", pat, "）"); return(invisible(NULL)) }
  cnt_of <- function(f) toupper(substr(f, nchar(prefix) + 1, nchar(prefix) + 3))

  dict <- NULL
  out <- rbindlist(lapply(fs, function(f) {
    raw <- read_one(file.path(sdir, f))
    if (is.null(dict)) dict <<- var_dict(raw)
    d <- strip_labels(raw)
    if (!is.null(keep)) {
      k <- unique(unlist(lapply(keep, function(r) grep(r, names(d), value = TRUE))))
      k <- intersect(k, names(d))
      if (length(k)) d <- d[, ..k]
    }
    d[, CNT := cnt_of(f)]; d
  }), fill = TRUE)

  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
  write_parquet(out, outfile, compression = "zstd")
  log_msg(sprintf("  %-4s %2d 國  %7d 列 × %3d 欄  → %s (%.1f MB)",
                  toupper(prefix), length(fs), nrow(out), ncol(out),
                  basename(outfile), file.info(outfile)$size / 2^20))
  if (!is.null(dictfile)) { fwrite(dict, dictfile); log_msg("       字典 ", nrow(dict), " 個變數 → ", basename(dictfile)) }
  invisible(out)
}

# ---- 各評比的欄位保留規則 -------------------------------------------------
# 學生背景／教師／校長問卷都只留 ID、權重、合成量表與少數關鍵題項，
# 原始題項動輒數百欄，全留會讓 parquet 膨脹且對本平台的分析無用。
KEEP_IDW   <- "^(IDCNTRY|IDSCHOOL|IDSTUD|IDCLASS|IDTEACH|IDLINK|IDGRADE|IDPOP)$"
KEEP_WGT   <- "^(TOTWGT|HOUWGT|SENWGT|MATWGT|SCIWGT|TCHWGT|TOTWGTS|TOTWGTT|TOTWGTC|SENWGTS)$"
KEEP_JK    <- "^(JKZONE|JKREP|JKZONES|JKREPS|JKZONET|JKREPT)$"
KEEP_SCALE_T <- "^[ABIT][SCTH][BD][GH][A-Z]{2,4}$"   # TIMSS／PIRLS 合成量表
KEEP_ICCS    <- "^[SCT]_[A-Z0-9]+$"                  # ICCS 合成量表

# ---- 轉檔主程序 -----------------------------------------------------------
# 教師檔本身不含權重：TIMSS／PIRLS 的教師權重（TCHWGT／MATWGT／SCIWGT）
# 存放於學生—教師連結檔（xST），必須以 IDTEACH + IDLINK 併回才能做加權估計。
# 直接以教師檔計算會得到未加權的結果，這是教師層次分析最常見的錯誤。
convert_all <- function() {
  TQ <- "~/TIMSS/parquet"; PQ <- "~/PIRLS/parquet"; IQ <- "~/ICCS/parquet"
  DD <- "~/PISA/output/dict"; dir.create(path.expand(DD), recursive = TRUE, showWarnings = FALSE)
  pe <- path.expand

  keep_tch  <- c(KEEP_IDW, KEEP_SCALE_T, "^[AB]TBG0[1-4]$")
  keep_link <- c(KEEP_IDW, KEEP_WGT, KEEP_JK, "^IDTEALIN$|^IDSUBJ$")
  keep_home <- c(KEEP_IDW, "^AS[BD][GH][A-Z]{2,4}$")
  keep_stu  <- c(KEEP_IDW, "^AS[BD]G[A-Z]{2,4}$", "^ITSEX$")
  keep_iccs <- c(KEEP_IDW, KEEP_WGT, KEEP_JK, KEEP_ICCS)

  log_msg("TIMSS 八年級")
  convert_q(pe("~/TIMSS/staging/g8"), "btm", "m8", keep_tch,  pe(file.path(TQ, "BTM_2023.parquet")), pe(file.path(DD, "timss_g8_teacher.csv")))
  convert_q(pe("~/TIMSS/staging/g8"), "bts", "m8", keep_tch,  pe(file.path(TQ, "BTS_2023.parquet")))
  convert_q(pe("~/TIMSS/staging/g8"), "bst", "m8", keep_link, pe(file.path(TQ, "BST_2023.parquet")))

  log_msg("TIMSS 四年級")
  convert_q(pe("~/TIMSS/staging/g4"), "atg", "m8", keep_tch,  pe(file.path(TQ, "g4/TG.parquet")), pe(file.path(DD, "timss_g4_teacher.csv")))
  convert_q(pe("~/TIMSS/staging/g4"), "ash", "m8", keep_home, pe(file.path(TQ, "g4/SH.parquet")), pe(file.path(DD, "timss_g4_home.csv")))
  convert_q(pe("~/TIMSS/staging/g4"), "ast", "m8", keep_link, pe(file.path(TQ, "g4/ST.parquet")))

  log_msg("PIRLS 2021（R5 為數位施測主樣本）")
  convert_q(pe("~/PIRLS/staging"), "asg", "r5", keep_stu,  pe(file.path(PQ, "SG.parquet")), pe(file.path(DD, "pirls_student.csv")))
  convert_q(pe("~/PIRLS/staging"), "acg", "r5", NULL,      pe(file.path(PQ, "CG.parquet")), pe(file.path(DD, "pirls_school.csv")))
  convert_q(pe("~/PIRLS/staging"), "ash", "r5", keep_home, pe(file.path(PQ, "SH.parquet")), pe(file.path(DD, "pirls_home.csv")))
  convert_q(pe("~/PIRLS/staging"), "atg", "r5", keep_tch,  pe(file.path(PQ, "TG.parquet")), pe(file.path(DD, "pirls_teacher.csv")))

  log_msg("ICCS 2022")
  convert_q(pe("~/ICCS/staging/ICCS2022_IDB_R/Data"), "isg", "c4", keep_iccs, pe(file.path(IQ, "ISG_2022.parquet")), pe(file.path(DD, "iccs22_student.csv")))
  convert_q(pe("~/ICCS/staging/ICCS2022_IDB_R/Data"), "icg", "c4", keep_iccs, pe(file.path(IQ, "ICG_2022.parquet")), pe(file.path(DD, "iccs22_school.csv")))
  convert_q(pe("~/ICCS/staging/ICCS2022_IDB_R/Data"), "itg", "c4", keep_iccs, pe(file.path(IQ, "ITG_2022.parquet")), pe(file.path(DD, "iccs22_teacher.csv")))

  log_msg("ICCS 2016（趨勢用）")
  convert_q(pe("~/ICCS/staging16"), "isa", "c3", c(KEEP_IDW, KEEP_WGT, KEEP_JK, "^PV[0-9]CIV$"), pe(file.path(IQ, "ISA_2016.parquet")))
  convert_q(pe("~/ICCS/staging16"), "isg", "c3", keep_iccs, pe(file.path(IQ, "ISG_2016.parquet")), pe(file.path(DD, "iccs16_student.csv")))
  convert_q(pe("~/ICCS/staging16"), "icg", "c3", keep_iccs, pe(file.path(IQ, "ICG_2016.parquet")))
  invisible(NULL)
}
