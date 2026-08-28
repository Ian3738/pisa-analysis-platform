# ============================================================
# 02_extract_convert.R
# 解壓官方 SPSS 檔 → 產出變數字典 → 抽取分析欄位 → 寫入 Parquet
#
# 分層原則：
#   raw（zip）   = 來源忠實副本，永不覆寫，即 bronze 層
#   silver       = 跨年命名對齊後的分析欄位，Parquet
#   codebook     = 完整變數清單與標籤，供日後擴充欄位
# ============================================================
source("~/PISA/R/00_config.R")
suppressPackageStartupMessages({
  library(haven); library(data.table); library(arrow); library(labelled)
})

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# ---- 要保留的欄位（樣式比對，存在才留）-----------------------------------
KEEP_PATTERNS <- c(
  # 識別與抽樣設計
  "^CNT$", "^CNTRYID$", "^CNTSCHID$", "^CNTSTUID$", "^CYC$", "^OECD$",
  "^STRATUM$", "^SUBNATIO$", "^REGION$", "^ADMINMODE$", "^BOOKID$",
  "^SCHOOLID$", "^STIDSTD$", "^LANGTEST",
  # 權重
  "^W_FSTUWT$", "^W_FSTURWT[0-9]+$", "^W_FSTR[0-9]+$", "^SENWT$", "^W_FSTUWT_SCH_SUM$",
  # 合理推估值（全領域）
  # 核心三領域、各輪創新領域，以及分測驗／子樣本量尺
  #   GLCM=全球素養(2018) CLPS/CPRO=合作解題(2015) FLIT=財金素養 CRTH=創造思考(2022)
  #   MATC/REAC/SCIC=在創造思考子樣本上重算的三科（CRT 檔）
  #   MC**/MP**=數學的內容與歷程分測驗（2022 主檔）
  "^PV[0-9]+(MATH|READ|SCIE|GLCM|CLPS|CPRO|FLIT|CRTH|MATC|REAC|SCIC)",
  # 2022 數學分測驗：內容向度 MC** 與歷程向度 MP**
  #   MCCR 變化與關係、MCQN 數量、MCSS 空間與形狀、MCUD 不確定性與資料
  #   MPFS 形成、MPEM 應用、MPIN 詮釋、MPRE 推理
  "^PV[0-9]+(MCCR|MCQN|MCSS|MCUD|MPEM|MPFS|MPIN|MPRE)$",
  # 社經地位
  "^ESCS$", "^HISEI$", "^HOMEPOS$", "^PAREDINT$", "^BMMJ1$", "^BFMJ2$",
  "^MISCED$", "^FISCED$", "^HISCED$", "^ICTRES$", "^CULTPOSS$", "^HEDRES$", "^WEALTH$",
  # 人口變項
  "^ST004D01T$", "^ST04Q01$", "^AGE$", "^GRADE$", "^IMMIG$", "^REPEAT$",
  "^LANGN$", "^ISCEDL$", "^ISCEDO$", "^ISCEDP$", "^PROGN$",
  # 常用態度／環境量表
  "^BELONG$", "^BULLIED$", "^DISCLIM$", "^TEACHSUP$", "^FAMSUP$", "^ANXMAT$",
  "^MATHEFF$", "^MATHPERS$", "^ASSERAGG$", "^COOPAGR$", "^CURIOAGR$",
  "^EMOCOAGR$", "^STRESAGR$", "^EXPOFA$", "^EXPO21ST$", "^FEELSAFE$",
  "^SCHRISK$", "^TEACHINT$", "^RELATST$", "^GROSAGR$", "^LIFESAT$",
  "^EXERPRAC$", "^STUDYHMW$", "^SKIPPING$", "^TARDYSD$"
)

sav_vars <- function(path) {
  h <- haven::read_sav(path, n_max = 0)
  # 少數變數的 label 屬性長度不為 1（PISA 2018 學校檔即有此情形），一律壓成單一字串
  labs <- vapply(h, function(x) {
    l <- attr(x, "label")
    if (is.null(l) || !length(l)) NA_character_ else paste(as.character(l), collapse = " | ")
  }, character(1), USE.NAMES = FALSE)
  data.table(variable = names(h), label = labs)
}

# 學校檔只有 400 餘欄，直接保留所有衍生指標，僅排除原始題項
SCH_DROP_PATTERN <- "^SC[0-9]+Q"

pick_cols <- function(all_vars, kind = "STU") {
  if (kind == "SCH") {
    hit <- grep(SCH_DROP_PATTERN, all_vars, value = TRUE, invert = TRUE)
  } else {
    hit <- unique(unlist(lapply(KEEP_PATTERNS, function(p) grep(p, all_vars, value = TRUE))))
  }
  hit[order(match(hit, all_vars))]
}

# kind 決定欄位選取規則與輸出檔名。
#   "STU" / "SCH" 為主檔；其他名稱（如 "CRT"、"FLT"）視同學生層級的附加檔，
#   各自寫成獨立的 <kind>_<cycle>.parquet，不會覆蓋主檔。
process_cycle <- function(cycle, zip_path, kind = "STU", file_pattern = NULL) {
  stopifnot(length(kind) == 1L, nzchar(kind))
  if (!kind %in% c("STU", "SCH")) {
    # 附加檔沿用學生檔的欄位規則
    attr(kind, "col_rule") <- "STU"
  }
  stage <- file.path(DIR_STAGE, sprintf("%s%d", tolower(kind), cycle))
  unlink(stage, recursive = TRUE); dir.create(stage, recursive = TRUE)

  log_msg("[", cycle, " ", kind, "] 解壓 ", basename(zip_path))
  ok <- tryCatch(utils::unzip(zip_path, exdir = stage), error = function(e) NULL)
  if (is.null(ok) || !length(ok)) {           # 部分 OECD 檔為 Deflate64，R 內建解不開
    log_msg("  R unzip 失敗，改用系統 unzip")
    system2("unzip", c("-o", "-q", shQuote(zip_path), "-d", shQuote(stage)))
  }
  sav <- list.files(stage, pattern = "\\.sav$", ignore.case = TRUE,
                    full.names = TRUE, recursive = TRUE)
  if (!length(sav)) stop("找不到 .sav：", stage)
  # 部分 zip 內含多個檔（例如 FLT 有 COG 試題檔、QQQ 問卷檔、TIM 作答時間檔），
  # 合理推估值與權重在 QQQ，試題檔沒有。file_pattern 用來指定要哪一個。
  if (!is.null(file_pattern)) {
    hit <- grep(file_pattern, basename(sav), ignore.case = TRUE, value = TRUE)
    if (!length(hit)) stop("zip 內沒有符合 \"", file_pattern, "\" 的檔案；實際有：",
                           paste(basename(sav), collapse = ", "))
    sav <- sav[basename(sav) %in% hit]
  }
  if (length(sav) > 1) sav <- sav[which.max(file.info(sav)$size)]
  log_msg("  檔案 ", basename(sav), "  ",
          round(file.info(sav)$size / 2^20, 1), " MB")

  # 完整變數字典
  vars <- sav_vars(sav)
  vars[, `:=`(cycle = cycle, kind = kind)]
  cb <- file.path(DIR_SILVER, sprintf("codebook_%s_%d.csv", kind, cycle))
  fwrite(vars, cb)
  log_msg("  變數字典 ", nrow(vars), " 個變數 → ", basename(cb))

  col_rule <- if (kind == "SCH") "SCH" else "STU"
  keep <- pick_cols(vars$variable, col_rule)
  log_msg("  抽取 ", length(keep), " 欄")

  d <- haven::read_sav(sav, col_select = all_of(keep), user_na = FALSE)
  setDT(d)
  d[, cycle := cycle]
  # 去掉 haven 標籤屬性，Parquet 只留數值／字串
  d <- d[, lapply(.SD, function(x) if (inherits(x, "haven_labelled")) labelled::remove_labels(x) else x)]

  out <- file.path(DIR_SILVER, sprintf("%s_%d.parquet", kind, cycle))
  arrow::write_parquet(d, out, compression = "zstd")
  log_msg("  寫出 ", basename(out), "  ", nrow(d), " 列 × ", ncol(d), " 欄  ",
          round(file.info(out)$size / 2^20, 1), " MB")

  unlink(stage, recursive = TRUE)             # 立刻清掉 .sav 以省磁碟
  log_msg("  已清除暫存")
  invisible(out)
}
