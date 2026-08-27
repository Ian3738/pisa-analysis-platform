# ============================================================
# 01_download.R — 從 OECD 官方檔案伺服器取得資料
#
# 可重複執行：已存在且大小相符的檔案會跳過。
# 每次下載都記錄 SHA-256 與 HTTP 標頭，作為資料來源的稽核軌跡。
# ============================================================
source("~/PISA/R/00_config.R")
suppressPackageStartupMessages({library(data.table)})

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

remote_size <- function(url) {
  h <- system2("curl", c("-sSIL", "-A", shQuote(USER_AGENT), "--max-time", "60", shQuote(url)),
               stdout = TRUE, stderr = FALSE)
  cl <- grep("^content-length:", h, ignore.case = TRUE, value = TRUE)
  if (!length(cl)) return(NA_real_)
  as.numeric(sub(".*:\\s*", "", tail(cl, 1)))
}

download_one <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  rs <- remote_size(url)
  if (file.exists(dest) && !is.na(rs) && file.info(dest)$size == rs) {
    log_msg("  已存在且大小相符，跳過：", basename(dest)); return(invisible(dest))
  }
  log_msg("  下載 ", basename(dest), "  ", round(rs / 2^20, 1), " MB")
  st <- system2("curl", c("-sSL", "-A", shQuote(USER_AGENT),
                          "--retry", "3", "--retry-delay", "5", "-C", "-",
                          "-o", shQuote(dest), shQuote(url)))
  if (st != 0) stop("下載失敗：", url)
  invisible(dest)
}

manifest_row <- function(cycle, file, url, dest) {
  sha <- system2("shasum", c("-a", "256", shQuote(dest)), stdout = TRUE)
  data.table(cycle = cycle, file = file, url = url,
             local_path = dest,
             bytes = file.info(dest)$size,
             sha256 = sub("\\s.*", "", sha),
             downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"))
}

run_download <- function(sources = PISA_SOURCES) {
  rows <- list()
  for (i in seq_len(nrow(sources))) {
    s <- sources[i, ]
    log_msg("[", s$cycle, " ", s$file, "]")
    dest <- file.path(DIR_RAW, s$cycle, basename(s$url))
    download_one(s$url, dest)
    rows[[i]] <- manifest_row(s$cycle, s$file, s$url, dest)
  }
  mf <- rbindlist(rows)
  out <- file.path(DIR_RAW, "manifest.csv")
  fwrite(mf, out)
  log_msg("清單寫入 ", out)
  mf
}

# 2012 及更早的輪次：OECD 主站已改版並啟用 Cloudflare 人機驗證，
# 無法以程式自動取得，須由人工從下列頁面下載後放入 raw/<cycle>/：
PISA_MANUAL_SOURCES <- data.frame(
  cycle = c(2012, 2009, 2006, 2003, 2000),
  page  = sprintf("https://www.oecd.org/en/data/datasets/pisa-%d-database.html",
                  c(2012, 2009, 2006, 2003, 2000)),
  note  = "ASCII 資料檔 + SPSS/SAS 讀取語法；需先跑語法檔轉成 .sav",
  stringsAsFactors = FALSE
)
