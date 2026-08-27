# ============================================================
# PISA 分析系統 — 全域設定
# ============================================================
PISA_ROOT   <- path.expand("~/PISA")
DIR_RAW     <- file.path(PISA_ROOT, "raw")        # 原始 zip，永不覆寫
DIR_STAGE   <- file.path(PISA_ROOT, "staging")    # 解壓的 .sav，轉檔後刪除
DIR_BRONZE  <- file.path(PISA_ROOT, "parquet", "bronze")  # 全欄位原樣
DIR_SILVER  <- file.path(PISA_ROOT, "parquet", "silver")  # 跨年變數對齊
DIR_GOLD    <- file.path(PISA_ROOT, "parquet", "gold")    # 分析用寬表
DIR_WH      <- file.path(PISA_ROOT, "warehouse")
DIR_OUT     <- file.path(PISA_ROOT, "output")
DIR_LOG     <- file.path(PISA_ROOT, "logs")
DB_PATH     <- file.path(DIR_WH, "pisa.duckdb")

for (d in c(DIR_RAW, DIR_STAGE, DIR_BRONZE, DIR_SILVER, DIR_GOLD,
            DIR_WH, DIR_OUT, DIR_LOG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# 官方檔案來源（EdSurvey::downloadPISA 內建對照表，已於 2026-08-27 逐一驗證 HTTP 狀態）
# 注意 2015 的網址在 webfs.oecd.org/pisa/（無年份），不是 /pisa2015/
PISA_SOURCES <- data.frame(
  cycle = c(2022, 2022, 2022, 2022, 2022,
            2018, 2018, 2018,
            2015, 2015),
  file  = c("STU_QQQ", "SCH_QQQ", "ESCS_TREND", "CRT", "FLT",
            "STU_QQQ", "SCH_QQQ", "VNM_PV",
            "STU_QQQ", "SCH_QQQ"),
  # kind 對應 process_cycle() 的輸出檔名；file_pattern 指定 zip 內要哪個 .sav
  kind  = c("STU", "SCH", NA, "CRT", "FLT",
            "STU", "SCH", "VNM",
            "STU", "SCH"),
  file_pattern = c(NA, NA, NA, NA, "QQQ",
                   NA, NA, "PVS",
                   NA, NA),
  url   = c("https://webfs.oecd.org/pisa2022/STU_QQQ_SPSS.zip",
            "https://webfs.oecd.org/pisa2022/SCH_QQQ_SPSS.zip",
            "https://webfs.oecd.org/pisa2022/escs_trend.zip",
            "https://webfs.oecd.org/pisa2022/CRT_SPSS.zip",
            "https://webfs.oecd.org/pisa2022/FLT_SPSS.zip",
            "https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip",
            "https://webfs.oecd.org/pisa2018/SPSS_SCH_QQQ.zip",
            "https://webfs.oecd.org/pisa2018/SPSS_VNM_PV_COG.zip",
            "https://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip",
            "https://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip"),
  stringsAsFactors = FALSE
)

# 一鍵重建：依 PISA_SOURCES 逐檔下載並轉檔
rebuild_all <- function() {
  source("~/PISA/R/01_download.R"); source("~/PISA/R/02_extract_convert.R")
  source("~/PISA/R/04_warehouse.R")
  run_download()
  for (i in seq_len(nrow(PISA_SOURCES))) {
    s <- PISA_SOURCES[i, ]
    if (is.na(s$kind)) next            # ESCS_TREND 由 load_escs_trend() 自行解壓
    process_cycle(s$cycle, file.path(DIR_RAW, s$cycle, basename(s$url)),
                  s$kind, file_pattern = if (is.na(s$file_pattern)) NULL else s$file_pattern)
  }
  build_warehouse()
}

# BRR 參數：PISA 使用 Fay 平衡重複半樣本法，80 組重複權重，Fay 係數 0.5
PISA_BRR_G   <- 80
PISA_BRR_FAY <- 0.5
PISA_N_PV    <- 10   # 2015 年起每個領域 10 個合理推估值（2012 以前為 5 個）

USER_AGENT <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36")
