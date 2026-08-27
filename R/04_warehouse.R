# ============================================================
# 04_warehouse.R — 建立 DuckDB 分析倉儲
#
# 為什麼用 DuckDB：PISA 單一輪次即數十萬列 × 上千欄，且分析多為
# 「依國家／年份切片後彙總」。DuckDB 直接查詢 Parquet，不必先載入
# 記憶體，也不需要伺服器；資料量再大十倍仍是同一套語法。
# ============================================================
source("~/PISA/R/00_config.R")
suppressPackageStartupMessages({library(DBI); library(duckdb); library(data.table)})

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

build_warehouse <- function(db_path = DB_PATH, overwrite = TRUE) {
  if (overwrite && file.exists(db_path)) unlink(c(db_path, paste0(db_path, ".wal")))
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  stu <- list.files(DIR_SILVER, pattern = "^STU_[0-9]{4}\\.parquet$", full.names = TRUE)
  sch <- list.files(DIR_SILVER, pattern = "^SCH_[0-9]{4}\\.parquet$", full.names = TRUE)
  if (!length(stu)) stop("silver 層沒有學生檔，請先跑 02_extract_convert.R")

  # 各輪次分別建 view（各輪欄位不同，不強行合併）
  for (f in c(stu, sch)) {
    nm <- sub("\\.parquet$", "", basename(f))
    dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s')", nm, f))
    n <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", nm))$n
    log_msg("view ", nm, "  ", format(n, big.mark = ","), " 列")
  }

  # 跨輪共同欄位的長表：只放趨勢分析真正需要的欄位
  parts <- vapply(stu, function(f) {
    cy <- sub(".*STU_([0-9]{4}).*", "\\1", f)
    sprintf("SELECT CNT, CNTRYID, CNTSCHID, CNTSTUID, OECD, %s AS cycle,
                    ST004D01T, ESCS, W_FSTUWT,
                    PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH,
                    PV6MATH, PV7MATH, PV8MATH, PV9MATH, PV10MATH,
                    PV1READ, PV2READ, PV3READ, PV4READ, PV5READ,
                    PV6READ, PV7READ, PV8READ, PV9READ, PV10READ,
                    PV1SCIE, PV2SCIE, PV3SCIE, PV4SCIE, PV5SCIE,
                    PV6SCIE, PV7SCIE, PV8SCIE, PV9SCIE, PV10SCIE
             FROM read_parquet('%s')", cy, f)
  }, character(1))
  dbExecute(con, paste("CREATE OR REPLACE VIEW stu_all AS",
                       paste(parts, collapse = "\nUNION ALL\n")))
  n <- dbGetQuery(con, "SELECT COUNT(*) n FROM stu_all")$n
  log_msg("view stu_all  ", format(n, big.mark = ","), " 列（", length(stu), " 輪）")

  # 常用彙總：各國各輪的樣本數與未加權平均（快速健檢用，非正式估計）
  dbExecute(con, "
    CREATE OR REPLACE TABLE qa_cycle_country AS
    SELECT cycle, CNT, COUNT(*) AS n_students,
           COUNT(DISTINCT CNTSCHID) AS n_schools,
           AVG(PV1MATH) AS pv1math_unweighted,
           AVG(PV1READ) AS pv1read_unweighted,
           AVG(PV1SCIE) AS pv1scie_unweighted,
           SUM(CASE WHEN ESCS IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS escs_missing_rate
    FROM stu_all GROUP BY cycle, CNT ORDER BY cycle, CNT")
  log_msg("表 qa_cycle_country 建立完成")
  invisible(db_path)
}

pisa_con <- function(db_path = DB_PATH, read_only = TRUE) {
  dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}
