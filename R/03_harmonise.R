# ============================================================
# 03_harmonise.R — 跨輪次變數對齊
#
# PISA 各輪的變數命名並不一致，直接疊在一起會出錯。本檔把差異集中處理，
# 讓下游分析只需面對一套名稱。
#
# 已知的跨輪差異：
#   1. 性別：2015 起為 ST004D01T（1=女 2=男）；2012 以前為 ST04Q01
#   2. 重複權重：2015 起為 W_FSTURWT1..80；2012 以前為 W_FSTR1..80
#   3. 合理推估值個數：2015 起 10 個；2012 以前 5 個
#   4. ESCS：2022 重新校準過，跨輪必須改用官方 escs_trend（僅涵蓋 2012–2018）
#   5. 施測模式：2015 起以電腦施測為主，與紙筆時期存在模式效應
# ============================================================
source("~/PISA/R/00_config.R")
suppressPackageStartupMessages({library(data.table); library(arrow)})

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# 官方 trend ESCS（2012 / 2015 / 2018 重新校準到 2022 量尺）
load_escs_trend <- function(csv = file.path(DIR_SILVER, "escs_trend.csv")) {
  if (!file.exists(csv)) {
    # 首次使用時自動從原始 zip 解出，之後常駐 silver 層（staging 會被清空）
    z <- file.path(DIR_RAW, "2022", "escs_trend.zip")
    if (!file.exists(z)) stop("找不到 escs_trend.csv，也找不到 ", z)
    tmp <- tempfile(); dir.create(tmp)
    utils::unzip(z, exdir = tmp)
    src <- list.files(tmp, pattern = "escs_trend[.]csv$", recursive = TRUE, full.names = TRUE)[1]
    file.copy(src, csv); unlink(tmp, recursive = TRUE)
  }
  d <- fread(csv, na.strings = c("", ".", "NA"))
  d[, cycle_year := c(`5` = 2012L, `6` = 2015L, `7` = 2018L)[as.character(cycle)]]
  setnames(d, c("cnt", "schoolid", "studentid"), c("CNT", "SCHOOLID_T", "STUDENTID_T"))
  d[, .(CNT, cycle = cycle_year, SCHOOLID_T, STUDENTID_T,
        escs_trend, hisei_trend, homepos_trend, paredint_trend)]
}

harmonise_stu <- function(cycle) {
  f <- file.path(DIR_SILVER, sprintf("STU_%d.parquet", cycle))
  d <- as.data.table(arrow::read_parquet(f))

  # 性別 → 統一為 FEMALE（1 = 女）
  if ("ST004D01T" %in% names(d))      d[, FEMALE := as.integer(ST004D01T == 1)]
  else if ("ST04Q01" %in% names(d))   d[, FEMALE := as.integer(ST04Q01 == 1)]

  # 重複權重 → 統一為 W_FSTURWT*
  if (!any(grepl("^W_FSTURWT", names(d))) && any(grepl("^W_FSTR[0-9]", names(d)))) {
    old <- grep("^W_FSTR[0-9]+$", names(d), value = TRUE)
    setnames(d, old, sub("^W_FSTR", "W_FSTURWT", old))
  }

  d[, cycle := cycle]
  d <- patch_vnm_2018(d, cycle)
  d[]
}

# PISA 2018 的越南：OECD 未將其合理推估值放進主學生檔，另以 SPSS_VNM_PV_COG.zip
# 單獨發布（檔內 CY07_VNM_STU_PVS.sav 為完整的學生層級替代檔，含 PV 與 80 組重複權重）。
# 不做這一步，越南 2018 的所有估計都會是 NA。
patch_vnm_2018 <- function(d, cycle) {
  if (cycle != 2018) return(d)
  f <- file.path(DIR_SILVER, "VNM_2018.parquet")
  if (!file.exists(f)) {
    warning("找不到 VNM_2018.parquet，越南 2018 的估計將為 NA。",
            "請先 process_cycle(2018, <SPSS_VNM_PV_COG.zip>, \"VNM\", file_pattern = \"PVS\")",
            call. = FALSE)
    return(d)
  }
  v <- as.data.table(arrow::read_parquet(f))
  common <- intersect(names(d), names(v))
  d <- rbind(d[CNT != "VNM"], v[, ..common], fill = TRUE)
  log_msg("  以官方單獨發布檔補上越南 2018：", nrow(v), " 名學生")
  d[]
}

# 併入官方 trend ESCS
# ID 對應（已用臺灣 2015 資料 100% 比對驗證）：
#   CNTSCHID = CNTRYID × 100000 + schoolid
#   CNTSTUID = CNTRYID × 100000 + studentid
attach_escs_trend <- function(d, cycle) {
  if (cycle == 2022) {                      # 2022 的 ESCS 本身即為基準量尺
    d[, escs_cmp := ESCS]
    return(d[])
  }
  cy_target <- cycle
  et <- load_escs_trend()
  et <- et[et$cycle == cy_target]
  if (!nrow(et)) {
    warning("PISA ", cycle, " 不在 trend ESCS 涵蓋範圍（僅 2012/2015/2018），escs_cmp 設為 NA")
    d[, escs_cmp := NA_real_]
    return(d[])
  }
  d[, `:=`(SCHOOLID_T = CNTSCHID - CNTRYID * 100000L,
           STUDENTID_T = CNTSTUID - CNTRYID * 100000L)]
  d <- merge(d, et[, .(CNT, SCHOOLID_T, STUDENTID_T, escs_trend,
                       hisei_trend, homepos_trend, paredint_trend)],
             by = c("CNT", "SCHOOLID_T", "STUDENTID_T"), all.x = TRUE)
  d[, escs_cmp := escs_trend]
  matched <- sum(!is.na(d$escs_trend))
  log_msg("  trend ESCS 比對 ", matched, "/", nrow(d),
          " (", round(100 * matched / nrow(d), 1), "%)")
  d[]
}

# 建立可跨輪次分析的長表：每列 = 一位學生 × 一個領域
build_trend_table <- function(cycles = c(2015, 2018, 2022),
                              domains = c("MATH", "READ", "SCIE")) {
  out <- list()
  for (cy in cycles) {
    log_msg("對齊 ", cy)
    d <- harmonise_stu(cy)
    n_pv <- length(grep(sprintf("^PV[0-9]+%s$", domains[1]), names(d)))
    keep_base <- intersect(c("CNT", "CNTRYID", "CNTSCHID", "CNTSTUID", "OECD",
                             "cycle", "FEMALE", "ESCS", "W_FSTUWT", "SENWT"),
                           names(d))
    rw <- grep("^W_FSTURWT[0-9]+$", names(d), value = TRUE)
    for (dom in domains) {
      pvs <- sprintf("PV%d%s", seq_len(n_pv), dom)
      if (!all(pvs %in% names(d))) { log_msg("  略過 ", dom, "（此輪無資料）"); next }
      sub <- d[, c(keep_base, rw, pvs), with = FALSE]
      sub[, domain := dom]
      setnames(sub, pvs, sprintf("PV%d", seq_len(n_pv)))
      out[[paste0(cy, dom)]] <- sub
    }
    rm(d); gc()
  }
  rbindlist(out, fill = TRUE)
}


# ---- 併入選考領域的認知檔（創造思考、財金素養）---------------------------
# PISA 2022 的 CRT（創造思考）與 FLT（財金素養）是獨立的認知試題檔，
# 內含合理推估值但**不含權重**。權重在主學生檔，必須以 CNTSTUID 併回，
# 否則無法做任何加權估計。
#
#   CRT 檔的 PV：PV1CRTH_NC..PV10CRTH_NC（創造思考，Number Correct 量尺）
#                PV1MATC / PV1REAC / PV1SCIC（在創造思考子樣本上重算的三科）
#   FLT 檔的 PV：PV1FLIT..PV10FLIT
#
# 注意：選考領域只有部分國家參加，且只有部分學生受測，
# 併檔後樣本數會少於主檔，這是設計使然，不是資料遺失。
attach_optional_domain <- function(d, cycle, domain_file) {
  f <- file.path(DIR_SILVER, sprintf("%s_%d.parquet", domain_file, cycle))
  if (!file.exists(f)) stop("找不到 ", basename(f),
                            "；請先以 process_cycle(", cycle,
                            ", <zip>, \"", domain_file, "\") 轉檔")
  x <- as.data.table(arrow::read_parquet(f))
  pv_cols <- grep("^PV[0-9]+", names(x), value = TRUE)
  if (!length(pv_cols)) stop(domain_file, " 檔中找不到合理推估值")
  x <- x[, c("CNTSTUID", pv_cols), with = FALSE]
  n0 <- nrow(d)
  d <- merge(d, x, by = "CNTSTUID", all.x = FALSE)   # 只保留有受測的學生
  log_msg("  併入 ", domain_file, "：", nrow(d), "/", n0,
          " 名學生有 ", domain_file, " 分數 (",
          round(100 * nrow(d) / n0, 1), "%)")
  d[]
}
