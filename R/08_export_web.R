# ============================================================
# 08_export_web.R — 產出網站用的 JSON
# 全部 80 個國家 × 3 輪 × 3 領域，估計方法與終端機分析完全相同
# ============================================================
source("~/PISA/R/00_config.R"); source("~/PISA/R/lib_pisa.R")
source("~/PISA/R/03_harmonise.R"); source("~/PISA/R/05_analysis.R")
source("~/PISA/R/06_trend.R")
suppressPackageStartupMessages({library(data.table); library(arrow); library(jsonlite)})

CYCLES <- c(2015, 2018, 2022); DOMAINS <- c("MATH","READ","SCIE")
DIR_WEB <- file.path(PISA_ROOT, "web"); dir.create(DIR_WEB, showWarnings = FALSE)

# ---- 國名對照：從三輪原始 sav 的數值標籤取聯集 ---------------------------
# 只讀 2022 會漏掉僅參加 2015／2018 的國家與次國家地區（共 19 個）。
get_country_names <- function(force = FALSE) {
  f <- file.path(DIR_WEB, "country_names.csv")
  if (file.exists(f) && !force) return(fread(f))
  zips <- list(
    c(2022, file.path(DIR_RAW, "2022", "STU_QQQ_SPSS.zip")),
    c(2018, file.path(DIR_RAW, "2018", "SPSS_STU_QQQ.zip")),
    c(2015, file.path(DIR_RAW, "2015", "PUF_SPSS_COMBINED_CMB_STU_QQQ.zip"))
  )
  out <- list()
  for (z in zips) {
    stage <- file.path(DIR_STAGE, paste0("cnt", z[1]))
    unlink(stage, recursive = TRUE); dir.create(stage, recursive = TRUE, showWarnings = FALSE)
    system2("unzip", c("-o","-q", shQuote(z[2]), "-d", shQuote(stage)))
    sav <- list.files(stage, pattern="[.]sav$", ignore.case=TRUE, recursive=TRUE, full.names=TRUE)[1]
    h <- haven::read_sav(sav, col_select = c("CNT","CNTRYID"), n_max = 0)
    lb <- attr(h$CNTRYID, "labels")
    out[[as.character(z[1])]] <- data.table(CNTRYID = as.integer(lb), name = names(lb))
    unlink(stage, recursive = TRUE)
    log_msg("  ", z[1], " 取得 ", length(lb), " 個標籤")
  }
  res <- unique(rbindlist(out), by = "CNTRYID")   # 較新的輪次優先
  setorder(res, CNTRYID)
  fwrite(res, f); res
}

log_msg("=== 國名對照 ===")
cn <- get_country_names(force = TRUE)
log_msg("  ", nrow(cn), " 個國家／經濟體")

# ---- 平均分數 -------------------------------------------------------------
log_msg("=== 平均分數（80 國 × 3 輪 × 3 領域）===")
means <- rbindlist(lapply(CYCLES, function(cy) {
  d <- load_stu(cy)
  rbindlist(lapply(DOMAINS, function(dm) {
    log_msg("  ", cy, " ", dm)
    r <- pisa_pv_by(d, pv_names(dm, 10), by = c("CNT","CNTRYID","OECD"), FUN = wmean)
    r[, `:=`(cycle = cy, domain = dm)]
    r[, .(CNT, CNTRYID, OECD, cycle, domain,
          mean = round(estimate,2), se = round(se,3), n)]
  }))
}))
log_msg("  ", nrow(means), " 列")

# ---- 標準差（分數離散程度）------------------------------------------------
log_msg("=== 分數離散程度 ===")
sds <- rbindlist(lapply(CYCLES, function(cy) {
  d <- load_stu(cy)
  rbindlist(lapply(DOMAINS, function(dm) {
    log_msg("  ", cy, " ", dm)
    r <- pisa_pv_by(d, pv_names(dm, 10), by = "CNT", FUN = wsd)
    r[, `:=`(cycle = cy, domain = dm)]
    r[, .(CNT, cycle, domain, sd = round(estimate,2), se_sd = round(se,3))]
  }))
}))

# ---- 精熟等級 -------------------------------------------------------------
log_msg("=== 精熟等級 ===")
prof <- rbindlist(lapply(CYCLES, function(cy) {
  d <- load_stu(cy)
  rbindlist(lapply(DOMAINS, function(dm) {
    log_msg("  ", cy, " ", dm)
    cuts <- PISA_CUTS[[dm]]; pvs <- pv_names(dm, 10)
    lo <- pisa_pv_by(d, pvs, by="CNT", FUN=function(x,w) 100 - wpct_above(x,w,cuts[["2"]]))
    hi <- pisa_pv_by(d, pvs, by="CNT", FUN=wpct_above, cut=cuts[["5"]])
    merge(lo[, .(CNT, below_L2 = round(estimate,2), se_low = round(se,3))],
          hi[, .(CNT, above_L5 = round(estimate,2), se_high = round(se,3))],
          by="CNT")[, `:=`(cycle = cy, domain = dm)][]
  }))
}))

# ---- 性別差距 -------------------------------------------------------------
log_msg("=== 性別差距 ===")
gg <- rbindlist(lapply(CYCLES, function(cy) {
  d <- load_stu(cy)[FEMALE %in% c(0,1)]
  rbindlist(lapply(DOMAINS, function(dm) {
    log_msg("  ", cy, " ", dm)
    pvs <- pv_names(dm, 10)
    m <- pisa_pv_by(d[FEMALE==0], pvs, by="CNT", FUN=wmean)
    f <- pisa_pv_by(d[FEMALE==1], pvs, by="CNT", FUN=wmean)
    r <- merge(m[, .(CNT, male=estimate, se_m=se)], f[, .(CNT, female=estimate, se_f=se)], by="CNT")
    r[, `:=`(gap = female - male, se_gap = sqrt(se_m^2 + se_f^2), cycle = cy, domain = dm)]
    r[, .(CNT, cycle, domain, male = round(male,1), female = round(female,1),
          gap = round(gap,1), se_gap = round(se_gap,3),
          p = round(2*pnorm(-abs(gap/se_gap)), 4))]
  }))
}))

# ---- 社經梯度 -------------------------------------------------------------
log_msg("=== 社經梯度（trend ESCS）===")
esc <- rbindlist(lapply(CYCLES, function(cy) {
  d <- attach_escs_trend(load_stu(cy), cy)
  rbindlist(lapply(DOMAINS, function(dm) {
    log_msg("  ", cy, " ", dm)
    rbindlist(lapply(unique(d$CNT), function(cnt) {
      sub <- d[CNT == cnt & is.finite(escs_cmp)]
      if (nrow(sub) < 200) return(NULL)
      r <- tryCatch(pisa_pv_lm(sub, PV_ ~ escs_cmp, domain = dm, n_pv = 10),
                    error = function(e) NULL)
      if (is.null(r)) return(NULL)
      x <- r[term == "escs_cmp"]
      data.table(CNT = cnt, cycle = cy, domain = dm,
                 slope = round(x$estimate,2), se_slope = round(x$se,3))
    }))
  }))
}))

# ---- 跨輪比較（含連結誤差）------------------------------------------------
log_msg("=== 跨輪比較 ===")
cmp <- rbindlist(lapply(DOMAINS, function(dm)
  rbindlist(lapply(c(2015, 2018), function(fr) {
    le <- get_link_error(fr, 2022, dm)
    a <- means[cycle==fr & domain==dm, .(CNT, m1=mean, se1=se)]
    b <- means[cycle==2022 & domain==dm, .(CNT, m2=mean, se2=se)]
    m <- merge(a, b, by="CNT")
    m[, `:=`(domain=dm, from=fr, link_error=le, diff=round(m2-m1,2),
             se_naive=round(sqrt(se1^2+se2^2),3),
             se_correct=round(sqrt(se1^2+se2^2+le^2),3))]
    m[, `:=`(p_naive=round(2*pnorm(-abs((m2-m1)/sqrt(se1^2+se2^2))),4),
             p_correct=round(2*pnorm(-abs((m2-m1)/sqrt(se1^2+se2^2+le^2))),4))]
    m[, .(CNT, domain, from, m1, m2, diff, link_error, se_naive, p_naive, se_correct, p_correct)]
  }))))

# ---- 創造思考（2022）------------------------------------------------------
log_msg("=== 創造思考 2022 ===")
crt <- tryCatch({
  d <- attach_optional_domain(load_stu(2022), 2022, "CRT")
  r <- pisa_pv_by(d, paste0("PV",1:10,"CRTH_NC"), by="CNT", FUN=wmean)
  r[, .(CNT, crt = round(estimate,2), se_crt = round(se,3), n)]
}, error = function(e) { log_msg("  略過：", conditionMessage(e)); NULL })

# ---- 輸出 -----------------------------------------------------------------
payload <- list(
  meta = list(
    generated  = format(Sys.time(), "%Y-%m-%d %H:%M"),
    cycles     = CYCLES,
    domains    = DOMAINS,
    nStudents  = 1745082L,
    nCountries = length(unique(means$CNT)),
    source     = "OECD PISA Public Use Files (webfs.oecd.org)",
    linkErrorSource = "PISA 2022 Technical Report, Annex Table 14.A.19"
  ),
  countryNames = cn,
  linkErrors   = PISA_LINK_ERROR,
  cuts         = PISA_CUTS,
  means = means, sds = sds, prof = prof,
  gender = gg, escs = esc, trend = cmp, crt = crt
)
out <- file.path(DIR_WEB, "pisa_data.json")
write_json(payload, out, auto_unbox = TRUE, digits = 4, na = "null")
log_msg("寫出 ", out, "  ", round(file.info(out)$size/2^20, 2), " MB")
