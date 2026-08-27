# ============================================================
# 05_analysis.R — 常用分析
#
# 所有估計都經過 PV × BRR 處理。函式庫已與 intsvy 對照驗證：
# 臺灣 PISA 2015 三領域的平均數與標準誤完全一致
# （MATH 542.32/3.03、READ 497.10/2.50、SCIE 532.35/2.69）。
# ============================================================
source("~/PISA/R/00_config.R")
source("~/PISA/R/lib_pisa.R")
source("~/PISA/R/03_harmonise.R")
suppressPackageStartupMessages({library(data.table); library(arrow)})

load_stu <- function(cycle, countries = NULL) {
  d <- harmonise_stu(cycle)
  if (!is.null(countries)) d <- d[CNT %in% countries]
  d[]
}

# ---- 各國平均分數 ---------------------------------------------------------
country_means <- function(cycle, domain, countries = NULL,
                          n_pv = 10, var_method = "average", verbose = TRUE) {
  d <- load_stu(cycle, countries)
  r <- pisa_pv_by(d, pv_names(domain, n_pv), by = "CNT",
                  FUN = wmean, var_method = var_method, verbose = verbose)
  r[, `:=`(cycle = cycle, domain = toupper(domain))]
  r[order(-estimate), .(CNT, cycle, domain, mean = round(estimate, 2),
                        se = round(se, 2), n)]
}

# ---- 分數分散程度（標準差）------------------------------------------------
country_sd <- function(cycle, domain, countries = NULL, n_pv = 10) {
  d <- load_stu(cycle, countries)
  r <- pisa_pv_by(d, pv_names(domain, n_pv), by = "CNT", FUN = wsd)
  r[, `:=`(cycle = cycle, domain = toupper(domain))]
  r[, .(CNT, cycle, domain, sd = round(estimate, 2), se = round(se, 2), n)]
}

# ---- 精熟等級分布 ---------------------------------------------------------
# 回傳各等級的人數比率（%），含 PV × BRR 標準誤
proficiency_dist <- function(cycle, domain, countries = NULL, n_pv = 10) {
  d <- load_stu(cycle, countries)
  cuts <- PISA_CUTS[[toupper(domain)]]
  pvs  <- pv_names(domain, n_pv)
  out  <- list()
  for (lv in names(cuts)) {
    r <- pisa_pv_by(d, pvs, by = "CNT", FUN = wpct_above, cut = cuts[[lv]])
    r[, `:=`(level = lv, cut = cuts[[lv]])]
    out[[lv]] <- r
  }
  res <- rbindlist(out)
  res[, `:=`(cycle = cycle, domain = toupper(domain))]
  res[, .(CNT, cycle, domain, level, cut,
          pct_at_or_above = round(estimate, 2), se = round(se, 2))]
}

# 低成就（未達 Level 2）與高成就（Level 5 以上）比率
low_high_performers <- function(cycle, domain, countries = NULL, n_pv = 10) {
  d <- load_stu(cycle, countries)
  cuts <- PISA_CUTS[[toupper(domain)]]
  pvs  <- pv_names(domain, n_pv)
  lo <- pisa_pv_by(d, pvs, by = "CNT", FUN = function(x, w)
    100 - wpct_above(x, w, cuts[["2"]]))
  hi <- pisa_pv_by(d, pvs, by = "CNT", FUN = wpct_above, cut = cuts[["5"]])
  merge(lo[, .(CNT, below_L2 = round(estimate, 2), se_below = round(se, 2))],
        hi[, .(CNT, at_or_above_L5 = round(estimate, 2), se_above = round(se, 2))],
        by = "CNT")[order(-below_L2)]
}

# ---- 社經梯度 -------------------------------------------------------------
# ESCS 每增加一個標準差，分數平均變動多少（社經梯度斜率）
escs_slope <- function(cycle, domain, countries = NULL, n_pv = 10) {
  d <- load_stu(cycle, countries)
  out <- list()
  for (cn in unique(d$CNT)) {
    sub <- d[CNT == cn & is.finite(ESCS)]
    if (nrow(sub) < 100) next
    r <- tryCatch(pisa_pv_lm(sub, PV_ ~ ESCS, domain = domain, n_pv = n_pv),
                  error = function(e) NULL)
    if (is.null(r)) next
    out[[cn]] <- cbind(CNT = cn, r[term == "ESCS"])
  }
  res <- rbindlist(out)
  res[, `:=`(cycle = cycle, domain = toupper(domain))]
  res[order(-estimate), .(CNT, cycle, domain,
                          slope = round(estimate, 2), se = round(se, 2),
                          t = round(t, 2))]
}

# ---- 性別差距 -------------------------------------------------------------
gender_gap <- function(cycle, domain, countries = NULL, n_pv = 10) {
  d <- load_stu(cycle, countries)
  d <- d[FEMALE %in% c(0, 1)]
  m <- pisa_pv_by(d[FEMALE == 0], pv_names(domain, n_pv), by = "CNT", FUN = wmean)
  f <- pisa_pv_by(d[FEMALE == 1], pv_names(domain, n_pv), by = "CNT", FUN = wmean)
  r <- merge(m[, .(CNT, male = estimate, se_m = se)],
             f[, .(CNT, female = estimate, se_f = se)], by = "CNT")
  # 兩組獨立，差異標準誤取平方和開根號（同一輪次內不需連結誤差）
  r[, `:=`(gap_f_minus_m = female - male,
           se_gap = sqrt(se_m^2 + se_f^2))]
  r[, `:=`(t = gap_f_minus_m / se_gap,
           p = 2 * pnorm(-abs(gap_f_minus_m / se_gap)),
           cycle = cycle, domain = toupper(domain))]
  r[order(gap_f_minus_m),
    .(CNT, cycle, domain, male = round(male, 1), female = round(female, 1),
      gap = round(gap_f_minus_m, 1), se = round(se_gap, 2), p = round(p, 4))]
}
