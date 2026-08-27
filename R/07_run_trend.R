# ============================================================
# 07_run_trend.R — 產出趨勢分析結果
# 預設焦點：臺灣（CNT = "TAP"），並與四個東亞經濟體及 OECD 平均對照
# ============================================================
source("~/PISA/R/00_config.R")
source("~/PISA/R/lib_pisa.R")
source("~/PISA/R/03_harmonise.R")
source("~/PISA/R/05_analysis.R")
source("~/PISA/R/06_trend.R")
suppressPackageStartupMessages({library(data.table); library(arrow)})

FOCUS   <- c("TAP", "JPN", "KOR", "SGP", "HKG", "MAC", "FIN", "EST", "USA")
CYCLES  <- c(2015, 2018, 2022)
DOMAINS <- c("MATH", "READ", "SCIE")

log_msg("=== 步驟 1：各輪各國平均（PV × BRR）===")
means <- rbindlist(lapply(CYCLES, function(cy)
  rbindlist(lapply(DOMAINS, function(dm) {
    log_msg("  ", cy, " ", dm)
    d <- load_stu(cy, FOCUS)
    r <- pisa_pv_by(d, pv_names(dm, 10), by = "CNT", FUN = wmean)
    r[, `:=`(cycle = cy, domain = dm)]
    r[, .(CNT, cycle, domain, mean = estimate, se, n)]
  }))))
fwrite(means, file.path(DIR_OUT, "trend_means.csv"))
log_msg("寫出 trend_means.csv  ", nrow(means), " 列")

log_msg("=== 步驟 2：跨輪比較（含連結誤差）===")
cmp <- rbindlist(lapply(DOMAINS, function(dm)
  rbindlist(lapply(c(2015, 2018), function(fr) {
    le <- get_link_error(fr, 2022, dm)
    a <- means[cycle == fr   & domain == dm, .(CNT, m1 = mean, se1 = se)]
    b <- means[cycle == 2022 & domain == dm, .(CNT, m2 = mean, se2 = se)]
    m <- merge(a, b, by = "CNT")
    m[, `:=`(domain = dm, from = fr, to = 2022, link_error = le,
             diff = m2 - m1,
             se_correct = sqrt(se1^2 + se2^2 + le^2),
             se_naive   = sqrt(se1^2 + se2^2))]
    m[, `:=`(p_correct = 2 * pnorm(-abs(diff / se_correct)),
             p_naive   = 2 * pnorm(-abs(diff / se_naive)))]
    m[, .(CNT, domain, from, to, m1 = round(m1,1), m2 = round(m2,1),
          diff = round(diff,1), link_error,
          se_naive = round(se_naive,2),   p_naive = round(p_naive,4),
          se_correct = round(se_correct,2), p_correct = round(p_correct,4),
          結論差異 = fifelse((p_naive < .05) != (p_correct < .05),
                             "★ 忽略連結誤差會下錯結論", ""))]
  }))))
fwrite(cmp, file.path(DIR_OUT, "trend_comparison.csv"))
log_msg("寫出 trend_comparison.csv  ", nrow(cmp), " 列")

log_msg("=== 步驟 3：低成就與高成就比率 ===")
prof <- rbindlist(lapply(CYCLES, function(cy)
  rbindlist(lapply(DOMAINS, function(dm) {
    d <- load_stu(cy, FOCUS); cuts <- PISA_CUTS[[dm]]; pvs <- pv_names(dm, 10)
    lo <- pisa_pv_by(d, pvs, by = "CNT", FUN = function(x, w) 100 - wpct_above(x, w, cuts[["2"]]))
    hi <- pisa_pv_by(d, pvs, by = "CNT", FUN = wpct_above, cut = cuts[["5"]])
    merge(lo[, .(CNT, below_L2 = round(estimate,2), se_below = round(se,2))],
          hi[, .(CNT, above_L5 = round(estimate,2), se_above = round(se,2))],
          by = "CNT")[, `:=`(cycle = cy, domain = dm)][]
  }))))
fwrite(prof, file.path(DIR_OUT, "proficiency.csv"))
log_msg("寫出 proficiency.csv  ", nrow(prof), " 列")

log_msg("完成。結果在 ", DIR_OUT)
