# ============================================================
# 06_trend.R — 跨輪次趨勢分析
#
# 【方法學前提，務必先讀】
# PISA 是重複橫斷（repeated cross-sectional）設計，不是追蹤（panel）設計。
# 每一輪抽的是不同的 15 歲學生，沒有任何學生被重複測量。因此：
#   ✔ 可以做：國家／群體層次的趨勢、世代間差異、跨輪次的關係強度變化
#   ✘ 不能做：個人成長軌跡、潛在成長模型（LGM）、交叉延宕（CLPM）、
#             個體層次的固定效果模型
# 若研究問題必須談個人成長，PISA 本身做不到，需改用具追蹤設計的資料
# （如 TEPS、TEPS-B，或 PISA 的後續追蹤研究 PISA-L / YAFS）。
#
# 跨輪比較的標準誤必須納入連結誤差（link error），否則型一錯誤率被低估。
# 連結誤差來源：PISA 2022 Technical Report, Annex Table 14.A.19
# ============================================================
source("~/PISA/R/00_config.R")
source("~/PISA/R/lib_pisa.R")
source("~/PISA/R/03_harmonise.R")
suppressPackageStartupMessages({library(data.table); library(arrow)})

# ---- 各輪各國平均分數（PV × BRR）------------------------------------------
cycle_means <- function(cycle, domain, countries = NULL, n_pv = 10) {
  d <- harmonise_stu(cycle)
  if (!is.null(countries)) d <- d[CNT %in% countries]
  pvs <- pv_names(domain, n_pv)
  if (!all(pvs %in% names(d))) stop(domain, " 在 PISA ", cycle, " 找不到合理推估值")
  res <- pisa_pv_by(d, pvs, by = "CNT", FUN = wmean)
  res[, `:=`(cycle = cycle, domain = toupper(domain))]
  res[, .(CNT, cycle, domain, mean = estimate, se, n,
          var_sampling, var_imputation)]
}

# ---- 兩輪次比較（含連結誤差）----------------------------------------------
compare_cycles <- function(tab, from_cycle, to_cycle = 2022, domain) {
  le <- get_link_error(from_cycle, to_cycle, domain)
  a <- tab[cycle == from_cycle & domain == toupper(get("domain"))]
  b <- tab[cycle == to_cycle   & domain == toupper(get("domain"))]
  m <- merge(a[, .(CNT, m1 = mean, se1 = se)],
             b[, .(CNT, m2 = mean, se2 = se)], by = "CNT")
  m[, `:=`(diff = m2 - m1,
           se_diff = sqrt(se1^2 + se2^2 + le^2))]
  m[, `:=`(t = diff / se_diff,
           p = 2 * pnorm(-abs(diff / se_diff)),
           ci_lo = diff - 1.96 * se_diff,
           ci_hi = diff + 1.96 * se_diff,
           sig = fifelse(2 * pnorm(-abs(diff / se_diff)) < .05, "顯著", "不顯著"),
           link_error = le, from = from_cycle, to = to_cycle,
           domain = toupper(domain))]
  m[order(diff)]
}

# ---- 對照：忽略連結誤差會如何低估不確定性 --------------------------------
compare_cycles_naive <- function(tab, from_cycle, to_cycle = 2022, domain) {
  r <- compare_cycles(tab, from_cycle, to_cycle, domain)
  r[, se_naive := sqrt(se1^2 + se2^2)]
  r[, p_naive := 2 * pnorm(-abs(diff / se_naive))]
  r[, .(CNT, diff,
        se_naive, p_naive, sig_naive = fifelse(p_naive < .05, "顯著", "不顯著"),
        se_correct = se_diff, p_correct = p, sig_correct = sig,
        se_inflation = round(se_diff / se_naive, 3))]
}

# ---- 社經梯度（ESCS 對成績的迴歸斜率）跨輪比較 ---------------------------
# 使用官方 trend ESCS，僅 2012–2022 可比
escs_gradient <- function(cycle, domain, countries = NULL, n_pv = 10,
                          use_trend_escs = TRUE) {
  d <- harmonise_stu(cycle)
  if (!is.null(countries)) d <- d[CNT %in% countries]
  escs_var <- "ESCS"
  if (use_trend_escs) {
    d <- attach_escs_trend(d, cycle)
    escs_var <- "escs_cmp"
    if (all(is.na(d[[escs_var]])))
      stop("PISA ", cycle, " 無可比 ESCS（trend ESCS 僅涵蓋 2012/2015/2018）")
  }
  d <- d[is.finite(get(escs_var))]
  f <- as.formula(paste("PV_ ~", escs_var))
  res <- pisa_pv_lm(d, f, domain = domain, n_pv = n_pv)
  res[, `:=`(cycle = cycle, domain = toupper(domain),
             escs_source = escs_var)]
  res
}
