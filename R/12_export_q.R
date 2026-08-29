# ============================================================
# 12_export_q.R — 把問卷分析結果輸出成網站用的 JSON
#
# 只輸出彙總統計量（各國加權平均、標準誤、名次、梯度係數），
# 不含任何個體資料。原始資料依 IEA 與 OECD 的授權條款不得再散布。
# ============================================================
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
source("~/PISA/R/iea/05_analysis_q.R")

export_q <- function() {
  QT <- readRDS(path.expand("~/TIMSS/output/q_timss.rds"))
  QP <- readRDS(path.expand("~/PIRLS/output/q_pirls.rds"))
  QI <- readRDS(path.expand("~/ICCS/output/q_iccs.rds"))

  sc <- function(x, lab = zh_of) {
    if (is.null(x) || !nrow(x)) return(data.table())
    x[is.finite(estimate), .(CNT, idx, zh = lab(idx), mean = round(estimate, 3),
                             se = round(se, 4), rank, ntot, n)]
  }
  gr <- function(x, lab = zh_of) {
    if (is.null(x) || !nrow(x)) return(data.table())
    x[is.finite(beta), .(CNT, idx, zh = lab(idx), beta = round(beta, 3),
                         se = round(se, 4), n)]
  }
  iv <- function(x, lab = zh_of) {
    if (is.null(x) || !nrow(x)) return(data.table())
    x[, .(v, zh = lab(v), range = round(range, 4), distinct, n_country)]
  }
  ic <- function(x) {
    if (is.null(x) || !nrow(x)) return(data.table())
    x[is.finite(icc), .(CNT, icc = round(icc, 4), se = round(se, 4),
                        total = round(total, 1), between = round(between, 1))]
  }

  # ICCS 的國內置中剖面
  a <- QI$stu_scales[idx != "S_NISB" & is.finite(estimate)]
  cw <- a[, .(cmean = round(mean(estimate), 3)), by = CNT][order(-cmean)]
  a <- merge(a, cw, by = "CNT"); a[, ips := estimate - cmean]
  a[, `:=`(rank_raw = frank(-estimate, ties.method = "min"),
           rank_ips = frank(-ips,      ties.method = "min")), by = idx]

  K <- dcast(QI$know_trend[, .(CNT, cycle, estimate, se)], CNT ~ cycle,
             value.var = c("estimate","se"))
  K <- K[!is.na(estimate_2016) & !is.na(estimate_2022)]
  K[, `:=`(diff = round(estimate_2022 - estimate_2016, 2),
           sed  = round(sqrt(se_2016^2 + se_2022^2), 3))]
  K[, sig := abs(diff) > 1.96 * sed]

  out <- list(
    meta = list(
      timss = list(g8_country = uniqueN(QT$g8_scales$CNT), g4_country = uniqueN(QT$g4_scales$CNT),
                   g8_teacher = sum(QT$g8_tch_n$n_teacher)),
      pirls = list(country = uniqueN(QP$stu_scales$CNT),
                   teacher = if (!is.null(QP$tch_n)) sum(QP$tch_n$n_teacher) else NA,
                   home_resp_tw = round(QP$home_cov[CNT=="TWN"]$resp, 3),
                   home_resp_med = round(median(QP$home_cov$resp), 3)),
      iccs  = list(country = uniqueN(QI$stu_scales$CNT), n_scale = uniqueN(a$idx),
                   teacher = sum(QI$tch_n$n_teacher), trend_country = nrow(K),
                   trend_common = length(QI$common_scales))),
    timss = list(
      g8 = list(scales = sc(QT$g8_scales), inv = iv(QT$g8_inv), grad = gr(QT$g8_grad),
                icc = ic(QT$g8_icc), sch = sc(QT$g8_sch_scales), tch = sc(QT$g8_tch_scales)),
      g4 = list(scales = sc(QT$g4_scales), inv = iv(QT$g4_inv), grad = gr(QT$g4_grad),
                icc = ic(QT$g4_icc))),
    pirls = list(stu = sc(QP$stu_scales), home = sc(QP$home_scales),
                 sch = sc(QP$sch_scales), tch = sc(QP$tch_scales),
                 stu_grad = gr(QP$stu_grad), home_grad = gr(QP$home_grad),
                 inv = rbind(iv(QP$stu_inv), iv(QP$home_inv), iv(QP$sch_inv), iv(QP$tch_inv)),
                 icc = ic(QP$icc)),
    iccs = list(
      stu = sc(QI$stu_scales, zh_iccs), tch = sc(QI$tch_scales, zh_iccs),
      sch = sc(QI$sch_scales, zh_iccs), grad = gr(QI$stu_grad, zh_iccs),
      inv = iv(QI$stu_inv, zh_iccs), icc = ic(QI$icc),
      resp = cw,
      ips = a[, .(CNT, idx, zh = zh_iccs(idx), mean = round(estimate,2),
                  ips = round(ips,2), rank_raw, rank_ips)],
      trend_know = K[, .(CNT, y16 = round(estimate_2016,1), y22 = round(estimate_2022,1),
                         diff, sed, sig)],
      trend_att = dcast(QI$att_trend[, .(CNT, idx, cycle, estimate)], CNT + idx ~ cycle,
                        value.var = "estimate")[
        , .(CNT, idx, zh = zh_iccs(idx), y16 = round(`2016`,2), y22 = round(`2022`,2))][
        !is.na(y16) & !is.na(y22)]))

  p <- path.expand("~/PISA/web/q_data.json")
  write_json(out, p, auto_unbox = TRUE, digits = 6, na = "null")
  cat("寫出 q_data.json ", round(file.info(p)$size/1024, 1), " KB\n", sep = "")
  invisible(out)
}

# ---- TALIS 2024 AI 模組 -----------------------------------------------------
export_ai <- function() {
  A <- readRDS(path.expand("~/TALIS/output/ai_2024.rds"))
  tidy <- function(x) {
    if (is.null(x) || !nrow(x)) return(data.table())
    x[is.finite(estimate), .(CNT = CNTRY, idx, zh, grp,
                             pct = round(100 * estimate, 2), se = round(100 * se, 3),
                             rank, ntot, n)]
  }
  B <- A$belief
  pos <- B[grp == "正向信念", .(pos = round(100 * mean(estimate), 2)), by = CNTRY]
  neg <- B[grp == "風險認知", .(neg = round(100 * mean(estimate), 2)), by = CNTRY]
  P <- dcast(A$pd[is.finite(estimate)], CNTRY ~ idx, value.var = c("estimate", "se"))
  setnames(P, c("estimate_TT4G21G","estimate_TT4G24G","se_TT4G21G","se_TT4G24G"),
              c("got","need","got_se","need_se"))

  out <- list(
    meta = c(A$meta, list(cycle = 2024L)),
    usage  = tidy(A$usage),  use = tidy(A$use),   why = tidy(A$why),
    belief = tidy(A$belief), dontknow = tidy(A$dontknow), pd = tidy(A$pd),
    summary = merge(merge(pos, neg, by = "CNTRY", all = TRUE),
                    A$usage[, .(CNTRY, use = round(100 * estimate, 2))], by = "CNTRY", all = TRUE),
    pdgap = P[, .(CNT = CNTRY, got = round(100*got,2), need = round(100*need,2),
                  gap = round(100*(need - got), 2))][order(-gap)])
  p <- path.expand("~/PISA/web/ai_data.json")
  write_json(out, p, auto_unbox = TRUE, digits = 6, na = "null")
  cat("寫出 ai_data.json ", round(file.info(p)$size/1024, 1), " KB\n", sep = "")
  invisible(out)
}
