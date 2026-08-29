# ============================================================
# 14_figure_rankorder.R — 各參與單位內部的用途排序比較
#
# 這是「研究問題二的後半」：不只看各體制用得多少，而是看各體制內部
# 九項用途的先後順序是否一致。統計量為 Kendall 和諧係數 W，
# 圖則直接呈現每一題的名次分布，讓讀者看見集中程度而非只看一個係數。
#
# 分析基礎為 53 個不重複計算的參與單位（排除比利時的兩個語言社群，
# 其樣本與比利時整體重疊）。
# ============================================================
source("~/PISA/R/09_figures.R")
suppressPackageStartupMessages({library(ggplot2); library(data.table); library(jsonlite)})
BASE_FAMILY <- setup_fonts()
DUP <- c("BFL","BFR")

rank_data <- function() {
  A <- readRDS(path.expand("~/TALIS/output/ai_2024.rds"))
  U <- A$use[!CNTRY %in% DUP & is.finite(estimate)]
  U[, rk := frank(-estimate, ties.method = "average"), by = CNTRY]
  U[, grp := fifelse(idx %in% c("TT4G37B","TT4G37C"), "備課",
             fifelse(idx %in% c("TT4G37A","TT4G37G"), "評量", "其他"))]
  U[]
}

kendall_w <- function(U, drop_other = FALSE) {
  d <- if (drop_other) U[idx != "TT4G37I"] else U
  d <- copy(d)[, rk := frank(-estimate, ties.method = "average"), by = CNTRY]
  M <- as.matrix(dcast(d, idx ~ CNTRY, value.var = "rk")[, -1])
  n <- nrow(M); m <- ncol(M); Rj <- rowSums(M)
  W <- 12 * sum((Rj - mean(Rj))^2) / (m^2 * (n^3 - n))
  chi <- m * (n - 1) * W
  list(W = W, chi = chi, df = n - 1, p = pchisq(chi, n - 1, lower.tail = FALSE), m = m, n = n)
}

build_rankorder_fig <- function() {
  U <- rank_data(); K <- kendall_w(U)
  ord <- U[, .(med = median(rk)), by = .(idx, zh, grp)][order(med)]
  # 名次分布：每一題 × 每一個名次的單位數
  H <- U[, .N, by = .(idx, zh, rk)]
  H[, zh := factor(zh, levels = rev(ord$zh))]
  gcol <- setNames(ord$grp, ord$zh)

  save_fig("ai_rankorder", function(mode) {
    p <- PAL[[mode]]
    lv <- levels(H$zh)
    axis_cols <- ifelse(gcol[lv] == "備課", p$series[3],
                 ifelse(gcol[lv] == "評量", p$series[2], p$ink2))
    ggplot(H, aes(x = rk, y = zh)) +
      geom_tile(aes(fill = N), colour = p$surface, linewidth = .8) +
      geom_text(aes(label = N), family = BASE_FAMILY, size = 2.7,
                colour = ifelse(H$N > 18, p$surface, p$ink2)) +
      scale_fill_gradient(low = p$grid, high = p$accent, guide = "none") +
      scale_x_continuous(breaks = 1:9, position = "top",
                         expand = expansion(add = .5)) +
      labs(title = "九項用途在各參與單位內部的名次分布",
           subtitle = sprintf("格內數字為落在該名次的參與單位數（共 %d 個）。Kendall 和諧係數 W = %.3f，χ²(%d) = %.1f，p < .001",
                              K$m, K$W, K$df, K$chi),
           x = "該單位內部的名次（1 = 該單位比率最高者）", y = NULL,
           caption = "綠色標籤為備課類，橘色為評量類。備課類集中於左側、評量類集中於右側，代表各體制不只用得多寡不同，先後順序也大致相同") +
      theme_pisa(mode, grid = "none") +
      theme(axis.text.y = element_text(colour = axis_cols, size = 8.2),
            panel.grid = element_blank())
  }, w = 8.6, h = 4.4)
  invisible(list(K = K, ord = ord))
}

# 供正文與表格引用的統計量
rankorder_stats <- function() {
  U <- rank_data()
  R <- dcast(U, CNTRY ~ idx, value.var = "rk")
  R[, `:=`(pb = pmin(TT4G37B, TT4G37C), ab = pmin(TT4G37A, TT4G37G),
           pw = pmax(TT4G37B, TT4G37C), aw = pmax(TT4G37A, TT4G37G))]
  med <- U[, .(m = median(rk)), by = idx]
  sp <- U[, .(rho = cor(rk, med$m[match(idx, med$idx)], method = "spearman")), by = CNTRY]
  list(K = kendall_w(U), K8 = kendall_w(U, drop_other = TRUE),
       n_unit = uniqueN(U$CNTRY),
       prep_top3 = R[pw <= 3, .N], asse_bot3 = R[ab >= 7, .N],
       best_order = R[pb < ab, .N], all_order = R[pw < ab, .N],
       practice = R[TT4G37H < ab, .N],
       exceptions = R[pw >= ab]$CNTRY,
       rho_med = median(sp$rho), rho_q = quantile(sp$rho, c(.25, .75)),
       rho_min = min(sp$rho), rho_min_c = sp[which.min(rho)]$CNTRY,
       rho_lo = sp[rho < .5]$CNTRY,
       tab = U[, .(中位數 = median(rk), 四分位距 = paste0(quantile(rk,.25), "–", quantile(rk,.75)),
                   全距 = paste0(min(rk), "–", max(rk)),
                   前三名 = sum(rk <= 3), 後三名 = sum(rk >= 7),
                   比率 = round(100*median(estimate), 1)), by = .(idx, zh, grp)][order(中位數, -比率)])
}
