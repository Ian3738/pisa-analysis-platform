# ============================================================
# 13_figures_ai.R — TALIS 2024 人工智慧模組的圖表
#
# 全部以「比率」呈現而非量表平均。理由：AI 題組沒有官方的合成量表，
# 而自建 Likert 平均要面對跨國作答風格的問題（見 ICCS 那一頁的討論）；
# 是非題的加權比率相對穩健，且分母講得清楚。
# ============================================================
source("~/PISA/R/09_figures.R")
suppressPackageStartupMessages({library(ggplot2); library(data.table); library(jsonlite)})
BASE_FAMILY <- setup_fonts()
QA <- readRDS(path.expand("~/TALIS/output/ai_2024.rds"))
.tn <- as.data.table(fromJSON(path.expand("~/PISA/web/talis_data.json"))$names)
ANAME <- setNames(.tn$zh, .tn$CNT)
zh_c <- function(x) ifelse(is.na(ANAME[x]), x, ANAME[x])
EA <- c("JPN","KOR","SGP","VNM")          # TALIS 2024 的東亞參與者（無臺灣）

# 各國比率的橫條圖，東亞標示
rank_bar <- function(d, title, sub, cap, xlab, w = 7.6, h = 8.4) {
  d <- copy(d)[is.finite(estimate)][order(estimate)]
  d[, `:=`(zh = zh_c(CNTRY), ea = CNTRY %in% EA)][, zh := factor(zh, levels = zh)]
  function(mode) {
    p <- PAL[[mode]]
    ggplot(d, aes(estimate, zh, fill = ea)) +
      geom_col(width = .72) +
      geom_linerange(aes(xmin = estimate - 1.96*se, xmax = estimate + 1.96*se),
                     colour = p$ink2, linewidth = .32) +
      scale_fill_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$accent)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = title, subtitle = sub, x = xlab, y = NULL, caption = cap) +
      theme_pisa(mode, grid = "x") +
      theme(legend.position = "none", axis.text.y = element_text(size = 6.6))
  }
}

build_ai_figures <- function() {
  U <- QA$usage
  save_fig("ai_usage", rank_bar(U,
    "過去 12 個月使用過 AI 的國中教師比率",
    sprintf("TALIS 2024，%d 個參與者。橫線為 95%% 信賴區間；橘色為東亞參與者", nrow(U)),
    "分母為 AI 模組的全體作答教師。臺灣未參加 TALIS 2024，故無臺灣數值",
    "使用過 AI 的教師比率"), w = 7.6, h = 8.4)

  # 用途：以國際中位數排序，並標出最高與最低的參與者
  W <- QA$use[is.finite(estimate)]
  ws <- W[, .(med = median(estimate), lo = min(estimate), hi = max(estimate),
              lo_c = CNTRY[which.min(estimate)], hi_c = CNTRY[which.max(estimate)]), by = .(idx, zh)]
  ws <- ws[order(med)][, zh := factor(zh, levels = zh)]
  save_fig("ai_use_what", function(mode) {
    p <- PAL[[mode]]
    ggplot(ws, aes(y = zh)) +
      geom_linerange(aes(xmin = lo, xmax = hi), colour = p$grid, linewidth = 2.6) +
      geom_point(aes(x = med), size = 3, colour = p$accent) +
      geom_text(aes(x = hi, label = zh_c(hi_c)), hjust = -0.22,
                family = BASE_FAMILY, size = 2.7, colour = p$ink2) +
      geom_text(aes(x = lo, label = zh_c(lo_c)), hjust = 1.22,
                family = BASE_FAMILY, size = 2.7, colour = p$ink2) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         expand = expansion(mult = .16)) +
      labs(title = "教師用 AI 做什麼",
           subtitle = "橘點為 55 個參與者的中位數，灰帶為全距，兩端標示最低與最高的參與者",
           x = "全體教師中曾以 AI 從事該項工作的比率", y = NULL,
           caption = "分母為全體作答教師，未使用 AI 者一律計為 0，故各項比率可直接相互比較") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "none")
  }, w = 8.4, h = 5.0)

  # 十個信念題目的國際同意率：風險類的同意率其實高於助益類
  B <- QA$belief[is.finite(estimate)]
  bs <- B[, .(med = median(estimate), lo = min(estimate), hi = max(estimate)), by = .(idx, zh, grp)]
  bs <- bs[order(med)][, zh := factor(zh, levels = zh)]
  save_fig("ai_belief", function(mode) {
    p <- PAL[[mode]]
    ggplot(bs, aes(y = zh, colour = grp)) +
      geom_linerange(aes(xmin = lo, xmax = hi), colour = p$grid, linewidth = 2.6) +
      geom_point(aes(x = med), size = 3.2) +
      scale_colour_manual(values = c(`正向信念` = p$series[3], `風險認知` = p$series[2]), name = NULL) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         expand = expansion(mult = .08)) +
      labs(title = "教師對 AI 的看法：風險的同意率高於助益",
           subtitle = "點為 55 個參與者的中位數，灰帶為全距",
           x = "同意或非常同意的教師比率", y = NULL,
           caption = "分母已排除回答「不知道」者——該比率的中位數為 23.9%，最高的參與者達 70.4%") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "top")
  }, w = 8.4, h = 4.6)

  # 使用率與正向信念：國家層次的強關聯
  pos <- B[grp == "正向信念", .(pos = mean(estimate)), by = CNTRY]
  neg <- B[grp == "風險認知", .(neg = mean(estimate)), by = CNTRY]
  bb  <- merge(merge(pos, neg, by = "CNTRY"), QA$usage[, .(CNTRY, use = estimate)], by = "CNTRY")
  bb[, `:=`(zh = zh_c(CNTRY), ea = CNTRY %in% EA)]
  r1 <- cor(bb$use, bb$pos); r2 <- cor(bb$pos, bb$neg)
  save_fig("ai_use_belief", function(mode) {
    p <- PAL[[mode]]
    ggplot(bb, aes(pos, use)) +
      geom_smooth(method = "lm", se = TRUE, colour = p$ink2, fill = p$grid,
                  linewidth = .6, alpha = .35) +
      geom_point(aes(colour = ea, size = ea)) +
      ggrepel::geom_text_repel(data = bb[ea == TRUE], aes(label = zh), family = BASE_FAMILY,
        size = 3.1, colour = p$accent, seed = 11, box.padding = .5) +
      scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$accent)) +
      scale_size_manual(values = c(`FALSE` = 1.9, `TRUE` = 3.1)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "認為 AI 有用的地方，教師也用得多",
           subtitle = sprintf("每一點為一個參與者，r = %.2f。東亞四個參與者以強調色標示", r1),
           x = "五項正向信念的平均同意率", y = "過去 12 個月使用過 AI 的比率",
           caption = sprintf("值得注意的是正向信念與風險認知之間幾乎無關（r = %.2f）——看見助益與看見風險不是同一條軸的兩端，而是兩件事", r2)) +
      theme_pisa(mode, grid = "both") + theme(legend.position = "none")
  }, w = 8.2, h = 5.4)

  # 不使用的原因
  Y <- QA$why[is.finite(estimate)]
  ys <- Y[, .(med = median(estimate), lo = min(estimate), hi = max(estimate)), by = .(idx, zh)]
  ys <- ys[order(med)][, zh := factor(zh, levels = zh)]
  save_fig("ai_why_not", function(mode) {
    p <- PAL[[mode]]
    ggplot(ys, aes(y = zh)) +
      geom_linerange(aes(xmin = lo, xmax = hi), colour = p$grid, linewidth = 2.6) +
      geom_point(aes(x = med), size = 3, colour = p$series[2]) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         expand = expansion(mult = .1)) +
      labs(title = "沒有使用 AI 的教師，理由是什麼",
           subtitle = "點為 55 個參與者的中位數，灰帶為全距",
           x = "未使用 AI 的教師中，勾選該項理由的比率", y = NULL,
           caption = "分母為未使用 AI 的教師，非全體教師。可複選，故各項合計超過 100%") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "none")
  }, w = 8.0, h = 4.0)

  # 專業發展的落差
  P <- dcast(QA$pd[is.finite(estimate)], CNTRY ~ idx, value.var = "estimate")
  setnames(P, c("TT4G21G","TT4G24G"), c("got","need"))
  P <- P[is.finite(got) & is.finite(need)][, `:=`(zh = zh_c(CNTRY), gap = need - got,
                                                  ea = CNTRY %in% EA)]
  save_fig("ai_pd_gap", function(mode) {
    p <- PAL[[mode]]
    ggplot(P, aes(got, need)) +
      geom_abline(slope = 1, intercept = 0, linetype = "22", colour = p$axis, linewidth = .5) +
      geom_point(aes(colour = ea, size = ea)) +
      ggrepel::geom_text_repel(data = P[ea == TRUE | gap > quantile(P$gap, .93)],
        aes(label = zh), family = BASE_FAMILY, size = 3, seed = 12, box.padding = .45,
        colour = p$ink2, max.overlaps = 20) +
      scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$accent)) +
      scale_size_manual(values = c(`FALSE` = 1.9, `TRUE` = 3.1)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "想學的人遠多於學過的人",
           subtitle = "橫軸為專業發展已包含 AI 的比率，縱軸為對 AI 技能有中度以上需求的比率",
           x = "已接受的專業發展含 AI", y = "對 AI 技能有中度以上需求",
           caption = sprintf("虛線為兩者相等。%d 個參與者中有 %d 個落在線上方，即需求高於供給；僅阿聯與斯洛維尼亞例外", nrow(P), P[need > got, .N])) +
      theme_pisa(mode, grid = "both") + theme(legend.position = "none")
  }, w = 8.0, h = 5.6)
  invisible(NULL)
}
