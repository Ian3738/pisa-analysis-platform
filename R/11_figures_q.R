# ============================================================
# 11_figures_q.R — 問卷分析的圖表
#
# 讀 05_analysis_q.R 產出的三個 RDS，畫成網站與 Word 報告共用的圖。
# 命名前綴：timssq_ / pirlsq_ / iccsq_
#
# 一個貫穿全檔的原則：IEA 的量表一律「高分＝較有利」（已逐一實測確認），
# 因此所有圖的「往右／往上＝較好」都成立，不需逐圖說明方向。
# ============================================================
source("~/PISA/R/09_figures.R")
source("~/PISA/R/iea/05_analysis_q.R")
suppressPackageStartupMessages({library(ggplot2); library(data.table); library(jsonlite)})

BASE_FAMILY <- setup_fonts()
QT <- readRDS(path.expand("~/TIMSS/output/q_timss.rds"))
QP <- readRDS(path.expand("~/PIRLS/output/q_pirls.rds"))
QI <- readRDS(path.expand("~/ICCS/output/q_iccs.rds"))
.nm <- unique(rbind(as.data.table(fromJSON(path.expand("~/PISA/web/iea_data.json"))$names),
                    as.data.table(fromJSON(path.expand("~/PISA/web/timss_data.json"))$names),
                    as.data.table(fromJSON(path.expand("~/PISA/web/iea_data.json"))$iccs$names)),
                   by = "CNT")
NAME <- setNames(.nm$zh, .nm$CNT)
zh_cnt <- function(x) ifelse(is.na(NAME[x]), x, NAME[x])
EA <- c("TWN","JPN","KOR","SGP","HKG","MAC","CHN")   # 東亞比較組

# 臺灣 vs 全體中位數的差距圖（共用元件）
tw_profile <- function(tab, labeller, title, sub, cap, w = 8.6, h = NULL, fill_by = NULL) {
  tw  <- tab[CNT == "TWN" & is.finite(estimate)]
  med <- tab[, .(med = median(estimate, na.rm = TRUE)), by = idx]
  d   <- merge(tw, med, by = "idx")
  d[, `:=`(zh = labeller(idx), diff = estimate - med)]
  d[, zh := factor(zh, levels = d[order(diff)]$zh)]
  d[, grp := if (is.null(fill_by)) "一般" else fill_by(idx)]
  if (is.null(h)) h <- 1.7 + 0.30 * nrow(d)
  function(mode) {
    p <- PAL[[mode]]
    ggplot(d, aes(x = diff, y = zh, fill = grp)) +
      geom_vline(xintercept = 0, colour = p$axis, linewidth = .5) +
      geom_col(width = .62) +
      geom_text(aes(label = sprintf("%d/%d", rank, ntot)),
                hjust = ifelse(d$diff > 0, -0.18, 1.18),
                family = BASE_FAMILY, size = 2.9, colour = p$ink2) +
      scale_fill_manual(values = setNames(p$series[seq_len(uniqueN(d$grp))], sort(unique(d$grp)))) +
      scale_x_continuous(expand = expansion(mult = .22)) +
      labs(title = title, subtitle = sub, x = "與全體中位數之差（量表分數）", y = NULL, caption = cap) +
      theme_pisa(mode, grid = "x") +
      theme(legend.position = if (is.null(fill_by)) "none" else "top")
  }
}

# ---- TIMSS ------------------------------------------------------------------
build_timss_q <- function() {
  # 1 臺灣八年級學生量表剖面
  g8 <- QT$g8_scales
  fb <- function(v) fifelse(grepl("SCM|SLM|SVM|ICM|DML", v), "數學相關",
                     fifelse(grepl("SCS|SLS|SVS|ICS|DSL", v), "自然相關", "學校與家庭"))
  save_fig("timssq_g8_tw", tw_profile(g8, zh_of,
    "TIMSS 2023 八年級：臺灣學生問卷量表與全體中位數之差",
    "標籤為臺灣名次／參與者數。IEA 量表一律高分為佳，故往右即為較有利",
    "自然相關量表僅分科授課的 30 個參與者有值。量表為 IRT 量尺，國際平均 10、標準差 2",
    fill_by = fb), w = 8.6, h = 6.4)

  # 2 高成就低自信：數學成就 vs 數學自信
  TIM <- fromJSON(path.expand("~/PISA/web/timss_data.json"))
  mm  <- as.data.table(TIM$means)[domain == "MATH", .(CNT, score = mean)]
  cf  <- g8[idx == "BSBGSCM", .(CNT, conf = estimate)]
  d2  <- merge(mm, cf, by = "CNT")[is.finite(score) & is.finite(conf)]
  d2[, `:=`(zh = zh_cnt(CNT), ea = CNT %in% EA)]
  rr <- cor(d2$score, d2$conf)
  save_fig("timssq_paradox", function(mode) {
    p <- PAL[[mode]]
    ggplot(d2, aes(score, conf)) +
      geom_smooth(method = "lm", se = TRUE, colour = p$ink2, fill = p$grid, linewidth = .6, alpha = .35) +
      geom_point(aes(colour = ea, size = ea)) +
      ggrepel::geom_text_repel(data = d2[ea == TRUE], aes(label = zh),
        family = BASE_FAMILY, size = 3.1, colour = p$accent, seed = 1, box.padding = .5, max.overlaps = 20) +
      scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$accent)) +
      scale_size_manual(values = c(`FALSE` = 1.9, `TRUE` = 3.1)) +
      labs(title = "數學成就愈高，學生的數學自信未必愈高",
           subtitle = sprintf("TIMSS 2023 八年級，%d 個參與者，相關係數 r = %.2f", nrow(d2), rr),
           x = "數學平均分數", y = "數學自信量表（國際平均 10）",
           caption = "東亞體制以強調色標示。兩軸皆為國家層級的加權平均") +
      theme_pisa(mode, grid = "both") + theme(legend.position = "none")
  }, w = 8.2, h = 5.4)

  # 3 家庭教育資源梯度
  gr <- QT$g8_grad[idx == "BSBGHER" & is.finite(beta)]
  gr[, `:=`(zh = zh_cnt(CNT), tw = CNT == "TWN")]
  gr <- gr[order(beta)][, zh := factor(zh, levels = zh)]
  save_fig("timssq_grad", function(mode) {
    p <- PAL[[mode]]
    ggplot(gr, aes(beta, zh, fill = tw)) +
      geom_col(width = .72) +
      geom_linerange(aes(xmin = beta - 1.96*se, xmax = beta + 1.96*se),
                     colour = p$ink2, linewidth = .35) +
      scale_fill_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$accent)) +
      labs(title = "家庭教育資源的社經梯度：每提高一個標準差，數學分數的增幅",
           subtitle = sprintf("TIMSS 2023 八年級，%d 個參與者。橫線為 95%% 信賴區間", nrow(gr)),
           x = "每一個標準差對應的分數（分）", y = NULL,
           caption = "以 5 個推估值搭配 JK2 折刀法估計，預測變數已標準化") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "none",
        axis.text.y = element_text(size = 6.6))
  }, w = 7.6, h = 8.2)

  # 4 校間變異比例
  ic <- QT$g8_icc[is.finite(icc)][order(icc)]
  ic[, `:=`(zh = zh_cnt(CNT), tw = CNT == "TWN")][, zh := factor(zh, levels = zh)]
  save_fig("timssq_icc", function(mode) {
    p <- PAL[[mode]]
    ggplot(ic, aes(icc, zh, fill = tw)) +
      geom_col(width = .72) +
      scale_fill_manual(values = c(`FALSE` = p$series[2], `TRUE` = p$accent)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = "數學成就的校間變異比例",
           subtitle = sprintf("TIMSS 2023 八年級，%d 個參與者。比例愈高代表學校之間的落差愈大", nrow(ic)),
           x = "校間變異佔總變異的比例", y = NULL,
           caption = "設計基礎分解，使用完整抽樣權重；校間與校內恆等相加為總變異") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "none",
        axis.text.y = element_text(size = 6.6))
  }, w = 7.6, h = 8.2)

  # 5 校長與教師眼中的學校
  # 校長與教師都有 EAS（學校對學業的強調），直接合併會產生重複的因子水準，
  # 故在標籤前加上回答者的角色。
  act <- rbind(QT$g8_sch_scales[, .(CNT, idx, estimate, se, rank, ntot)],
               QT$g8_tch_scales[, .(CNT, idx, estimate, se, rank, ntot)])
  lab_act <- function(v) paste0(fifelse(grepl("^BC", v), "校長：", "教師："), zh_of(v))
  save_fig("timssq_actors", tw_profile(act, lab_act,
    "TIMSS 2023 八年級：臺灣的校長與教師問卷",
    "標籤為臺灣名次／參與者數。校長量表併至學生層次後以學生權重估計",
    "教師量表以 BST 連結檔的數學教師權重 MATWGT 估計；教師檔本身不含權重",
    fill_by = function(v) fifelse(grepl("^BC", v), "校長回答", "教師回答")), w = 8.6, h = 4.4)

  # 6 四年級
  save_fig("timssq_g4_tw", tw_profile(QT$g4_scales, zh_of,
    "TIMSS 2023 四年級：臺灣學生問卷量表與全體中位數之差",
    "標籤為臺灣名次／參與者數",
    "共 63 個參與者。量表為 IRT 量尺，國際平均 10、標準差 2"), w = 8.6, h = 5.8)
  invisible(NULL)
}

# ---- PIRLS ------------------------------------------------------------------
build_pirls_q <- function() {
  save_fig("pirlsq_stu_tw", tw_profile(QP$stu_scales, zh_of,
    "PIRLS 2021：臺灣學生問卷量表與全體中位數之差",
    "標籤為臺灣名次／參與者數。IEA 量表一律高分為佳",
    "共 64 個參與者。量表為 IRT 量尺，國際平均 10、標準差 2"), w = 8.6, h = 4.6)

  save_fig("pirlsq_home_tw", tw_profile(QP$home_scales, zh_of,
    "PIRLS 2021：臺灣家長問卷量表與全體中位數之差",
    "標籤為臺灣名次／參與者數。臺灣家長問卷回收率 99.4%，全體中位數 89.6%",
    "共 61 個參與者。回收率高代表此結果不是低回收造成的選樣偏誤",
    fill_by = function(v) fifelse(grepl("ELA|ENA|ELN|ELT", v), "入學前的家庭活動", "其他")),
    w = 8.6, h = 4.4)

  # 入學前讀寫活動 vs 閱讀成就：臺灣是明顯的離群點
  IEA <- fromJSON(path.expand("~/PISA/web/iea_data.json"))
  pm  <- as.data.table(IEA$pirls_means)[, .(CNT, score = mean)]
  ela <- QP$home_scales[idx == "ASBHELA", .(CNT, ela = estimate)]
  d <- merge(pm, ela, by = "CNT")[is.finite(score) & is.finite(ela)]
  d[, `:=`(zh = zh_cnt(CNT), hl = CNT %in% c("TWN","SGP","HKG","JPN","KOR","MAC"))]
  rr <- cor(d$score, d$ela)
  save_fig("pirlsq_early", function(mode) {
    p <- PAL[[mode]]
    ggplot(d, aes(ela, score)) +
      geom_smooth(method = "lm", se = TRUE, colour = p$ink2, fill = p$grid, linewidth = .6, alpha = .35) +
      geom_point(aes(colour = hl, size = hl)) +
      ggrepel::geom_text_repel(data = d[hl == TRUE], aes(label = zh), family = BASE_FAMILY,
        size = 3.1, colour = p$accent, seed = 2, box.padding = .5, max.overlaps = 20) +
      scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$accent)) +
      scale_size_manual(values = c(`FALSE` = 1.9, `TRUE` = 3.1)) +
      labs(title = "入學前的家庭讀寫活動與四年級閱讀成就",
           subtitle = sprintf("PIRLS 2021，%d 個參與者，相關係數 r = %.2f。東亞體制以強調色標示", nrow(d), rr),
           x = "入學前讀寫活動量表（家長回答，國際平均 10）", y = "閱讀平均分數",
           caption = "臺灣家長回報的入學前讀寫活動在 61 個參與者中排名第 59，但閱讀成就仍在中上") +
      theme_pisa(mode, grid = "both") + theme(legend.position = "none")
  }, w = 8.2, h = 5.4)

  # 校長與教師對同一所學校的觀感落差
  sc <- QP$sch_scales[idx == "ACBGDAS", .(CNT, prin = estimate)]
  tc <- QP$tch_scales[idx == "ATBGSOS", .(CNT, tch = estimate)]
  g <- merge(sc, tc, by = "CNT")[is.finite(prin) & is.finite(tch)]
  g[, `:=`(zh = zh_cnt(CNT), tw = CNT == "TWN")]
  rr2 <- cor(g$prin, g$tch)
  save_fig("pirlsq_gap", function(mode) {
    p <- PAL[[mode]]
    ggplot(g, aes(prin, tch)) +
      geom_abline(slope = 1, intercept = 0, linetype = "22", colour = p$axis, linewidth = .5) +
      geom_point(aes(colour = tw, size = tw)) +
      ggrepel::geom_text_repel(data = g[tw == TRUE], aes(label = zh), family = BASE_FAMILY,
        size = 3.3, colour = p$accent, seed = 3, box.padding = .8) +
      scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$accent)) +
      scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 3.4)) +
      labs(title = "同一所學校，校長與教師的觀感未必一致",
           subtitle = sprintf("PIRLS 2021，%d 個參與者，相關係數僅 r = %.2f", nrow(g), rr2),
           x = "校長：學校紀律良好程度", y = "教師：學校安全有序程度",
           caption = "虛線為兩者相等。臺灣校長的紀律評價排第 5，教師的安全評價排第 54，落差在全體中最大之一") +
      theme_pisa(mode, grid = "both") + theme(legend.position = "none")
  }, w = 8.0, h = 5.4)

  gr <- QP$home_grad[idx == "ASBHELA" & is.finite(beta)]
  gr[, `:=`(zh = zh_cnt(CNT), tw = CNT == "TWN")]
  gr <- gr[order(beta)][, zh := factor(zh, levels = zh)]
  save_fig("pirlsq_grad", function(mode) {
    p <- PAL[[mode]]
    ggplot(gr, aes(beta, zh, fill = tw)) +
      geom_col(width = .72) +
      geom_linerange(aes(xmin = beta-1.96*se, xmax = beta+1.96*se),
                     colour = p$ink2, linewidth = .35) +
      scale_fill_manual(values = c(`FALSE` = p$series[3], `TRUE` = p$accent)) +
      labs(title = "入學前讀寫活動的梯度：每提高一個標準差，閱讀分數的增幅",
           subtitle = sprintf("PIRLS 2021，%d 個參與者。橫線為 95%% 信賴區間", nrow(gr)),
           x = "每一個標準差對應的分數（分）", y = NULL,
           caption = "以 5 個推估值搭配 JK2 折刀法估計。此為關聯而非因果，家庭社經地位等混淆因素未控制") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "none",
        axis.text.y = element_text(size = 6.6))
  }, w = 7.6, h = 8.2)
  invisible(NULL)
}

# ---- ICCS -------------------------------------------------------------------
# ICCS 的態度量表全部是 Likert 自陳，跨國比較會混入「作答傾向」：
# 某些體制的學生對任何正向敘述都比較容易同意。因此除了原始名次，
# 另計算「國內置中」的相對剖面——扣掉各國自身在 30 個量表上的平均，
# 只留下該國相對於自己的強弱。兩者都不是唯一的真相，並列才能框出範圍。
iccs_att <- function() {
  a <- QI$stu_scales[idx != "S_NISB" & is.finite(estimate)]
  cw <- a[, .(cmean = mean(estimate)), by = CNT]
  a <- merge(a, cw, by = "CNT")
  a[, ips := estimate - cmean]
  a[, `:=`(rank_raw = frank(-estimate, ties.method = "min"),
           rank_ips = frank(-ips,      ties.method = "min")), by = idx]
  list(a = a, cw = cw[order(-cmean)])
}

build_iccs_q <- function() {
  X <- iccs_att(); a <- X$a; cw <- X$cw

  save_fig("iccsq_tw", tw_profile(QI$stu_scales[idx != "S_NISB"], zh_iccs,
    "ICCS 2022：臺灣學生公民態度量表與全體中位數之差",
    "標籤為臺灣名次／參與者數。共 24 個參與者",
    "量表為 IRT 量尺，國際平均 50、標準差 10。S_NISB 因在各國內標準化而排除",
    fill_by = function(v) fifelse(v %in% c("S_CIVLRN","S_SCHPART","S_SCACT","S_INFDEC","S_INTACT","S_STUTREL","S_OPDISC"),
                                  "校內經驗", "校外態度與參與")), w = 8.8, h = 8.0)

  # 作答傾向
  cw[, `:=`(zh = zh_cnt(CNT), tw = CNT == "TWN")]
  cw2 <- cw[order(cmean)][, zh := factor(zh, levels = zh)]
  save_fig("iccsq_resp", function(mode) {
    p <- PAL[[mode]]
    ggplot(cw2, aes(cmean, zh, fill = tw)) +
      geom_vline(xintercept = 50, colour = p$axis, linetype = "22", linewidth = .5) +
      geom_col(width = .7) +
      coord_cartesian(xlim = c(min(cw2$cmean) - .6, max(cw2$cmean) + .3)) +
      scale_fill_manual(values = c(`FALSE` = p$series[4], `TRUE` = p$accent)) +
      labs(title = "各參與者在 30 個態度量表上的整體平均",
           subtitle = "此值反映的是整體作答傾向，而非某一項態度的高低",
           x = "30 個量表的平均分數", y = NULL,
           caption = "虛線為國際平均 50。臺灣 52.91 為全體最高，荷蘭 47.33 最低——跨國比較單一量表時須考慮此一成分") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "none")
  }, w = 7.4, h = 6.0)

  # 原始名次 vs 國內置中名次：名次放在 y 軸才看得出「掉下去」，
  # 若把量表名放 y 軸，兩個點會落在同一條水平線上，變化只剩數字可讀。
  tw <- a[CNT == "TWN"][, zh := zh_iccs(idx)]
  tw[, move := rank_ips - rank_raw]
  sl <- rbind(tw[, .(zh, x = "原始名次", rank = rank_raw, move)],
              tw[, .(zh, x = "國內置中後", rank = rank_ips, move)])
  sl[, x := factor(x, levels = c("原始名次", "國內置中後"))]
  keep1 <- tw[rank_ips == 1]$zh                      # 置中後仍第 1
  fall  <- tw[rank_ips >= 24 & rank_raw <= 20]$zh     # 掉到最後
  sl[, kind := fifelse(zh %in% keep1, "置中後仍第 1",
               fifelse(zh %in% fall, "置中後落到最後", "其他"))]
  labL <- tw[zh %in% c(keep1, fall)]
  save_fig("iccsq_ips", function(mode) {
    p <- PAL[[mode]]
    ggplot(sl, aes(x, rank, group = zh, colour = kind)) +
      geom_line(aes(linewidth = kind, alpha = kind)) +
      geom_point(aes(size = kind)) +
      ggrepel::geom_text_repel(data = labL, aes(x = "原始名次", y = rank_raw, label = zh),
        family = BASE_FAMILY, size = 3, direction = "y", hjust = 1, nudge_x = -0.13,
        segment.size = .25, box.padding = .18, seed = 7, max.overlaps = 30, inherit.aes = FALSE,
        colour = ifelse(labL$zh %in% keep1, p$series[3], p$series[2])) +
      scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 24), limits = c(24.6, 0.4)) +
      scale_x_discrete(expand = expansion(mult = c(.62, .10))) +
      scale_colour_manual(values = c(`置中後仍第 1` = p$series[3],
                                     `置中後落到最後` = p$series[2], `其他` = p$axis)) +
      scale_linewidth_manual(values = c(`置中後仍第 1` = 1.1, `置中後落到最後` = 1.1, `其他` = .5)) +
      scale_alpha_manual(values = c(`置中後仍第 1` = 1, `置中後落到最後` = 1, `其他` = .55)) +
      scale_size_manual(values = c(`置中後仍第 1` = 2.4, `置中後落到最後` = 2.4, `其他` = 1.5)) +
      labs(title = "扣掉整體作答傾向之後，臺灣的公民態度剖面",
           subtitle = "縱軸為 24 個參與者中的名次，愈上面愈前面。灰線為其餘量表",
           x = NULL, y = "名次",
           caption = "國內置中＝各量表分數減去該國在 30 個量表上的平均。與學校生活直接相關的四項不受影響，校外政治參與類則全部落到最後") +
      theme_pisa(mode, grid = "y") + theme(legend.position = "top")
  }, w = 8.2, h = 6.6)

  # 公民知識趨勢
  K <- dcast(QI$know_trend[, .(CNT, cycle, estimate, se)], CNT ~ cycle, value.var = c("estimate","se"))
  K <- K[!is.na(estimate_2016) & !is.na(estimate_2022)]
  K[, `:=`(diff = estimate_2022 - estimate_2016, zh = zh_cnt(CNT))]
  K[, sed := sqrt(se_2016^2 + se_2022^2)]
  K[, sig := abs(diff) > 1.96 * sed]
  K <- K[order(diff)][, zh := factor(zh, levels = zh)]
  Kl <- melt(K[, .(zh, `2016` = estimate_2016, `2022` = estimate_2022)], id.vars = "zh",
             variable.name = "cycle", value.name = "score")
  save_fig("iccsq_trend_know", function(mode) {
    p <- PAL[[mode]]
    ggplot() +
      geom_segment(data = K, aes(x = estimate_2016, xend = estimate_2022, y = zh, yend = zh,
                                 colour = sig), linewidth = 1.1,
                   arrow = arrow(length = unit(.11, "cm"), type = "closed")) +
      geom_point(data = Kl, aes(score, zh, shape = cycle), size = 2, colour = p$ink) +
      scale_colour_manual(values = c(`FALSE` = p$grid, `TRUE` = p$accent),
                          labels = c("未達顯著", "達統計顯著"), name = NULL) +
      scale_shape_manual(values = c(`2016` = 1, `2022` = 16), name = NULL) +
      labs(title = "ICCS 公民知識的變化：2016 → 2022",
           subtitle = sprintf("%d 個兩輪皆參與者。箭頭指向 2022，橘色為達統計顯著者", nrow(K)),
           x = "公民知識平均分數", y = NULL,
           caption = "標準誤僅含兩輪的抽樣與推估變異，未納入跨輪連結誤差，故顯著性略為寬鬆（見方法註記）") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "top")
  }, w = 8.0, h = 5.6)

  # 臺灣態度量表趨勢
  at <- dcast(QI$att_trend[CNT == "TWN", .(idx, cycle, estimate)], idx ~ cycle, value.var = "estimate")
  setnames(at, c("2016","2022"), c("y16","y22"))
  at <- at[!is.na(y16) & !is.na(y22)][, `:=`(zh = zh_iccs(idx), d = y22 - y16)]
  at <- at[order(d)][, zh := factor(zh, levels = zh)]
  save_fig("iccsq_trend_att", function(mode) {
    p <- PAL[[mode]]
    ggplot(at, aes(d, zh, fill = d > 0)) +
      geom_vline(xintercept = 0, colour = p$axis, linewidth = .5) +
      geom_col(width = .66) +
      scale_fill_manual(values = c(`FALSE` = p$series[5], `TRUE` = p$series[1])) +
      labs(title = "臺灣學生公民態度的變化：2016 → 2022",
           subtitle = sprintf("%d 個兩輪共通的量表。正值代表 2022 年較高", nrow(at)),
           x = "量表分數的變化", y = NULL,
           caption = "兩輪的量尺是否完全等價未經本平台驗證，此圖僅供型態參考，不宜作為效果量的精確估計") +
      theme_pisa(mode, grid = "x") + theme(legend.position = "none")
  }, w = 7.8, h = 5.4)
  invisible(NULL)
}

build_all_q <- function() { build_timss_q(); build_pirls_q(); build_iccs_q() }
