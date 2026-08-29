# ============================================================
# 07_figures_talis.R — TALIS 2024／2018 的圖表
#
# 全部由 talis_data.json 的彙總結果重繪，不需重讀原始資料。
#
# 東亞參與單位的定義為 JPN、KOR、SGP、VNM 與 CSH（上海，中國）。
# 早期版本漏了上海，圖與圖說都寫「東亞四地」——上海 10.78 其實是五者中
# 最高的，比越南還高，漏掉它會讓「東亞偏低」的印象失真。
# ============================================================
source("~/PISA/R/09_figures.R")
suppressPackageStartupMessages({library(ggplot2); library(data.table); library(jsonlite)})
BASE_FAMILY <- setup_fonts()
TAL  <- fromJSON(path.expand("~/PISA/web/talis_data.json"))
TT   <- as.data.table(TAL$teachers)
TNM  <- setNames(as.data.table(TAL$names)$zh, as.data.table(TAL$names)$CNT)
zt   <- function(x) ifelse(is.na(TNM[x]), x, TNM[x])
EA5  <- c("JPN","KOR","SGP","VNM","CSH")
EA_LAB <- "東亞五個單位"

build_talis_figures <- function() {
  d <- TT[idx == "T4SELF" & is.finite(mean)][order(mean)]
  d[, `:=`(zh = zt(CNT), ea = CNT %in% EA5)][, zh := factor(zh, levels = zh)]
  save_fig("talis_selfeff", function(mode) {
    p <- PAL[[mode]]
    ggplot(d, aes(mean, zh, colour = ea)) +
      geom_linerange(aes(xmin = mean - 1.96*se, xmax = mean + 1.96*se), linewidth = .5) +
      geom_point(aes(size = ea)) +
      scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$series[2]),
                          labels = c("其他", EA_LAB), name = NULL) +
      scale_size_manual(values = c(`FALSE` = 1.7, `TRUE` = 2.9), guide = "none") +
      labs(title = "TALIS 2024：教師自我效能感（整體）",
           subtitle = sprintf("%d 個參與單位，%s 名國中教師；橘色為日本、韓國、新加坡、越南、上海（中國）",
                              nrow(d), format(TAL$meta$n_teacher, big.mark = ",")),
           x = "自我效能感量表分數", y = NULL,
           caption = "日本 7.14 為全體最低，上海（中國）10.78 為東亞五者最高——區域內的差距大於區域間。臺灣未參加 TALIS 2024") +
      theme_pisa(mode, grid = "x") +
      theme(legend.position = "top", axis.text.y = element_text(size = 6.4))
  }, w = 7.6, h = 9.0)

  w <- dcast(TT[idx %in% c("T4SELF","T4JOBSAT")], CNT ~ idx, value.var = "mean")
  w <- w[is.finite(T4SELF) & is.finite(T4JOBSAT)][, `:=`(zh = zt(CNT), ea = CNT %in% EA5)]
  rr <- cor(w$T4SELF, w$T4JOBSAT)
  save_fig("talis_scatter", function(mode) {
    p <- PAL[[mode]]
    ggplot(w, aes(T4SELF, T4JOBSAT)) +
      geom_smooth(method = "lm", se = TRUE, colour = p$ink2, fill = p$grid,
                  linewidth = .6, alpha = .35) +
      geom_point(aes(colour = ea, size = ea)) +
      ggrepel::geom_text_repel(data = w[ea == TRUE], aes(label = zh), family = BASE_FAMILY,
        size = 3.1, colour = p$series[2], seed = 5, box.padding = .5, max.overlaps = 20) +
      scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$series[2])) +
      scale_size_manual(values = c(`FALSE` = 1.9, `TRUE` = 3.1)) +
      labs(title = "教師自我效能感與工作滿意度",
           subtitle = sprintf("每一點為一個參與單位，%d 個，r = %.2f", nrow(w), rr),
           x = "自我效能感量表分數", y = "工作滿意度量表分數",
           caption = "東亞五個參與單位以橘色標示。日本與韓國落在左下，上海與越南則落在右上——東亞內部並非同一群") +
      theme_pisa(mode, grid = "both") + theme(legend.position = "none")
  }, w = 8.2, h = 5.4)
  invisible(NULL)
}
