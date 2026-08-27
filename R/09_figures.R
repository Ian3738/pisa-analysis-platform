# ============================================================
# 09_figures.R — 產出網站用的圖表
#
# 本檔在持續的 R 工作階段中執行（透過 r-stats MCP），
# 與終端機分析共用 lib_pisa.R 的估計核心。
# 每張圖產出亮色與暗色兩個 SVG 版本，由網頁依讀者主題切換。
#
# 使用：
#   source("~/PISA/R/09_figures.R"); build_figures()
#   接著 python3 ~/PISA/R/build_site.py 組成網頁
# ============================================================
source("~/PISA/R/00_config.R")
source("~/PISA/R/lib_pisa.R")
source("~/PISA/R/03_harmonise.R")
suppressPackageStartupMessages({
  library(arrow); library(data.table); library(ggplot2)
  library(svglite); library(systemfonts); library(jsonlite); library(ragg)
})

FIGDIR <- file.path(PISA_ROOT, "web", "figs")
PNGDIR <- file.path(PISA_ROOT, "web", "png")   # Word 文件用
DOM_ZH <- c(MATH = "數學", READ = "閱讀", SCIE = "科學")

# svglite 把字型名稱直接寫進 SVG，由瀏覽器解析。網頁已從 Google Fonts
# 載入 Noto Sans TC，故此處指定同名；版面度量在本機以 PingFang TC 近似。
setup_fonts <- function() {
  try(systemfonts::register_font(
    name  = "Noto Sans TC",
    plain = systemfonts::match_fonts("PingFang TC")$path,
    bold  = systemfonts::match_fonts("PingFang TC", weight = "bold")$path
  ), silent = TRUE)
  "Noto Sans TC"
}

PAL <- list(
  light = list(surface = "#FAFCFA", ink = "#111917", ink2 = "#46524E", ink3 = "#7A8783",
               grid = "#E1E6E2", axis = "#C3CCC7", accent = "#0E5F57", me = "#0E5F57",
               series = c("#2a78d6","#eb6834","#1baf7a","#eda100",
                          "#e87ba4","#008300","#4a3aa7","#e34948")),
  dark  = list(surface = "#111917", ink = "#E4EDE9", ink2 = "#A3B0AB", ink3 = "#78857F",
               grid = "#222D2B", axis = "#33403C", accent = "#4FBFB0", me = "#4FBFB0",
               series = c("#3987e5","#d95926","#199e70","#c98500",
                          "#d55181","#21a021","#9085e9","#e66767"))
)

theme_pisa <- function(mode = "light", grid = "y", base_family = "Noto Sans TC") {
  p <- PAL[[mode]]
  th <- theme_minimal(base_family = base_family, base_size = 11) +
    theme(
      plot.background  = element_rect(fill = p$surface, colour = NA),
      panel.background = element_rect(fill = p$surface, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = p$grid, linewidth = 0.35),
      axis.text  = element_text(colour = p$ink3, size = 9),
      axis.title = element_text(colour = p$ink2, size = 9.5),
      axis.line  = element_line(colour = p$axis, linewidth = 0.4),
      axis.ticks = element_line(colour = p$axis, linewidth = 0.3),
      plot.title    = element_text(colour = p$ink, face = "bold", size = 12.5, margin = margin(b = 3)),
      plot.subtitle = element_text(colour = p$ink3, size = 9.5, margin = margin(b = 10)),
      plot.caption  = element_text(colour = p$ink3, size = 8, hjust = 0, margin = margin(t = 10)),
      legend.text = element_text(colour = p$ink2, size = 9),
      legend.title = element_blank(), legend.key = element_blank(),
      legend.position = "top", legend.justification = "left", legend.margin = margin(b = 2),
      strip.text = element_text(colour = p$ink, face = "bold", size = 10),
      plot.margin = margin(10, 12, 8, 10))
  if (grid == "y") th <- th + theme(panel.grid.major.x = element_blank())
  if (grid == "x") th <- th + theme(panel.grid.major.y = element_blank())
  th
}

# 網頁用 SVG（亮暗兩版）＋ Word 用高解析 PNG（僅亮版，白底）
save_fig <- function(fn, builder, w = 8, h = 5, png = TRUE) {
  for (mode in c("light", "dark")) {
    f <- file.path(FIGDIR, sprintf("%s_%s.svg", fn, mode))
    svglite::svglite(f, width = w, height = h, bg = PAL[[mode]]$surface)
    print(builder(mode)); dev.off()
  }
  if (png) {
    dir.create(PNGDIR, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(PNGDIR, sprintf("%s.png", fn))
    ragg::agg_png(f, width = w, height = h, units = "in", res = 200,
                  background = "#FFFFFF")
    print(builder("light") + ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA)))
    dev.off()
  }
  log_msg("  ", fn)
}

# ---- 加權統計輔助 ---------------------------------------------------------
wq <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  approx(cumsum(w) / sum(w), x, xout = p, rule = 2, ties = "ordered")$y
}

# 把 10 個合理推估值攤平，每個權重取 1/10 —— 描述分配時的正確作法
pv_long <- function(d, dom) {
  rbindlist(lapply(sprintf("PV%d%s", 1:10, dom), function(p)
    data.table(cycle = d$cycle, score = d[[p]], w = d$W_FSTUWT / 10)))[is.finite(score)]
}

# ---- 主流程 ---------------------------------------------------------------
build_figures <- function() {
  dir.create(FIGDIR, recursive = TRUE, showWarnings = FALSE)
  BASE_FAMILY <<- setup_fonts()

  log_msg("載入三輪資料")
  pisa <- rbindlist(lapply(c(2015, 2018, 2022), function(cy) {
    d <- harmonise_stu(cy)                 # 內含越南 2018 的補檔處理
    keep <- intersect(c("CNT","CNTRYID","OECD","W_FSTUWT","SENWT","ST004D01T","ESCS","cycle",
                        rw_names(), as.vector(outer(1:10, names(DOM_ZH),
                          function(i, d) sprintf("PV%d%s", i, d)))), names(d))
    d[, ..keep]
  }), fill = TRUE)
  pisa[, FEMALE := as.integer(ST004D01T == 1)]
  log_msg("  ", nrow(pisa), " 列")

  # ---------- 描述統計 ----------
  TWL <- rbindlist(lapply(names(DOM_ZH), function(dm) {
    x <- pv_long(pisa[CNT == "TAP"], dm); x[, domain := DOM_ZH[dm]]; x }))
  TWL[, `:=`(domain = factor(domain, levels = DOM_ZH),
             cyc = factor(cycle, levels = c(2015, 2018, 2022)))]

  save_fig("desc_density", function(mode) {
    p <- PAL[[mode]]
    ggplot(TWL, aes(x = score, weight = w, colour = cyc, fill = cyc)) +
      geom_density(alpha = 0.10, linewidth = 0.8, n = 160) +
      facet_wrap(~ domain, nrow = 1) +
      scale_colour_manual(values = p$series[1:3]) + scale_fill_manual(values = p$series[1:3]) +
      labs(title = "臺灣學生分數的加權分配",
           subtitle = "10 個合理推估值攤平後各取 1/10 權重，再以最終學生權重加權",
           x = "PISA 量尺分數", y = "密度",
           caption = "資料：OECD PISA 2015／2018／2022 公開使用檔") +
      theme_pisa(mode) +
      theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  }, w = 9, h = 4.2)

  BOX <- TWL[, .(ymin = wq(score, w, .05), lower = wq(score, w, .25), middle = wq(score, w, .50),
                 upper = wq(score, w, .75), ymax = wq(score, w, .95)), by = .(domain, cyc)]
  save_fig("desc_box", function(mode) {
    p <- PAL[[mode]]
    ggplot(BOX, aes(x = cyc, fill = cyc)) +
      geom_boxplot(aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
                   stat = "identity", width = 0.55, colour = p$ink3, linewidth = 0.4) +
      facet_wrap(~ domain, nrow = 1) +
      scale_fill_manual(values = p$series[1:3], guide = "none") +
      labs(title = "臺灣分數分布的加權五數概括",
           subtitle = "盒為加權四分位距，鬚為加權第 5 與第 95 百分位",
           x = NULL, y = "PISA 量尺分數",
           caption = "以最終學生權重計算加權分位數，非樣本分位數") +
      theme_pisa(mode)
  }, w = 9, h = 4.2)

  ESC <- pisa[CNT == "TAP" & is.finite(ESCS),
              .(cyc = factor(cycle, levels = c(2015,2018,2022)), ESCS, w = W_FSTUWT)]
  save_fig("desc_escs", function(mode) {
    p <- PAL[[mode]]
    ggplot(ESC, aes(x = ESCS, weight = w, colour = cyc, fill = cyc)) +
      geom_density(alpha = 0.10, linewidth = 0.8, n = 160) +
      geom_vline(xintercept = 0, colour = p$axis, linetype = "dashed", linewidth = 0.4) +
      scale_colour_manual(values = p$series[1:3]) + scale_fill_manual(values = p$series[1:3]) +
      labs(title = "臺灣學生家庭社經地位的加權分布",
           subtitle = "ESCS 指標；0 為 2022 年 OECD 國家平均，虛線標示該基準",
           x = "ESCS（經濟社會文化地位指標）", y = "密度",
           caption = "各輪原始 ESCS 量尺不同，跨輪比較須改用官方 trend ESCS") +
      theme_pisa(mode) +
      theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  }, w = 7.5, h = 4)

  # 加權與未加權的差距：說明為何不能省略權重
  P22 <- pisa[cycle == 2022]
  uw <- P22[, .(wt = wmean(PV1MATH, W_FSTUWT), uwt = mean(PV1MATH, na.rm = TRUE)), by = CNT]
  uw <- uw[is.finite(wt)][, diff := wt - uwt][order(diff)]
  uw[, `:=`(CNT = factor(CNT, levels = CNT), hl = CNT == "TAP")]
  save_fig("desc_weight_effect", function(mode) {
    p <- PAL[[mode]]
    ggplot(uw, aes(x = CNT, y = diff, fill = hl)) +
      geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$me), guide = "none") +
      coord_flip() +
      labs(title = "不加權會偏多少：各國加權與未加權平均之差",
           subtitle = "PISA 2022 數學。差值為加權平均減未加權平均，臺灣以深色標示",
           x = NULL, y = "加權平均 − 未加權平均（分）",
           caption = "PISA 依學校規模與抽樣機率賦權；忽略權重描述的是樣本，不是母體") +
      theme_pisa(mode, grid = "x") + theme(axis.text.y = element_text(size = 5.6))
  }, w = 7.5, h = 9)

  # ---------- 焦點國家趨勢與變異數分解 ----------
  FOCUS <- c("TAP","JPN","KOR","SGP","HKG","MAC","FIN","EST","USA")
  FOCUS_ZH <- c(TAP="臺灣", JPN="日本", KOR="韓國", SGP="新加坡", HKG="香港",
                MAC="澳門", FIN="芬蘭", EST="愛沙尼亞", USA="美國")
  E <- rbindlist(lapply(c(2015,2018,2022), function(cy)
    rbindlist(lapply(names(DOM_ZH), function(dm) {
      r <- pisa_pv_by(pisa[cycle == cy & CNT %in% FOCUS], pv_names(dm, 10), by = "CNT", FUN = wmean)
      r[, .(CNT, cycle = cy, domain = dm, mean = estimate, se,
            se_samp = sqrt(var_sampling), se_imp = sqrt(var_imputation))] }))))
  E[, `:=`(name = factor(FOCUS_ZH[CNT], levels = FOCUS_ZH[c("SGP","MAC","TAP","HKG","JPN","KOR","EST","FIN","USA")]),
           dom_zh = factor(DOM_ZH[domain], levels = DOM_ZH))]

  save_fig("trend_focus", function(mode) {
    p <- PAL[[mode]]
    # 臺灣使用強調色（它是被標示的焦點，不是類別序列之一），
    # 其餘 8 國才配類別色盤——色盤只有 8 個槽，硬塞第 9 色會產生 NA 並丟掉資料
    others <- setdiff(levels(E$name), "臺灣")
    stopifnot(length(others) <= length(p$series))
    cols <- setNames(p$series[seq_along(others)], others)
    cols[["臺灣"]] <- p$me
    ggplot(E, aes(x = cycle, y = mean, colour = name, fill = name, group = name)) +
      geom_ribbon(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), alpha = 0.12, colour = NA) +
      geom_line(aes(linewidth = name == "臺灣")) + geom_point(aes(size = name == "臺灣")) +
      facet_wrap(~ dom_zh, nrow = 1) +
      scale_x_continuous(breaks = c(2015, 2018, 2022)) +
      scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
      scale_linewidth_manual(values = c(`FALSE` = .6, `TRUE` = 1.4), guide = "none") +
      scale_size_manual(values = c(`FALSE` = 1.3, `TRUE` = 2.4), guide = "none") +
      labs(title = "各輪平均分數與 95% 信賴區間",
           subtitle = "10 個合理推估值搭配 80 組 Fay 重複權重估計；臺灣以粗線標示",
           x = NULL, y = "PISA 量尺分數",
           caption = "區間重疊不等於差異不顯著；跨輪次的正式檢定另需納入連結誤差") +
      theme_pisa(mode) +
      guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2, size = 2)))
  }, w = 9.5, h = 4.6)

  VL <- melt(E[cycle == 2022, .(name, dom_zh, 抽樣變異 = se_samp^2, 推估變異 = se_imp^2)],
             id.vars = c("name","dom_zh"), variable.name = "src", value.name = "v")
  save_fig("variance_decomp", function(mode) {
    p <- PAL[[mode]]
    ggplot(VL, aes(x = name, y = v, fill = src)) +
      geom_col(width = 0.68, position = "stack", colour = p$surface, linewidth = 0.6) +
      facet_wrap(~ dom_zh, nrow = 1) +
      scale_fill_manual(values = c(抽樣變異 = p$series[1], 推估變異 = p$series[2])) +
      coord_flip() +
      labs(title = "標準誤的兩個來源",
           subtitle = "PISA 2022。抽樣變異來自複雜抽樣設計，推估變異來自能力值是推估而非實測",
           x = NULL, y = "變異數（分²）",
           caption = "只用單一個推估值會完全漏掉橘色部分；兩者相加開根號才是正確的標準誤") +
      theme_pisa(mode, grid = "x")
  }, w = 9.5, h = 4.6)

  log_msg("焦點圖表完成；各國圖表請見 build_country_figures()")
  invisible(list(pisa = pisa, focus = E))
}

# ---- 各國圖表（排名、精熟等級、社經梯度、性別差距、連結誤差）--------------
# 需先有 08_export_web.R 產出的 pisa_data.json（提供國名對照與跨輪比較結果）
build_country_figures <- function(pisa) {
  J     <- fromJSON(file.path(PISA_ROOT, "web", "pisa_data.json"))
  ZH    <- fromJSON(file.path(PISA_ROOT, "web", "country_zh.json"))
  CNM   <- as.data.table(J$countryNames)
  TREND <- as.data.table(J$trend)
  MEANS <- as.data.table(J$means)

  nm_map <- merge(unique(MEANS[, .(CNT, CNTRYID)]), CNM, by = "CNTRYID", all.x = TRUE)
  nm_map[, zh := sapply(name, function(x) { v <- ZH[[x]]; if (is.null(v)) x else v })]
  nm_map <- unique(nm_map, by = "CNT")          # 科索沃有兩個國碼，須去重
  NMV <- setNames(nm_map$zh, nm_map$CNT)

  P22 <- pisa[cycle == 2022]
  P22[, FEMALE := as.integer(ST004D01T == 1)]

  res22 <- rbindlist(lapply(names(DOM_ZH), function(dm) {
    pvs <- pv_names(dm, 10); cuts <- PISA_CUTS[[dm]]
    m  <- pisa_pv_by(P22, pvs, by = "CNT", FUN = wmean)
    lo <- pisa_pv_by(P22, pvs, by = "CNT", FUN = function(x, w) 100 - wpct_above(x, w, cuts[["2"]]))
    hi <- pisa_pv_by(P22, pvs, by = "CNT", FUN = wpct_above, cut = cuts[["5"]])
    gm <- pisa_pv_by(P22[FEMALE == 0], pvs, by = "CNT", FUN = wmean)
    gf <- pisa_pv_by(P22[FEMALE == 1], pvs, by = "CNT", FUN = wmean)
    out <- Reduce(function(a, b) merge(a, b, by = "CNT"), list(
      m [, .(CNT, mean = estimate, se)], lo[, .(CNT, below_L2 = estimate)],
      hi[, .(CNT, above_L5 = estimate)], gm[, .(CNT, male = estimate, se_m = se)],
      gf[, .(CNT, female = estimate, se_f = se)]))
    out[, `:=`(domain = dm, gap = female - male, se_gap = sqrt(se_m^2 + se_f^2))]
    out[, p_gap := 2 * pnorm(-abs(gap / se_gap))]
    out
  }))

  esc22 <- rbindlist(lapply(names(DOM_ZH), function(dm)
    rbindlist(lapply(unique(P22$CNT), function(cn) {
      sub <- P22[CNT == cn & is.finite(ESCS)]
      if (nrow(sub) < 200) return(NULL)
      r <- tryCatch(pisa_pv_lm(sub, PV_ ~ ESCS, domain = dm, n_pv = 10), error = function(e) NULL)
      if (is.null(r)) return(NULL)
      x <- r[term == "ESCS"]
      data.table(CNT = cn, domain = dm, slope = x$estimate, se_slope = x$se)
    }))))
  res22 <- merge(res22, esc22, by = c("CNT","domain"), all.x = TRUE)
  res22[, name := NMV[CNT]]

  for (dm in names(DOM_ZH)) {
    dz <- DOM_ZH[dm]; tag <- tolower(dm)

    d <- res22[domain == dm][order(mean)]
    d[, `:=`(name = factor(name, levels = name), hl = CNT == "TAP")]
    local({ dd <- copy(d); zz <- dz
      save_fig(paste0("rank_", tag), function(mode) {
        p <- PAL[[mode]]
        ggplot(dd, aes(x = mean, y = name)) +
          geom_errorbar(aes(xmin = mean - 1.96*se, xmax = mean + 1.96*se, colour = hl),
                        orientation = "y", width = 0, linewidth = 0.55) +
          geom_point(aes(colour = hl, size = hl)) +
          scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$me), guide = "none") +
          scale_size_manual(values = c(`FALSE` = 1.5, `TRUE` = 2.8), guide = "none") +
          labs(title = paste0("PISA 2022 ", zz, "：各國平均與 95% 信賴區間"),
               subtitle = "以最終學生權重、80 組 Fay 重複權重與 10 個合理推估值估計；臺灣以深色標示",
               x = "PISA 量尺分數", y = NULL,
               caption = "橫線為 95% 信賴區間。兩國區間重疊時，差異未必達統計顯著") +
          theme_pisa(mode, grid = "x") + theme(axis.text.y = element_text(size = 6))
      }, w = 7.5, h = 9.2) })

    d2 <- res22[domain == dm][order(-below_L2)]
    d2[, name := factor(name, levels = name)]
    BF <- rbind(d2[, .(name, v = -below_L2, lab = "未達 Level 2")],
                d2[, .(name, v =  above_L5, lab = "Level 5 以上")])
    BF[, lab := factor(lab, levels = c("未達 Level 2", "Level 5 以上"))]
    local({ bb <- copy(BF); zz <- dz
      save_fig(paste0("prof_", tag), function(mode) {
        p <- PAL[[mode]]
        ggplot(bb, aes(x = v, y = name, fill = lab)) +
          geom_col(width = 0.72, colour = p$surface, linewidth = 0.3) +
          geom_vline(xintercept = 0, colour = p$axis, linewidth = 0.5) +
          scale_fill_manual(values = c(`未達 Level 2` = p$series[2], `Level 5 以上` = p$series[1])) +
          scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
          labs(title = paste0("PISA 2022 ", zz, "：兩端的學生比率"),
               subtitle = "左為未達基礎水準（Level 2）的比率，右為高表現（Level 5 以上）的比率",
               x = NULL, y = NULL,
               caption = "依未達 Level 2 的比率由高至低排序。切點取自 PISA 2022 技術報告") +
          theme_pisa(mode, grid = "x") + theme(axis.text.y = element_text(size = 6))
      }, w = 7.8, h = 9.2) })

    local({ ee <- res22[domain == dm & is.finite(slope)]
      ee[, hl := CNT == "TAP"]
      mx <- median(ee$mean); my <- median(ee$slope); zz <- dz
      save_fig(paste0("escs_", tag), function(mode) {
        p <- PAL[[mode]]
        ggplot(ee, aes(x = mean, y = slope)) +
          geom_vline(xintercept = mx, colour = p$axis, linetype = "dashed", linewidth = 0.4) +
          geom_hline(yintercept = my, colour = p$axis, linetype = "dashed", linewidth = 0.4) +
          geom_point(aes(colour = hl, size = hl), alpha = 0.75) +
          geom_text(data = ee[hl == TRUE], aes(label = name), hjust = -0.25, vjust = -0.6,
                    colour = p$me, family = BASE_FAMILY, size = 3.4, fontface = "bold") +
          scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$me), guide = "none") +
          scale_size_manual(values = c(`FALSE` = 1.9, `TRUE` = 3.6), guide = "none") +
          labs(title = paste0("PISA 2022 ", zz, "：成績水準與社經梯度"),
               subtitle = "縱軸為 ESCS 每增加一個標準差對應的分數差；越高代表家庭背景影響越強",
               x = paste0(zz, "平均分數"), y = "ESCS 斜率（分／標準差）",
               caption = "虛線為全體中位數。右下象限為「高分且相對公平」，右上為「高分但不公平」") +
          theme_pisa(mode, grid = "both")
      }, w = 7.8, h = 5.4) })

    local({ gg <- res22[domain == dm][order(gap)]
      gg[, `:=`(name = factor(name, levels = name), sig = p_gap < 0.05, hl = CNT == "TAP")]
      zz <- dz
      save_fig(paste0("gap_", tag), function(mode) {
        p <- PAL[[mode]]
        ggplot(gg, aes(x = gap, y = name)) +
          geom_vline(xintercept = 0, colour = p$axis, linewidth = 0.5) +
          geom_errorbar(aes(xmin = gap - 1.96*se_gap, xmax = gap + 1.96*se_gap,
                            colour = hl, alpha = sig), orientation = "y",
                        width = 0, linewidth = 0.55) +
          geom_point(aes(colour = hl, size = hl, alpha = sig)) +
          scale_colour_manual(values = c(`FALSE` = p$series[1], `TRUE` = p$me), guide = "none") +
          scale_size_manual(values = c(`FALSE` = 1.5, `TRUE` = 2.8), guide = "none") +
          scale_alpha_manual(values = c(`FALSE` = 0.3, `TRUE` = 0.95), guide = "none") +
          labs(title = paste0("PISA 2022 ", zz, "：女生與男生的分數差"),
               subtitle = "正值代表女生較高；淡色表示差距未達統計顯著",
               x = "女生 − 男生（分）", y = NULL,
               caption = "橫線為 95% 信賴區間；同一輪次內的兩組比較不需納入連結誤差") +
          theme_pisa(mode, grid = "x") + theme(axis.text.y = element_text(size = 6))
      }, w = 7.5, h = 9.2) })

    for (fr in c(2015, 2018)) {
      dd <- TREND[domain == dm & from == fr]
      if (!nrow(dd)) next
      dd <- merge(dd, nm_map[, .(CNT, zh)], by = "CNT", all.x = TRUE)
      dd[, flip := p_naive < 0.05 & p_correct >= 0.05]
      dd <- dd[order(diff)]; dd[, zh := factor(zh, levels = unique(zh))]
      local({ x <- copy(dd); lee <- x$link_error[1]; zz <- dz; ff <- fr
        save_fig(sprintf("linkerr_%s_%d", tag, ff), function(mode) {
          p <- PAL[[mode]]
          ggplot(x, aes(y = zh)) +
            geom_vline(xintercept = 0, colour = p$axis, linewidth = 0.5) +
            geom_errorbar(aes(xmin = diff - 1.96*se_correct, xmax = diff + 1.96*se_correct),
                          orientation = "y", width = 0, linewidth = 0.5,
                          colour = p$series[1], alpha = 0.55) +
            geom_errorbar(aes(xmin = diff - 1.96*se_naive, xmax = diff + 1.96*se_naive),
                          orientation = "y", width = 0, linewidth = 1.7,
                          colour = p$series[1], alpha = 0.9) +
            geom_point(aes(x = diff, colour = flip), size = 1.9) +
            scale_colour_manual(values = c(`FALSE` = p$ink3, `TRUE` = "#d03b3b"),
                                labels = c("結論不變", "忽略連結誤差會下錯結論"), name = NULL) +
            labs(title = sprintf("%s：%d 年到 2022 年的變化", zz, ff),
                 subtitle = sprintf("粗線為忽略連結誤差的 95%% 區間，細線為正確區間（連結誤差 %.2f 分）", lee),
                 x = "分數變化", y = NULL,
                 caption = "細線比粗線長出的部分，就是連結誤差帶來的額外不確定性") +
            theme_pisa(mode, grid = "x") +
            theme(axis.text.y = element_text(size = 6), legend.position = "top")
        }, w = 7.8, h = 9.2) })
    }
  }
  invisible(res22)
}

# 一鍵重建全部圖表
build_all_figures <- function() {
  x <- build_figures()
  build_country_figures(x$pisa)
  log_msg("全部圖表完成；接著執行 python3 ~/PISA/R/build_site.py")
}
