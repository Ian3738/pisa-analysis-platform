# ============================================================
# 10_report_docx.R — 產出 APA 第七版格式的 Word 報告
#
# 版面規範：
#   中文字體 標楷體（BiauKaiTC）、西文與數字 Times New Roman、12 pt
#   兩倍行高、內文左右對齊、A4、上下左右邊界 2.54 公分
#   圖表編號與標題置於圖表上方，說明（註）置於下方 —— APA 第七版體例
#
# 註：APA 第七版正文原訂為靠左對齊、右側不齊，本文件依需求改為左右對齊。
# ============================================================
source("~/PISA/R/00_config.R")
suppressPackageStartupMessages({
  library(officer); library(flextable); library(data.table); library(jsonlite); library(png)
})

PNGDIR   <- file.path(PISA_ROOT, "web", "png")
DOCX_OUT <- file.path(PISA_ROOT, "output", "PISA分析報告_APA7.docx")

CJK   <- "BiauKaiTC"          # 標楷體
LATIN <- "Times New Roman"
PAGE_W <- 6.27                # A4 寬 8.27 吋減去左右各 1 吋

# ---- 字體與段落樣式 -------------------------------------------------------
tx <- function(size = 12, bold = FALSE, italic = FALSE, color = "#000000")
  fp_text(font.size = size, bold = bold, italic = italic, color = color,
          font.family = LATIN, eastasia.family = CJK,
          hansi.family = LATIN, cs.family = CJK)

P_BODY  <- fp_par(text.align = "justify", line_spacing = 2,
                  padding.top = 0, padding.bottom = 0, first_line = 0.35)
P_PLAIN <- fp_par(text.align = "justify", line_spacing = 2,
                  padding.top = 0, padding.bottom = 0)
P_CENTER<- fp_par(text.align = "center", line_spacing = 2)
P_LEFT  <- fp_par(text.align = "left", line_spacing = 2,
                  padding.top = 6, padding.bottom = 0)
P_NOTE  <- fp_par(text.align = "justify", line_spacing = 1.5,
                  padding.top = 3, padding.bottom = 12)
P_LABEL <- fp_par(text.align = "left", line_spacing = 1.5, padding.top = 12)
P_TITLE <- fp_par(text.align = "left", line_spacing = 1.5, padding.bottom = 4)

# ---- 版面元件 -------------------------------------------------------------
add_body <- function(d, s, indent = TRUE)
  body_add_fpar(d, fpar(ftext(s, tx()), fp_p = if (indent) P_BODY else P_PLAIN))

add_h1 <- function(d, s)
  body_add_fpar(d, fpar(ftext(s, tx(14, bold = TRUE)), fp_p = P_CENTER))

add_h2 <- function(d, s)
  body_add_fpar(d, fpar(ftext(s, tx(12, bold = TRUE)), fp_p = P_LEFT))

add_gap <- function(d) body_add_fpar(d, fpar(ftext("", tx(6)), fp_p = P_PLAIN))

# APA 第七版：編號列（粗體）、標題列（斜體），置於圖表上方
add_caption <- function(d, kind, num, title) {
  d <- body_add_fpar(d, fpar(ftext(sprintf("%s %d", kind, num), tx(12, bold = TRUE)),
                             fp_p = P_LABEL))
  body_add_fpar(d, fpar(ftext(title, tx(12, italic = TRUE)), fp_p = P_TITLE))
}

# 註置於圖表下方，「註.」二字為斜體
add_note <- function(d, s)
  body_add_fpar(d, fpar(ftext("註. ", tx(11, italic = TRUE)), ftext(s, tx(11)),
                        fp_p = P_NOTE))

# 圖：依原始像素比例縮放到版面寬度
add_figure <- function(d, num, file, title, note, max_h = 8.0) {
  p <- file.path(PNGDIR, paste0(file, ".png"))
  if (!file.exists(p)) stop("找不到圖檔：", p)
  dim_px <- dim(png::readPNG(p))          # 高, 寬, 通道
  w <- PAGE_W; h <- w * dim_px[1] / dim_px[2]
  if (h > max_h) { h <- max_h; w <- h * dim_px[2] / dim_px[1] }
  d <- add_caption(d, "圖", num, title)
  d <- body_add_img(d, p, width = w, height = h,
                    style = "centered")
  add_note(d, note)
}

# 表：APA 體例——只有頂線、標題列下方線、底線，無直線
apa_table <- function(dt, header_labels = NULL) {
  ft <- flextable(as.data.frame(dt))
  if (!is.null(header_labels)) ft <- set_header_labels(ft, values = header_labels)
  ft <- ft |>
    # ascii/hAnsi 設為 Times New Roman（數字與西文）；eastAsia 槽由 fix_eastasia()
    # 於文件寫出後統一改為標楷體——flextable 0.10 的 font() 會忽略 eastasia.family
    font(fontname = LATIN, part = "all") |>
    fontsize(size = 10, part = "all") |>
    bold(part = "header", bold = FALSE) |>
    border_remove() |>
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) |>
    hline_bottom(part = "header", border = fp_border(color = "black", width = 0.75)) |>
    hline_bottom(part = "body", border = fp_border(color = "black", width = 1)) |>
    padding(padding.top = 2, padding.bottom = 2, part = "all") |>
    align(align = "center", part = "header") |>
    align(align = "right", part = "body") |>
    align(j = 1, align = "left", part = "body") |>
    line_spacing(space = 1, part = "all") |>
    autofit() |>
    fit_to_width(PAGE_W)
  ft
}

# flextable 0.10 的 font() 不接受 eastasia.family（會把四個字型槽全設成同一個），
# 故於文件寫出後直接改寫 XML：ascii/hAnsi 留 Times New Roman 供數字與西文使用，
# eastAsia 與 cs 槽改為標楷體供中文使用。這是 Word 呈現中英混排的標準作法。
fix_eastasia <- function(path, cjk = CJK, latin = LATIN) {
  tmp <- file.path(tempdir(), paste0("docxfix_", basename(path)))
  unlink(tmp, recursive = TRUE); dir.create(tmp, recursive = TRUE)
  utils::unzip(path, exdir = tmp)
  f <- file.path(tmp, "word", "document.xml")
  x <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "")
  n0 <- lengths(regmatches(x, gregexpr(sprintf('w:eastAsia="%s"', latin), x)))
  x <- gsub(sprintf('w:eastAsia="%s"', latin), sprintf('w:eastAsia="%s"', cjk), x, fixed = TRUE)
  x <- gsub(sprintf('w:cs="%s"', latin), sprintf('w:cs="%s"', cjk), x, fixed = TRUE)
  writeLines(x, f, useBytes = TRUE)
  wd <- setwd(tmp); on.exit(setwd(wd), add = TRUE)
  unlink(path)
  utils::zip(path, list.files(".", recursive = TRUE, all.files = TRUE), flags = "-qr9X")
  setwd(wd)
  message(sprintf("  eastAsia 字型槽改為 %s：%d 處", cjk, n0))
  invisible(path)
}

add_table <- function(d, num, ft, title, note) {
  d <- add_caption(d, "表", num, title)
  d <- body_add_flextable(d, ft, align = "left")
  add_note(d, note)
}

# ============================================================
# 報告內容
# ============================================================
build_report <- function() {
  J     <- fromJSON(file.path(PISA_ROOT, "web", "pisa_data.json"))
  ZH    <- fromJSON(file.path(PISA_ROOT, "web", "country_zh.json"))
  MEANS <- as.data.table(J$means);  PROF  <- as.data.table(J$prof)
  GEN   <- as.data.table(J$gender); ESCJ  <- as.data.table(J$escs)
  TREND <- as.data.table(J$trend);  LE    <- as.data.table(J$linkErrors)
  CNM   <- as.data.table(J$countryNames)
  SCH   <- fromJSON(file.path(PISA_ROOT, "web", "school_data.json"))
  OPT   <- fromJSON(file.path(PISA_ROOT, "web", "optional_data.json"))
  TIM   <- fromJSON(file.path(PISA_ROOT, "web", "timss_data.json"))
  ICC   <- as.data.table(SCH$icc); R2B <- as.data.table(SCH$r2b); MUN <- as.data.table(SCH$mund)
  CRTd  <- as.data.table(OPT$crt); SUBd <- as.data.table(OPT$sub); FLTd <- as.data.table(OPT$flt)
  SUBN  <- as.data.table(OPT$subscales)
  IEA   <- fromJSON(file.path(PISA_ROOT, "web", "iea_data.json"))
  T4M   <- as.data.table(IEA$timss4_means); T4B <- as.data.table(IEA$timss4_bench)
  PRM   <- as.data.table(IEA$pirls_means);  PRS <- as.data.table(IEA$pirls_sub)
  ICM   <- as.data.table(IEA$iccs$means)
  ICN2  <- setNames(as.data.table(IEA$iccs$names)$zh, as.data.table(IEA$iccs$names)$CNT)
  PSN   <- as.data.table(IEA$pirls_subnames); POS <- as.data.table(IEA$position)
  IEAN  <- setNames(as.data.table(IEA$names)$zh, as.data.table(IEA$names)$CNT)
  TAL   <- fromJSON(file.path(PISA_ROOT, "web", "talis_data.json"))
  TLT   <- as.data.table(TAL$teachers); TLP <- as.data.table(TAL$principals)
  TL18  <- as.data.table(TAL$t2018$principals); TL18I <- as.data.table(TAL$t2018$idx)
  TLINV <- as.data.table(TAL$t2018$invariance)
  TLN   <- setNames(as.data.table(TAL$names)$zh, as.data.table(TAL$names)$CNT)
  TMM   <- as.data.table(TIM$means); TBEN <- as.data.table(TIM$bench)
  TNM   <- setNames(as.data.table(TIM$names)$zh, as.data.table(TIM$names)$CNT)
  gi <- function(cnt, cy, dm, col) { z <- ICC[CNT==cnt & cycle==cy & domain==dm]; if (nrow(z)) z[[col]] else NA }
  gr <- function(cnt, cy, dm, col) { z <- R2B[CNT==cnt & cycle==cy & domain==dm]; if (nrow(z)) z[[col]] else NA }
  gm <- function(cnt, dm, col)     { z <- MUN[CNT==cnt & domain==dm]; if (nrow(z)) z[[col]] else NA }

  nmm <- merge(unique(MEANS[, .(CNT, CNTRYID)]), CNM, by = "CNTRYID", all.x = TRUE)
  nmm[, zh := sapply(name, function(x) { v <- ZH[[x]]; if (is.null(v)) x else v })]
  nmm <- unique(nmm, by = "CNT"); NMV <- setNames(nmm$zh, nmm$CNT)

  DZ <- c(MATH = "數學", READ = "閱讀", SCIE = "科學")
  rk <- MEANS[cycle == 2022 & !is.na(mean)]
  rk[, rank := frank(-mean, ties.method = "min"), by = domain]
  TWR <- setNames(rk[CNT == "TAP"]$rank, rk[CNT == "TAP"]$domain)
  TWM <- dcast(MEANS[CNT == "TAP"], domain ~ cycle, value.var = c("mean", "se"))
  TWP <- dcast(PROF[CNT == "TAP"],  domain ~ cycle, value.var = c("below_L2", "above_L5"))
  FL  <- TREND[, .(n = .N, sig_naive = sum(p_naive < .05), sig_corr = sum(p_correct < .05),
                   flip = sum(p_naive < .05 & p_correct >= .05), le = link_error[1],
                   infl = max(se_correct / se_naive)), by = .(domain, from)]
  FLT <- TREND[, .(n = .N, flip = sum(p_naive < .05 & p_correct >= .05),
                   infl = max(se_correct / se_naive))]
  gv <- function(d, dm, col) d[domain == dm][[col]]

  doc <- read_docx()
  doc <- body_set_default_section(doc, prop_section(
    page_size = page_size(width = 8.27, height = 11.69),
    page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1,
                            header = .5, footer = .5, gutter = 0)))

  # ---------- 封面 ----------
  doc <- body_add_fpar(doc, fpar(ftext("", tx(12)), fp_p = P_CENTER))
  doc <- body_add_fpar(doc, fpar(ftext("", tx(12)), fp_p = P_CENTER))
  doc <- body_add_fpar(doc, fpar(
    ftext("大型數據分析平台：國際大型評比結果分析", tx(16, bold = TRUE)), fp_p = P_CENTER))
  doc <- body_add_fpar(doc, fpar(
    ftext("PISA、TIMSS、PIRLS、ICCS 與 TALIS 的加權估計、學校層級分解與跨評比對照", tx(12)), fp_p = P_CENTER))
  doc <- add_gap(doc)
  doc <- body_add_fpar(doc, fpar(ftext(
    sprintf("分析樣本：PISA %s 名、TIMSS %s 名、PIRLS %s 名、ICCS %s 名學生",
            format(J$meta$nStudents, big.mark = ","),
            format(TIM$meta$n_student + IEA$meta$timss_g4$n_student, big.mark = ","),
            format(IEA$meta$pirls$n_student, big.mark = ","),
            format(IEA$iccs$meta$n_student, big.mark = ",")), tx(11)),
    fp_p = P_CENTER))
  doc <- body_add_fpar(doc, fpar(ftext(
    "資料來源：OECD PISA 與 TALIS 公開使用檔、IEA TIMSS／PIRLS／ICCS 國際資料庫",
    tx(11)), fp_p = P_CENTER))
  doc <- body_add_fpar(doc, fpar(ftext(
    paste0("報告產生日期：", format(Sys.Date(), "%Y 年 %m 月 %d 日")), tx(12)), fp_p = P_CENTER))
  doc <- body_add_break(doc)

  # ---------- 一、研究方法 ----------
  doc <- add_h1(doc, "壹、研究方法")
  doc <- add_h2(doc, "一、資料來源與樣本")
  doc <- add_body(doc, sprintf(
    "本研究使用經濟合作暨發展組織（OECD）國際學生能力評量計畫（Programme for International Student Assessment, PISA）2015、2018 與 2022 三個輪次的公開使用檔（Public Use Files）。三輪合計 %s 名 15 歲學生，涵蓋 %d 個國家與經濟體。各輪參與數分別為 2015 年 73 個、2018 年 80 個、2022 年 80 個。臺灣三輪的學生樣本數分別為 7,708 人、7,243 人與 5,857 人。",
    format(J$meta$nStudents, big.mark = ","), J$meta$nCountries))
  doc <- add_body(doc,
    "須特別說明者，OECD 並未將越南 2018 年的合理推估值置於主學生檔，而是另以獨立檔案發布；本研究已將該檔併入，故越南 2018 年的估計值完整可用。")

  doc <- add_h2(doc, "二、抽樣設計與權重")
  doc <- add_body(doc,
    "PISA 採兩階段叢集抽樣：第一階段依學校規模機率抽取學校，第二階段於校內隨機抽取學生。由於各校與各生被抽中的機率不同，每筆資料均帶有最終學生權重（W_FSTUWT）。本研究所有估計值一律加權；未加權的統計量描述的是樣本而非母體，不具推論意義。")
  doc <- add_body(doc,
    "變異數估計採用 Fay 平衡重複半樣本法（balanced repeated replication, BRR），使用官方提供的 80 組重複權重（W_FSTURWT1 至 W_FSTURWT80），Fay 係數設為 0.5。此方法可正確反映叢集抽樣造成的設計效應；若逕以簡單隨機抽樣公式計算標準誤，將嚴重低估不確定性。")

  doc <- add_h2(doc, "三、合理推估值與變異數合併")
  doc <- add_body(doc,
    "PISA 的能力分數並非直接測得，而是以項目反應理論搭配潛在迴歸模型，自學生能力的後驗分配抽取 10 個合理推估值（plausible values）。本研究對每個推估值分別估計，再依 Rubin 規則合併，總變異數為抽樣變異與推估變異之和：")
  doc <- body_add_fpar(doc, fpar(
    ftext("V = V", tx(12)), ftext("抽樣", tx(9)), ftext(" + (1 + 1/M) × B", tx(12)),
    ftext("M", tx(9)), ftext("，M = 10", tx(12)), fp_p = P_CENTER))
  doc <- add_body(doc,
    "其中抽樣變異以 80 組重複權重估計，推估變異為 10 個推估值之間的變異。僅使用單一個推估值，或先將 10 個推估值平均再視為觀測值，兩者皆會低估標準誤。")

  doc <- add_h2(doc, "四、跨輪次比較與連結誤差")
  doc <- add_body(doc,
    "PISA 各輪分數雖位於同一連結量尺上，但量尺連結本身存在不確定性，稱為連結誤差（link error）。跨輪次差異檢定的標準誤應為三項之和開根號：")
  doc <- body_add_fpar(doc, fpar(
    ftext("SE", tx(12)), ftext("差異", tx(9)),
    ftext(" = √(SE₁² + SE₂² + 連結誤差²)", tx(12)), fp_p = P_CENTER))
  doc <- add_body(doc,
    "連結誤差與樣本大小無關，增加樣本並不能使其縮小。本研究所採用的數值取自 PISA 2022 技術報告 Annex Table 14.A.19（見表 3）。省略此項是趨勢分析最常見的錯誤，其後果詳見本報告第參節。")

  doc <- add_h2(doc, "五、分析工具與驗證")
  doc <- add_body(doc,
    "全部估計於 R 4.5.2 環境完成，圖表以 ggplot2 繪製。估計核心經三種獨立實作交叉驗證：自建函式庫、intsvy 套件，以及 survey 套件為基礎的複雜抽樣估計程序。以臺灣 2022 年數學為例，三者均得平均數 547.09、標準誤 3.778（抽樣成分 3.728、推估成分 0.611）。臺灣 2015 年三領域的估計值亦與 OECD 公布結果一致。")

  # ---------- 表 1 ----------
  t1 <- data.table(
    輪次 = c(2015, 2018, 2022),
    學生數 = format(c(519334, 612004, 613744), big.mark = ","),
    國家與經濟體數 = c(73, 80, 80),
    臺灣學生數 = format(c(7708, 7243, 5857), big.mark = ","),
    臺灣學校數 = c(214, 192, 182))
  doc <- add_table(doc, 1, apa_table(t1),
    "各輪次樣本結構",
    "學生數為未加權的樣本人數。加權後各輪代表該國全體 15 歲在學學生母體。2015 年參與數較少，係因部分國家自 2018 年起始加入。")

  doc <- body_add_break(doc)

  # ---------- 二、描述統計 ----------
  doc <- add_h1(doc, "貳、描述統計結果")

  doc <- add_figure(doc, 1, "desc_weight_effect",
    "PISA 2022 數學各國加權平均與未加權平均之差",
    "差值為加權平均減未加權平均，臺灣以深色標示。80 個國家與經濟體的差異中位數為 2.07 分，最大達 20.60 分（泰國，加權後較低）。臺灣為正向差異最大者：未加權平均 534.02 分，加權後 547.13 分，相差 13.11 分。此結果顯示忽略抽樣權重並非小數點後的誤差問題，而會實質改變國際比較的相對位置。")

  doc <- add_figure(doc, 2, "desc_density",
    "臺灣學生分數的加權分配（2015、2018、2022）",
    "本圖將 10 個合理推估值全數攤平並各給予十分之一權重，再以最終學生權重加權，此為描述能力分配的正確作法。三個領域在 2022 年的分配整體右移，惟分配寬度亦同步增加，顯示平均數上升的同時個別差異亦擴大。若先將 10 個推估值平均後再繪圖，會系統性低估分配的離散程度。")

  doc <- add_figure(doc, 3, "desc_box",
    "臺灣分數分布的加權五數概括",
    "盒為加權四分位距，鬚為加權第 5 與第 95 百分位，均以最終學生權重計算加權分位數，非樣本分位數。2022 年三領域的中位數皆高於 2018 年，惟盒身與鬚長亦同時增加，與圖 2 的分配形態一致。")

  doc <- add_figure(doc, 4, "desc_escs",
    "臺灣學生家庭社經地位（ESCS）的加權分布",
    "ESCS 為 OECD 建構之經濟社會文化地位綜合指標，由父母職業地位、父母教育程度與家庭教育資產三者合成，並標準化為 2022 年 OECD 國家平均為 0、標準差為 1。虛線標示該基準。須注意各輪原始 ESCS 的量尺並不相同，跨輪比較須改用 OECD 另行發布的 trend ESCS。")

  doc <- add_figure(doc, 5, "variance_decomp",
    "標準誤的兩個變異來源（PISA 2022）",
    "抽樣變異源自複雜抽樣設計，以 80 組 Fay 重複權重估計；推估變異源自能力值係自後驗分配抽取而非直接測得。在九個焦點經濟體中，推估變異占總變異的比例介於 1.99%（臺灣科學）至 39.89%（新加坡閱讀）之間，中位數為 5.97%。僅使用單一個推估值會完全遺漏此一成分。")

  doc <- body_add_break(doc)

  # ---------- 三、趨勢分析 ----------
  doc <- add_h1(doc, "參、趨勢分析結果")

  doc <- add_figure(doc, 6, "trend_focus",
    "東亞主要經濟體與對照國的各輪平均分數與 95% 信賴區間",
    sprintf("陰影帶為 95%% 信賴區間，臺灣以粗線標示。臺灣三輪數學平均分別為 %.1f、%.1f 與 %.1f 分；閱讀為 %.1f、%.1f 與 %.1f 分；科學為 %.1f、%.1f 與 %.1f 分。2018 年為三輪中的低點，選擇不同基期將導致不同的趨勢結論。信賴區間重疊並不等同於差異未達顯著，正式檢定須另納入連結誤差。",
      gv(TWM,"MATH","mean_2015"), gv(TWM,"MATH","mean_2018"), gv(TWM,"MATH","mean_2022"),
      gv(TWM,"READ","mean_2015"), gv(TWM,"READ","mean_2018"), gv(TWM,"READ","mean_2022"),
      gv(TWM,"SCIE","mean_2015"), gv(TWM,"SCIE","mean_2018"), gv(TWM,"SCIE","mean_2022")))

  t2 <- data.table(領域 = DZ[TWM$domain])
  for (cy in c(2015, 2018, 2022))
    t2[[as.character(cy)]] <- sprintf("%.1f (%.2f)", TWM[[paste0("mean_", cy)]],
                                      TWM[[paste0("se_", cy)]])
  tw18 <- TREND[CNT == "TAP" & from == 2018]
  setkey(tw18, domain)
  t2[["2018→2022 變化"]] <- sprintf("%+.1f", tw18[TWM$domain]$diff)
  t2[["p 值"]] <- sprintf("%.3f", tw18[TWM$domain]$p_correct)
  doc <- add_table(doc, 2, apa_table(t2), "臺灣三輪次平均分數與跨輪次檢定",
    "括號內為標準誤。變化欄為 2022 年減 2018 年。p 值以納入連結誤差之標準誤計算，數學連結誤差為 2.24 分、閱讀 1.47 分、科學 1.61 分。三個領域自 2018 年至 2022 年的進步均達統計顯著。惟若以 2015 年為基期，數學與科學的變化則未達顯著，顯示基期選擇對結論具決定性影響。")

  t3 <- data.table(比較 = sprintf("PISA %d → 2022", LE$from))
  for (d in c("MATH","READ","SCIE","FLIT")) {
    lab <- c(MATH="數學", READ="閱讀", SCIE="科學", FLIT="財金素養")[d]
    t3[[lab]] <- ifelse(is.na(LE[[d]]), "—", sprintf("%.2f", LE[[d]]))
  }
  doc <- add_table(doc, 3, apa_table(t3), "PISA 2022 與各輪次比較之連結誤差",
    "資料來源：PISA 2022 技術報告 Annex Table 14.A.19。破折號表示該領域於該輪次尚非主測領域，OECD 明示不可比較：數學最早可回溯至 2003 年、閱讀至 2000 年、科學至 2006 年。連結誤差數值差異甚大，2018 年至 2022 年的閱讀僅 1.47 分，2006 年至 2022 年則達 8.56 分。")

  doc <- add_body(doc, sprintf(
    "本研究對全部 %d 組跨輪次比較（3 個領域 × 2 個基期 × 各國）同時計算兩種標準誤：忽略連結誤差者與正確納入者。結果顯示，其中 %d 組在忽略連結誤差時判定為統計顯著，正確計算後則否；標準誤最多被低估至 %.2f 倍。此比例雖非多數，但足以說明省略連結誤差會實質提高型一錯誤率。",
    FLT$n, FLT$flip, FLT$infl))

  fn <- 7
  for (dm in c("MATH","READ","SCIE")) for (fr in c(2015, 2018)) {
    row <- FL[domain == dm & from == fr]
    doc <- add_figure(doc, fn, sprintf("linkerr_%s_%d", tolower(dm), fr),
      sprintf("%s：%d 年至 2022 年變化的兩種信賴區間對照", DZ[dm], fr),
      sprintf("粗線為忽略連結誤差的 95%% 信賴區間，細線為正確納入後的區間，兩者之差即為連結誤差帶來的額外不確定性；紅點標示結論因此改變的國家。本組比較的連結誤差為 %.2f 分，共 %d 個國家與經濟體參與比較。忽略連結誤差時有 %d 組判為顯著，正確計算後為 %d 組，其中 %d 組結論改變，標準誤最多被低估至 %.2f 倍。",
        row$le, row$n, row$sig_naive, row$sig_corr, row$flip, row$infl))
    fn <- fn + 1
  }

  flips <- TREND[p_naive < .05 & p_correct >= .05][order(domain, from, p_correct)]
  t4 <- data.table(
    `國家／經濟體` = NMV[flips$CNT], 領域 = DZ[flips$domain],
    基期 = flips$from, 變化 = sprintf("%+.1f", flips$diff),
    `SE 忽略` = sprintf("%.2f", flips$se_naive), `p 忽略` = sprintf("%.4f", flips$p_naive),
    `SE 正確` = sprintf("%.2f", flips$se_correct), `p 正確` = sprintf("%.4f", flips$p_correct))
  doc <- add_table(doc, 4, apa_table(t4), "因納入連結誤差而改變統計結論的比較",
    "下列各組於忽略連結誤差時判定為顯著（p < .05），正確計算後則未達顯著。變化欄為 2022 年減基期年之分數差。此表為圖 7 至圖 12 中紅點所標示者之完整清單。")

  doc <- body_add_break(doc)

  # ---------- 四、各國比較 ----------
  doc <- add_h1(doc, "肆、各國比較結果")
  doc <- add_body(doc, sprintf(
    "PISA 2022 共 80 個國家與經濟體參與三個核心領域。臺灣的表現在數學居第 %d 位、科學第 %d 位、閱讀第 %d 位。須注意排名之間的差距未必具統計意義，相鄰名次的信賴區間多有重疊。",
    TWR[["MATH"]], TWR[["SCIE"]], TWR[["READ"]]))

  for (i in seq_along(c("MATH","READ","SCIE"))) {
    dm <- c("MATH","READ","SCIE")[i]
    doc <- add_figure(doc, 12 + i, paste0("rank_", tolower(dm)),
      sprintf("PISA 2022 %s各國平均分數與 95%% 信賴區間", DZ[dm]),
      sprintf("橫線為 95%% 信賴區間，圓點為加權平均分數，臺灣以深色標示。臺灣%s平均 %.1f 分（SE = %.2f），在 80 個國家與經濟體中居第 %d 位。兩國信賴區間重疊時，其差異未必達統計顯著。",
        DZ[dm], gv(TWM, dm, "mean_2022"), gv(TWM, dm, "se_2022"), TWR[[dm]]))
  }

  m22 <- MEANS[cycle == 2022 & !is.na(mean)][order(-mean)]
  m22 <- m22[domain == "MATH"]
  t5 <- merge(m22[, .(CNT, mean, se, n)],
              PROF[cycle == 2022 & domain == "MATH", .(CNT, below_L2, above_L5)], by = "CNT")
  t5 <- merge(t5, ESCJ[cycle == 2022 & domain == "MATH", .(CNT, slope)], by = "CNT", all.x = TRUE)
  t5 <- t5[order(-mean)]
  t5out <- data.table(
    名次 = seq_len(nrow(t5)), `國家／經濟體` = NMV[t5$CNT],
    平均 = sprintf("%.1f", t5$mean), SE = sprintf("%.2f", t5$se),
    `未達 L2 (%)` = sprintf("%.1f", t5$below_L2),
    `L5 以上 (%)` = sprintf("%.1f", t5$above_L5),
    `ESCS 斜率` = ifelse(is.na(t5$slope), "—", sprintf("%.1f", t5$slope)),
    樣本數 = format(t5$n, big.mark = ","))
  doc <- add_table(doc, 5, apa_table(t5out), "PISA 2022 數學各國估計值一覽",
    "依平均分數排序。未達 L2 為未達基礎精熟水準（Level 2）的加權學生比率，L5 以上為達高表現水準的比率，切點取自 PISA 2022 技術報告 Annex Table 17.A.2。ESCS 斜率為家庭社經地位每增加一個標準差對應的分數變動，以合理推估值加權迴歸估計。閱讀與科學的對應數值請見隨附之互動式平台。")

  doc <- body_add_break(doc)

  # ---------- 五、公平性 ----------
  doc <- add_h1(doc, "伍、教育公平性結果")
  doc <- add_body(doc,
    "本節以兩個指標檢視教育公平性：其一為社經梯度，即家庭社經地位對成績的迴歸斜率，斜率越大表示家庭背景的影響越強；其二為精熟等級兩端的學生比率，用以判斷分數變化係整體平移或分配拉開。")
  doc <- add_body(doc, sprintf(
    "臺灣的數學社經梯度自 2015 年的 %.1f 分、2018 年的 %.1f 分，上升至 2022 年的 %.1f 分（每一個標準差）。2022 年 80 個國家與經濟體的中位數為 %.1f 分，臺灣明顯高於中位數。此結果顯示臺灣在平均成績上升的同時，家庭背景對學習成就的解釋力亦同步增強。",
    ESCJ[CNT=="TAP" & domain=="MATH" & cycle==2015]$slope,
    ESCJ[CNT=="TAP" & domain=="MATH" & cycle==2018]$slope,
    ESCJ[CNT=="TAP" & domain=="MATH" & cycle==2022]$slope,
    median(ESCJ[cycle==2022 & domain=="MATH"]$slope, na.rm = TRUE)))

  for (i in seq_along(c("MATH","READ","SCIE"))) {
    dm <- c("MATH","READ","SCIE")[i]
    doc <- add_figure(doc, 15 + i, paste0("escs_", tolower(dm)),
      sprintf("PISA 2022 %s：平均分數與社經梯度", DZ[dm]),
      sprintf("縱軸為 ESCS 每增加一個標準差對應的分數變動，橫軸為平均分數，虛線為全體中位數。右下象限代表高成就且相對公平，右上象限代表高成就但家庭背景影響強。%s領域全體中位數為 %.1f 分，全距自 %.1f 分至 %.1f 分，臺灣為 %.1f 分。",
        DZ[dm], median(ESCJ[cycle==2022 & domain==dm]$slope, na.rm=TRUE),
        min(ESCJ[cycle==2022 & domain==dm]$slope, na.rm=TRUE),
        max(ESCJ[cycle==2022 & domain==dm]$slope, na.rm=TRUE),
        ESCJ[CNT=="TAP" & cycle==2022 & domain==dm]$slope))
  }

  for (i in seq_along(c("MATH","READ","SCIE"))) {
    dm <- c("MATH","READ","SCIE")[i]
    doc <- add_figure(doc, 18 + i, paste0("prof_", tolower(dm)),
      sprintf("PISA 2022 %s：未達基礎水準與高表現學生比率", DZ[dm]),
      sprintf("左側為未達 Level 2（基礎精熟水準）的加權學生比率，右側為達 Level 5 以上者，依前者由高至低排序。臺灣%s未達 Level 2 的比率自 2018 年的 %.2f%% 上升至 2022 年的 %.2f%%，同期達 Level 5 以上者自 %.2f%% 上升至 %.2f%%。兩端比率同時上升，顯示變化並非整體平移，而是分配向兩側拉開。",
        DZ[dm], gv(TWP,dm,"below_L2_2018"), gv(TWP,dm,"below_L2_2022"),
        gv(TWP,dm,"above_L5_2018"), gv(TWP,dm,"above_L5_2022")))
  }

  for (i in seq_along(c("MATH","READ","SCIE"))) {
    dm <- c("MATH","READ","SCIE")[i]
    g <- GEN[CNT == "TAP" & cycle == 2022 & domain == dm]
    doc <- add_figure(doc, 21 + i, paste0("gap_", tolower(dm)),
      sprintf("PISA 2022 %s：女學生與男學生之分數差", DZ[dm]),
      sprintf("正值代表女學生分數較高，橫線為 95%% 信賴區間，淡色表示差異未達統計顯著。臺灣%s領域的性別差距為 %+.1f 分（SE = %.2f，p = %.3f）。同一輪次內的兩組比較位於同一次量尺連結上，故不需納入連結誤差。",
        DZ[dm], g$gap, g$se_gap, g$p))
  }

  doc <- body_add_break(doc)

  # ---------- 陸、學校層級 ----------
  doc <- add_h1(doc, "陸、學校層級分析結果")
  doc <- add_body(doc,
    "學生嵌套於學校，成績的總變異可依全變異數分解律精確拆成兩塊：同一所學校之內的差異（校內變異），與學校平均數之間的差異（校間變異）。兩者相加恆等於總變異。校間變異占總變異的比例，即為組內相關係數（ICC），是大型評比最常引用的公平性指標之一——數值越高，代表學生成績越取決於其就讀的學校而非個人條件。")
  doc <- add_body(doc, sprintf(
    "臺灣數學的校間變異比例自 2015 年的 %.1f%%、2018 年的 %.1f%%，上升至 2022 年的 %.1f%%。作為對照，分流體制明顯的荷蘭為 61.9%%、匈牙利 60.8%%，綜合型學制的芬蘭僅 11.6%%、冰島 11.8%%。臺灣在 2018 年至 2022 年之間的上升幅度值得注意，顯示校際隔離加深。",
    100*gi("TAP",2015,"MATH","icc"), 100*gi("TAP",2018,"MATH","icc"), 100*gi("TAP",2022,"MATH","icc")))

  doc <- add_figure(doc, 25, "trend_icc", "校間變異比例的跨輪次變化",
    sprintf("縱軸為校間變異占總變異的百分比，臺灣以粗線標示。臺灣數學自 2018 年的 %.1f%% 升至 2022 年的 %.1f%%。芬蘭三輪均維持在 12%% 上下，是綜合型學制的代表。分解為恆等式：校間加校內等於總變異；本研究的實作在未加權情形下已與單因子變異數分析的平方和逐項比對，差異量級為 10 的負 11 次方。",
      100*gi("TAP",2018,"MATH","icc"), 100*gi("TAP",2022,"MATH","icc")))

  fn <- 26
  for (dm in c("MATH","READ","SCIE")) {
    doc <- add_figure(doc, fn, paste0("icc_", tolower(dm)),
      sprintf("PISA 2022 %s：校間變異占總變異的比例", DZ[dm]),
      sprintf("橫線為 95%% 信賴區間。臺灣%s的校間變異比例為 %.1f%%，全體 80 個國家與經濟體的中位數為 %.1f%%。數值越高，代表學生成績越取決於其就讀的學校。",
        DZ[dm], 100*gi("TAP",2022,dm,"icc"), 100*median(ICC[cycle==2022 & domain==dm]$icc, na.rm=TRUE)))
    fn <- fn + 1
  }

  doc <- add_body(doc,
    "校間差異本身並不必然代表不公平，關鍵在於這些差異對應到什麼。若學校之間的成績落差幾乎就是學生社經組成落差的投影，則校際差異即為社會分層在教育體系中的再現。本研究以校平均分數對校平均 ESCS 的加權迴歸（學校依其學生權重總和加權）估計此一比例。")
  doc <- add_body(doc, sprintf(
    "臺灣數學的此一比例自 2015 年的 %.1f%%、2018 年的 %.1f%%，升至 2022 年的 %.1f%%。亦即校際成績落差中，有超過七成對應到學生組成的社經落差，且比例仍在上升。",
    100*gr("TAP",2015,"MATH","r2"), 100*gr("TAP",2018,"MATH","r2"), 100*gr("TAP",2022,"MATH","r2")))

  for (dm in c("MATH","READ","SCIE")) {
    doc <- add_figure(doc, fn, paste0("r2b_", tolower(dm)),
      sprintf("PISA 2022 %s：學校社經組成解釋的校間差異比例", DZ[dm]),
      sprintf("校平均分數對校平均 ESCS 迴歸的加權 R²，學校依學生權重總和加權，使結果代表母體而非樣本學校。臺灣%s為 %.1f%%，全體中位數 %.1f%%。",
        DZ[dm], 100*gr("TAP",2022,dm,"r2"), 100*median(R2B[cycle==2022 & domain==dm]$r2, na.rm=TRUE)))
    fn <- fn + 1
  }

  doc <- add_body(doc,
    "為釐清機制，本研究採 Mundlak 分解，將學生 ESCS 拆為兩個成分：其一為該生與同校同學的社經差距（校內成分），其二為該校的平均社經水準（校間成分）。兩者係數之差即脈絡效應，可解讀為：同一名學生若轉入社經背景較佳的學校，預期能多得幾分。")
  doc <- add_body(doc, sprintf(
    "臺灣 2022 年數學的校內個人效應為每一個標準差 %.1f 分，學校社經組成效應則高達 %.1f 分，脈絡效應 %.1f 分（SE = %.2f）。換言之，在臺灣「就讀哪一所學校」對成績的影響，約為「個人家庭背景在校內」影響的六倍。學校組成效應在全體國家中排名第三，僅次於荷蘭（157.9 分）與日本（134.3 分）。",
    gm("TAP","MATH","b_within"), gm("TAP","MATH","b_school"),
    gm("TAP","MATH","contextual"), gm("TAP","MATH","se_ctx")))

  for (dm in c("MATH","READ","SCIE")) {
    doc <- add_figure(doc, fn, paste0("mundlak_", tolower(dm)),
      sprintf("PISA 2022 %s：個人社經效應與學校社經組成效應", DZ[dm]),
      sprintf("橫軸為校內個人 ESCS 效應，縱軸為學校平均 ESCS 效應，虛線為兩者相等。落在虛線上方越遠，代表「就讀哪所學校」比「個人家庭背景」更決定成績。臺灣%s的兩個係數分別為 %.1f 與 %.1f 分。",
        DZ[dm], gm("TAP",dm,"b_within"), gm("TAP",dm,"b_school")))
    fn <- fn + 1
  }

  t6 <- ICC[cycle==2022 & domain=="MATH"][order(-icc)]
  t6 <- merge(t6, R2B[cycle==2022 & domain=="MATH", .(CNT, r2)], by="CNT", all.x=TRUE)
  t6 <- merge(t6, MUN[domain=="MATH", .(CNT, b_within, b_school, contextual)], by="CNT", all.x=TRUE)
  t6 <- t6[order(-icc)]
  t6out <- data.table(
    名次 = seq_len(nrow(t6)), `國家／經濟體` = NMV[t6$CNT],
    `校間變異 %` = sprintf("%.1f", 100*t6$icc), SE = sprintf("%.1f", 100*t6$se),
    `社經解釋 %` = ifelse(is.na(t6$r2), "—", sprintf("%.1f", 100*t6$r2)),
    `校內效應` = ifelse(is.na(t6$b_within), "—", sprintf("%.1f", t6$b_within)),
    `學校組成效應` = ifelse(is.na(t6$b_school), "—", sprintf("%.1f", t6$b_school)),
    `脈絡效應` = ifelse(is.na(t6$contextual), "—", sprintf("%.1f", t6$contextual)),
    學校數 = t6$n_sch)
  doc <- add_table(doc, 6, apa_table(t6out), "PISA 2022 數學各國學校層級指標",
    "依校間變異比例排序。校內效應與學校組成效應為 Mundlak 分解的兩個迴歸係數，單位為每一個 ESCS 標準差對應的分數；脈絡效應為兩者之差。閱讀與科學的對應數值請見隨附之互動式平台。")

  doc <- add_h2(doc, "方法交叉驗證：兩種估計標的")
  doc <- add_body(doc,
    "同一份臺灣 2022 年數學資料，本研究的設計基礎分解得校間變異比例 0.405，Mplus 的兩層隨機效果模型得 0.422（預設權重縮放）或 0.455（不縮放）。三者並非彼此矛盾，而是在估計不同的量。")
  doc <- add_body(doc,
    "設計基礎分解回答的是描述性問題：在全體 15 歲學生的成績變異中，有多少比例落在學校之間。它使用完整抽樣權重，且校間與校內恆等相加為總變異。模型基礎的變異成分則是階層模型中的潛在參數，其總和不必等於觀測變異，且會隨權重縮放方式改變——三者的總變異數本身即不相同（12,413、13,118、13,580），此即差異不在計算而在估計標的的直接證據。")
  doc <- add_body(doc,
    "迴歸係數的一致性則高得多：Mundlak 分解在本研究得校內效應 20.8、學校組成效應 131.6；Mplus 兩層模型得 19.9 與 125.0，差異來自 Mplus 僅使用單一個推估值且校層採隨機效果設定。Mplus 空模型的校間殘差變異自 5,537 降至 1,485，即學校社經組成解釋 73.2% 的校間變異；本研究的加權 R² 對同一問題給出 71.2%。兩條獨立路徑收斂至同一結論。")

  doc <- body_add_break(doc)

  # ---------- 柒、選考領域 ----------
  doc <- add_h1(doc, "柒、選考領域與數學分測驗結果")
  doc <- add_body(doc,
    "PISA 2022 除核心三領域外，另有創造思考與財金素養兩個選考領域，以及數學的八個分測驗（內容向度四項、歷程向度四項）。這些資料與核心領域同時發布，惟使用率遠低於核心領域。")

  twc <- CRTd[CNT=="TAP"]
  doc <- add_body(doc, sprintf(
    "臺灣參加創造思考評量，得分 %.1f（SE = %.2f），在 %d 個參與經濟體中排名第 %d。同一年臺灣數學排名第 3。名次落差最大的三個體制為澳門（差 19 名）、香港（18 名）與臺灣（13 名）；韓國僅差 4 名、新加坡兩項皆為第 1，可見此一落差並非東亞的共同現象，而是特定體制的特徵。",
    twc$crt, twc$se, nrow(CRTd), twc$rank))

  doc <- add_figure(doc, fn, "crt_vs_math", "數學表現與創造思考表現的對照",
    "橫軸為 PISA 2022 數學平均分數，縱軸為創造思考分數（0 至 60 分量尺），虛線為線性趨勢。落在趨勢線下方，代表創造思考低於同等數學水準所預期的表現。此結果對強調解題訓練的課程體制具政策意涵。"); fn <- fn + 1

  doc <- add_figure(doc, fn, "crt_rank", "PISA 2022 創造思考：各國平均與 95% 信賴區間",
    sprintf("共 %d 個國家與經濟體參加。須特別說明資料處理上的一項陷阱：創造思考的認知檔本身不含抽樣權重，權重存放於主學生檔，必須以學生編號併回後方能進行加權估計；直接以認知檔計算會得到未加權的結果。", nrow(CRTd))); fn <- fn + 1

  twsub <- merge(SUBd[CNT=="TAP"], SUBN, by.x="scale", by.y="code")
  doc <- add_body(doc, sprintf(
    "數學分測驗方面，臺灣八個向度的分數介於 %.1f 至 %.1f 分之間，全距僅 %.1f 分，輪廓異常平坦，並無明顯的相對強項或弱項。此一結果與部分國家在特定向度上有顯著優勢的形態不同。",
    min(twsub$mean), max(twsub$mean), max(twsub$mean) - min(twsub$mean)))

  doc <- add_figure(doc, fn, "subscale_profile", "PISA 2022 數學分測驗的相對強弱",
    "各分測驗分數減去該國整體數學分數，正值代表相對強項。內容向度包含變化與關係、數量、空間與形狀、不確定性與資料；歷程向度包含形成、應用、詮釋、推理。四個採紙筆施測的國家不產出分測驗分數，屬測驗設計使然而非資料缺漏。"); fn <- fn + 1

  doc <- add_figure(doc, fn, "flit_rank", "PISA 2022 財金素養：各國平均與 95% 信賴區間",
    sprintf("僅 %d 個國家與經濟體參加此選考領域，臺灣未參加。資料處理上須注意：該領域的壓縮檔內含三個資料檔，惟僅問卷檔含推估值與權重，試題檔與作答時間檔皆無。", nrow(FLTd))); fn <- fn + 1

  t7 <- CRTd[order(-crt)]
  mrk <- MEANS[cycle==2022 & domain=="MATH" & !is.na(mean)][order(-mean)]
  mrk[, mrank := .I]
  t7 <- merge(t7, mrk[, .(CNT, math = mean, mrank)], by="CNT", all.x=TRUE)[order(-crt)]
  t7out <- data.table(
    `國家／經濟體` = NMV[t7$CNT],
    創造思考 = sprintf("%.1f", t7$crt), 創造名次 = t7$rank,
    數學 = ifelse(is.na(t7$math), "—", sprintf("%.1f", t7$math)),
    數學名次 = ifelse(is.na(t7$mrank), NA, t7$mrank),
    名次落差 = ifelse(is.na(t7$mrank), "—", sprintf("%+d", t7$rank - t7$mrank)))
  doc <- add_table(doc, 7, apa_table(t7out), "PISA 2022 創造思考與數學表現對照",
    "依創造思考分數排序。名次落差為創造思考名次減數學名次，正值代表創造思考的相對表現落後於數學。創造思考採 0 至 60 分量尺，與數學的 PISA 量尺不同，不可直接比較分數，僅能比較相對位置。")

  doc <- body_add_break(doc)

  # ---------- 捌、TIMSS ----------
  doc <- add_h1(doc, "捌、IEA 系列評比結果與跨評比對照")
  doc <- add_body(doc, sprintf(
    "為避免單一評比的結論受其測驗取向影響，本研究另納入國際教育成就評鑑協會（IEA）主辦的國際數學與科學教育成就趨勢調查（TIMSS）2023 年八年級資料，共 %d 個國家與經濟體、%s 名學生。TIMSS 測量的是各國課程實際教授的內容，PISA 測量的是將知識應用於真實情境的能力，兩者評量的並非同一構念。",
    TIM$meta$n_country, format(TIM$meta$n_student, big.mark=",")))

  doc <- add_h2(doc, "一、TIMSS 與 PISA 的設計差異")
  doc <- add_body(doc,
    "兩套評比的抽樣與計分設計不同，直接沿用 PISA 的分析程式雖不會產生錯誤訊息，卻會得到錯誤的標準誤。主要差異有三：其一，TIMSS 每個量尺提供 5 個合理推估值，PISA 為 10 個；其二，TIMSS 的變異數以 JK2 折刀法估計，依 JKZONE 與 JKREP 兩欄即時建構重複權重，PISA 則使用 Fay 平衡重複半樣本法搭配 80 組現成的重複權重；其三，權重欄位名稱不同。")
  doc <- add_body(doc,
    "JK2 的作法為：將學校在每個折刀區內兩兩配對，對第 h 區產生一組重複權重——該區中 JKREP 為 1 者權重加倍、為 0 者歸零，其餘各區維持原權重；變異數為各區估計值與全樣本估計值離差平方和。此處不含 Fay 法的縮放係數，因加倍與歸零本身已內含縮放。")
  doc <- add_body(doc,
    "本研究為此另行撰寫估計核心，並以獨立實作交叉驗證：臺灣八年級數學平均 602.430，標準誤 3.074（抽樣成分 2.758、推估成分 1.359，93 組重複權重），兩者逐位數一致。")

  t8 <- data.table(
    資料庫 = c("PISA", "TIMSS", "PIRLS", "ICCS", "TALIS", "TASA", "TEPS／TEPS-B"),
    設計 = c("重複橫斷","重複橫斷","重複橫斷","重複橫斷","重複橫斷","重複橫斷","追蹤（panel）"),
    對象 = c("15 歲學生","四與八年級","四年級閱讀","八年級公民","教師與校長",
             "國中小各年級","同一批學生多波"),
    推估值 = c("10 個","5 個","5 個","5 個","無","依年份","多波測量"),
    變異數估計 = c("Fay BRR（80 組）","JK2 折刀","JK2 折刀","JK2 折刀",
                   "Fay BRR（100 組）","依技術報告","追蹤樣本權重"),
    取得方式 = c("直接下載","直接下載","直接下載","需接受授權","直接下載","需申請","需申請"))
  doc <- add_table(doc, 8, apa_table(t8), "七個大型教育資料庫的設計比較",
    "TIMSS、PIRLS 與 ICCS 同屬 IEA 系列，設計一致故可共用估計核心；TALIS 為教師與校長調查，不測學生能力因而沒有合理推估值，其 Fay 係數為本研究自資料判定（重複權重與最終權重的比值集中於 0.5 與 1.5）。TEPS 為其中唯一的追蹤設計，自 2001 年起追蹤同一批學生，因此也是唯一能進行個人成長軌跡、交叉延宕模型與個體固定效果分析的資料庫。PISA 與 TIMSS 皆為重複橫斷設計，每輪抽取不同學生，僅能進行群體層次的趨勢比較。")

  doc <- add_h2(doc, "二、TIMSS 2023 八年級結果")
  twt <- TMM[CNT=="TWN"]
  doc <- add_body(doc, sprintf(
    "臺灣八年級數學平均 %.1f 分（SE = %.2f），在 %d 個參與經濟體中排名第 %d，僅次於新加坡的 605.3 分；科學平均 %.1f 分（SE = %.2f），同樣排名第 %d。",
    twt[domain=="MATH"]$mean, twt[domain=="MATH"]$se, TIM$meta$n_country, twt[domain=="MATH"]$rank,
    twt[domain=="SCIE"]$mean, twt[domain=="SCIE"]$se, twt[domain=="SCIE"]$rank))

  for (dm in c("MATH","SCIE")) {
    zz <- c(MATH="數學", SCIE="科學")[dm]
    doc <- add_figure(doc, fn, paste0("timss_rank_", tolower(dm)),
      sprintf("TIMSS 2023 八年級%s：各國平均與 95%% 信賴區間", zz),
      sprintf("以 5 個合理推估值搭配 JK2 折刀法估計，虛線為四個國際基準點（400、475、550、625 分）。臺灣%s平均 %.1f 分（SE = %.2f），排名第 %d。",
        zz, twt[domain==dm]$mean, twt[domain==dm]$se, twt[domain==dm]$rank))
    fn <- fn + 1
  }

  tb <- TBEN[CNT=="TWN"]
  doc <- add_body(doc, sprintf(
    "以國際基準點觀之，臺灣八年級學生達數學低標（400 分）者占 %.1f%%、中標（475 分）%.1f%%、高標（550 分）%.1f%%、高階（625 分）%.1f%%。達高階基準的比率在全體經濟體中排名第二，僅次於新加坡的 46.3%%。科學的對應比率分別為 %.1f%%、%.1f%%、%.1f%% 與 %.1f%%。",
    tb[domain=="MATH" & bench=="Low"]$pct, tb[domain=="MATH" & bench=="Intermediate"]$pct,
    tb[domain=="MATH" & bench=="High"]$pct, tb[domain=="MATH" & bench=="Advanced"]$pct,
    tb[domain=="SCIE" & bench=="Low"]$pct, tb[domain=="SCIE" & bench=="Intermediate"]$pct,
    tb[domain=="SCIE" & bench=="High"]$pct, tb[domain=="SCIE" & bench=="Advanced"]$pct))

  for (dm in c("MATH","SCIE")) {
    zz <- c(MATH="數學", SCIE="科學")[dm]
    doc <- add_figure(doc, fn, paste0("timss_bench_", tolower(dm)),
      sprintf("TIMSS 2023 八年級%s：達各國際基準的學生比率", zz),
      sprintf("四個基準為巢狀關係，達高階者必然亦達其餘三標，故長條相互覆蓋而非堆疊。依達高階基準的比率排序。臺灣%s達高階基準者占 %.1f%%。",
        zz, tb[domain==dm & bench=="Advanced"]$pct))
    fn <- fn + 1
  }

  XC <- as.data.table(TIM$cross)
  doc <- add_figure(doc, fn, "pisa_vs_timss", "PISA 2022 與 TIMSS 2023 數學表現的對照",
    sprintf("橫軸為 PISA 2022 數學（15 歲，應用素養），縱軸為 TIMSS 2023 八年級數學（課程本位），虛線為線性趨勢。共 %d 個同時參加兩項評比的經濟體，兩者相關係數為 %.3f。兩套量尺不同，不可直接相減，此圖呈現的是相對位置。臺灣在兩項評比中均居前列，與創造思考的第 %d 名形成對比。",
      nrow(XC), cor(XC$pisa, XC$timss), twc$rank)); fn <- fn + 1

  doc <- add_h2(doc, "三、TIMSS 2023 四年級與 PIRLS 2021")
  t4tw <- T4M[CNT=="TWN"]; prtw <- PRM[CNT=="TWN"]
  doc <- add_body(doc, sprintf(
    "為避免結論受單一年級或單一學科取向影響，本研究另納入 TIMSS 2023 四年級（%d 個經濟體、%s 名學生）與 PIRLS 2021 四年級閱讀（%d 個經濟體、%s 名學生）。三套 IEA 評比的抽樣與計分設計相同，均為 5 個合理推估值搭配 JK2 折刀法，故可共用同一估計核心。",
    IEA$meta$timss_g4$n_country, format(IEA$meta$timss_g4$n_student, big.mark=","),
    IEA$meta$pirls$n_country, format(IEA$meta$pirls$n_student, big.mark=",")))
  doc <- add_body(doc, sprintf(
    "臺灣四年級數學平均 %.1f 分排名第 %d、科學 %.1f 分排名第 %d，表現與八年級一致。惟四年級閱讀（PIRLS）平均 %.1f 分（SE = %.2f）僅排名第 %d，與數理科目的排名形成明顯落差。須特別說明，TIMSS 四年級與八年級為獨立抽取的樣本，並非同一批學生的追蹤，兩者分數之差不可解讀為成長。",
    t4tw[domain=="MATH"]$mean, t4tw[domain=="MATH"]$rank,
    t4tw[domain=="SCIE"]$mean, t4tw[domain=="SCIE"]$rank,
    prtw$mean, prtw$se, prtw$rank))

  doc <- add_figure(doc, fn, "taiwan_position", "臺灣在九項國際評比中的相對位置",
    "各評比參與國數不同，故以百分位表示相對位置，100% 代表第 1 名。臺灣在數學與科學各項均在 96 百分位以上，惟四年級閱讀落在 78.5、創造思考 76.2。此一落差並非量測誤差，而是穩定出現於不同評比、不同年齡與不同主辦單位的一致形態，具政策意涵。"); fn <- fn + 1

  for (dm in c("MATH","SCIE")) {
    zz <- c(MATH="數學", SCIE="科學")[dm]
    doc <- add_figure(doc, fn, paste0("timss4_rank_", tolower(dm)),
      sprintf("TIMSS 2023 四年級%s：各國平均與 95%% 信賴區間", zz),
      sprintf("臺灣四年級%s平均 %.1f 分（SE = %.2f），排名第 %d。虛線為四個國際基準點。四年級與八年級為獨立樣本，不可解讀為同一批學生的成長。",
        zz, t4tw[domain==dm]$mean, t4tw[domain==dm]$se, t4tw[domain==dm]$rank))
    fn <- fn + 1
  }

  doc <- add_figure(doc, fn, "pirls_rank", "PIRLS 2021 四年級閱讀：各國平均與 95% 信賴區間",
    sprintf("臺灣平均 %.1f 分（SE = %.2f），在 %d 個參與者中排名第 %d。部分參與者為次國家層級的標竿實體，如莫斯科市、加拿大各省與比利時語區。本研究採數位施測（digitalPIRLS）的主樣本；紙筆檔為橋接研究，用於連結歷年趨勢，未納入本次估計。",
      prtw$mean, prtw$se, nrow(PRM), prtw$rank)); fn <- fn + 1

  prsub <- merge(PRS[CNT=="TWN"], PSN, by.x="scale", by.y="code")[order(-rel)]
  doc <- add_figure(doc, fn, "pirls_subscale", "PIRLS 2021 閱讀分測驗的相對強弱",
    sprintf("各分測驗分數減去該國整體閱讀分數，正值代表相對強項。臺灣四年級學生在「%s」相對強（%+.1f 分），在「%s」相對弱（%+.1f 分）。此一形態與 PISA 創造思考的相對落後，可能指向同一個課程面向。",
      prsub$zh[1], prsub$rel[1], prsub$zh[nrow(prsub)], prsub$rel[nrow(prsub)])); fn <- fn + 1

  doc <- add_h2(doc, "四、ICCS 2022 公民知識")
  ictw <- ICM[CNT=="TWN"]
  doc <- add_body(doc, sprintf(
    "國際公民教育與素養調查（ICCS）由 IEA 主辦，測量八年級學生的公民與公民素養知識。2022 年共 %d 個參與者、%s 名學生，其抽樣與計分設計與 TIMSS 及 PIRLS 相同，可共用同一估計核心。",
    IEA$iccs$meta$n_country, format(IEA$iccs$meta$n_student, big.mark=",")))
  doc <- add_body(doc, sprintf(
    "臺灣公民知識平均 %.1f 分（SE = %.2f），在 %d 個參與者中排名第 %d，領先第二名瑞典 %.1f 分。這是臺灣在本研究涵蓋的十項國際評比中相對位置最高的一項，與創造思考的第 %d 名形成強烈對比。",
    ictw$mean, ictw$se, nrow(ICM), ictw$rank,
    ictw$mean - ICM[rank==2]$mean, twc$rank))

  doc <- add_figure(doc, fn, "iccs_rank", "ICCS 2022 公民知識：各參與者平均與 95% 信賴區間",
    sprintf("臺灣 %.1f 分（SE = %.2f）居首。參與者中包含德國的兩個邦（什列斯威－霍爾斯坦、北萊茵－西發利亞），屬次國家層級的參與實體。折刀區數依各參與者的抽樣設計而異，介於 15 至 75 之間。",
      ictw$mean, ictw$se)); fn <- fn + 1

  t10out <- data.table(
    評比 = POS$label, 名次 = POS$rank, 參與者數 = POS$total,
    `百分位 %` = sprintf("%.1f", POS$pct), 類別 = POS$kind)
  doc <- add_table(doc, 9, apa_table(t10out), "臺灣在九項國際評比中的相對位置",
    "百分位以 100 乘以（1 減去（名次減 1）除以參與者數）計算，100% 代表第 1 名。各評比的量尺、施測年齡與主辦單位不同，分數不可直接比較，此表僅比較相對位置。")

  t9 <- TMM[domain=="MATH"][order(rank)]
  t9 <- merge(t9, TBEN[domain=="MATH" & bench=="Advanced", .(CNT, adv = pct)], by="CNT")
  t9 <- merge(t9, TMM[domain=="SCIE", .(CNT, scie = mean, srank = rank)], by="CNT")[order(rank)]
  t9out <- data.table(
    名次 = t9$rank, `國家／經濟體` = TNM[t9$CNT],
    數學 = sprintf("%.1f", t9$mean), SE = sprintf("%.2f", t9$se),
    `高階 %` = sprintf("%.1f", t9$adv),
    科學 = sprintf("%.1f", t9$scie), 科學名次 = t9$srank,
    樣本數 = format(t9$n, big.mark=","))
  doc <- add_table(doc, 10, apa_table(t9out), "TIMSS 2023 八年級各國估計值",
    "依數學平均分數排序。高階欄為達 625 分國際基準的加權學生比率。標準誤以 5 個合理推估值搭配 JK2 折刀法估計，折刀區數依各國抽樣設計而異，介於 50 至 125 之間。")

  doc <- body_add_break(doc)

  # ---------- 玖、TALIS ----------
  doc <- add_h1(doc, "玖、教師與校長調查結果")
  doc <- add_h2(doc, "一、資料與估計方法")
  doc <- add_body(doc, sprintf(
    "教學與學習國際調查（Teaching and Learning International Survey, TALIS）由 OECD 主辦，調查對象為國中階段的教師與校長，而非學生。TALIS 不施測學業成就，因此不產生合理推估值，量表分數由題項直接合成。變異數以 Fay 平衡重複半樣本法估計，重複權重為 %d 組（PISA 為 80 組），Fay 係數 k = 0.5。",
    TAL$meta$G))
  doc <- add_body(doc, sprintf(
    "本研究納入兩個輪次。TALIS 2024 共 %d 個參與者、%s 名教師與 %s 名校長，臺灣未參加該輪。臺灣參加的是 TALIS 2018，樣本為 %d 位國中校長。兩輪的量表題項與計分方式不同，不可跨輪比較，故以下分別呈現。",
    TAL$meta$n_country, format(TAL$meta$n_teacher, big.mark = ","),
    format(TAL$meta$n_principal, big.mark = ","), TAL$t2018$meta$n_principal))
  doc <- add_body(doc,
    "TALIS 的技術文件僅載明變異數以平衡重複半樣本法估計，未直接指明係採傳統 BRR 或 Fay 法，而兩者的縮放係數不同。本研究不倚賴文件記載，改以資料本身判定：檢查重複權重與最終權重的比值，其取值集中於 0.5 與 1.5，對應 Fay 係數 k = 0.5；若為傳統 BRR，比值應集中於 0 與 2。")
  doc <- add_body(doc,
    "以 survey 套件交叉驗證時另有一項易被忽略之處。該套件的 svrepdesign 預設 mse = FALSE，離差取自重複估計值的平均，而技術報告要求取自全樣本估計值。設定 mse = TRUE 後，本研究的實作與其差異為 5.55 × 10 的負 17 次方，屬浮點運算誤差；採預設值則有約 2% 的差距。")

  doc <- add_h2(doc, "二、TALIS 2024 國際結果")
  doc <- add_figure(doc, fn, "talis_selfeff",
    "TALIS 2024 教師自我效能感：各參與者平均與 95% 信賴區間",
    sprintf("共 %d 個參與者。日本教師的自我效能感 %.2f 為全體最低，且與第二低者有明顯差距。東亞四個參與體制中，越南反而偏高。臺灣未參加此輪。",
            TAL$meta$n_country,
            min(TLT[idx == "T4SELF"]$mean))); fn <- fn + 1
  doc <- add_figure(doc, fn, "talis_scatter",
    "教師自我效能感與工作滿意度的國家層級關聯",
    sprintf("每一點為一個參與者的加權平均。兩者呈正向關聯，惟日本、韓國與新加坡系統性地落在左下象限：學生成就名列前茅的體制，其教師的自我效能感與工作滿意度反而偏低。日本工作滿意度 %.2f 亦為全體最低。",
            min(TLT[idx == "T4JOBSAT"]$mean))); fn <- fn + 1
  doc <- add_figure(doc, fn, "talis_prin_tch",
    "教師問卷與校長問卷估計值的國家層級並列",
    "教師問卷與校長問卷分屬不同樣本，使用不同的權重與重複權重組（TCHWGT 與 SCHWGTC），兩者不可於個體層次配對。本圖呈現的是同一參與者在兩份問卷上的國家層級估計值，非個體層次的對應關係。"); fn <- fn + 1

  doc <- add_h2(doc, "三、量表的測量不變性：一個不會報錯的陷阱")
  doc <- add_body(doc, sprintf(
    "分析 TALIS 2018 校長問卷時，本研究發現 %d 個量表中有 %d 個算出的各國加權平均幾乎完全相同。以校園違規與暴力量表（T3PDELI）為例，個體分數介於 2.277 至 20.323，未加權的各國平均亦介於 6.763 至 7.440，但加權後 %d 個參與者的平均全部落在 6.8500 至 6.8504 之間，全距僅 0.001。",
    nrow(TLINV), TLINV[range < 0.01, .N], TL18[idx == "T3PAUTS", .N]))
  doc <- add_body(doc,
    "本研究先以手算的加權平均逐國核對，結果與估計核心完全一致，確認並非程式錯誤後方回頭檢視量表本身。原因記載於變數標籤：這些量表標示為 Configural 或 Metric，即僅達構形不變性或metric不變性。對於未達純量不變性（scalar invariance）的量表，TALIS 於各參與者內部進行中心化，各國平均因而被設計成相等，比較其平均數等同於比較捨入誤差。")
  doc <- add_body(doc,
    "若未查核標籤而直接排序，會得到「臺灣校園違規程度在 47 個參與者中排名第 3 高」這類結論——分數差異在小數點後第四位，卻被轉換成名次。此一錯誤不會產生任何警告訊息，估計程序本身完全正確，錯的是把不可比較的量表拿來比較。本研究因此僅採用實質具跨國變異的九個量表進行後續分析，其全距介於 1.1 至 2.8，相異值 43 至 47 個。")
  doc <- add_body(doc,
    "標示為部分純量不變的教學領導量表（T3PLEADS）雖有 3.206 的全距，但 47 個參與者僅呈現 12 個相異值，顯示多數參與者仍被中心化，其名次不可靠，故一併排除。TALIS 2024 的量表標籤未帶不變性限定詞，且實際變異充分（教師自我效能感介於 7.14 至 12.04，55 個參與者呈現 55 個相異值），本章第二節的跨國比較不受此問題影響。")

  tinv <- TLINV[order(range)][, .(
    量表 = v, 不變性 = ifelse(不變性 == "" | is.na(不變性), "未標示", 不變性),
    全距 = sprintf("%.3f", range), 相異值數 = distinct, 參與者數 = n_country)]
  doc <- add_table(doc, 11, apa_table(head(tinv, 20)),
    "TALIS 2018 校長量表的測量不變性層級與實際跨國變異",
    "依全距由小至大排序，列出前 20 個量表。全距為各參與者加權平均的最大值減最小值。全距趨近於零者，係因 TALIS 於各參與者內部中心化，其平均數不具跨國比較意義。相異值數為四捨五入至小數點後三位後的相異平均數個數。")

  doc <- add_h2(doc, "四、TALIS 2018 臺灣校長結果")
  tw18 <- merge(TL18[CNT == "TWN"], TL18I, by.x = "idx", by.y = "code")[order(rank)]
  doc <- add_body(doc, sprintf(
    "臺灣在 TALIS 2018 的 %d 位校長樣本上呈現一組互補的形態（各量表有效樣本 %d 至 %d 位）。資源面相對充裕：教材不足排第 %d、教學人力不足排第 %d、整體資源不足排第 %d（共 %d 個參與者，名次依短缺程度由高至低排列，故名次越後代表短缺越少）。自主權面則相對受限：教育政策自主排第 %d、預算自主排第 %d，均低於全體中位數；人事自主為唯一略高於中位數者，排第 %d。",
    TAL$t2018$meta$n_twn, min(tw18$n), max(tw18$n),
    tw18[idx == "T3PLACMA"]$rank, tw18[idx == "T3PLACPE"]$rank,
    tw18[idx == "T3PLACRE"]$rank, tw18[idx == "T3PAUTS"]$ntot,
    tw18[idx == "T3PAUTP"]$rank, tw18[idx == "T3PAUTB"]$rank,
    tw18[idx == "T3PAUTS"]$rank))
  doc <- add_figure(doc, fn, "talis18_tw",
    "TALIS 2018 臺灣校長指標與全體中位數之差",
    sprintf("正值代表高於全體中位數。共 %d 個參與者，僅採實質可跨國比較的九個量表。資源短缺類的名次依短缺程度由高至低排列，故名次越後代表短缺程度越低。臺灣未參加 TALIS 2024，故本圖無法與該輪對照。",
            tw18[idx == "T3PAUTS"]$ntot)); fn <- fn + 1

  t11 <- data.table(
    指標 = tw18$zh, 臺灣 = sprintf("%.2f", tw18$mean), SE = sprintf("%.3f", tw18$se),
    名次 = tw18$rank, 參與者數 = tw18$ntot,
    `百分位 %` = sprintf("%.1f", 100 * (1 - (tw18$rank - 1) / tw18$ntot)))
  doc <- add_table(doc, 12, apa_table(t11), "TALIS 2018 臺灣校長各指標估計值與國際位置",
    sprintf("標準誤以 Fay 平衡重複半樣本法估計，%d 組重複權重，Fay 係數 k = 0.5，權重為 SCHWGT 與 SRWGT1 至 SRWGT%d。名次依分數由高至低排列；資源短缺類量表分數越高代表短缺越嚴重，故該類名次越後代表狀況越佳。",
            TAL$meta$G, TAL$meta$G))

  doc <- body_add_break(doc)

  # ---------- 拾、結論與限制 ----------
  doc <- add_h1(doc, "拾、結論與研究限制")
  doc <- add_h2(doc, "一、主要發現")
  doc <- add_body(doc, sprintf(
    "第一，臺灣學生在兩套國際評比中均居前列，惟兩者測量的構念不同。PISA 2022 數學排名第 %d、科學第 %d、閱讀第 %d；TIMSS 2023 八年級數學與科學均排名第 2。前者測應用素養，後者測課程本位的學習成果，兩項評比在 %d 個共同參與的經濟體間相關達 %.3f，臺灣在兩者皆表現優異，顯示課程實施與素養轉化均有成效。",
    TWR[["MATH"]], TWR[["SCIE"]], TWR[["READ"]], nrow(XC), cor(XC$pisa, XC$timss)))
  doc <- add_body(doc, sprintf(
    "第二，創造思考是明顯的例外。臺灣創造思考在 %d 個經濟體中排名第 %d，與數學的第 %d 名落差 %d 個名次。名次落差最大的三個體制為澳門、香港與臺灣，而韓國僅差 4 名、新加坡兩項皆為第 1，可見此非東亞的共同現象。此結果指向特定課程與評量取向的影響，值得政策層面關注。",
    nrow(CRTd), twc$rank, TWR[["MATH"]], twc$rank - TWR[["MATH"]]))
  doc <- add_body(doc, sprintf(
    "第三，成績提升伴隨分配擴大而非整體平移。臺灣數學未達基礎水準者的比率自 2018 年的 %.2f%% 上升至 2022 年的 %.2f%%，同期達高表現水準者自 %.2f%% 上升至 %.2f%%，兩端同時擴張。",
    gv(TWP,"MATH","below_L2_2018"), gv(TWP,"MATH","below_L2_2022"),
    gv(TWP,"MATH","above_L5_2018"), gv(TWP,"MATH","above_L5_2022")))
  doc <- add_body(doc, sprintf(
    "第四，教育機會的分化同時出現在個人與學校兩個層次。個人層次上，社經梯度自 2018 年的 %.1f 分升至 2022 年的 %.1f 分（每一個 ESCS 標準差），高於全體中位數 %.1f 分。學校層次上，校間變異比例自 %.1f%% 升至 %.1f%%，且校間差異中有 %.1f%% 對應到學生組成的社經落差，較 2015 年的 %.1f%% 為高。",
    ESCJ[CNT=="TAP" & domain=="MATH" & cycle==2018]$slope,
    ESCJ[CNT=="TAP" & domain=="MATH" & cycle==2022]$slope,
    median(ESCJ[cycle==2022 & domain=="MATH"]$slope, na.rm = TRUE),
    100*gi("TAP",2018,"MATH","icc"), 100*gi("TAP",2022,"MATH","icc"),
    100*gr("TAP",2022,"MATH","r2"), 100*gr("TAP",2015,"MATH","r2")))
  doc <- add_body(doc, sprintf(
    "第五，Mundlak 分解顯示分化的主要載體是學校而非家庭。臺灣的校內個人社經效應為每一個標準差 %.1f 分，學校社經組成效應則達 %.1f 分，脈絡效應 %.1f 分（SE = %.2f）。就讀哪一所學校對成績的影響約為個人家庭背景在校內影響的六倍，學校組成效應在全體國家中排名第三。此結果指出，若政策目標為縮小成就落差，介入點應在學校之間的資源與生源分布，而非僅止於個別學生的補救。",
    gm("TAP","MATH","b_within"), gm("TAP","MATH","b_school"),
    gm("TAP","MATH","contextual"), gm("TAP","MATH","se_ctx")))

  doc <- add_body(doc, sprintf(
    "第六，跨五套評比十個項目的相對位置呈現一致形態。臺灣公民知識居 24 個參與者之首（百分位 100.0），數學與科學各項均在 96 百分位以上，惟 PIRLS 四年級閱讀落在 %.1f 百分位、PISA 創造思考 %.1f 百分位。此一落差跨越不同年齡、不同主辦單位與不同測驗取向而穩定存在，難以歸因於單一評比的特性，指向課程與評量重心的結構性議題：臺灣學生在有明確知識結構與標準答案的領域表現卓越，在開放性與詮釋性任務上的相對位置則明顯較低。",
    POS[label=="PIRLS 四年級閱讀"]$pct, POS[label=="PISA 創造思考"]$pct))

  doc <- add_h2(doc, "二、方法學上的提醒")
  doc <- add_body(doc, sprintf(
    "第一，跨輪次比較必須納入連結誤差。本研究實證顯示，在全部 %d 組跨輪次比較中，有 %d 組於忽略連結誤差時會得到錯誤的顯著性結論，標準誤最多被低估至 %.2f 倍。",
    FLT$n, FLT$flip, FLT$infl))
  doc <- add_body(doc,
    "第二，加權與否對結果的影響遠大於一般預期。以 PISA 2022 數學為例，80 個國家與經濟體的加權與未加權平均差異中位數為 2.07 分，最大達 20.60 分。臺灣為正向差異最大者：未加權 534.02 分，加權後 547.13 分，相差 13.11 分。")
  doc <- add_body(doc,
    "第三，不同資料庫不可共用同一套分析程式。TIMSS 與 PIRLS 使用 5 個合理推估值與 JK2 折刀法，PISA 使用 10 個推估值與 Fay 平衡重複半樣本法。將 PISA 的程式直接套用於 IEA 系列評比不會產生錯誤訊息，但標準誤是錯的。本研究為此另行撰寫估計核心，並以獨立實作逐位數驗證；TIMSS 與 PIRLS 因設計相同，可共用同一核心。")
  doc <- add_body(doc,
    "第五，量表分數在使用前必須查核其測量不變性層級。本研究於 TALIS 2018 的 20 個校長量表中發現 7 個的各國加權平均全距不足 0.01，原因是該類量表未達純量不變性，資料提供者已於各參與者內部進行中心化。直接排名會把小數點後第四位的差異轉換成名次，且整個過程不會產生任何警告訊息。變數標籤所載的不變性層級，應與權重、推估值同列為分析前的必要查核項目。")
  doc <- add_body(doc,
    "第四，設計基礎分解與模型基礎的變異成分是不同的估計標的，不宜互相取代或視為驗證失敗。本研究對同一份資料所得的 0.405 與 0.422、0.455，差異來自估計標的與權重縮放方式，三者的總變異數本身即不相同。研究報告宜明確交代所採用的定義。")

  doc <- add_body(doc, sprintf(
    "第七，教師與校長層面的資料呈現另一組議題。TALIS 2018 顯示臺灣校長回報的資源短缺程度在 %d 個參與者中排名倒數（教材不足第 %d、教學人力不足第 %d），但學校自主權亦偏低（教育政策自主第 %d、預算自主第 %d）。TALIS 2024 則顯示日本教師的自我效能感與工作滿意度均為 %d 個參與者中最低，且日本、韓國、新加坡三個學生成就名列前茅的體制，其教師的效能感與滿意度皆落在分布的左下象限。學生表現與教師感受並非同向，此一落差值得在政策討論中一併考量。",
    tw18[idx == "T3PAUTS"]$ntot, tw18[idx == "T3PLACMA"]$rank, tw18[idx == "T3PLACPE"]$rank,
    tw18[idx == "T3PAUTP"]$rank, tw18[idx == "T3PAUTB"]$rank, TAL$meta$n_country))

  doc <- add_h2(doc, "三、研究限制")
  doc <- add_body(doc,
    "第一，PISA 與 TIMSS 均屬重複橫斷設計而非追蹤設計。每一輪抽取的是不同學生，並無任何學生被重複測量。因此本研究的所有趨勢結果均為群體層次的比較，不得詮釋為個別學生的成長軌跡，亦不適用潛在成長模型、交叉延宕模型或個體層次的固定效果模型。若研究問題必須探討個人成長，須改用具追蹤設計的資料，如臺灣教育長期追蹤資料庫（TEPS、TEPS-B）。")
  doc <- add_body(doc,
    "第二，跨輪次可比性有其上限。各領域最早僅能回溯至其首次成為主測領域的輪次：閱讀為 2000 年、數學為 2003 年、科學為 2006 年。此外，PISA 自 2015 年起主要改採電腦施測，跨越該年的比較其不確定性本質上高於其後各輪之間的比較。")
  doc <- add_body(doc,
    "第三，ESCS 指標於 2022 年重新校準，各輪原始 ESCS 不可直接比較。本研究涉及社經地位的跨輪分析已改用 OECD 發布之 trend ESCS，惟該檔僅涵蓋 2012、2015 與 2018 三輪，故社經地位的可比區間較成績趨勢為窄。")
  doc <- add_body(doc,
    "第四，PISA 2022 的施測期間橫跨 COVID-19 疫情，部分國家延後施測或調整樣本。詮釋 2018 年至 2022 年的變化時，教育政策效果與疫情衝擊難以分離。")
  doc <- add_body(doc,
    "第五，PISA 2012 年以前的輪次未納入。OECD 於 2024 年改版官方網站，舊有檔案網址失效，新頁面啟用人機驗證機制，無法以程式自動取得。此部分需以人工方式下載，且提供的是 ASCII 定寬檔配合統計軟體讀取語法，而非現成的資料檔。")
  doc <- add_body(doc,
    "第六，TALIS 兩輪之間不可比較，且臺灣僅有單一輪次。TALIS 2018 與 2024 的量表題項與計分方式不同，本研究未進行跨輪比較。臺灣參加 2018 而未參加 2024，因此臺灣的教師與校長資料僅有單一時點，無法檢視變化趨勢；本研究的 2024 年結果不含臺灣數值。")
  doc <- add_body(doc,
    "第七，臺灣本土大型資料庫尚未納入。TASA 與 TEPS 均需經申請程序方能取得，本研究已完成接入流程的建置，惟資料到位前無法進行實際分析。TEPS 為其中唯一的追蹤設計，若能納入，將可補足本研究在個人成長軌跡上的空缺。")

  doc <- body_add_break(doc)
  doc <- add_h1(doc, "參考文獻")
  refs <- c(
    "OECD (2023). PISA 2022 results (Volume I): The state of learning and equity in education. OECD Publishing. https://doi.org/10.1787/53f23881-en",
    "OECD (2024). PISA 2022 technical report. OECD Publishing. https://doi.org/10.1787/01820d6d-en",
    "OECD (2009). PISA data analysis manual: SPSS (2nd ed.). OECD Publishing.",
    "Mullis, I. V. S., von Davier, M., Foy, P., Fishbein, B., Reynolds, K. A., & Wry, E. (2023). TIMSS 2023 international results in mathematics and science. TIMSS & PIRLS International Study Center, Boston College.",
    "von Davier, M., Kennedy, A., Reynolds, K., Fishbein, B., Khorramdel, L., Aldrich, C., Bookbinder, A., Bezirhan, U., & Yin, L. (2024). TIMSS 2023 technical report. TIMSS & PIRLS International Study Center, Boston College.",
    "Mundlak, Y. (1978). On the pooling of time series and cross section data. Econometrica, 46(1), 69-85.",
    "Mullis, I. V. S., von Davier, M., Foy, P., Fishbein, B., Reynolds, K. A., & Wry, E. (2023). PIRLS 2021 international results in reading. TIMSS & PIRLS International Study Center, Boston College.",
    "Schulz, W., Ainley, J., Fraillon, J., Losito, B., Agrusti, G., Damiani, V., & Friedman, T. (2024). Education for citizenship in times of global challenge: IEA International Civic and Citizenship Education Study 2022 international report. IEA.",
    "Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. John Wiley & Sons.",
    "Judkins, D. R. (1990). Fay's method for variance estimation. Journal of Official Statistics, 6(3), 223–239.",
    "Wolter, K. M. (2007). Introduction to variance estimation (2nd ed.). Springer.")
  for (r in refs)
    doc <- body_add_fpar(doc, fpar(ftext(r, tx(12)),
      fp_p = fp_par(text.align = "justify", line_spacing = 2, hanging = 0.5,
                    padding.bottom = 0)))

  dir.create(dirname(DOCX_OUT), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = DOCX_OUT)
  fix_eastasia(DOCX_OUT)
  cat("寫出 ", DOCX_OUT, "  ", round(file.info(DOCX_OUT)$size / 2^20, 2), " MB\n", sep = "")
  invisible(DOCX_OUT)
}
