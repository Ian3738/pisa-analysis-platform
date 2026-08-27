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
    ftext("PISA 2015–2022 國際學生能力評量結果分析", tx(16, bold = TRUE)), fp_p = P_CENTER))
  doc <- body_add_fpar(doc, fpar(
    ftext("加權估計、精熟等級分布與跨輪次趨勢", tx(13)), fp_p = P_CENTER))
  doc <- add_gap(doc)
  doc <- body_add_fpar(doc, fpar(ftext(
    sprintf("分析樣本：%s 名學生，%d 個國家與經濟體",
            format(J$meta$nStudents, big.mark = ","), J$meta$nCountries), tx(12)),
    fp_p = P_CENTER))
  doc <- body_add_fpar(doc, fpar(ftext(
    paste0("資料來源：OECD PISA 公開使用檔（", paste(J$meta$cycles, collapse = "、"), "）"),
    tx(12)), fp_p = P_CENTER))
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

  # ---------- 六、結論與限制 ----------
  doc <- add_h1(doc, "陸、結論與研究限制")
  doc <- add_h2(doc, "一、主要發現")
  doc <- add_body(doc, sprintf(
    "第一，臺灣在 PISA 2022 三個核心領域的表現均居國際前列（數學第 %d 位、科學第 %d 位、閱讀第 %d 位），且相較 2018 年均達統計顯著的進步。惟若以 2015 年為基期，數學與科學的變化則未達顯著，顯示 2018 年係三輪中的低點，基期選擇對趨勢結論具決定性影響。",
    TWR[["MATH"]], TWR[["SCIE"]], TWR[["READ"]]))
  doc <- add_body(doc,
    "第二，臺灣的成績提升伴隨分配擴大而非整體平移。數學未達基礎水準者的比率自 2018 年的 13.98% 上升至 2022 年的 14.61%，同期高表現者自 23.19% 上升至 31.73%，兩端同時擴張。")
  doc <- add_body(doc, sprintf(
    "第三，社經梯度明顯變陡。臺灣數學的社經梯度自 2018 年的 %.1f 分升至 2022 年的 %.1f 分，高於全體中位數 %.1f 分。成績提升與教育機會均等並未同步。",
    ESCJ[CNT=="TAP" & domain=="MATH" & cycle==2018]$slope,
    ESCJ[CNT=="TAP" & domain=="MATH" & cycle==2022]$slope,
    median(ESCJ[cycle==2022 & domain=="MATH"]$slope, na.rm = TRUE)))

  doc <- add_h2(doc, "二、方法學上的提醒")
  doc <- add_body(doc, sprintf(
    "本研究實證顯示，在全部 %d 組跨輪次比較中，有 %d 組於忽略連結誤差時會得到錯誤的顯著性結論，標準誤最多被低估至 %.2f 倍。國內外以 PISA 資料進行趨勢分析的研究，宜明確交代是否納入連結誤差。",
    FLT$n, FLT$flip, FLT$infl))
  doc <- add_body(doc,
    "此外，加權與否對結果的影響遠大於一般預期。以 PISA 2022 數學為例，臺灣未加權平均為 534.02 分，加權後為 547.13 分，相差 13.11 分，為全體 80 個國家與經濟體中差異最大者。")

  doc <- add_h2(doc, "三、研究限制")
  doc <- add_body(doc,
    "第一，PISA 屬重複橫斷設計而非追蹤設計。每一輪抽取的是不同的 15 歲學生，並無任何學生被重複測量。因此本研究的所有趨勢結果均為群體層次的比較，不得詮釋為個別學生的成長軌跡，亦不適用潛在成長模型、交叉延宕模型或個體層次的固定效果模型。若研究問題必須探討個人成長，須改用具追蹤設計的資料。")
  doc <- add_body(doc,
    "第二，跨輪次可比性有其上限。各領域最早僅能回溯至其首次成為主測領域的輪次：閱讀為 2000 年、數學為 2003 年、科學為 2006 年。此外，PISA 自 2015 年起主要改採電腦施測，跨越該年的比較其不確定性本質上高於其後各輪之間的比較。")
  doc <- add_body(doc,
    "第三，ESCS 指標於 2022 年重新校準，各輪原始 ESCS 不可直接比較。本研究涉及社經地位的跨輪分析已改用 OECD 發布之 trend ESCS，惟該檔僅涵蓋 2012、2015 與 2018 三輪，故社經地位的可比區間較成績趨勢為窄。")
  doc <- add_body(doc,
    "第四，PISA 2022 的施測期間橫跨 COVID-19 疫情，部分國家延後施測或調整樣本。詮釋 2018 年至 2022 年的變化時，教育政策效果與疫情衝擊難以分離。")

  doc <- body_add_break(doc)
  doc <- add_h1(doc, "參考文獻")
  refs <- c(
    "OECD (2023). PISA 2022 results (Volume I): The state of learning and equity in education. OECD Publishing. https://doi.org/10.1787/53f23881-en",
    "OECD (2024). PISA 2022 technical report. OECD Publishing. https://doi.org/10.1787/01820d6d-en",
    "OECD (2009). PISA data analysis manual: SPSS (2nd ed.). OECD Publishing.",
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
