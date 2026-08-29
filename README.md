# 大型數據分析平台

PISA、TIMSS、PIRLS、ICCS 與 TALIS 的整合分析平台。從官方檔案伺服器下載、轉檔、建立倉儲，
到以複雜抽樣權重進行估計、產出圖表與 APA 第七版報告，全程可重複執行。

三套評比的抽樣與計分設計不同，共用同一套程式會得到錯誤的標準誤，本平台
為此撰寫兩套估計核心並逐一驗證。

**線上平台：** https://ian3738.github.io/pisa-analysis-platform/

## 這個專案在做什麼

PISA 資料有兩個地方容易做錯，而且錯了不會報錯：

1. **標準誤只算了一半。** 學生能力不是實測值，而是從後驗分配抽出的 10 個合理推估值
   （plausible values）。只取一個推估值、或先把 10 個平均再當觀測值，都會遺漏推估變異。
   加上複雜抽樣設計本身的效應，正確的標準誤必須同時包含兩者。

2. **跨輪次比較漏掉連結誤差。** 各輪分數雖在同一量尺上，但量尺連結本身有不確定性。
   本專案實測：在 396 組跨輪比較中，有 16 組忽略連結誤差就會把不顯著的差異判成顯著，
   標準誤最多被低估至 2.44 倍。

本專案把這兩件事做對，並把過程完整寫下來。

## 現況

| 項目 | 內容 |
|---|---|
| 資料庫 | PISA 2015／2018／2022、TIMSS 2023 四與八年級、PIRLS 2021、ICCS 2022、TALIS 2018／2024 |
| 選考領域 | PISA 2022 創造思考（63 國）、財金素養（20 國） |
| 樣本 | PISA 174.5 萬、TIMSS 72.0 萬、PIRLS 36.8 萬、ICCS 8.2 萬名學生，合計逾 291 萬；另 TALIS 19.5 萬名教師與 2.1 萬名校長 |
| 儲存 | 632 MB Parquet（原始 `.sav` 合計約 6 GB） |
| 倉儲 | DuckDB，view 直接查 Parquet |
| 估計核心 | `lib_pisa.R`（10 個推估值 × Fay BRR 80 組）、`lib_timss.R`（5 個推估值 × JK2 折刀）、`lib_talis.R`（無推估值 × Fay BRR 100 組） |
| 驗證 | PISA 與 `intsvy`、`survey_mean` 三方一致；TIMSS／ICCS 與獨立實作逐位數一致；TALIS 與 `survey` 差 5.55 × 10⁻¹⁷ |
| 圖表 | 106 張 SVG（亮暗各一版）+ 53 張 200 dpi PNG，全部由 ggplot2 產出 |
| 報告 | APA 第七版 Word 檔，53 圖、12 表，每項均附研究結果說明 |

## 幾個實測結果

**加權不是小數點後的問題。** PISA 2022 數學，80 個國家與經濟體的加權與未加權平均
差異中位數為 2.07 分，最大 20.60 分（泰國）。臺灣是正向差異最大者：未加權 534.0，
加權後 547.1，相差 13.1 分。

**基期選擇會改變結論。** 臺灣三個領域自 2018 年到 2022 年的進步均達統計顯著；
但以 2015 年為基期，數學（+4.8, p = .39）與科學（+5.0, p = .26）都不顯著。
2018 年是三輪中的低點。

**成績上升與公平性未同步。** 臺灣數學的社經梯度自 2018 年的 38.1 分升至 2022 年的
49.3 分（每一個 ESCS 標準差），高於全體中位數 33.3 分。同期數學未達 Level 2 的比率
自 13.98% 升至 14.61%，Level 5 以上者自 23.19% 升至 31.73%——兩端同時擴大。

**量表分數不是拿來就能排名的。** TALIS 2018 的 20 個校長量表中，有 7 個的各國加權平均
全距不足 0.01——例如「校園違規與暴力」，個體分數在 2.277 至 20.323 之間，加權後 47 個
參與者的平均全部落在 6.8500 至 6.8504。原因不是程式錯誤（已逐國以手算核對），而是這些
量表未達純量不變性，TALIS 已在各國內部中心化。若不查變數標籤直接排序，會得到
「臺灣校園違規排第 3 高」這種把第四位小數轉換成名次的結論，而且全程不會有任何警告。

## 目錄結構

```
.
├── R/
│   ├── 00_config.R           路徑、來源清單、BRR 參數、rebuild_all()
│   ├── 01_download.R         下載與 SHA-256 校驗
│   ├── 02_extract_convert.R  解壓、變數字典、抽欄、轉 Parquet
│   ├── 03_harmonise.R        跨輪對齊、trend ESCS、選考領域併檔
│   ├── 04_warehouse.R        DuckDB view 與品質檢核表
│   ├── 05_analysis.R         平均、精熟等級、社經梯度、性別差距
│   ├── 06_trend.R            跨輪比較（含連結誤差）
│   ├── 07_run_trend.R        一次跑完趨勢分析
│   ├── 08_export_web.R       產出網站用的估計結果 JSON
│   ├── 09_figures.R          產出全部圖表（SVG + PNG）
│   ├── 10_report_docx.R      產出 APA 第七版 Word 報告
│   ├── build_site.py         組裝網站
│   └── lib_pisa.R            估計核心（三方驗證）
├── docs/                     GitHub Pages 來源目錄
│   ├── index.html            分析平台網站
│   ├── large-scale-data-analysis-report-APA7.docx
│   ├── methodology.md        方法學：縱貫分析的限制、複雜抽樣、連結誤差
│   └── architecture.md       系統架構與擴充方式
├── web/
│   ├── template.html         網站樣板
│   ├── figs/                 106 張 SVG（亮暗各一版）
│   ├── png/                  53 張 200 dpi PNG（Word 報告用）
│   ├── pisa_data.json        全部估計結果
│   └── country_zh.json       國名中文對照
├── output/                   分析結果 CSV
└── deploy.sh                 建置並複製到 docs/
```

## 從零重建

資料檔未納入版本控制（原始 zip 約 1.9 GB）。首次執行會自動從 OECD 官方
檔案伺服器下載：

```bash
# 1. 下載、轉檔、建立 DuckDB 倉儲（約 15 分鐘，視網速）
Rscript -e 'source("R/00_config.R"); rebuild_all()'

# 2. 產出網站用的估計結果（80 國 × 3 輪 × 3 領域，約 7 分鐘）
Rscript R/08_export_web.R

# 3. 產出全部圖表：網頁用 SVG + Word 用 200 dpi PNG（約 13 分鐘）
Rscript -e 'source("R/09_figures.R"); build_all_figures()'

# 4. 產出 APA 第七版 Word 報告
Rscript -e 'source("R/10_report_docx.R"); build_report()'

# 5. 建置網站並複製到 docs/
./deploy.sh
```

需要的 R 套件：`data.table`、`arrow`、`duckdb`、`haven`、`survey`、`intsvy`、
`ggplot2`、`svglite`、`ragg`、`officer`、`flextable`、`jsonlite`、`systemfonts`。

## 快速開始（不重建，直接分析）

```r
source("R/05_analysis.R")

# 臺灣 2022 數學平均（含正確標準誤）
tw <- load_stu(2022, "TAP")
pisa_pv_stat(tw, pv_names("MATH", 10))
#> estimate 547.09   se 3.78   (抽樣 3.728 / 推估 0.611)

# 跨輪次比較——務必納入連結誤差
source("R/06_trend.R")
get_link_error(2018, 2022, "MATH")   #> 2.24
pisa_trend_diff(531.1, 2.89, 547.1, 3.78, get_link_error(2018, 2022, "MATH"))
```

## 資料來源與授權

本專案**不重新散布任何原始資料**，僅提供取得與處理的程式。所有資料檔均已排除於
版本控制之外，首次執行時由使用者自官方來源取得。

### OECD（PISA、TALIS）

著作權屬 OECD 所有。連結誤差數值取自 PISA 2022 技術報告 Annex Table 14.A.19，
精熟等級切點取自同報告 Annex Tables 17.A.2、17.A.12、17.A.13、17.A.14。

- OECD (2023). *PISA 2022 results (Volume I)*. https://doi.org/10.1787/53f23881-en
- OECD (2024). *PISA 2022 technical report*. https://doi.org/10.1787/01820d6d-en

### IEA（TIMSS、PIRLS）

TIMSS、PIRLS、ICCS 與 ICILS 為 IEA 的註冊商標。IEA 的授權條款要求引用時
註明來源、年份與標題，且僅限非商業之教育與研究用途。本專案依其建議格式標註：

> SOURCE: TIMSS 2023 Assessment. Copyright © 2024 International Association for the
> Evaluation of Educational Achievement (IEA). Publisher: TIMSS & PIRLS International
> Study Center, Lynch School of Education, Boston College.

> SOURCE: PIRLS 2021 Assessment. Copyright © 2023 International Association for the
> Evaluation of Educational Achievement (IEA). Publisher: TIMSS & PIRLS International
> Study Center, Lynch School of Education, Boston College.

- Mullis, I. V. S., von Davier, M., Foy, P., Fishbein, B., Reynolds, K. A., & Wry, E. (2023).
  *TIMSS 2023 international results in mathematics and science*. Boston College.
- Mullis, I. V. S., von Davier, M., Foy, P., Fishbein, B., Reynolds, K. A., & Wry, E. (2023).
  *PIRLS 2021 international results in reading*. Boston College.

**IEA 授權條款中兩條需留意者**（見 IEA Disclaimer and License Agreement）：

- 第 2.1 條：未經 IEA 書面許可，不得重製、散布或以任何形式傳輸其出版品與
  限制使用項目。本專案僅發布自行計算的彙總統計量並註明來源，不散布原始資料。
- 第 1.3 條：**將 IEA 資料用於評量或學習材料時，須事先通知 IEA。**
  若本專案的產出將作為正式課程教材，使用者應自行向 IEA 完成此一告知。

## 已知限制

**PISA 不能做個人層次的縱貫分析。** 它是重複橫斷設計，每輪抽取不同的 15 歲學生，
沒有任何學生被重複測量。潛在成長模型、交叉延宕模型、個體固定效果模型皆不適用。
詳見 [docs/methodology.md](docs/methodology.md)。

**2012 以前的輪次無法自動取得。** OECD 於 2024 年改版官網，舊網址全部失效，
新的資料集頁面啟用了人機驗證。需以瀏覽器手動下載，且提供的是 ASCII 定寬檔
加上 SPSS 讀取語法。詳見 [docs/architecture.md](docs/architecture.md)。

**ESCS 只能回溯到 2012。** 該指標於 2022 年重新校準，OECD 另發布的 trend ESCS
僅涵蓋 2012、2015、2018 三輪。

**PISA 2025** 首波結果預定 2026 年 9 月 8 日發布，屆時在來源清單加一列網址、
重跑流程、更新連結誤差表即可納入。


## 跨評比的一致形態

臺灣在九項國際評比中的相對位置（以百分位表示，100% 為第 1 名）：

| 評比 | 名次 | 參與者 | 百分位 |
|---|---|---|---|
| TIMSS 2023 四年級數學 | 2 | 63 | 98.4 |
| TIMSS 2023 八年級數學 | 2 | 47 | 97.9 |
| TIMSS 2023 八年級科學 | 2 | 47 | 97.9 |
| PISA 2022 數學 | 3 | 80 | 97.5 |
| TIMSS 2023 四年級科學 | 3 | 63 | 96.8 |
| PISA 2022 科學 | 4 | 80 | 96.2 |
| PISA 2022 閱讀 | 5 | 80 | 95.0 |
| **PIRLS 2021 四年級閱讀** | **15** | **65** | **78.5** |
| **PISA 2022 創造思考** | **16** | **63** | **76.2** |

數理項目全部在 96 百分位以上，閱讀（尤其四年級）與創造思考則明顯落後。
這個落差跨越不同年齡、不同主辦單位與不同測驗取向而穩定存在，難以歸因於
單一評比的特性。

## 教師與校長

臺灣參加 TALIS 2018（202 位國中校長）而未參加 2024。僅採用實質可跨國比較的九個量表，
臺灣呈現一組互補的形態：

| 面向 | 指標 | 名次／47 |
|---|---|---|
| 資源相對充裕 | 教材不足 | 43 |
| | 整體資源不足 | 42 |
| | 教學人力不足 | 41 |
| 自主權偏低 | 教育政策自主 | 40 |
| | 預算自主 | 36 |
| | 人事自主 | 21 |

資源短缺類的名次依短缺程度由高至低排列，故名次越後代表狀況越佳。

TALIS 2024（不含臺灣）另顯示：日本教師的自我效能感 7.14 與工作滿意度 8.64 皆為
55 個參與者中最低，且日本、韓國、新加坡三個學生成就名列前茅的體制，教師的效能感
與滿意度都落在分布的左下象限。

## 學校層級的分化

臺灣數學的校間變異占總變異 40.5%（2018 年為 33.8%），其中 71.2% 對應到
學生組成的社經落差（2015 年為 61.3%）。Mundlak 分解顯示校內個人社經效應
為每一個標準差 20.8 分，學校社經組成效應則達 131.6 分——「就讀哪一所學校」
的影響約為「個人家庭背景在校內」的六倍，學校組成效應在全體國家中排名第三。
