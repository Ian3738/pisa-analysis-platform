# PISA 分析系統架構

## 目錄結構

```
~/PISA/
├── raw/                    原始 zip，來源忠實副本，永不覆寫（= bronze 層）
│   ├── 2015/ 2018/ 2022/
│   └── manifest.csv        每個檔案的 URL、位元組數、SHA-256、下載時間
├── staging/                解壓的 .sav，轉檔後立即刪除
├── parquet/
│   └── silver/             跨年命名對齊後的分析欄位 + 完整變數字典
├── warehouse/pisa.duckdb   DuckDB 分析倉儲
├── R/
│   ├── 00_config.R         路徑、來源清單、BRR 參數
│   ├── 01_download.R       下載 + 校驗和 + 稽核清單
│   ├── 02_extract_convert.R 解壓 → 變數字典 → 抽欄 → Parquet
│   ├── 03_harmonise.R      跨輪變數對齊、trend ESCS 併入
│   ├── 04_warehouse.R      建立 DuckDB view 與 QA 表
│   ├── 05_analysis.R       平均數、精熟等級、社經梯度、性別差距
│   ├── 06_trend.R          跨輪比較（含連結誤差）
│   └── lib_pisa.R          PV × BRR 估計核心
├── output/                 分析結果
├── logs/
└── docs/                   architecture.md、methodology.md
```

## 為什麼原始 zip 就是 bronze 層

一般資料平台會把來源資料原封不動落到 bronze 層再往下加工。這裡做了調整：
PISA 的 `.sav` 展開後是 `.zip` 的三到四倍（2022 學生檔 651 MB → 2.0 GB），
而 OECD 的 zip 本身就是不可變的公開發布檔，有官方網址與校驗和可追溯。
把它當成 bronze 層，既保有可重建性，又省下數 GB 磁碟。

代價是每次要新增欄位就得重跑解壓。這在 PISA 這種一年更新一次的資料上完全可接受，
換成每日進資料的系統就不該這樣設計。

## 為什麼用 Parquet + DuckDB

- 單一輪次是 50 萬到 60 萬列、上千欄。`.sav` 讀一次要一分半，Parquet 只要幾秒。
- 壓縮效果顯著：2022 學生檔 2.0 GB 的 `.sav`，抽出分析欄位後的 Parquet 約 200 MB。
- DuckDB 直接查 Parquet，不需伺服器、不需先載入記憶體，語法是標準 SQL。
  資料量再大十倍，同一套程式不用改。
- 換成 Spark 之類的分散式運算在這個量級是浪費：PISA 全部輪次合計也就數千萬列，
  單機處理綽綽有餘。

## 資料血緣

```
OECD webfs.oecd.org
  └─ raw/<cycle>/*.zip                    manifest.csv 記錄 SHA-256
       └─ staging/<cycle>/*.sav           暫存，轉檔後刪除
            ├─ codebook_<kind>_<cycle>.csv 完整變數清單與標籤
            └─ silver/<kind>_<cycle>.parquet
                 └─ warehouse/pisa.duckdb  view: STU_2015/2018/2022, stu_all
                      └─ output/*.csv
```

## 擴充到新輪次

PISA 2025 首波結果預定 2026 年 9 月 8 日發布，資料庫通常同日或稍後上架。
屆時只需三步：

1. `00_config.R` 的 `PISA_SOURCES` 加一列 2025 的網址
   （依歷史慣例會是 `https://webfs.oecd.org/pisa2025/STU_QQQ_SPSS.zip`，
   上架後請先以 curl 確認實際路徑）
2. 跑 `01_download.R` → `02_extract_convert.R` → `04_warehouse.R`
3. 更新 `lib_pisa.R` 的 `PISA_LINK_ERROR`，填入 PISA 2025 技術報告的新連結誤差

`KEEP_PATTERNS` 以正規表示式比對，新輪次新增的量表若命名沿用慣例會自動納入；
沒納入的可查 `codebook_STU_2025.csv` 後補上樣式。

## 目前的資料範圍

| 輪次 | 學生檔 | 學校檔 | 選考領域 | 取得方式 |
|---|---|---|---|---|
| 2022 | ✔ | ✔ | 創造思考 63 國、財金素養 20 國 | 自動 |
| 2018 | ✔ | ✔ | 全球素養 PV 在主檔 | 自動 |
| 2015 | ✔ | ✔ | 合作解題 PV 在主檔 | 自動 |
| 2012 及更早 | ✘ | ✘ | — | 須人工下載（見下） |

### 選考領域檔的陷阱

2022 的創造思考（`CRT_SPSS.zip`）與財金素養（`FLT_SPSS.zip`）是獨立檔案，
兩個地方容易出錯：

1. **CRT 的認知檔沒有權重。** `CY08MSP_CRT_COG.SAV` 只有合理推估值與試題反應，
   權重在主學生檔，必須以 `CNTSTUID` 併回才能做加權估計。
   用 `attach_optional_domain()` 處理。
2. **FLT 的 zip 內有三個 `.sav`。** `FLT_COG` 是試題檔（無 PV、無權重）、
   `FLT_QQQ` 才是含 PV 與權重的問卷檔、`FLT_TIM` 是作答時間。
   `process_cycle()` 預設挑最大的檔案，會挑到錯的 `FLT_COG`，
   須指定 `file_pattern = "QQQ"`。

### 2012 以前為何無法自動取得

OECD 於 2024 年改版官網，舊的 `oecd.org/pisa/pisaproducts/` 網址全部失效（HTTP 403），
新的資料集頁面則啟用 Cloudflare 人機驗證。自動化工具無法通過該驗證，
也不應該試圖繞過。

若需要 2012 及更早的輪次，請以瀏覽器手動下載：

- https://www.oecd.org/en/data/datasets/pisa-2012-database.html
- https://www.oecd.org/en/data/datasets/pisa-2009-database.html
- https://www.oecd.org/en/data/datasets/pisa-2006-database.html

這些輪次提供的是 ASCII 定寬資料檔加上 SPSS/SAS 讀取語法，不是現成的 `.sav`，
需先在 SPSS 或以 R 的 `readr::read_fwf()` 搭配語法檔轉檔。
下載後放進 `raw/<cycle>/`，`02_extract_convert.R` 的其餘流程可沿用。

注意 2012 以前的變數命名不同（性別為 `ST04Q01`、重複權重為 `W_FSTR1`–`80`、
每領域只有 5 個 PV），`03_harmonise.R` 已預留對應規則。
