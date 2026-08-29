# ============================================================
# 02_tasa_teps_scaffold.R — 臺灣本土大型資料庫的接入骨架
#
# TASA（臺灣學生學習成就評量資料庫）與 TEPS／TEPS-B 都需要申請取得，
# 無法如 PISA、TIMSS 自官方伺服器直接下載。本檔先把流程與介面寫好，
# 資料到位後只需把檔案放進指定目錄即可沿用同一套估計程序。
#
# ---------------- 取得方式 ----------------
# TASA　國家教育研究院「臺灣學生學習成就評量資料庫」
#   線上申請，需檢附研究計畫書與 IRB 核可文件；核准後於指定平臺下載或
#   於院內資料室分析。抽樣設計為分層叢集（依縣市、學校類型分層），
#   提供學生權重與複本權重，能力值以 IRT 估計並產生多重推估值。
#   → 資料放置於 ~/TASA/raw/<年份>/
#
# TEPS／TEPS-B　中央研究院調查研究專題中心學術調查研究資料庫（SRDA）
#   線上申請帳號後下載；部分敏感變項需另行申請。TEPS 為真正的追蹤設計
#   （2001 年起追蹤同一批學生），可做個人成長軌跡——這正是 PISA 做不到的。
#   → 資料放置於 ~/TEPS/raw/
#
# ---------------- 三個資料庫的設計差異 ----------------
#   資料庫   設計         推估值   變異數估計        本專案的估計核心
#   PISA     重複橫斷     10 個    Fay BRR（80 組）  lib_pisa.R
#   TIMSS    重複橫斷     5 個     JK2 折刀          lib_timss.R
#   TASA     重複橫斷     依年份   依技術報告        待資料到位後確認
#   TEPS     追蹤（panel）多波測量 追蹤樣本權重      可用潛在成長模型
#
# TEPS 是四者中唯一能做個人層次縱貫分析的資料庫。PISA 與 TIMSS 皆為
# 重複橫斷，每輪抽取不同學生，只能做群體層次的趨勢比較。
# ============================================================
suppressPackageStartupMessages({library(data.table)})

TASA_ROOT <- path.expand("~/TASA")
TEPS_ROOT <- path.expand("~/TEPS")

# 檢查資料是否到位，未到位時明確說明取得方式而非靜默失敗
check_local_db <- function(which = c("TASA", "TEPS")) {
  which <- match.arg(which)
  root <- if (which == "TASA") TASA_ROOT else TEPS_ROOT
  raw  <- file.path(root, "raw")
  if (!dir.exists(raw) || !length(list.files(raw, recursive = TRUE))) {
    message(sprintf(
      "%s 資料尚未到位。\n  預期路徑：%s\n  取得方式：%s",
      which, raw,
      if (which == "TASA") "向國家教育研究院線上申請，需檢附研究計畫書與 IRB 核可文件"
      else "向中央研究院學術調查研究資料庫（SRDA）申請帳號後下載"))
    return(invisible(FALSE))
  }
  message(sprintf("%s：找到 %d 個檔案", which, length(list.files(raw, recursive = TRUE))))
  invisible(TRUE)
}

# 資料到位後，依其技術報告填入下列設定即可沿用既有估計流程
TASA_DESIGN <- list(
  weight        = NA_character_,   # 例如 "W_STU"
  replicate     = NA_character_,   # 重複權重欄位前綴，或 NA 表示改用 JK 分區
  jk_zone       = NA_character_,
  jk_rep        = NA_character_,
  pv_pattern    = NA_character_,   # 例如 "PV{i}MATH"
  n_pv          = NA_integer_,
  variance_type = NA_character_    # "fay" / "jk2" / "brr"
)

TEPS_DESIGN <- list(
  waves         = NA,              # 追蹤波次
  panel_weight  = NA_character_,   # 追蹤樣本權重
  attrition     = NA_character_    # 流失調整權重
)

# TEPS 到位後可做、而 PISA／TIMSS 做不到的分析
TEPS_ANALYSES <- c(
  "潛在成長模型：估計個別學生的成長軌跡與其變異",
  "交叉延宕模型：釐清變項之間的時序方向",
  "個體固定效果：控制所有不隨時間變動的個人特質",
  "存活分析：升學與輟學的時間歷程",
  "成長混合模型：辨識不同的成長組型"
)
