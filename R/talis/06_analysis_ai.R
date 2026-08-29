# ============================================================
# 06_analysis_ai.R — TALIS 2024 教師問卷的人工智慧模組
#
# 這是本平台涵蓋的五套評比中，唯一有完整 AI 題組的資料：
#   TT4G35A–J  對 AI 在教育中的信念（5 正向 + 5 風險，四點同意量表 + 「不知道」）
#   TT4G36     過去 12 個月是否使用過 AI
#   TT4G37A–I  用 AI 做什麼（9 項，僅使用者作答）
#   TT4G38A–F  不使用的原因（6 項，僅未使用者作答）
#   TT4G21G    已接受的專業發展是否包含 AI
#   TT4G24G    對 AI 技能的專業發展需求（四點）
#
# 兩件必須先確認的事：
# 1. AI 模組是輪換題本的一部分——55 國全部施測，但每位教師只拿到三份
#    題本之一，作答率一致落在 0.29–0.33。這是設計上的計畫性缺失，
#    不是無回應。答題者與未答者的平均權重為 28.18 與 28.11，確認隨機分派，
#    因此沿用 TCHWGT 即為不偏估計，只是有效樣本約為三分之一、標準誤較大。
# 2. 三種題目的分母不同，混用會得到無法解釋的數字：
#    TT4G36 分母為全體作答者；TT4G37 僅使用者作答，換算成全體時未使用者
#    視為 0；TT4G38 僅未使用者作答，分母即為未使用者。
# 3. 專業發展的兩題落在不同的題本上：TT4G21G（已接受）與 AI 模組重疊 95.5%，
#    但 TT4G24G（需求）與 TT4G36 的重疊為 0——兩者互斥。因此這兩題改以
#    全體教師檔各自估計。國家層次的兩個比率仍可並列比較，但個體層次
#    無法交叉（不能問「想學 AI 的人是否正在用 AI」，資料設計上就答不了）。
# ============================================================
suppressPackageStartupMessages({library(data.table); library(arrow); library(jsonlite)})
source("~/PISA/R/talis/lib_talis.R")
log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

AI_ZH <- c(
  TT4G35A = "幫助教師撰寫或改進教案",       TT4G35B = "讓教師調整教材以適應不同能力",
  TT4G35C = "協助教師個別支持學生",         TT4G35D = "支援特殊教育需求學生",
  TT4G35E = "幫助教師自動化行政工作",
  TT4G35F = "讓學生把他人作品冒充為己作",   TT4G35G = "做出不當或錯誤的建議",
  TT4G35H = "放大既有偏見",                 TT4G35I = "危及學生資料的隱私與安全",
  TT4G35J = "建議不合適的教學取向",
  TT4G36  = "過去 12 個月使用過 AI",
  TT4G37A = "評閱或批改學生作業",           TT4G37B = "快速了解與摘要主題",
  TT4G37C = "產生教案或教學活動",           TT4G37D = "支援特殊教育需求學生",
  TT4G37E = "自動調整教材難度",             TT4G37F = "產生學生回饋或親師溝通文字",
  TT4G37G = "檢視學生參與或表現的資料",     TT4G37H = "協助學生在真實情境中練習",
  TT4G37I = "其他用途",
  TT4G38A = "學校缺乏基礎設施",             TT4G38B = "沒有相關知識與技能",
  TT4G38C = "不認為應該使用 AI",            TT4G38D = "學校不允許",
  TT4G38E = "對整合新科技感到吃不消",       TT4G38F = "其他原因",
  TT4G21G = "已接受的專業發展含 AI",        TT4G24G = "對 AI 技能有中度以上需求")
# TALIS 的參與單位不全是主權國家，比利時同時以整體（BEL）與兩個語言社群
# （BFL 荷語區、BFR 法語區）出現，三者的樣本重疊：BEL 的模組作答為 1,663 人，
# BFL 798 + BFR 865 亦為 1,663，同一批教師被計入兩次。逐單位的估計值仍全部
# 保留以利與官方報告對照，但中位數、全距與計數等跨單位統計量一律以 53 個
# 不重複計算的單位為基礎，作法是保留比利時整體、排除兩個語言社群。
TALIS_DUP <- c("BFL", "BFR")
dedup <- function(d) d[!CNTRY %in% TALIS_DUP]

POS5 <- paste0("TT4G35", LETTERS[1:5])   # 正向信念
NEG5 <- paste0("TT4G35", LETTERS[6:10])  # 風險認知
USE9 <- paste0("TT4G37", LETTERS[1:9])
WHY6 <- paste0("TT4G38", LETTERS[1:6])

# 逐國的加權比率（Fay BRR 100 組）；var 須為 0/1 且 NA 代表不納入分母
pct_by <- function(d, var, tag) {
  sub <- d[is.finite(get(var))]
  if (!nrow(sub)) stop(sprintf("%s 沒有任何有效值——請確認該題是否與目前的子樣本落在同一份輪換題本上", var))
  r <- talis_by(sub, var, by = "CNTRY",
                weight = "TCHWGT", rw_prefix = "TRWGT", G = TALIS_G, fay = TALIS_FAY)
  r[, `:=`(idx = var, zh = AI_ZH[var], grp = tag,
           rank = frank(-estimate, ties.method = "min"), ntot = .N)]
  r[]
}

# parquet 會保留 haven_labelled 的類別。若執行時未載入 haven，對這種欄位
# 做算術會失敗，而 talis_by 的 tryCatch 會把錯誤吞掉、回傳一整欄 NA——
# 看起來像「這個國家沒資料」而不是「程式壞了」。轉檔時已一律去除標籤，
# 此處再確認一次，避免倚賴呼叫端剛好載過 haven。
as_plain <- function(d) {
  for (v in names(d)) if (!is.character(d[[v]])) set(d, j = v, value = as.numeric(d[[v]]))
  d[]
}

analyse_ai <- function(rds = "~/TALIS/parquet/ai_2024.parquet") {
  d <- as_plain(as.data.table(read_parquet(path.expand(rds))))
  log_msg("AI 模組作答者 ", nrow(d), " 人 / ", uniqueN(d$CNTRY), " 個參與者")

  out <- list()
  # 使用率：分母為全體作答者
  d[, u_any := as.integer(TT4G36 == 1)]
  out$usage <- pct_by(d, "u_any", "使用率")[, zh := AI_ZH["TT4G36"]][]

  # 用途：換算成全體教師的比率，未使用者視為 0
  for (v in USE9) d[, (paste0("u_", v)) := fifelse(is.na(TT4G36), NA_integer_,
                                     fifelse(TT4G36 == 2, 0L, as.integer(get(v) == 1)))]
  out$use <- rbindlist(lapply(USE9, function(v) {
    log_msg("  用途 ", v); r <- pct_by(d, paste0("u_", v), "用途")
    r[, `:=`(idx = v, zh = AI_ZH[v])][] }))

  # 不使用的原因：分母為未使用者
  dn <- d[TT4G36 == 2]
  for (v in WHY6) dn[, (paste0("w_", v)) := as.integer(get(v) == 1)]
  out$why <- rbindlist(lapply(WHY6, function(v) {
    log_msg("  原因 ", v); r <- pct_by(dn, paste0("w_", v), "不使用的原因")
    r[, `:=`(idx = v, zh = AI_ZH[v])][] }))

  # 信念：選項 5 為「不知道」，不能當作量表的一點。
  # 同意率的分母排除「不知道」，另外單獨報告「不知道」的比率。
  for (v in c(POS5, NEG5)) {
    d[, (paste0("a_", v)) := fifelse(get(v) %in% 1:4, as.integer(get(v) >= 3), NA_integer_)]
    d[, (paste0("d_", v)) := fifelse(is.na(get(v)), NA_integer_, as.integer(get(v) == 5))]
  }
  out$belief <- rbindlist(lapply(c(POS5, NEG5), function(v) {
    log_msg("  信念 ", v)
    r <- pct_by(d, paste0("a_", v), if (v %in% POS5) "正向信念" else "風險認知")
    r[, `:=`(idx = v, zh = AI_ZH[v])][] }))
  out$dontknow <- rbindlist(lapply(c(POS5, NEG5), function(v) {
    r <- pct_by(d, paste0("d_", v), "不知道"); r[, `:=`(idx = v, zh = AI_ZH[v])][] }))

  # 專業發展：兩題在不同題本上，故改以全體教師檔各自估計
  pd <- as_plain(as.data.table(read_parquet(path.expand("~/TALIS/parquet/pd_2024.parquet"))))
  pd[, pd_got  := fifelse(is.na(TT4G21G), NA_integer_, as.integer(TT4G21G == 1))]
  pd[, pd_need := fifelse(is.na(TT4G24G), NA_integer_, as.integer(TT4G24G >= 3))]
  log_msg("  專業發展（全體教師檔：已接受 ", sum(!is.na(pd$pd_got)),
          " 人、需求 ", sum(!is.na(pd$pd_need)), " 人）")
  out$pd <- rbind(pct_by(pd, "pd_got",  "專業發展")[, `:=`(idx="TT4G21G", zh=AI_ZH["TT4G21G"])][],
                  pct_by(pd, "pd_need", "專業發展")[, `:=`(idx="TT4G24G", zh=AI_ZH["TT4G24G"])][])

  out$meta <- list(n_teacher = nrow(d), n_country = uniqueN(d$CNTRY),
                   n_country_dedup = uniqueN(d$CNTRY) - length(TALIS_DUP),
                   dup = TALIS_DUP, n_teacher_full = nrow(pd),
                   n_item = length(c(POS5, NEG5, USE9, WHY6)) + 3L,
                   taiwan = "TWN" %in% d$CNTRY)
  saveRDS(out, path.expand("~/TALIS/output/ai_2024.rds"))
  log_msg("寫出 ai_2024.rds"); invisible(out)
}
