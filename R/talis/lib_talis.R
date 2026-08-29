# ============================================================
# lib_talis.R — TALIS 估計核心
#
# TALIS 是「教師與校長調查」，不是學生成就評量。與 PISA／TIMSS 的三個差異：
#
#   1. 沒有合理推估值。TALIS 不測學生能力，量表分數是直接由題項合成的
#      IRT 量尺分數或指標，因此不存在推估變異，Rubin 合併不適用。
#   2. 分析單位是教師或學校，不是學生。權重欄位為 TCHWGT（教師）與
#      SCHWGTC（學校／校長）。
#   3. 重複權重 100 組，PISA 為 80 組。
#
# 變異數估計法為 Fay 平衡重複半樣本法。本檔的 Fay 係數並非查文件得來，
# 而是自資料判定：重複權重與最終權重的比值集中於 0.5 與 1.5，
# 對應 k = 0.5（若為傳統 BRR 則會集中於 0 與 2）。
#     V = 1 / (G × (1 − k)²) × Σ (θ_r − θ)²，G = 100、k = 0.5，係數為 1/25
#
# 使用前請以 detect_fay() 對手上的檔案重新確認，不同輪次可能不同。
# ============================================================
suppressPackageStartupMessages({library(data.table)})

TALIS_G   <- 100L
TALIS_FAY <- 0.5

talis_rw <- function(prefix, G = TALIS_G) sprintf("%s%d", prefix, seq_len(G))

# 由資料判定重複權重的建構方式，不倚賴文件
detect_fay <- function(data, weight, rw_prefix, n_check = 10L) {
  d <- as.data.table(data)
  w <- d[[weight]]
  ok <- is.finite(w) & w > 0
  rw <- as.matrix(d[ok, talis_rw(rw_prefix)[seq_len(n_check)], with = FALSE]) / w[ok]
  tb <- sort(table(round(as.vector(rw), 2)), decreasing = TRUE)
  top <- as.numeric(names(tb)[1:2])
  method <- if (all(sort(top) == c(0, 2))) "傳統 BRR（k = 0）"
            else if (all(abs(sort(top) - c(0.5, 1.5)) < 0.01)) "Fay 法（k = 0.5）"
            else "無法判定，請檢視取值分布"
  list(method = method, top_ratios = top, table = head(tb, 4))
}

wmean_l <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}
wpct_l <- function(x, w, val) {
  ok <- is.finite(w) & !is.na(x)
  if (!any(ok)) return(NA_real_)
  100 * sum(w[ok] * (x[ok] %in% val)) / sum(w[ok])
}

# 主引擎：Fay BRR，無推估值
talis_stat <- function(data, var, FUN = wmean_l, weight = "TCHWGT",
                       rw_prefix = "TRWGT", G = TALIS_G, fay = TALIS_FAY, ...) {
  d <- as.data.table(data)
  wv <- d[[weight]]
  est <- FUN(d[[var]], wv, ...)
  rws <- talis_rw(rw_prefix, G)
  miss <- setdiff(rws, names(d))
  if (length(miss)) stop("缺少重複權重欄位：", paste(head(miss, 3), collapse = ", "))
  rep_est <- vapply(rws, function(r) FUN(d[[var]], d[[r]], ...), numeric(1))
  v <- sum((rep_est - est)^2, na.rm = TRUE) / (G * (1 - fay)^2)
  list(estimate = est, se = sqrt(v), n = nrow(d), G = G)
}

talis_by <- function(data, var, by, FUN = wmean_l, ...) {
  d <- as.data.table(data)
  keys <- unique(d[, ..by]); setorderv(keys, by)
  rbindlist(lapply(seq_len(nrow(keys)), function(i) {
    sub <- d[keys[i], on = by, nomatch = 0L]
    r <- tryCatch(talis_stat(sub, var, FUN = FUN, ...),
                  error = function(e) list(estimate = NA_real_, se = NA_real_,
                                           n = nrow(sub), G = NA_integer_))
    cbind(keys[i], as.data.table(r))
  }))
}
