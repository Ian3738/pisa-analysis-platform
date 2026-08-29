# ============================================================
# lib_timss.R — TIMSS／PIRLS 估計核心
#
# 與 PISA 的差異（照抄 PISA 的程式會錯）：
#   推估值      TIMSS 每個量尺 5 個（BSMMAT01–05），PISA 為 10 個
#   變異數估計  TIMSS 用 JK2 折刀法搭配 JKZONE／JKREP 兩欄，
#               PISA 用 Fay 平衡重複半樣本法搭配 80 組現成的重複權重
#   權重欄位    TIMSS 為 TOTWGT，PISA 為 W_FSTUWT
#
# JK2 的作法：把學校在每個折刀區（zone）內兩兩配對，JKREP 標示配對中的哪一個。
# 對第 h 區產生一組重複權重——該區中 JKREP=1 者權重加倍、JKREP=0 者歸零，
# 其餘各區維持原權重。變異數為
#     V = Σ_{h=1}^{H} (θ_h − θ)²
# 注意此處沒有 PISA Fay 法的 1/(G(1−k)²) 係數，JK2 的加倍與歸零已內含縮放。
# 參考：TIMSS 2023 Technical Report, Ch. 13（Estimating Sampling Variance）
# ============================================================
suppressPackageStartupMessages({library(data.table)})

# 某量尺的推估值欄名，例如 timss_pv("BSMMAT") → BSMMAT01..BSMMAT05
timss_pv <- function(scale, n = 5) sprintf("%s%02d", scale, seq_len(n))

wmean_t <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}
wsd_t <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (sum(ok) < 2) return(NA_real_)
  x <- x[ok]; w <- w[ok]; m <- sum(x * w) / sum(w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}
wpct_above_t <- function(x, w, cut) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  100 * sum(w[ok] * (x[ok] >= cut)) / sum(w[ok])
}

# 產生第 h 個折刀區的重複權重
jk2_weight <- function(w, zone, rep, h) {
  ifelse(zone == h, w * 2 * rep, w)
}

# 主引擎：5 個推估值 × JK2
timss_pv_stat <- function(data, pv_vars, FUN = wmean_t,
                          w = "TOTWGT", zone = "JKZONE", rep = "JKREP", ...) {
  data <- as.data.table(data)
  M  <- length(pv_vars)
  wv <- data[[w]]; zv <- data[[zone]]; rv <- data[[rep]]
  zones <- sort(unique(zv[!is.na(zv)]))
  H <- length(zones)
  if (H < 2L) stop("折刀區不足：", H)

  est_m <- vapply(pv_vars, function(p) FUN(data[[p]], wv, ...), numeric(1))
  est   <- mean(est_m)

  # 抽樣變異：對每個推估值各跑一輪全部折刀區後平均
  v_samp <- mean(vapply(seq_len(M), function(m) {
    xv <- data[[pv_vars[m]]]
    th <- vapply(zones, function(h) FUN(xv, jk2_weight(wv, zv, rv, h), ...), numeric(1))
    sum((th - est_m[m])^2, na.rm = TRUE)
  }, numeric(1)))

  # 推估變異：Rubin 規則
  b_m   <- if (M > 1) sum((est_m - est)^2) / (M - 1) else 0
  v_imp <- (1 + 1 / M) * b_m

  list(estimate = est, se = sqrt(v_samp + v_imp),
       var_sampling = v_samp, var_imputation = v_imp,
       n = nrow(data), M = M, H = H)
}

timss_pv_by <- function(data, pv_vars, by, FUN = wmean_t, ...) {
  data <- as.data.table(data)
  keys <- unique(data[, ..by]); setorderv(keys, by)
  rbindlist(lapply(seq_len(nrow(keys)), function(i) {
    sub <- data[keys[i], on = by, nomatch = 0L]
    r <- tryCatch(timss_pv_stat(sub, pv_vars, FUN = FUN, ...),
                  error = function(e) {
                    warning(paste(unlist(keys[i]), collapse = "/"), "：",
                            conditionMessage(e), call. = FALSE)
                    list(estimate = NA_real_, se = NA_real_, var_sampling = NA_real_,
                         var_imputation = NA_real_, n = nrow(sub), M = length(pv_vars), H = NA_integer_)
                  })
    cbind(keys[i], as.data.table(r))
  }))
}

# TIMSS 國際基準點（四個標準參照點，2023 沿用歷來設定）
TIMSS_BENCH <- c(Low = 400, Intermediate = 475, High = 550, Advanced = 625)
