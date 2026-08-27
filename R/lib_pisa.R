# ============================================================
# lib_pisa.R — PISA 估計核心
#
# PISA 的兩個變異來源必須同時處理，缺一結果就是錯的：
#   (1) 抽樣變異：複雜抽樣設計 → Fay 平衡重複半樣本法（BRR），80 組重複權重
#   (2) 推估變異：能力值不是實測而是後驗分配的抽樣 → 10 個合理推估值（PV），Rubin 合併
# 參考：OECD PISA Data Analysis Manual (2nd ed.)；PISA 2022 Technical Report Ch. 12
# ============================================================

suppressPackageStartupMessages({library(data.table)})

# ---- 變數名稱工具 ---------------------------------------------------------
pv_names <- function(domain, n = 10) sprintf("PV%d%s", seq_len(n), toupper(domain))
rw_names <- function(prefix = "W_FSTURWT", G = 80) sprintf("%s%d", prefix, seq_len(G))

# ---- 加權統計量 -----------------------------------------------------------
wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

wvar <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (sum(ok) < 2) return(NA_real_)
  x <- x[ok]; w <- w[ok]; m <- sum(x * w) / sum(w)
  sum(w * (x - m)^2) / sum(w)          # 母體變異數定義，與 OECD 一致
}

wsd <- function(x, w) sqrt(wvar(x, w))

wquantile <- function(x, w, probs = 0.5) {
  ok <- is.finite(x) & is.finite(w)
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  approx(cw, x, xout = probs, method = "linear", rule = 2, ties = "ordered")$y
}

# 到達某精熟等級以上的比率（%）
wpct_above <- function(x, w, cut) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  100 * sum(w[ok] * (x[ok] >= cut)) / sum(w[ok])
}

# ---- BRR 抽樣變異 ---------------------------------------------------------
# Fay 法：V = 1 / (G * (1-k)^2) * Σ (θ_r − θ)^2，PISA 取 G = 80, k = 0.5 → 1/20
brr_var <- function(theta_rep, theta_full, G = 80, fay = 0.5) {
  sum((theta_rep - theta_full)^2, na.rm = TRUE) / (G * (1 - fay)^2)
}

# ---- 主引擎：PV × BRR ------------------------------------------------------
# FUN 須為 function(x, w, ...) 回傳單一數值
#
# var_method:
#   "average"（預設，OECD Data Analysis Manual 完整作法）
#       對每個 PV 各跑 80 組重複權重，得 M 個抽樣變異後取平均。
#       計算量 M×G 次；與 intsvy、IDB Analyzer 結果一致。
#   "pv1"（簡化作法）
#       僅用第一個 PV 搭配 80 組重複權重。計算量 G 次，
#       結果通常與完整作法差在小數點後一、二位，適合大規模掃描時使用。
pisa_pv_stat <- function(data, pv_vars, FUN = wmean,
                         w = "W_FSTUWT", rw_prefix = "W_FSTURWT",
                         G = 80, fay = 0.5, var_method = c("average", "pv1"), ...) {
  var_method <- match.arg(var_method)
  data <- as.data.table(data)
  M <- length(pv_vars)
  wv <- data[[w]]

  # 各 PV 以最終權重計算 → 點估計
  est_m <- vapply(pv_vars, function(p) FUN(data[[p]], wv, ...), numeric(1))
  est   <- mean(est_m)

  rws  <- rw_names(rw_prefix, G)
  miss <- setdiff(rws, names(data))
  if (length(miss)) stop("缺少重複權重欄位：", paste(head(miss, 5), collapse = ", "))

  if (var_method == "pv1") {
    rep_est <- vapply(rws, function(r) FUN(data[[pv_vars[1]]], data[[r]], ...), numeric(1))
    v_samp  <- brr_var(rep_est, est_m[1], G, fay)
  } else {
    v_samp_m <- vapply(seq_len(M), function(m) {
      rep_est <- vapply(rws, function(r) FUN(data[[pv_vars[m]]], data[[r]], ...), numeric(1))
      brr_var(rep_est, est_m[m], G, fay)
    }, numeric(1))
    v_samp <- mean(v_samp_m)
  }

  # 推估變異：Rubin
  if (M > 1) {
    b_m     <- sum((est_m - est)^2) / (M - 1)
    v_imp   <- (1 + 1 / M) * b_m
    v_total <- v_samp + v_imp
    df <- if (b_m > 0) (M - 1) * (1 + v_samp / v_imp)^2 else Inf
  } else {
    b_m <- 0; v_imp <- 0; v_total <- v_samp; df <- Inf
  }

  list(estimate = est, se = sqrt(v_total),
       var_sampling = v_samp, var_imputation = v_imp,
       df = df, n = nrow(data), M = M, var_method = var_method)
}

# 依分組跑（by 可為多欄）
pisa_pv_by <- function(data, pv_vars, by, FUN = wmean, ..., verbose = FALSE) {
  data <- as.data.table(data)
  keys <- unique(data[, ..by])
  setorderv(keys, by)
  res <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    sub <- data[keys[i], on = by, nomatch = 0L]
    if (verbose) cat(sprintf("  [%d/%d] %s (n=%d)\n", i, nrow(keys),
                             paste(unlist(keys[i]), collapse = "/"), nrow(sub)))
    r <- tryCatch(pisa_pv_stat(sub, pv_vars, FUN = FUN, ...),
                  error = function(e) {
                    warning("分組 ", paste(unlist(keys[i]), collapse = "/"),
                            " 估計失敗：", conditionMessage(e), call. = FALSE)
                    # 欄位須與成功時完全一致，否則 rbindlist 會失敗
                    list(estimate = NA_real_, se = NA_real_,
                         var_sampling = NA_real_, var_imputation = NA_real_,
                         df = NA_real_, n = nrow(sub), M = length(pv_vars),
                         var_method = NA_character_)
                  })
    res[[i]] <- cbind(keys[i], as.data.table(r))
  }
  rbindlist(res)
}

# ---- 非 PV 變數（如 ESCS）的加權統計 + BRR --------------------------------
pisa_var_stat <- function(data, x, FUN = wmean, w = "W_FSTUWT",
                          rw_prefix = "W_FSTURWT", G = 80, fay = 0.5, ...) {
  data <- as.data.table(data)
  est  <- FUN(data[[x]], data[[w]], ...)
  rws  <- rw_names(rw_prefix, G)
  rep_est <- vapply(rws, function(r) FUN(data[[x]], data[[r]], ...), numeric(1))
  v <- brr_var(rep_est, est, G, fay)
  list(estimate = est, se = sqrt(v), var_sampling = v, var_imputation = 0,
       df = Inf, n = nrow(data), M = 1L)
}

# ---- PV 為依變數的加權迴歸 -------------------------------------------------
# formula 中依變數請寫 PV_（會被逐一替換為 PV1..PV10）
pisa_pv_lm <- function(data, formula, domain, n_pv = 10,
                       w = "W_FSTUWT", rw_prefix = "W_FSTURWT", G = 80, fay = 0.5) {
  data <- as.data.table(data)
  pvs  <- pv_names(domain, n_pv)
  fstr <- paste(deparse(formula), collapse = " ")

  fit_coef <- function(pv, wt) {
    f <- as.formula(sub("PV_", pv, fstr, fixed = TRUE))
    m <- stats::lm(f, data = data, weights = wt)
    coef(m)
  }

  wv    <- data[[w]]
  C     <- sapply(pvs, function(p) fit_coef(p, wv))      # k × M
  est   <- rowMeans(C)
  rws   <- rw_names(rw_prefix, G)
  R     <- sapply(rws, function(r) fit_coef(pvs[1], data[[r]]))  # k × G
  v_samp <- apply(sweep(R, 1, C[, 1], "-")^2, 1, sum) / (G * (1 - fay)^2)
  b_m    <- apply(sweep(C, 1, est, "-")^2, 1, sum) / (n_pv - 1)
  v_imp  <- (1 + 1 / n_pv) * b_m
  se     <- sqrt(v_samp + v_imp)

  data.table(term = names(est), estimate = est, se = se,
             t = est / se, var_sampling = v_samp, var_imputation = v_imp)
}

# ---- 跨輪次差異：必須加入連結誤差 -----------------------------------------
# PISA 各輪分數位於同一連結量尺上，跨輪比較的標準誤要納入連結誤差，
# 否則型一錯誤率會被嚴重低估。數值須查對應輪次的 Technical Report。
pisa_trend_diff <- function(est1, se1, est2, se2, link_error = 0) {
  d  <- est2 - est1
  se <- sqrt(se1^2 + se2^2 + link_error^2)
  data.table(diff = d, se = se, t = d / se,
             p = 2 * stats::pnorm(-abs(d / se)),
             ci_lo = d - 1.96 * se, ci_hi = d + 1.96 * se,
             link_error = link_error)
}

# ---- 精熟等級切點 ---------------------------------------------------------
# 來源：PISA 2022 Technical Report, Annex Tables 17.A.2 / 17.A.12 / 17.A.13 / 17.A.14
# （已於 2026-08-27 逐值核對原始技術報告）
PISA_CUTS <- list(
  MATH = c(`1c` = 233.17, `1b` = 295.47, `1a` = 357.77, `2` = 420.07,
           `3` = 482.38, `4` = 544.68, `5` = 606.99, `6` = 669.30),
  READ = c(`1c` = 189.33, `1b` = 262.04, `1a` = 334.75, `2` = 407.47,
           `3` = 480.18, `4` = 552.89, `5` = 625.61, `6` = 698.32),
  SCIE = c(`1b` = 260.54, `1a` = 334.94, `2` = 409.54, `3` = 484.14,
           `4` = 558.73, `5` = 633.33, `6` = 707.93),
  FLIT = c(`1` = 325.57, `2` = 400.33, `3` = 475.10, `4` = 549.86, `5` = 624.63)
)

# 給定分數向量，回傳精熟等級（factor）
pisa_level <- function(x, domain) {
  cuts <- PISA_CUTS[[toupper(domain)]]
  if (is.null(cuts)) stop("未知領域：", domain)
  lv <- cut(x, breaks = c(-Inf, cuts, Inf), right = FALSE,
            labels = c(paste0("Below ", names(cuts)[1]), names(cuts)))
  lv
}

# ---- 連結誤差對照表 -------------------------------------------------------
# 來源：PISA 2022 Technical Report, Annex Table 14.A.19
#       "Linking error for score comparisons between PISA 2022 and previous PISA cycles"
# NA 表示該領域在該輪次尚非主測領域，OECD 明示不可比較：
#   數學最早可比到 2003，閱讀到 2000，科學到 2006。
PISA_LINK_ERROR <- data.table::data.table(
  from  = c(2000, 2003, 2006, 2009, 2012, 2015, 2018),
  to    = 2022,
  MATH  = c(   NA, 5.55, 4.09, 4.28, 3.58, 2.74, 2.24),
  READ  = c( 6.67, 5.25, 8.56, 4.66, 6.01, 3.63, 1.47),
  SCIE  = c(   NA,   NA, 3.68, 5.92, 5.20, 1.38, 1.61),
  FLIT  = c(   NA,   NA,   NA,   NA, 4.05, 3.47, 2.20)
)

# 查表：取得某兩輪次、某領域的連結誤差
get_link_error <- function(from_cycle, to_cycle = 2022, domain) {
  d <- toupper(domain)
  tbl <- as.data.frame(PISA_LINK_ERROR)
  if (!d %in% names(tbl)) stop("未知領域：", domain)
  if (to_cycle != 2022) stop("目前僅內建與 PISA 2022 比較的連結誤差；",
                             "其他組合請查對應輪次的 Technical Report。")
  i <- which(tbl$from == from_cycle)
  if (!length(i)) stop("查無 PISA ", from_cycle, " 的連結誤差")
  v <- tbl[i, d]
  if (is.na(v)) stop(domain, " 在 PISA ", from_cycle,
                     " 尚非主測領域，OECD 明示不可與 2022 比較")
  v
}
