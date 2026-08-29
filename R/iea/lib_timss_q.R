# ============================================================
# lib_timss_q.R — lib_timss.R 的延伸：問卷量表、迴歸與變異數分解
#
# lib_timss.R 只處理「推估值 × JK2」。問卷量表不是推估值——它是每位受訪者
# 的單一觀測分數，所以沒有推估變異，變異數只有抽樣一項：
#     V = Σ_h (θ_h − θ)²
# 形式與推估值版相同，少的是 Rubin 規則那一層。把量表當成推估值來跑
# （例如硬湊 5 個相同欄位）會多算一次測量變異，標準誤偏大。
#
# 教師層次另有一個坑：教師檔（BTM／BTS／ATG）本身不含權重。教師權重
# TCHWGT 與折刀分區存放在學生—教師連結檔（BST／AST），必須先以
# IDTEACH + IDLINK 併回。直接算教師檔會得到未加權的結果。
# ============================================================
suppressPackageStartupMessages({library(data.table)})

# ---- 單一觀測變數的 JK2 估計 ---------------------------------------------
timss_stat <- function(data, var, FUN = wmean_t, w = "TOTWGT",
                       zone = "JKZONE", rep = "JKREP") {
  d <- data[is.finite(get(var)) & is.finite(get(w)) & !is.na(get(zone))]
  if (nrow(d) < 30L)
    return(list(estimate = NA_real_, se = NA_real_, n = nrow(d), G = 0L))
  x <- d[[var]]; wt <- d[[w]]; z <- d[[zone]]; r <- d[[rep]]
  est <- FUN(x, wt)
  zs  <- sort(unique(z))
  v   <- sum(vapply(zs, function(h) (FUN(x, jk2_weight(wt, z, r, h)) - est)^2, numeric(1)))
  list(estimate = est, se = sqrt(v), n = nrow(d), G = length(zs))
}

timss_stat_by <- function(data, var, by = "CNT", FUN = wmean_t, ...) {
  ks <- sort(unique(data[[by]]))
  rbindlist(lapply(ks, function(k) {
    r <- tryCatch(timss_stat(data[get(by) == k], var, FUN = FUN, ...),
                  error = function(e) list(estimate = NA_real_, se = NA_real_, n = 0L, G = 0L))
    c(setNames(list(k), by), r)
  }))
}

# ---- 推估值迴歸（JK2 × 5 個推估值，Rubin 規則）---------------------------
# formula 內以 PV_ 代表推估值的位置，例如 PV_ ~ BSBGHER
timss_pv_lm <- function(data, formula, pv_vars, w = "TOTWGT",
                        zone = "JKZONE", rep = "JKREP", std = FALSE) {
  f  <- deparse(formula)
  fs <- lapply(pv_vars, function(p) as.formula(sub("PV_", p, f, fixed = TRUE)))
  vars <- unique(c(all.vars(formula)[-1], pv_vars, w, zone, rep))
  vars <- intersect(vars, names(data))
  d <- na.omit(data[, ..vars])
  if (nrow(d) < 50L) return(NULL)
  if (std) {                                  # 標準化預測變數 → 係數即「每一個標準差」
    for (v in setdiff(all.vars(formula)[-1], pv_vars))
      if (is.numeric(d[[v]])) set(d, j = v, value = as.numeric(scale(d[[v]])))
  }
  zs <- sort(unique(d[[zone]]))
  # lm() 的 weights 先在 data 裡找，找不到才回到 formula 的環境。折刀權重是在
  # 內層匿名函式裡產生的，不在 formula 的環境鏈上，因此必須寫成 data 的欄位，
  # 否則會拋出「找不到物件」。
  set(d, j = ".wt", value = d[[w]])
  cf <- lapply(fs, function(ff) coef(lm(ff, data = d, weights = .wt)))
  est <- Reduce(`+`, cf) / length(cf)
  # 抽樣變異：對每個推估值各跑一次折刀，再取平均（OECD／IEA 手冊的完整作法）
  w0 <- d[[w]]; zv <- d[[zone]]; rv <- d[[rep]]
  vs <- Reduce(`+`, lapply(seq_along(fs), function(i) {
    rowSums(vapply(zs, function(h) {
      set(d, j = ".wt", value = jk2_weight(w0, zv, rv, h))
      (coef(lm(fs[[i]], data = d, weights = .wt)) - cf[[i]])^2
    }, numeric(length(est))))
  })) / length(fs)
  bm <- if (length(cf) > 1)
    rowSums(vapply(cf, function(c1) (c1 - est)^2, numeric(length(est)))) / (length(cf) - 1) else 0
  se <- sqrt(vs + (1 + 1 / length(cf)) * bm)
  data.table(term = names(est), estimate = as.numeric(est), se = as.numeric(se),
             t = as.numeric(est / se), n = nrow(d))
}

# ---- 設計基礎的變異數分解（校間／校內），與 PISA 版同一定義 --------------
timss_vdecomp <- function(data, pv_vars, sch = "IDSCHOOL", w = "TOTWGT",
                          zone = "JKZONE", rep = "JKREP") {
  raw <- function(x, wt, g) {
    ok <- is.finite(x) & is.finite(wt) & !is.na(g)
    if (sum(ok) < 30L) return(c(total = NA_real_, between = NA_real_, within = NA_real_, icc = NA_real_))
    x <- x[ok]; wt <- wt[ok]; g <- g[ok]
    sw <- sum(wt); gm <- sum(x * wt) / sw
    tot <- sum(wt * (x - gm)^2) / sw
    Wg <- rowsum(wt, g, reorder = FALSE); mg <- rowsum(x * wt, g, reorder = FALSE) / Wg
    btw <- sum(Wg * (mg - gm)^2) / sw
    c(total = tot, between = btw, within = tot - btw, icc = btw / tot)
  }
  d <- data[is.finite(get(w)) & !is.na(get(sch))]
  if (!nrow(d)) return(NULL)
  est <- Reduce(`+`, lapply(pv_vars, function(p) raw(d[[p]], d[[w]], d[[sch]]))) / length(pv_vars)
  zs <- sort(unique(d[[zone]]))
  v <- Reduce(`+`, lapply(pv_vars, function(p) {
    e1 <- raw(d[[p]], d[[w]], d[[sch]])
    rowSums(vapply(zs, function(h)
      (raw(d[[p]], jk2_weight(d[[w]], d[[zone]], d[[rep]], h), d[[sch]]) - e1)^2,
      numeric(4)))
  })) / length(pv_vars)
  as.list(c(est, setNames(sqrt(v), paste0("se_", names(est)))))
}

# ---- 教師檔併回權重 -------------------------------------------------------
# link 為 BST／AST，其 TCHWGT 逐「學生×教師」列重複，故先摺成教師層次
attach_teacher_weight <- function(tch, link, wcol = "TCHWGT") {
  keys <- c("CNT", "IDSCHOOL", "IDTEACH", "IDLINK")
  keys <- intersect(keys, intersect(names(tch), names(link)))
  agg <- link[, .(w = mean(get(wcol), na.rm = TRUE),
                  JKZONE = as.integer(names(sort(table(JKZONE), decreasing = TRUE))[1]),
                  JKREP  = as.integer(names(sort(table(JKREP),  decreasing = TRUE))[1]),
                  n_stu  = .N), by = keys]
  setnames(agg, "w", wcol)
  merge(tch, agg, by = keys, all.x = FALSE)
}
