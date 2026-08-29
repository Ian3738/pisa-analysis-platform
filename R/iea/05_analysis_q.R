# ============================================================
# 05_analysis_q.R — 四套評比的問卷分析
#
# 產出結構一致，方便網站以同一套元件呈現：
#   scales    各量表 × 各國的加權平均與 JK2 標準誤
#   profile   臺灣與國際中位數的差距（僅取有實質跨國變異者）
#   grad      量表對成就的梯度（每一個標準差幾分）
#   icc       校間變異比例
#   inv       跨國變異查核（全距、相異值數）——防止拿被中心化的量表排名
# ============================================================
suppressPackageStartupMessages({library(data.table); library(arrow); library(jsonlite)})
source("~/PISA/R/iea/lib_timss.R"); source("~/PISA/R/iea/lib_timss_q.R")
rp <- function(p) as.data.table(read_parquet(path.expand(p)))
log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# 跨國變異查核：全距趨近於零代表量表已在各國內中心化，排名無意義
inv_check <- function(d, vars, w, by = "CNT") {
  rbindlist(lapply(vars, function(v) {
    m <- d[is.finite(get(v)) & is.finite(get(w)), .(mu = weighted.mean(get(v), get(w))), by = by]
    if (nrow(m) < 3) return(data.table(v = v, n_country = nrow(m), range = NA_real_, distinct = NA_integer_))
    data.table(v = v, n_country = nrow(m), range = diff(range(m$mu)), distinct = uniqueN(round(m$mu, 3)))
  }))
}

# 一個量表 × 全部國家；回傳含名次
scale_by_country <- function(d, v, w, zone, rep) {
  r <- timss_stat_by(d[is.finite(get(v))], v, by = "CNT", w = w, zone = zone, rep = rep)
  r[, `:=`(idx = v, rank = frank(-estimate, ties.method = "min"), ntot = .N)]
  r[]
}

run_scales <- function(d, vars, w = "TOTWGT", zone = "JKZONE", rep = "JKREP", tag = "") {
  rbindlist(lapply(vars, function(v) {
    log_msg("    ", tag, " ", v)
    scale_by_country(d, v, w, zone, rep)
  }))
}

# 梯度：成就 ~ 標準化量表，逐國估計
grad_by_country <- function(d, pv, x, w = "TOTWGT", zone = "JKZONE", rep = "JKREP", min_n = 200L) {
  cs <- sort(unique(d$CNT))
  rbindlist(lapply(cs, function(k) {
    dk <- d[CNT == k & is.finite(get(x))]
    if (nrow(dk) < min_n) return(NULL)
    r <- tryCatch(timss_pv_lm(dk, as.formula(paste("PV_ ~", x)), pv, w, zone, rep, std = TRUE),
                  error = function(e) NULL)
    if (is.null(r)) return(NULL)
    b <- r[term == x]
    data.table(CNT = k, idx = x, beta = b$estimate, se = b$se, n = b$n)
  }))
}

# ---- 中文對照 -------------------------------------------------------------
# IEA 的合成量表一律「高分＝較有利」，包括反向構念：
#   BSBGSB  類別「幾乎從不（被霸凌）」對應最高分 11.75
#   BSBGDML 類別「很少或沒有（秩序問題）」對應最高分 13.17
#   BCBGMRS 類別「未受影響（資源短缺）」對應最高分 12.62
# 中文標籤因此一律改寫成正向說法，否則圖表會把意思講反。
# 上述方向皆以「IDX 各類別的 SCL 平均」實測確認，非依文件推定。
ZH_SCALE <- c(
  HER="家庭教育資源", HRL="家庭學習資源", SSB="學校歸屬感", SB="免於霸凌",
  SLM="喜歡學數學", ICM="數學教學清晰度", DML="數學課堂秩序",
  SCM="數學自信", SVM="重視數學",
  SLS="喜歡學自然", ICS="自然教學清晰度", DSL="自然課堂秩序",
  SCS="自然自信", SVS="重視自然",
  SEC="數位自我效能", VEP="重視環境保護",
  SLR="喜歡閱讀", SCR="閱讀自信", ERL="閱讀課投入程度", DRL="閱讀課堂秩序",
  EAS="學校對學業的強調", DAS="學校紀律良好程度", SOS="學校安全有序程度",
  MRS="數學教學資源充足度", SRS="自然教學資源充足度",
  RRS="閱讀教學資源充足度", LNS="學生入學準備度",
  TJS="教師工作滿意度", LSN="教學不受學生準備不足所限", SLI="教學不受學生準備不足所限",
  SES="家庭社經地位", ELA="入學前讀寫活動", ENA="入學前數學活動",
  ELN="入學前讀寫與數學活動", ELT="入學前識字任務",
  PCS="家長對學校的觀感", PLR="家長喜歡閱讀")
zh_of <- function(v) { k <- sub("^[A-Z]{2}[BD][GH]", "", v); ifelse(is.na(ZH_SCALE[k]), v, ZH_SCALE[k]) }

# ---- TIMSS -----------------------------------------------------------------
# 分科量表（生物／地科／化學／物理）僅分科授課的國家有值，臺灣為統整課程，
# 全部缺失。納入總表但排除於剖面圖之外，否則會出現空白列。
G8_CORE <- c("BSBGHER","BSBGSSB","BSBGSB","BSBGSLM","BSBGICM","BSBGDML","BSBGSCM",
             "BSBGSVM","BSBGSLS","BSBGICS","BSBGDSL","BSBGSCS","BSBGSVS","BSBGSEC","BSBGVEP")
G4_CORE <- c("ASBGHRL","ASBGSSB","ASBGSB","ASBGSLM","ASBGICM","ASBGDML","ASBGSCM",
             "ASBGSLS","ASBGICS","ASBGDSL","ASBGSCS","ASBGSEC","ASBGVEP")

analyse_timss <- function() {
  out <- list()
  log_msg("TIMSS 八年級：合併成就與學生問卷")
  A <- rp("~/TIMSS/parquet/BSA_2023.parquet"); G <- rp("~/TIMSS/parquet/BSG_2023.parquet")
  d <- merge(A, G, by = c("IDCNTRY","IDSCHOOL","IDSTUD"), all.x = TRUE)
  setnames(d, "CNT.x", "CNT"); d[, CNT.y := NULL]
  vars <- intersect(G8_CORE, names(d))
  out$g8_inv    <- inv_check(d, vars, "TOTWGT")
  out$g8_scales <- run_scales(d, vars, tag = "G8")
  log_msg("  G8 梯度（家庭資源／歸屬感／自信 → 數學）")
  out$g8_grad <- rbindlist(lapply(c("BSBGHER","BSBGSSB","BSBGSCM"),
    function(x) grad_by_country(d, timss_pv("BSMMAT"), x)))
  log_msg("  G8 校間變異分解")
  out$g8_icc <- rbindlist(lapply(sort(unique(d$CNT)), function(k) {
    r <- timss_vdecomp(d[CNT == k], timss_pv("BSMMAT")); if (is.null(r)) return(NULL)
    data.table(CNT = k, icc = r$icc, se = r$se_icc, total = r$total, between = r$between)
  }))

  log_msg("TIMSS 八年級：校長問卷")
  CG <- rp("~/TIMSS/parquet/BCG_2023.parquet")
  # 校長檔的權重是學校權重；併回學生檔後以學生權重估計「學生就讀學校的平均狀況」
  ds <- merge(d[, .(CNT, IDCNTRY, IDSCHOOL, IDSTUD, TOTWGT, JKZONE, JKREP)],
              CG[, c("IDCNTRY","IDSCHOOL", grep("^BCBG[A-Z]{2,4}$", names(CG), value=TRUE)), with=FALSE],
              by = c("IDCNTRY","IDSCHOOL"), all.x = TRUE)
  sv <- grep("^BCBG[A-Z]{2,4}$", names(ds), value = TRUE)
  out$g8_sch_inv    <- inv_check(ds, sv, "TOTWGT")
  out$g8_sch_scales <- run_scales(ds, sv, tag = "G8校長")

  log_msg("TIMSS 八年級：教師問卷（併回 BST 的教師權重）")
  TM <- rp("~/TIMSS/parquet/BTM_2023.parquet"); LK <- rp("~/TIMSS/parquet/BST_2023.parquet")
  tm <- attach_teacher_weight(TM, LK, "MATWGT")
  tv <- grep("^BTBG[A-Z]{2,4}$", names(tm), value = TRUE)
  out$g8_tch_inv    <- inv_check(tm, tv, "MATWGT")
  out$g8_tch_scales <- run_scales(tm, tv, w = "MATWGT", tag = "G8數學教師")
  out$g8_tch_n <- tm[, .(n_teacher = .N), by = CNT]

  log_msg("TIMSS 四年級")
  A4 <- rp("~/TIMSS/parquet/g4/SA.parquet"); G4 <- rp("~/TIMSS/parquet/g4/SG.parquet")
  d4 <- merge(A4, G4, by = c("IDCNTRY","IDSCHOOL","IDSTUD"), all.x = TRUE)
  setnames(d4, "CNT.x", "CNT"); d4[, CNT.y := NULL]
  v4 <- intersect(G4_CORE, names(d4))
  out$g4_inv    <- inv_check(d4, v4, "TOTWGT")
  out$g4_scales <- run_scales(d4, v4, tag = "G4")
  out$g4_grad   <- grad_by_country(d4, timss_pv("ASMMAT"), "ASBGHRL")
  out$g4_icc    <- rbindlist(lapply(sort(unique(d4$CNT)), function(k) {
    r <- timss_vdecomp(d4[CNT == k], timss_pv("ASMMAT")); if (is.null(r)) return(NULL)
    data.table(CNT = k, icc = r$icc, se = r$se_icc, total = r$total, between = r$between)
  }))
  saveRDS(out, path.expand("~/TIMSS/output/q_timss.rds"))
  log_msg("寫出 q_timss.rds")
  invisible(out)
}

# ---- PIRLS ------------------------------------------------------------------
analyse_pirls <- function() {
  out <- list()
  log_msg("PIRLS 2021：學生問卷")
  A <- rp("~/PIRLS/parquet/ASA_2021.parquet"); G <- rp("~/PIRLS/parquet/SG.parquet")
  d <- merge(A, G, by = c("IDCNTRY","IDSCHOOL","IDSTUD"), all.x = TRUE)
  if ("CNT.x" %in% names(d)) { setnames(d, "CNT.x", "CNT"); d[, CNT.y := NULL] }
  sv <- grep("^ASBG[A-Z]{2,4}$", names(d), value = TRUE)
  out$stu_inv    <- inv_check(d, sv, "TOTWGT")
  out$stu_scales <- run_scales(d, sv, tag = "PIRLS學生")
  pv <- grep("^ASRREA[0-9]{2}$", names(d), value = TRUE)
  log_msg("  梯度與校間變異")
  out$stu_grad <- rbindlist(lapply(c("ASBGHRL","ASBGSSB","ASBGSCR","ASBGSLR"),
                                   function(x) grad_by_country(d, pv, x)))
  out$icc <- rbindlist(lapply(sort(unique(d$CNT)), function(k) {
    r <- timss_vdecomp(d[CNT == k], pv); if (is.null(r)) return(NULL)
    data.table(CNT = k, icc = r$icc, se = r$se_icc, total = r$total, between = r$between)
  }))

  log_msg("PIRLS 2021：家長問卷")
  H <- rp("~/PIRLS/parquet/SH.parquet")
  dh <- merge(d[, c("CNT","IDCNTRY","IDSCHOOL","IDSTUD","TOTWGT","JKZONE","JKREP", pv), with=FALSE],
              H[, c("IDCNTRY","IDSCHOOL","IDSTUD",
                    grep("^ASBH[A-Z]{2,4}$", names(H), value=TRUE)), with=FALSE],
              by = c("IDCNTRY","IDSCHOOL","IDSTUD"), all.x = TRUE)
  hv <- grep("^ASBH[A-Z]{2,4}$", names(dh), value = TRUE)
  out$home_inv    <- inv_check(dh, hv, "TOTWGT")
  out$home_scales <- run_scales(dh, hv, tag = "PIRLS家長")
  out$home_grad   <- rbindlist(lapply(c("ASBHELA","ASBHELT","ASBHSES","ASBHPLR"),
                                      function(x) grad_by_country(dh, pv, x)))
  out$home_cov <- dh[, .(n = .N, n_home = sum(!is.na(ASBHSES))), by = CNT][, resp := n_home/n][]

  log_msg("PIRLS 2021：校長問卷")
  CG <- rp("~/PIRLS/parquet/CG.parquet")
  cv0 <- grep("^ACBG[A-Z]{2,4}$", names(CG), value = TRUE)
  ds <- merge(d[, .(CNT, IDCNTRY, IDSCHOOL, IDSTUD, TOTWGT, JKZONE, JKREP)],
              CG[, c("IDCNTRY","IDSCHOOL", cv0), with=FALSE],
              by = c("IDCNTRY","IDSCHOOL"), all.x = TRUE)
  out$sch_inv    <- inv_check(ds, cv0, "TOTWGT")
  out$sch_scales <- run_scales(ds, cv0, tag = "PIRLS校長")

  log_msg("PIRLS 2021：教師問卷")
  TG <- rp("~/PIRLS/parquet/TG.parquet"); LK <- rp("~/PIRLS/parquet/ST.parquet")
  if (!is.null(LK)) {
    tg <- attach_teacher_weight(TG, LK, "TCHWGT")
    tv <- grep("^ATBG[A-Z]{2,4}$", names(tg), value = TRUE)
    out$tch_inv    <- inv_check(tg, tv, "TCHWGT")
    out$tch_scales <- run_scales(tg, tv, w = "TCHWGT", tag = "PIRLS教師")
    out$tch_n <- tg[, .(n_teacher = .N), by = CNT]
  }
  saveRDS(out, path.expand("~/PIRLS/output/q_pirls.rds"))
  log_msg("寫出 q_pirls.rds"); invisible(out)
}

# ---- ICCS -------------------------------------------------------------------
# ICCS 的量表為 IRT 量尺化，國際平均 50、標準差 10（TIMSS／PIRLS 為 10／2）。
# 中文名稱依官方變數標籤逐條對照，未依代碼推測。
ZH_ICCS <- c(
  S_ATTENV="對環境保護的正向態度", S_CITCON="重視傳統型公民參與",
  S_CITEFF="公民自我效能", S_CITSOC="重視社會運動型公民參與",
  S_CIVLRN="在校的公民學習經驗", S_COMPART="參與社區團體",
  S_DEMTHRT="認為民主正受到威脅", S_ELECPART="預期未來的投票參與",
  S_ENGDM="以數位媒體參與公共議題", S_ENREST="支持緊急狀態下限縮權利",
  S_ENVACT="預期參與環保行動", S_ENVCON="關切全球環境威脅",
  S_ETHRGHT="支持各族群平等權利", S_GENEQL="支持性別平等",
  S_GLOBCIT="重視全球公民參與", S_ILLACT="預期參與非法抗議",
  S_IMMPOS="對移民的正向態度", S_INFDEC="認為能影響學校決策",
  S_INTACT="對同學互動的觀感", S_INTRUST="對公民機構的信任",
  S_LEGACT="預期參與合法抗議", S_OPDISC="課堂討論的開放程度",
  S_POLDISC="校外討論政治社會議題", S_POLPART="預期積極政治參與",
  S_RELINF="對宗教影響社會的態度", S_SCACT="願意參與學校活動",
  S_SCHPART="在校的公民活動參與", S_STUTREL="對師生關係的觀感",
  S_SYSCRT="對政治體制的批判", S_SYSSAT="對政治體制的滿意",
  S_NISB="家庭社經地位", S_HISEI="父母最高職業地位", S_HOMLIT="家庭藏書與讀寫資源",
  T_ACTDIG="數位科技相關活動", T_ACTGLOB="全球議題相關活動",
  T_CITCON="教師重視傳統型公民參與", T_CITSOC="教師重視社會運動型參與",
  T_CIVCLAS="課堂中的公民相關活動", T_DIVACT="處理多元差異的活動",
  T_GLOBCIT="教師重視全球公民", T_NEGCDIF="認為文化族群差異造成教學困難",
  T_NEGSDIF="認為社經差異造成教學困難", T_OPPLRN="學生學習公民主題的機會",
  T_PCCLIM="對課堂氣氛的觀感", T_PDACCE="公民教育主題的專業發展",
  T_PDATCH="教學方法的專業發展", T_POSCDIF="認為文化族群差異對教學有益",
  T_POSSDIF="認為社經差異對教學有益", T_PROBSC="覺察學校的社會問題",
  T_PRPCCE="教授公民教育的準備度", T_STDCOM="學生的社區活動",
  T_STDINV="學生的活動參與", T_TCHPRT="教師參與校務的程度",
  C_ACTDIGT="數位科技運用的培訓", C_AVRESCOM="社區資源的可得性",
  C_COMCRI="覺察社區的犯罪緊張", C_COMETN="覺察社區的族群緊張",
  C_COMPOV="覺察社區的貧窮緊張", C_ENPRAC="學校的環境友善作為",
  C_PARINV="家長參與學校的程度", C_STDCOM="學生的社區活動",
  C_STDINV="學生參與校務的程度", C_TCPART="教師參與校務治理")
zh_iccs <- function(v) ifelse(is.na(ZH_ICCS[v]), v, ZH_ICCS[v])

ICCS_ATT <- c("S_ATTENV","S_CITCON","S_CITEFF","S_CITSOC","S_CIVLRN","S_COMPART",
  "S_DEMTHRT","S_ELECPART","S_ENGDM","S_ENREST","S_ENVACT","S_ENVCON","S_ETHRGHT",
  "S_GENEQL","S_GLOBCIT","S_ILLACT","S_IMMPOS","S_INFDEC","S_INTACT","S_INTRUST",
  "S_LEGACT","S_OPDISC","S_POLDISC","S_POLPART","S_RELINF","S_SCACT","S_SCHPART",
  "S_STUTREL","S_SYSCRT","S_SYSSAT")

analyse_iccs <- function() {
  out <- list(); PVC <- paste0("PV", 1:5, "CIV")
  W <- list(w="TOTWGTS", zone="JKZONES", rep="JKREPS")

  log_msg("ICCS 2022：學生態度量表")
  A <- rp("~/ICCS/parquet/ISA_2022.parquet"); G <- rp("~/ICCS/parquet/ISG_2022.parquet")
  d <- merge(A, G[, setdiff(names(G), c("CNT","TOTWGTS","JKZONES","JKREPS")), with=FALSE],
             by = c("IDCNTRY","IDSTUD","IDSCHOOL"), all.x = TRUE)
  av <- intersect(c(ICCS_ATT, "S_NISB"), names(d))
  out$stu_inv    <- inv_check(d, av, "TOTWGTS")
  out$stu_scales <- run_scales(d, av, w=W$w, zone=W$zone, rep=W$rep, tag="ICCS學生")
  log_msg("  梯度（社經地位／公民自我效能／課堂開放度 → 公民知識）")
  out$stu_grad <- rbindlist(lapply(c("S_NISB","S_CITEFF","S_OPDISC","S_CIVLRN"),
    function(x) grad_by_country(d, PVC, x, w=W$w, zone=W$zone, rep=W$rep)))
  out$icc <- rbindlist(lapply(sort(unique(d$CNT)), function(k) {
    r <- timss_vdecomp(d[CNT==k], PVC, w=W$w, zone=W$zone, rep=W$rep); if (is.null(r)) return(NULL)
    data.table(CNT=k, icc=r$icc, se=r$se_icc, total=r$total, between=r$between)
  }))

  log_msg("ICCS 2022：教師問卷")
  TG <- rp("~/ICCS/parquet/ITG_2022.parquet")
  tv <- grep("^T_", names(TG), value=TRUE); tv <- setdiff(tv, c("T_TIME","T_AGE","T_CCESUB"))
  out$tch_inv    <- inv_check(TG, tv, "TOTWGTT")
  out$tch_scales <- run_scales(TG, tv, w="TOTWGTT", zone="JKZONET", rep="JKREPT", tag="ICCS教師")
  out$tch_n <- TG[, .(n_teacher=.N), by=CNT]

  log_msg("ICCS 2022：校長問卷（併至學生層次，以學生權重估計）")
  CG <- rp("~/ICCS/parquet/ICG_2022.parquet")
  cv <- grep("^C_", names(CG), value=TRUE); cv <- setdiff(cv, c("C_SCSIZE_CAT","C_GENROL_CAT","C_URBAN","C_TGPERC"))
  ds <- merge(d[, c("CNT","IDCNTRY","IDSCHOOL","TOTWGTS","JKZONES","JKREPS"), with=FALSE],
              CG[, c("IDCNTRY","IDSCHOOL", cv), with=FALSE], by=c("IDCNTRY","IDSCHOOL"), all.x=TRUE)
  out$sch_inv    <- inv_check(ds, cv, "TOTWGTS")
  out$sch_scales <- run_scales(ds, cv, w=W$w, zone=W$zone, rep=W$rep, tag="ICCS校長")

  log_msg("ICCS 2016 → 2022 趨勢")
  G16 <- rp("~/ICCS/parquet/ISG_2016.parquet")
  k22 <- timss_pv_by(d,  PVC, by="CNT", w=W$w, zone=W$zone, rep=W$rep)[, cycle := 2022L]
  k16 <- timss_pv_by(G16, PVC, by="CNT", w=W$w, zone=W$zone, rep=W$rep)[, cycle := 2016L]
  out$know_trend <- rbind(k16, k22, fill=TRUE)
  common <- intersect(intersect(ICCS_ATT, names(G16)), names(d))
  log_msg("  兩輪共通的態度量表 ", length(common), " 個")
  a22 <- run_scales(d,   common, w=W$w, zone=W$zone, rep=W$rep, tag="2022")[, cycle := 2022L]
  a16 <- run_scales(G16, common, w=W$w, zone=W$zone, rep=W$rep, tag="2016")[, cycle := 2016L]
  out$att_trend <- rbind(a16, a22, fill=TRUE)
  out$common_scales <- common
  saveRDS(out, path.expand("~/ICCS/output/q_iccs.rds"))
  log_msg("寫出 q_iccs.rds"); invisible(out)
}
