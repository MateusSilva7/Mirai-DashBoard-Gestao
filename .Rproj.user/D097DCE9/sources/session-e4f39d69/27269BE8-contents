# ============================================================
# MIRAI OKR — app.R  (estilo app_public.R / BSBStay)
# fluidPage + CSS inline + renderUI + helpers funcionais
# Sem bslib. Layout: sidebar fixa 220px + main margin-left.
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(plotly)
  library(DBI)
  library(RSQLite)
})

source("modules/database.R")

init_database()

# ── helpers de formatação ──────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

pct_fmt <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) return("—")
  paste0(round(v, 1), "%")
}

num_fmt <- function(x, digits = 0) {
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) return("—")
  formatC(v, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

# ── helper: kcard (igual ao BSBStay) ──────────────────────
kcard <- function(lbl, val, delta = "", dn = FALSE, vg = FALSE,
                  icon = "", extra_class = "", color = NULL) {
  style_val <- if (!is.null(color)) paste0("color:", color, ";") else ""
  div(
    class = paste("kcard", extra_class),
    div(class = "klbl", if (nzchar(icon)) paste(icon, lbl) else lbl),
    div(class = if (vg) "kval g" else "kval", style = style_val, val),
    div(class = if (dn) "kdelta dn" else "kdelta up", delta)
  )
}

# ── helper: linha de resultado financeiro ─────────────────
frow <- function(lbl, val, neg = FALSE) {
  div(class = "fr",
      span(class = "fl", lbl),
      span(class = if (neg) "fv r" else "fv", val))
}

# ── helper: badge semáforo ─────────────────────────────────
status_badge <- function(status) {
  if (is.na(status)) return(tags$span(class = "sbadge sb-nd", "SEM DADOS"))
  switch(status,
         verde    = tags$span(class = "sbadge sb-verde",    "● No Alvo"),
         amarelo  = tags$span(class = "sbadge sb-amarelo",  "◐ Em Risco"),
         vermelho = tags$span(class = "sbadge sb-vermelho", "● Crítico"),
         tags$span(class = "sbadge sb-nd", "SEM DADOS")
  )
}

# ── helper: barra de progresso inline ─────────────────────
prog_bar <- function(pct, status = "verde") {
  pct   <- min(max(pct %||% 0, 0), 100)
  color <- switch(status %||% "nd",
                  verde    = "#18B884",
                  amarelo  = "#F5A623",
                  vermelho = "#FF3B4E",
                  "#c8d4de"
  )
  div(class = "pb-wrap",
      div(class = "pb-fill",
          style = paste0("width:", pct, "%;background:", color, ";")))
}

# ── SVG logo (versão branca para sidebar) ─────────────────
# viewBox cobre todos os paths: x 82→1062, y 12→249
LOGO_SVG <- '
<svg viewBox="80 0 990 260" xmlns="http://www.w3.org/2000/svg"
     style="width:130px;height:auto;display:block;">
  <path fill="#fff" d="M282.36,107c17.44,0,31.28,5.29,41.52,15.87,10.4,10.58,15.6,25.27,15.6,44.07v82.1h-42.78v-76.3c0-9.07-2.43-16.03-7.3-20.9-4.7-5.04-11.24-7.55-19.63-7.55s-15.01,2.52-19.88,7.55c-4.7,4.87-7.04,11.84-7.04,20.9v76.3h-42.77v-76.3c0-9.07-2.43-16.03-7.3-20.9-4.7-5.04-11.24-7.55-19.63-7.55s-15.01,2.52-19.88,7.55c-4.7,4.87-7.05,11.84-7.05,20.9v76.3h-43.03v-140.52h43.03v17.63c4.36-5.88,10.06-10.49,17.11-13.85,7.05-3.53,15.01-5.29,23.9-5.29,10.57,0,19.96,2.27,28.18,6.8,8.39,4.53,14.93,11,19.63,19.39,4.87-7.72,11.49-14.02,19.88-18.89,8.39-4.87,17.53-7.3,27.43-7.3Z"/>
  <path fill="#fff" d="M405.05,108.5v140.52h-43.03v-140.52h43.03Z"/>
  <path fill="#fff" d="M471.7,131.9c5.03-7.72,11.32-13.77,18.87-18.13,7.55-4.53,15.94-6.8,25.16-6.8v45.58h-11.83c-10.74,0-18.79,2.35-24.15,7.05-5.37,4.53-8.05,12.59-8.05,24.18v65.22h-43.03v-140.52h43.03v23.42Z"/>
  <path fill="#fff" d="M520.2,178.5c0-14.44,2.68-27.11,8.05-38.03,5.54-10.91,13-19.31,22.39-25.18,9.39-5.88,19.88-8.81,31.45-8.81,9.9,0,18.53,2.01,25.92,6.04,7.55,4.03,13.34,9.32,17.36,15.86v-19.89h43.03v140.52h-43.03v-19.89c-4.19,6.55-10.07,11.84-17.61,15.87-7.38,4.03-16.02,6.04-25.92,6.04-11.41,0-21.81-2.94-31.2-8.81-9.39-6.04-16.86-14.52-22.39-25.43-5.37-11.08-8.05-23.84-8.05-38.28ZM625.38,178.7c0-10.74-3.02-19.22-9.06-25.43-5.87-6.21-13.09-9.32-21.64-9.32s-15.85,3.11-21.89,9.32c-5.87,6.04-8.81,14.44-8.81,25.18s2.94,19.31,8.81,25.69c6.04,6.21,13.33,9.32,21.89,9.32s15.77-3.11,21.64-9.32c6.04-6.21,9.06-14.69,9.06-25.43Z"/>
  <path fill="#fff" d="M735.01,108.5v140.52h-43.03v-140.52h43.03Z"/>
  <path fill="#3271FE" d="M1061.03,12.42h-43.64l-127.82,126.27v43.65h43.65l127.82-126.26v-43.65Z"/>
  <circle fill="#3271FE" cx="810.66" cy="44.14" r="31.8"/>
  <path fill="#3271FE" d="M950.35,12.42h-43.64l-127.82,126.27v43.65h43.65l127.82-126.26v-43.65Z"/>
</svg>'

# ── CSS completo (mesmo estilo app_public.R) ───────────────
APP_CSS <- "
/* ══ RESET ════════════════════════════════════════════════ */
*{box-sizing:border-box;margin:0;padding:0;}
body{font-family:'Manrope',sans-serif;background:#F5F6FA;color:#1B1B1C;font-size:14px;}
a{color:inherit;text-decoration:none;}
.form-group{margin-bottom:0!important;}
label{font-size:11px!important;font-weight:700!important;color:#6B7080!important;}

/* ══ SIDEBAR ═══════════════════════════════════════════════ */
.sidebar{
  width:220px;min-height:100vh;
  background:linear-gradient(175deg,#0A1E5E 0%,#0F2878 40%,#1742AD 100%);
  display:flex;flex-direction:column;
  position:fixed;left:0;top:0;bottom:0;z-index:100;overflow-y:auto;
}
.sb-logo{padding:20px 20px 18px;border-bottom:1px solid rgba(255,255,255,.08);}
.sb-nav{padding:14px 10px;flex:1;}
.sb-label{font-size:10px;font-weight:700;letter-spacing:.12em;color:rgba(255,255,255,.35);
  padding:0 10px;margin:16px 0 5px;text-transform:uppercase;display:block;}
.sb-item{display:flex;align-items:center;gap:10px;padding:9px 10px;border-radius:9px;
  color:rgba(255,255,255,.6);font-size:13px;font-weight:500;cursor:pointer;
  transition:all .18s;margin-bottom:2px;border:1px solid transparent;
  background:none;width:100%;text-align:left;font-family:'Manrope',sans-serif;}
.sb-item:hover{background:rgba(255,255,255,.08);color:rgba(255,255,255,.9);}
.sb-item.active{background:rgba(50,113,254,.25);color:#fff;font-weight:600;
  border-color:rgba(50,113,254,.4);}
.sb-icon{width:30px;height:30px;border-radius:7px;display:flex;align-items:center;
  justify-content:center;font-size:14px;flex-shrink:0;}
.sb-item.active .sb-icon{background:#3271FE;}
.sb-item:not(.active) .sb-icon{background:rgba(255,255,255,.07);}
.sb-badge{margin-left:auto;background:#FF3B4E;color:#fff;font-size:9px;
  font-weight:800;padding:2px 6px;border-radius:99px;}
.sb-footer{padding:14px 14px 22px;border-top:1px solid rgba(255,255,255,.08);}
.sb-user{display:flex;align-items:center;gap:10px;padding:8px 6px;
  border-radius:9px;cursor:pointer;transition:background .15s;}
.sb-user:hover{background:rgba(255,255,255,.08);}
.sb-avatar{width:32px;height:32px;border-radius:50%;background:#3271FE;
  display:flex;align-items:center;justify-content:center;
  font-size:11px;font-weight:700;color:#fff;flex-shrink:0;}
.sb-uname{font-size:12px;font-weight:600;color:#fff;}
.sb-urole{font-size:10px;color:rgba(255,255,255,.45);}

/* ══ MAIN ══════════════════════════════════════════════════ */
.main{margin-left:220px;min-height:100vh;display:flex;flex-direction:column;}

/* ══ TOPBAR ════════════════════════════════════════════════ */
.topbar{background:#fff;border-bottom:1px solid #E8EBF5;padding:0 28px;height:60px;
  display:flex;align-items:center;gap:12px;position:sticky;top:0;z-index:50;}
.tb-title{font-size:18px;font-weight:700;color:#1B1B1C;line-height:1.2;}
.tb-crumb{font-size:11px;color:#A0A6B8;}
.tb-crumb span{color:#1B1B1C;font-weight:600;}
.tb-spacer{flex:1;}
.tb-btn{background:#3271FE;color:#fff;border:none;border-radius:8px;padding:7px 18px;
  font-family:'Manrope',sans-serif;font-size:12px;font-weight:700;cursor:pointer;
  transition:background .15s;}
.tb-btn:hover{background:#1742AD;}
.tb-btn-ghost{background:none;border:1px solid #E8EBF5;border-radius:8px;padding:7px 14px;
  font-family:'Manrope',sans-serif;font-size:12px;font-weight:600;color:#6B7080;cursor:pointer;
  transition:all .15s;}
.tb-btn-ghost:hover{border-color:#3271FE;color:#3271FE;}

/* ══ FILTER BAR ════════════════════════════════════════════ */
.fbar{background:#fff;padding:10px 28px;border-bottom:2px solid #e8edf3;
  display:flex;gap:14px;align-items:center;flex-wrap:wrap;}
.fbar-lbl{font-size:11px;color:#6B7080;font-weight:700;letter-spacing:.6px;}

/* ══ CONTENT ═══════════════════════════════════════════════ */
.content{padding:22px 28px 56px;flex:1;}

/* ══ SECTION LABELS ════════════════════════════════════════ */
.sec{font-size:10px;font-weight:800;color:#6B7080;letter-spacing:1.5px;
  text-transform:uppercase;margin:26px 0 10px;padding-bottom:6px;
  border-bottom:2px solid #E8EBF5;}

/* ══ KPI CARDS ═════════════════════════════════════════════ */
.kgrid{display:grid;grid-template-columns:1.4fr repeat(4,1fr);gap:14px;margin-bottom:6px;}
@media(max-width:1200px){.kgrid{grid-template-columns:repeat(3,1fr);}}
@media(max-width:700px){.kgrid{grid-template-columns:repeat(2,1fr);}}

.kcard{background:#fff;border-radius:12px;padding:18px 20px;border:1px solid #E8EBF5;
  box-shadow:0 1px 4px rgba(0,0,0,.04);transition:box-shadow .15s;position:relative;overflow:hidden;}
.kcard::before{content:'';position:absolute;top:0;left:0;right:0;height:3px;border-radius:12px 12px 0 0;}
.kcard.kc-blue::before{background:#3271FE;}
.kcard.kc-green::before{background:#18B884;}
.kcard.kc-yellow::before{background:#F5A623;}
.kcard.kc-red::before{background:#FF3B4E;}
.kcard.kc-grad::before{background:linear-gradient(90deg,#FF3B4E 0%,#F5A623 50%,#18B884 100%);}
.kcard:hover{box-shadow:0 4px 16px rgba(50,113,254,.08);}
.kcard.hero{background:linear-gradient(145deg,#0A1E36 0%,#0F2D4A 100%);border:none;
  box-shadow:0 8px 32px rgba(0,30,60,.28);padding:22px 24px;}
.kcard.hero::before{background:rgba(255,255,255,.12);}
.kcard.hero .klbl{color:rgba(255,255,255,.55);}
.kcard.hero .kval{color:#fff;font-size:28px;}
.kcard.hero .kval.g{color:#34d99e;}
.kcard.hero .kdelta{color:rgba(255,255,255,.45);}
.kcard.hero .kdelta.up{color:#34d99e;}
.klbl{font-size:10px;font-weight:700;color:#6B7080;letter-spacing:.9px;
  text-transform:uppercase;margin-bottom:8px;}
.kval{font-size:24px;font-weight:800;color:#1B1B1C;line-height:1.1;}
.kval.g{color:#18B884;}
.kval.bl{color:#3271FE;}
.kval.yl{color:#F5A623;}
.kval.rd{color:#FF3B4E;}
.kdelta{font-size:11px;margin-top:6px;font-weight:600;color:#6B7080;}
.kdelta.up{color:#18B884;} .kdelta.dn{color:#FF3B4E;}

/* resumo mini-grid no card */
.resumo-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:8px;padding:8px 0;}
.resumo-item{display:flex;flex-direction:column;align-items:center;gap:3px;}
.resumo-num{font-size:20px;font-weight:800;line-height:1;}
.resumo-lbl{font-size:9px;font-weight:700;text-transform:uppercase;
  letter-spacing:.05em;color:#A0A6B8;}

/* ══ CARDS ══════════════════════════════════════════════════ */
.card{background:#fff;border-radius:12px;padding:20px;border:1px solid #E8EBF5;
  box-shadow:0 1px 5px rgba(0,0,0,.04);}
.cgrid{display:grid;grid-template-columns:1fr 340px;gap:14px;margin-bottom:6px;align-items:start;}
.cgrid-eq{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-bottom:6px;}
@media(max-width:1100px){.cgrid{grid-template-columns:1fr;}.cgrid-eq{grid-template-columns:1fr;}}
.card-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:14px;}
.card-ttl{font-size:13px;font-weight:700;color:#1B1B1C;}
.badge{background:#f0f4f8;color:#6B7080;font-size:10px;padding:3px 9px;
  border-radius:12px;font-weight:700;}
.badge-blue{background:#eff6ff;color:#2563eb;}
.badge-green{background:#f0fdf4;color:#16a34a;}
.badge-yellow{background:#fff7ed;color:#d97706;}
.badge-red{background:#fff1f0;color:#e03e3e;}

/* ══ OBJ CARDS ══════════════════════════════════════════════ */
.obj-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px;margin-bottom:6px;}
@media(max-width:900px){.obj-grid{grid-template-columns:1fr;}}
.obj-card{background:#fff;border-radius:12px;padding:14px 16px;border:1px solid #E8EBF5;
  box-shadow:0 1px 3px rgba(0,0,0,.04);transition:all .18s;cursor:pointer;}
.obj-card:hover{border-color:#3271FE;box-shadow:0 4px 16px rgba(50,113,254,.1);}
.obj-card.full{grid-column:1/-1;}
.obj-top{display:flex;align-items:center;gap:10px;margin-bottom:8px;}
.obj-letra{width:30px;height:30px;border-radius:8px;display:flex;align-items:center;
  justify-content:center;font-size:14px;font-weight:900;flex-shrink:0;color:#fff;}
.obj-nome{font-size:12px;font-weight:600;color:#1B1B1C;line-height:1.3;flex:1;}
.obj-pct{font-size:18px;font-weight:800;margin-left:auto;flex-shrink:0;}
.obj-footer{display:flex;align-items:center;justify-content:space-between;margin-top:6px;}
.obj-sub{font-size:10px;color:#A0A6B8;font-weight:500;}

/* semáforo badges */
.sbadge{display:inline-flex;align-items:center;gap:3px;padding:2px 8px;
  border-radius:99px;font-size:10px;font-weight:700;}
.sb-verde{background:rgba(24,184,132,.1);color:#18B884;}
.sb-amarelo{background:rgba(245,166,35,.1);color:#F5A623;}
.sb-vermelho{background:rgba(255,59,78,.1);color:#FF3B4E;}
.sb-nd{background:#f3f4f6;color:#9aa5b4;}

/* ══ PROGRESS BAR ══════════════════════════════════════════ */
.pb-wrap{height:5px;background:#F5F6FA;border-radius:99px;overflow:hidden;}
.pb-fill{height:100%;border-radius:99px;transition:width .5s ease;}

/* ══ KR ROWS (breakdown) ═══════════════════════════════════ */
.kr-row{display:flex;flex-direction:column;gap:7px;padding:12px 14px;
  background:#fafbfc;border-radius:9px;border-left:3px solid #e5e9ef;margin-bottom:8px;}
.kr-row.st-verde{border-left-color:#18B884;}
.kr-row.st-amarelo{border-left-color:#F5A623;}
.kr-row.st-vermelho{border-left-color:#FF3B4E;}
.kr-row-top{display:flex;justify-content:space-between;align-items:flex-start;gap:10px;}
.kr-nome{font-size:12.5px;font-weight:600;color:#1B1B1C;line-height:1.4;}
.kr-right{display:flex;align-items:center;gap:7px;flex-shrink:0;}
.kr-meta-txt{font-size:11px;color:#6B7080;}
.kr-pct{font-size:11px;font-weight:800;padding:2px 8px;border-radius:99px;color:#fff;}
.kr-pct.bg-verde{background:#18B884;} .kr-pct.bg-amarelo{background:#F5A623;} .kr-pct.bg-vermelho{background:#FF3B4E;}
.kr-bottom{display:flex;justify-content:space-between;align-items:center;}
.kr-data{font-size:10px;color:#A0A6B8;}
.kr-pill{font-size:10px;font-weight:700;padding:2px 7px;border-radius:4px;letter-spacing:.04em;}
.kr-pill.pill-verde{color:#18B884;background:rgba(24,184,132,.1);}
.kr-pill.pill-amarelo{color:#F5A623;background:rgba(245,166,35,.1);}
.kr-pill.pill-vermelho{color:#FF3B4E;background:rgba(255,59,78,.1);}
.kr-pill.pill-nd{color:#9aa5b4;background:#f3f4f6;}

/* ══ ALERTA ITEMS ══════════════════════════════════════════ */
.al-item{display:flex;gap:10px;padding:10px 12px;border-radius:9px;
  margin-bottom:8px;border:1px solid transparent;}
.al-item:last-child{margin-bottom:0;}
.al-crit{background:rgba(255,59,78,.05);border-color:rgba(255,59,78,.15);}
.al-warn{background:rgba(245,166,35,.06);border-color:rgba(245,166,35,.18);}
.al-ok{background:rgba(24,184,132,.05);border-color:rgba(24,184,132,.15);}
.al-dot{width:8px;height:8px;border-radius:50%;flex-shrink:0;margin-top:4px;}
.al-dot.red{background:#FF3B4E;} .al-dot.yellow{background:#F5A623;} .al-dot.green{background:#18B884;}
.al-body{flex:1;min-width:0;}
.al-kr{font-size:10px;font-weight:700;color:#A0A6B8;margin-bottom:2px;}
.al-title{font-size:11px;font-weight:700;margin-bottom:2px;}
.al-title.red{color:#FF3B4E;} .al-title.yellow{color:#F5A623;} .al-title.green{color:#18B884;}
.al-desc{font-size:11px;color:#6B7080;line-height:1.4;}

/* ══ PENDÊNCIAS ════════════════════════════════════════════ */
.pend-item{display:flex;align-items:center;gap:10px;padding:9px 0;
  border-bottom:1px solid #F5F6FA;}
.pend-item:last-child{border:none;}
.pend-kr{background:#f3f4f6;border-radius:6px;padding:3px 8px;font-size:10px;
  font-weight:700;color:#6B7080;flex-shrink:0;}
.pend-info{flex:1;min-width:0;}
.pend-nome{font-size:11px;font-weight:600;color:#1B1B1C;}
.pend-sub{font-size:10px;color:#A0A6B8;margin-top:1px;}
.pend-atraso{font-size:10px;font-weight:700;color:#FF3B4E;}

/* ══ CONF BARS ════════════════════════════════════════════ */
.conf-row{margin-bottom:8px;}
.conf-row:last-child{margin-bottom:0;}
.conf-hdr{display:flex;justify-content:space-between;margin-bottom:4px;}
.conf-lbl{font-size:11px;font-weight:600;color:#1B1B1C;}

/* ══ FORM INPUT (Coletar Dados) ═══════════════════════════ */
.form-grid{display:grid;grid-template-columns:1fr 1fr;gap:20px;}
@media(max-width:900px){.form-grid{grid-template-columns:1fr;}}
.form-card{background:#fff;border-radius:12px;border:1px solid #E8EBF5;
  box-shadow:0 1px 5px rgba(0,0,0,.04);overflow:hidden;}
.form-card-accent{height:3px;}
.fa-blue{background:#3271FE;} .fa-dark{background:linear-gradient(90deg,#0A1E5E,#1742AD);}
.form-card-body{padding:22px;}
.form-card-ttl{font-size:14px;font-weight:700;color:#1B1B1C;margin-bottom:4px;}
.form-card-sub{font-size:12px;color:#6B7080;margin-bottom:18px;}
.fg{margin-bottom:13px;}
.fg-lbl{display:block;font-size:11px;font-weight:700;color:#6B7080;
  text-transform:uppercase;letter-spacing:.05em;margin-bottom:5px;}
.fg-2{display:grid;grid-template-columns:1fr 1fr;gap:12px;}
.form-control{border:1px solid #E8EBF5!important;border-radius:8px!important;
  font-family:'Manrope',sans-serif!important;font-size:13px!important;
  padding:8px 12px!important;background:#F5F6FA!important;
  color:#1B1B1C!important;transition:border-color .15s,box-shadow .15s!important;
  box-shadow:none!important;}
.form-control:focus{outline:none!important;border-color:#3271FE!important;
  box-shadow:0 0 0 3px rgba(50,113,254,.1)!important;background:#fff!important;}
.selectize-input{border:1px solid #E8EBF5!important;border-radius:8px!important;
  font-family:'Manrope',sans-serif!important;font-size:13px!important;
  background:#F5F6FA!important;box-shadow:none!important;min-height:36px!important;
  padding:6px 12px!important;}
.selectize-input.focus{border-color:#3271FE!important;
  box-shadow:0 0 0 3px rgba(50,113,254,.1)!important;}

.kr-info-box{background:rgba(50,113,254,.05);border:1px solid rgba(50,113,254,.15);
  border-radius:8px;padding:10px 14px;display:flex;gap:18px;margin-bottom:13px;}
.ki-item{display:flex;align-items:center;gap:5px;}
.ki-lbl{font-size:11px;color:#A0A6B8;font-weight:600;}
.ki-val{font-size:12px;font-weight:700;color:#3271FE;}

.preview-box{border-radius:8px;padding:12px 14px;margin-bottom:13px;border:1px solid;}
.pv-verde{border-color:#18B884;background:rgba(24,184,132,.05);}
.pv-amarelo{border-color:#F5A623;background:rgba(245,166,35,.05);}
.pv-vermelho{border-color:#FF3B4E;background:rgba(255,59,78,.05);}
.pv-pct{font-size:26px;font-weight:800;line-height:1;margin-bottom:6px;letter-spacing:-.5px;}
.pv-verde .pv-pct{color:#18B884;} .pv-amarelo .pv-pct{color:#F5A623;} .pv-vermelho .pv-pct{color:#FF3B4E;}
.pv-status{font-size:11px;font-weight:700;margin-top:5px;}

/* ══ BOTÕES ════════════════════════════════════════════════ */
.btn-primary{width:100%;height:42px;border:none;border-radius:8px;
  background:#3271FE;color:#fff;font-family:'Manrope',sans-serif;
  font-size:13px;font-weight:700;cursor:pointer;margin-top:8px;
  transition:background .15s;letter-spacing:.01em;}
.btn-primary:hover{background:#1742AD;}
.btn-dark{width:100%;height:42px;border:none;border-radius:8px;
  background:#1B1B1C;color:#fff;font-family:'Manrope',sans-serif;
  font-size:13px;font-weight:700;cursor:pointer;margin-top:8px;
  transition:background .15s;}
.btn-dark:hover{background:#333;}

/* ══ TOAST ═════════════════════════════════════════════════ */
.toast{padding:11px 16px;border-radius:9px;font-weight:600;font-size:13px;margin-top:13px;}
.toast-ok{background:rgba(24,184,132,.08);border:1px solid #18B884;color:#065f46;}
.toast-err{background:rgba(255,59,78,.08);border:1px solid #FF3B4E;color:#7f1d1d;}

/* ══ FR (resultado financeiro) ════════════════════════════ */
.fr{display:flex;justify-content:space-between;padding:9px 0;
  border-bottom:1px solid #f3f6f9;font-size:13px;}
.fr:last-child{border:none;}
.fl{color:#374151;} .fv{font-weight:700;color:#1B1B1C;}
.fv.r{color:#FF3B4E;} .fv.g{color:#18B884;font-size:15px;}
.ftotal{display:flex;justify-content:space-between;padding:12px 0 4px;
  font-weight:800;font-size:14px;border-top:2px solid #E8EBF5;margin-top:4px;}

/* ══ OP-TABS ════════════════════════════════════════════════ */
.op-tabs{display:flex;gap:4px;margin-bottom:16px;background:#f3f4f6;
  border-radius:10px;padding:4px;}
.op-tab{flex:1;text-align:center;padding:8px 10px;border-radius:7px;font-size:12px;
  font-weight:700;color:#6B7080;cursor:pointer;border:none;background:none;
  font-family:'Manrope',sans-serif;transition:all .15s;}
.op-tab.active{background:#fff;color:#1B1B1C;box-shadow:0 1px 4px rgba(0,0,0,.1);}

/* ══ MISC ══════════════════════════════════════════════════ */
.sem-dados{color:#A0A6B8;font-size:13px;padding:20px 0;text-align:center;}
.chip{display:inline-flex;align-items:center;padding:3px 10px;border-radius:99px;
  font-size:11px;font-weight:600;background:rgba(50,113,254,.08);color:#3271FE;
  border:1px solid rgba(50,113,254,.15);}
::-webkit-scrollbar{width:5px;height:5px;}
::-webkit-scrollbar-track{background:transparent;}
::-webkit-scrollbar-thumb{background:#E8EBF5;border-radius:99px;}
"

# ── ui ─────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$title("MIRAI · OKR Dashboard"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Manrope:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML(APP_CSS))
  ),
  
  # Sidebar
  tags$aside(class = "sidebar",
             div(class = "sb-logo", HTML(LOGO_SVG)),
             div(class = "sb-nav",
                 tags$span(class = "sb-label", "Principal"),
                 tags$button(class = "sb-item active", id = "nav_dash",
                             div(class = "sb-icon", "📊"), "Dashboard",
                             onclick = "Shiny.setInputValue('nav_page', 'dashboard', {priority:'event'})"
                 ),
                 tags$button(class = "sb-item", id = "nav_input",
                             div(class = "sb-icon", "✏️"), "Coletar Dados",
                             onclick = "Shiny.setInputValue('nav_page', 'input', {priority:'event'})"
                 ),
                 tags$button(class = "sb-item", id = "nav_detail",
                             div(class = "sb-icon", "📈"), "KRs Detalhado",
                             onclick = "Shiny.setInputValue('nav_page', 'detail', {priority:'event'})"
                 ),
                 tags$span(class = "sb-label", "Análises"),
                 tags$button(class = "sb-item", id = "nav_alertas",
                             div(class = "sb-icon", "🔔"), "Alertas",
                             tags$span(class = "sb-badge", id = "badge_alertas", ""),
                             onclick = "Shiny.setInputValue('nav_page', 'dashboard', {priority:'event'})"
                 ),
                 tags$span(class = "sb-label", "Sistema"),
                 tags$button(class = "sb-item",
                             div(class = "sb-icon", "⚙️"), "Configurações"
                 )
             ),
             div(class = "sb-footer",
                 div(class = "sb-user",
                     div(class = "sb-avatar", "MR"),
                     div(div(class = "sb-uname", "MIRAI Team"),
                         div(class = "sb-urole", "Admin · OKR System"))
                 )
             )
  ),
  
  # Main
  div(class = "main",
      uiOutput("topbar_ui"),
      uiOutput("fbar_ui"),
      div(class = "content", uiOutput("body_ui"))
  ),
  
  # JS: atualiza classes ativas na sidebar
  tags$script(HTML("
    Shiny.addCustomMessageHandler('setNav', function(page) {
      document.querySelectorAll('.sb-item').forEach(function(el) {
        el.classList.remove('active');
        el.querySelector('.sb-icon') && (el.querySelector('.sb-icon').style.background = 'rgba(255,255,255,.07)');
      });
      var m = {'dashboard':'nav_dash','input':'nav_input','detail':'nav_detail'};
      var el = document.getElementById(m[page]);
      if (el) {
        el.classList.add('active');
        var ic = el.querySelector('.sb-icon');
        if (ic) ic.style.background = '#3271FE';
      }
    });
    Shiny.addCustomMessageHandler('setBadge', function(n) {
      var b = document.getElementById('badge_alertas');
      if (b) { b.textContent = n > 0 ? n : ''; }
    });
  "))
)

# ── server ─────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    page   = "dashboard",
    op_aba = "breakdown"
  )
  
  # ── Navegação ─────────────────────────────────────────────
  observeEvent(input$nav_page, {
    rv$page <- input$nav_page
    session$sendCustomMessage("setNav", input$nav_page)
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_aba_op, {
    rv$op_aba <- input$btn_aba_op
  }, ignoreInit = TRUE)
  
  # ── Dados reativos ────────────────────────────────────────
  refresh_trigger <- reactiveVal(Sys.time())
  
  observeEvent(input$btn_refresh, { refresh_trigger(Sys.time()) })
  
  # Auto-refresh 30s
  observe({
    invalidateLater(30000, session)
    refresh_trigger(Sys.time())
  })
  
  data_status <- reactive({
    refresh_trigger()
    get_ultimo_status()
  })
  
  data_indicadores <- reactive({
    refresh_trigger()
    get_indicadores()
  })
  
  # ── Atualiza badge de alertas ────────────────────────────
  observe({
    df <- data_status()
    n  <- sum(df$status %in% c("vermelho", "amarelo"), na.rm = TRUE)
    session$sendCustomMessage("setBadge", n)
  })
  
  # ══════════════════════════════════════════════════════════
  # TOPBAR
  # ══════════════════════════════════════════════════════════
  output$topbar_ui <- renderUI({
    titles <- list(
      dashboard = list(t = "Dashboard",    s = "Visão Executiva"),
      input     = list(t = "Coletar Dados", s = "Registro de Indicadores"),
      detail    = list(t = "KRs Detalhado", s = "Série Temporal & Análise")
    )
    info <- titles[[rv$page]] %||% titles$dashboard
    
    div(class = "topbar",
        div(
          div(class = "tb-title", info$t),
          div(class = "tb-crumb",
              HTML(paste0("Sistema OKR · <span>", info$s, "</span>")))
        ),
        div(class = "tb-spacer"),
        if (rv$page != "input") {
          tags$button(class = "tb-btn-ghost", id = "btn_refresh",
                      onclick = "Shiny.setInputValue('btn_refresh', Math.random())",
                      "↻ Atualizar")
        }
    )
  })
  
  # ══════════════════════════════════════════════════════════
  # FILTER BAR (apenas dashboard / detail)
  # ══════════════════════════════════════════════════════════
  output$fbar_ui <- renderUI({
    if (rv$page == "input") return(NULL)
    
    objs <- get_objetivos()
    obj_choices <- c("Todos os Objetivos" = "0",
                     setNames(objs$id_objetivo,
                              paste0(objs$letra, " — ", substr(objs$nome, 1, 40), "...")))
    
    div(class = "fbar",
        div(class = "fbar-lbl", "OBJETIVO:"),
        selectInput("fil_objetivo", NULL, choices = obj_choices,
                    selected = "0", width = "260px"),
        div(class = "fbar-lbl", "STATUS:"),
        selectInput("fil_status", NULL,
                    choices = c("Todos" = "todos", "No Alvo" = "verde",
                                "Em Risco" = "amarelo", "Crítico" = "vermelho"),
                    selected = "todos", width = "160px")
    )
  })
  
  # ══════════════════════════════════════════════════════════
  # BODY PRINCIPAL — renderUI como no app_public.R
  # ══════════════════════════════════════════════════════════
  output$body_ui <- renderUI({
    switch(rv$page,
           dashboard = render_dashboard(),
           input     = render_input(),
           detail    = render_detail(),
           render_dashboard()
    )
  })
  
  # ══════════════════════════════════════════════════════════
  # DASHBOARD PAGE
  # ══════════════════════════════════════════════════════════
  render_dashboard <- function() {
    df    <- data_status()
    indic <- data_indicadores()
    objs  <- get_objetivos()
    
    if (nrow(df) == 0)
      return(div(class = "sem-dados", "⏳ Carregando dados..."))
    
    # Filtros aplicados
    df_fil <- df
    if (!is.null(input$fil_objetivo) && input$fil_objetivo != "0")
      df_fil <- df_fil[df_fil$id_objetivo == as.integer(input$fil_objetivo), ]
    if (!is.null(input$fil_status) && input$fil_status != "todos")
      df_fil <- df_fil[!is.na(df_fil$status) & df_fil$status == input$fil_status, ]
    
    total    <- nrow(df)
    verde_n  <- sum(df$status == "verde",    na.rm = TRUE)
    amarelo_n<- sum(df$status == "amarelo",  na.rm = TRUE)
    verm_n   <- sum(df$status == "vermelho", na.rm = TRUE)
    sem_n    <- sum(is.na(df$status))
    avg_pct  <- round(mean(df$percentual_atingimento, na.rm = TRUE), 1)
    if (is.nan(avg_pct)) avg_pct <- 0
    
    # Cores por objetivo
    obj_cores <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
    obj_ltrs  <- c("M","I","R","A","I")
    
    tagList(
      # ── 1. KPI CARDS ──────────────────────────────────────
      div(class = "sec", "PERFORMANCE CONSOLIDADA"),
      div(class = "kgrid",
          # Hero card
          div(class = "kcard hero",
              div(class = "klbl", "📊 Atingimento Médio"),
              div(class = if (avg_pct >= 80) "kval g" else "kval",
                  paste0(avg_pct, "%")),
              div(class = "kdelta up",
                  paste0(verde_n, " de ", total, " KRs no alvo"))
          ),
          kcard("KRs no Alvo",   verde_n,   paste0("de ", total, " totais"),
                vg = TRUE, extra_class = "kc-green"),
          kcard("Em Risco",      amarelo_n, "requerem atenção",
                extra_class = "kc-yellow"),
          kcard("Crítico",       verm_n,    "abaixo de 60%",
                dn = verm_n > 0, extra_class = "kc-red"),
          # Card resumo
          div(class = "kcard kc-grad",
              div(class = "klbl", "RESUMO GERAL"),
              div(class = "resumo-grid",
                  div(class = "resumo-item",
                      div(class = "resumo-num", style = "color:#FF3B4E", verm_n),
                      div(class = "resumo-lbl", "Crítico")),
                  div(class = "resumo-item",
                      div(class = "resumo-num", style = "color:#F5A623", amarelo_n),
                      div(class = "resumo-lbl", "Revisão")),
                  div(class = "resumo-item",
                      div(class = "resumo-num", style = "color:#18B884", verde_n),
                      div(class = "resumo-lbl", "No Alvo")),
                  div(class = "resumo-item",
                      div(class = "resumo-num", style = "color:#A0A6B8", sem_n),
                      div(class = "resumo-lbl", "TBD"))
              )
          )
      ),
      
      # ── 2. OBJETIVOS ESTRATÉGICOS ─────────────────────────
      div(class = "sec", "OBJETIVOS ESTRATÉGICOS"),
      div(class = "cgrid",
          
          # LEFT: grid de objetivos + chart
          div(
            div(class = "obj-grid",
                lapply(seq_len(nrow(objs)), function(i) {
                  obj    <- objs[i, ]
                  krs_o  <- df[df$id_objetivo == obj$id_objetivo, ]
                  avg    <- round(mean(krs_o$percentual_atingimento, na.rm = TRUE), 1)
                  if (is.nan(avg)) avg <- 0
                  v_n    <- sum(krs_o$status == "verde",    na.rm = TRUE)
                  a_n    <- sum(krs_o$status == "amarelo",  na.rm = TRUE)
                  r_n    <- sum(krs_o$status == "vermelho", na.rm = TRUE)
                  st     <- if (r_n > 0) "vermelho" else if (a_n > 0) "amarelo" else "verde"
                  cor_pct<- switch(st, verde="#18B884", amarelo="#F5A623", vermelho="#FF3B4E")
                  cor_bar<- cor_pct
                  cls    <- if (i == 5) "obj-card full" else "obj-card"
                  col    <- obj_cores[min(i, length(obj_cores))]
                  ltr    <- obj_ltrs[min(i, length(obj_ltrs))]
                  
                  div(class = cls,
                      div(class = "obj-top",
                          div(class = "obj-letra", style = paste0("background:", col), ltr),
                          div(class = "obj-nome", substr(obj$nome, 1, 55)),
                          div(class = "obj-pct",  style = paste0("color:", cor_pct),
                              paste0(avg, "%"))
                      ),
                      prog_bar(avg, st),
                      div(class = "obj-footer",
                          div(class = "obj-sub",
                              paste0(nrow(krs_o), " KRs · ✓", v_n, " ⚠", a_n, " ✗", r_n)),
                          status_badge(st)
                      )
                  )
                })
            ),
            
            # Chart
            div(class = "card", style = "margin-top:14px",
                div(class = "card-hdr",
                    div(class = "card-ttl", "Progresso — % Atingimento por Período"),
                    tags$span(class = "badge badge-blue", "Média consolidada")
                ),
                plotlyOutput("chart_progress", height = "190px")
            )
          ),
          
          # RIGHT: alertas + pendências + confiança
          div(
            # Alertas
            div(class = "card",
                div(class = "card-hdr",
                    div(class = "card-ttl", "Alertas Principais"),
                    uiOutput("chip_alertas", inline = TRUE)
                ),
                uiOutput("alertas_ui")
            ),
            # Pendências
            div(class = "card", style = "margin-top:14px",
                div(class = "card-hdr",
                    div(class = "card-ttl", "Pendências de Coleta"),
                    tags$a(class = "badge badge-blue", style = "cursor:pointer",
                           onclick = "Shiny.setInputValue('nav_page','input',{priority:'event'})",
                           "Registrar →")
                ),
                uiOutput("pendencias_ui")
            ),
            # Confiança
            div(class = "card", style = "margin-top:14px",
                div(class = "card-hdr",
                    div(class = "card-ttl", "Confiança nas Metas")),
                uiOutput("confianca_ui")
            )
          )
      )
    )
  }
  
  # ── Chart progress ────────────────────────────────────────
  output$chart_progress <- renderPlotly({
    df <- data_indicadores()
    if (nrow(df) == 0)
      return(plotly_empty() %>% layout(paper_bgcolor = "transparent",
                                       plot_bgcolor = "transparent"))
    
    df$data <- as.Date(df$data)
    agg <- df %>%
      group_by(data) %>%
      summarise(avg = mean(percentual_atingimento, na.rm = TRUE), .groups = "drop") %>%
      arrange(data)
    
    plot_ly() %>%
      add_lines(data = agg, x = ~data, y = ~avg,
                line = list(color = "#3271FE", width = 2.5),
                fill = "tozeroy", fillcolor = "rgba(50,113,254,.08)",
                hovertemplate = "%{x|%b %Y}: %{y:.1f}%<extra></extra>") %>%
      add_lines(x = c(min(agg$data), max(agg$data)), y = c(80, 80),
                line = list(color = "rgba(50,113,254,.25)", width = 1.5, dash = "dot"),
                showlegend = FALSE, hoverinfo = "none") %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(family = "Manrope, sans-serif", size = 11, color = "#A0A6B8"),
        xaxis = list(title = "", showgrid = FALSE, showline = FALSE,
                     zeroline = FALSE, tickformat = "%b %y"),
        yaxis = list(title = "", showgrid = TRUE, gridcolor = "rgba(0,0,0,.04)",
                     showline = FALSE, zeroline = FALSE,
                     range = c(0, 110), ticksuffix = "%"),
        showlegend = FALSE,
        hovermode  = "x unified",
        margin = list(l = 30, r = 10, t = 4, b = 30)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Chip alertas ──────────────────────────────────────────
  output$chip_alertas <- renderUI({
    df <- data_status()
    n  <- sum(df$status %in% c("vermelho", "amarelo"), na.rm = TRUE)
    tags$span(class = "chip", paste(n, "ativos"))
  })
  
  # ── Alertas list ──────────────────────────────────────────
  output$alertas_ui <- renderUI({
    df   <- data_status()
    crit <- df[!is.na(df$status) & df$status == "vermelho", ]
    warn <- df[!is.na(df$status) & df$status == "amarelo",  ]
    bons <- head(df[!is.na(df$status) & df$status == "verde", ], 1)
    
    make_al <- function(row, tipo) {
      cls   <- switch(tipo, red = "al-crit", yellow = "al-warn", "al-ok")
      ttl   <- switch(tipo, red = "Crítico", yellow = "Em Risco", "No Alvo")
      pct_s <- if (!is.na(row$percentual_atingimento))
        paste0(row$percentual_atingimento, "% atingido") else "Sem dados"
      div(class = paste("al-item", cls),
          div(class = paste("al-dot", tipo)),
          div(class = "al-body",
              div(class = "al-kr",
                  paste0("KR", row$id_kr, " · ", substr(row$nome_kr, 1, 38))),
              div(class = paste("al-title", tipo), ttl),
              div(class = "al-desc", paste0("Meta: ",
                                            num_fmt(row$meta), " ", row$unidade, " — ", pct_s))
          )
      )
    }
    
    if (nrow(crit) == 0 && nrow(warn) == 0 && nrow(bons) == 0)
      return(div(class = "sem-dados", "✅ Todos os KRs no alvo!"))
    
    tagList(
      lapply(seq_len(min(nrow(crit), 2)), function(i) make_al(crit[i, ], "red")),
      lapply(seq_len(min(nrow(warn), 2)), function(i) make_al(warn[i, ], "yellow")),
      lapply(seq_len(min(nrow(bons), 1)), function(i) make_al(bons[i, ], "green"))
    )
  })
  
  # ── Pendências ────────────────────────────────────────────
  output$pendencias_ui <- renderUI({
    df  <- data_status()
    pnd <- head(df[is.na(df$status) | df$status != "verde", ], 3)
    
    if (nrow(pnd) == 0)
      return(div(class = "sem-dados", "Sem pendências."))
    
    lapply(seq_len(nrow(pnd)), function(i) {
      r <- pnd[i, ]
      div(class = "pend-item",
          div(class = "pend-kr", paste0("KR", r$id_kr)),
          div(class = "pend-info",
              div(class = "pend-nome", substr(r$nome_kr, 1, 42)),
              div(class = "pend-sub",
                  paste0("Meta: ", num_fmt(r$meta), " ", r$unidade))
          ),
          div(class = "pend-atraso", "→")
      )
    })
  })
  
  # ── Confiança bars ────────────────────────────────────────
  output$confianca_ui <- renderUI({
    df   <- data_status()
    objs <- get_objetivos()
    lbrs <- c("O1 · Impacto","O2 · Faturamento","O3 · Carteira",
              "O4 · Pessoas","O5 · Assets")
    cols <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
    
    lapply(seq_len(min(nrow(objs), 5)), function(i) {
      obj  <- objs[i, ]
      krs  <- df[df$id_objetivo == obj$id_objetivo, ]
      avg  <- round(mean(krs$percentual_atingimento, na.rm = TRUE), 1)
      if (is.nan(avg)) avg <- 0
      col  <- if (avg >= 80) "#18B884" else if (avg >= 60) "#3271FE"
      else if (avg >= 40) "#F5A623" else "#FF3B4E"
      
      div(class = "conf-row",
          div(class = "conf-hdr",
              div(class = "conf-lbl", lbrs[i]),
              div(style = paste0("font-size:11px;font-weight:700;color:", col),
                  paste0(avg, "%"))
          ),
          div(class = "pb-wrap",
              div(class = "pb-fill",
                  style = paste0("width:", min(avg, 100), "%;background:", col, ";")))
      )
    })
  })
  
  # ══════════════════════════════════════════════════════════
  # COLETAR DADOS PAGE
  # ══════════════════════════════════════════════════════════
  render_input <- function() {
    objs <- get_objetivos()
    obj_ch <- setNames(objs$id_objetivo,
                       paste0(objs$letra, " — ", substr(objs$nome, 1, 48), "..."))
    
    tagList(
      div(class = "sec", "REGISTRO DE DADOS"),
      div(class = "form-grid",
          
          # Card quantitativo
          div(class = "form-card",
              div(class = "form-card-accent fa-blue"),
              div(class = "form-card-body",
                  div(class = "form-card-ttl", "📊 Dados Quantitativos"),
                  div(class = "form-card-sub",
                      "Registre o valor realizado para o KR selecionado"),
                  
                  div(class = "fg",
                      tags$label(class = "fg-lbl", "Objetivo"),
                      selectInput("sel_obj", NULL, choices = obj_ch, width = "100%")
                  ),
                  div(class = "fg",
                      tags$label(class = "fg-lbl", "Key Result"),
                      selectInput("sel_kr", NULL,
                                  choices = c("Selecione um objetivo..." = ""), width = "100%")
                  ),
                  uiOutput("kr_info_ui"),
                  div(class = "fg-2",
                      div(class = "fg",
                          tags$label(class = "fg-lbl", "Data de Referência"),
                          dateInput("inp_data", NULL, value = Sys.Date(),
                                    language = "pt-BR", format = "dd/mm/yyyy", width = "100%")
                      ),
                      div(class = "fg",
                          tags$label(class = "fg-lbl", "Valor Realizado"),
                          numericInput("inp_valor", NULL, value = NULL, min = 0, width = "100%")
                      )
                  ),
                  uiOutput("preview_ui"),
                  tags$button(class = "btn-primary", "Salvar Registro",
                              onclick = "Shiny.setInputValue('btn_quant', Math.random())")
              )
          ),
          
          # Card qualitativo
          div(class = "form-card",
              div(class = "form-card-accent fa-dark"),
              div(class = "form-card-body",
                  div(class = "form-card-ttl", "💬 Análise Qualitativa"),
                  div(class = "form-card-sub",
                      "Diagnóstico, gargalos e próximas ações do período"),
                  
                  div(class = "fg-2",
                      div(class = "fg",
                          tags$label(class = "fg-lbl", "Key Result"),
                          selectInput("sel_kr_qual", NULL,
                                      choices = c("Selecione..." = ""), width = "100%")
                      ),
                      div(class = "fg",
                          tags$label(class = "fg-lbl", "Responsável"),
                          textInput("inp_resp", NULL,
                                    placeholder = "Nome do responsável", width = "100%")
                      )
                  ),
                  div(class = "fg",
                      tags$label(class = "fg-lbl", "Diagnóstico do Período"),
                      textAreaInput("inp_diag", NULL,
                                    placeholder = "Como foi o período?", rows = 3, width = "100%")
                  ),
                  div(class = "fg",
                      tags$label(class = "fg-lbl", "Gargalos"),
                      textAreaInput("inp_garg", NULL,
                                    placeholder = "O que está travando o avanço?", rows = 2, width = "100%")
                  ),
                  div(class = "fg-2",
                      div(class = "fg",
                          tags$label(class = "fg-lbl", "Ações Realizadas"),
                          textAreaInput("inp_acoes", NULL,
                                        placeholder = "O que foi feito?", rows = 3, width = "100%")
                      ),
                      div(class = "fg",
                          tags$label(class = "fg-lbl", "Próximas Ações"),
                          textAreaInput("inp_prox", NULL,
                                        placeholder = "O que será feito?", rows = 3, width = "100%")
                      )
                  ),
                  div(class = "fg",
                      tags$label(class = "fg-lbl", "Nível de Confiança (1-5)"),
                      radioButtons("inp_conf", NULL,
                                   choices = c("1 ☁️"=1,"2 🌤️"=2,"3 ⛅"=3,"4 🌞"=4,"5 🚀"=5),
                                   selected = 3, inline = TRUE)
                  ),
                  div(class = "fg",
                      tags$label(class = "fg-lbl", "Data"),
                      dateInput("inp_data_qual", NULL, value = Sys.Date(),
                                language = "pt-BR", format = "dd/mm/yyyy", width = "100%")
                  ),
                  tags$button(class = "btn-dark", "Salvar Análise",
                              onclick = "Shiny.setInputValue('btn_qual', Math.random())")
              )
          )
      ),
      uiOutput("toast_ui")
    )
  }
  
  # ── Atualiza KRs ao mudar objetivo ───────────────────────
  observeEvent(input$sel_obj, {
    req(input$sel_obj)
    krs <- get_krs(as.integer(input$sel_obj))
    ch  <- setNames(krs$id_kr, krs$nome)
    updateSelectInput(session, "sel_kr",      choices = ch)
    updateSelectInput(session, "sel_kr_qual", choices = ch)
  }, ignoreInit = TRUE)
  
  output$kr_info_ui <- renderUI({
    req(input$sel_kr)
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(input$sel_kr), ]
    if (nrow(kr) == 0) return(NULL)
    div(class = "kr-info-box",
        div(class = "ki-item",
            span(class = "ki-lbl", "Meta:"),
            span(class = "ki-val",
                 paste(num_fmt(kr$meta), kr$unidade))
        ),
        div(class = "ki-item",
            span(class = "ki-lbl", "Frequência:"),
            span(class = "ki-val", kr$frequencia)
        )
    )
  })
  
  output$preview_ui <- renderUI({
    req(input$sel_kr, input$inp_valor)
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(input$sel_kr), ]
    if (nrow(kr) == 0 || is.na(input$inp_valor)) return(NULL)
    
    pct  <- round((input$inp_valor / kr$meta) * 100, 1)
    st   <- if (pct >= 80) "verde" else if (pct >= 60) "amarelo" else "vermelho"
    lbl  <- switch(st, verde = "✅ No Alvo", amarelo = "⚠️ Em Risco", "🔴 Crítico")
    
    div(class = paste0("preview-box pv-", st),
        div(class = "pv-pct", paste0(pct, "%")),
        prog_bar(pct, st),
        div(class = "pv-status", lbl)
    )
  })
  
  # ── Toast feedback ────────────────────────────────────────
  toast_msg <- reactiveVal(NULL)
  
  output$toast_ui <- renderUI({
    msg <- toast_msg()
    if (is.null(msg)) return(NULL)
    div(class = paste0("toast ", msg$cls), msg$txt)
  })
  
  observeEvent(input$btn_quant, {
    req(input$sel_kr, input$inp_valor, input$inp_data)
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(input$sel_kr), ]
    tryCatch({
      res <- insert_indicador(as.integer(input$sel_kr), input$inp_data,
                              input$inp_valor, kr$meta)
      toast_msg(list(cls = "toast-ok",
                     txt = paste0("✅ Salvo! Atingimento: ", res$pct, "% — ", toupper(res$status))))
      refresh_trigger(Sys.time())
    }, error = function(e)
      toast_msg(list(cls = "toast-err", txt = paste("⚠ Erro:", e$message)))
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_qual, {
    req(input$sel_kr_qual, input$inp_resp, input$inp_diag)
    tryCatch({
      insert_qualitativo(
        id_kr            = as.integer(input$sel_kr_qual),
        data             = input$inp_data_qual,
        responsavel      = input$inp_resp,
        diagnostico      = input$inp_diag,
        gargalos         = input$inp_garg %||% "",
        acoes_realizadas = input$inp_acoes %||% "",
        proximas_acoes   = input$inp_prox %||% "",
        confianca        = as.integer(input$inp_conf)
      )
      toast_msg(list(cls = "toast-ok", txt = "✅ Análise qualitativa salva!"))
      refresh_trigger(Sys.time())
    }, error = function(e)
      toast_msg(list(cls = "toast-err", txt = paste("⚠ Erro:", e$message)))
    )
  }, ignoreInit = TRUE)
  
  # Limpa toast após 4s
  observe({
    req(toast_msg())
    invalidateLater(4000, session)
    toast_msg(NULL)
  })
  
  # ══════════════════════════════════════════════════════════
  # KRs DETALHADO PAGE
  # ══════════════════════════════════════════════════════════
  rv_sel_obj_det <- reactiveVal(1L)
  
  observeEvent(input$btn_obj_det, {
    rv_sel_obj_det(as.integer(input$btn_obj_det))
  }, ignoreInit = TRUE)
  
  render_detail <- function() {
    df   <- data_status()
    objs <- get_objetivos()
    obj_cores <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
    obj_ltrs  <- c("M","I","R","A","I")
    
    tagList(
      div(class = "sec", "ANÁLISE POR KEY RESULT"),
      
      # Tab de objetivos
      div(class = "op-tabs",
          lapply(seq_len(nrow(objs)), function(i) {
            obj <- objs[i, ]
            ltr <- obj_ltrs[min(i, length(obj_ltrs))]
            cls <- paste("op-tab",
                         if (rv_sel_obj_det() == obj$id_objetivo) "active" else "")
            tags$button(class = cls, ltr,
                        onclick = sprintf(
                          "Shiny.setInputValue('btn_obj_det', '%d', {priority:'event'})",
                          obj$id_objetivo))
          })
      ),
      
      # Breakdown KRs do objetivo selecionado
      uiOutput("kr_breakdown_ui"),
      
      # Série temporal
      div(class = "sec", "SÉRIE TEMPORAL — META VS REALIZADO"),
      div(class = "card",
          div(class = "card-hdr",
              div(class = "card-ttl", "Evolução do KR"),
              div(style = "display:flex;align-items:center;gap:10px",
                  tags$label(class = "fg-lbl", style = "margin:0!important",
                             "Key Result:"),
                  selectInput("sel_kr_chart", NULL,
                              choices = c("Carregando..." = ""), width = "320px")
              )
          ),
          plotlyOutput("chart_kr_det", height = "240px")
      ),
      
      # Qualitativo
      uiOutput("qual_det_ui")
    )
  }
  
  # Atualiza select de KR para chart
  observe({
    krs <- get_krs()
    ch  <- setNames(krs$id_kr,
                    paste0("[", krs$letra, "] ", krs$nome))
    updateSelectInput(session, "sel_kr_chart", choices = ch)
  })
  
  # Breakdown KRs
  output$kr_breakdown_ui <- renderUI({
    df   <- data_status()
    objs <- get_objetivos()
    oid  <- rv_sel_obj_det()
    krs  <- df[df$id_objetivo == oid, ]
    obj  <- objs[objs$id_objetivo == oid, ]
    obj_cores <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
    i    <- match(oid, objs$id_objetivo)
    col  <- obj_cores[min(i, length(obj_cores))]
    
    if (nrow(krs) == 0) return(div(class = "sem-dados", "Sem dados."))
    
    div(class = "card", style = "margin-bottom:14px",
        div(class = "card-hdr",
            div(class = "card-ttl", style = paste0("border-left:4px solid ", col,
                                                   ";padding-left:10px"), obj$nome)
        ),
        lapply(seq_len(nrow(krs)), function(j) {
          kr  <- krs[j, ]
          pct <- if (!is.na(kr$percentual_atingimento)) kr$percentual_atingimento else 0
          st  <- kr$status %||% "nd"
          row_cls  <- paste0("kr-row st-", st)
          pct_cls  <- paste0("kr-pct bg-", st)
          pill_cls <- paste0("kr-pill pill-", st)
          
          div(class = row_cls,
              div(class = "kr-row-top",
                  div(class = "kr-nome", kr$nome_kr),
                  div(class = "kr-right",
                      div(class = "kr-meta-txt",
                          if (!is.na(kr$valor_realizado))
                            paste0(num_fmt(kr$valor_realizado), "/",
                                   num_fmt(kr$meta), " ", kr$unidade)
                          else "—"),
                      div(class = pct_cls, paste0(pct, "%"))
                  )
              ),
              prog_bar(pct, st),
              div(class = "kr-bottom",
                  div(class = "kr-data",
                      if (!is.na(kr$data)) paste("Atualizado:", kr$data) else "Sem registros"),
                  div(class = pill_cls, toupper(st))
              )
          )
        })
    )
  })
  
  # Chart KR
  output$chart_kr_det <- renderPlotly({
    req(input$sel_kr_chart)
    df  <- get_indicadores(as.integer(input$sel_kr_chart))
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(input$sel_kr_chart), ]
    
    if (nrow(df) == 0)
      return(plotly_empty() %>%
               layout(title = list(text = "Sem dados para este KR",
                                   font = list(size = 13, color = "#A0A6B8")),
                      paper_bgcolor = "transparent", plot_bgcolor = "transparent"))
    
    df$data <- as.Date(df$data)
    df <- df[order(df$data), ]
    
    plot_ly() %>%
      add_lines(data = df, x = ~data, y = ~meta, name = "Meta",
                line = list(color = "rgba(50,113,254,.25)", width = 1.5, dash = "dot"),
                hoverinfo = "none") %>%
      add_lines(data = df, x = ~data, y = ~valor_realizado, name = "Realizado",
                line = list(color = "#3271FE", width = 2.5),
                fill = "tozeroy", fillcolor = "rgba(50,113,254,.08)",
                hovertemplate = paste0("%{x|%d %b}: %{y} ", kr$unidade, "<extra></extra>")) %>%
      add_markers(data = df, x = ~data, y = ~valor_realizado,
                  marker = list(
                    color = ifelse(df$status == "verde",    "#18B884",
                                   ifelse(df$status == "amarelo",  "#F5A623", "#FF3B4E")),
                    size = 8, line = list(color = "#fff", width = 2)
                  ), showlegend = FALSE, hoverinfo = "skip") %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font   = list(family = "Manrope, sans-serif", size = 11, color = "#A0A6B8"),
        xaxis  = list(title = "", showgrid = FALSE, showline = FALSE, zeroline = FALSE),
        yaxis  = list(title = kr$unidade, showgrid = TRUE,
                      gridcolor = "rgba(0,0,0,.04)", showline = FALSE, zeroline = FALSE),
        legend = list(orientation = "h", x = 0, y = 1.18, font = list(size = 11)),
        hovermode = "x unified",
        margin = list(l = 44, r = 12, t = 8, b = 36)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Qualitativo no detalhe
  output$qual_det_ui <- renderUI({
    req(input$sel_kr_chart)
    qual <- get_qualitativo(as.integer(input$sel_kr_chart))
    if (nrow(qual) == 0) return(NULL)
    q    <- qual[1, ]
    strs <- paste(rep("★", q$confianca), collapse = "")
    
    div(class = "card", style = "margin-top:14px",
        div(class = "card-hdr",
            div(class = "card-ttl", "💬 Última Análise Qualitativa"),
            div(style = "display:flex;align-items:center;gap:8px",
                tags$span(style = "font-size:11px;color:#A0A6B8", q$data),
                tags$span(style = "font-size:11px;font-weight:600;color:#6B7080",
                          paste("por", q$responsavel)),
                tags$span(style = "color:#FF9E62;font-size:13px", strs)
            )
        ),
        if (!is.na(q$diagnostico) && nchar(q$diagnostico) > 0)
          div(style = "margin-bottom:10px",
              tags$span(style = "font-size:10px;font-weight:700;color:#A0A6B8;
                             text-transform:uppercase;letter-spacing:.05em;display:block;
                             margin-bottom:3px", "Diagnóstico"),
              tags$p(style = "font-size:13px;color:#1B1B1C;line-height:1.5", q$diagnostico)
          ),
        if (!is.na(q$gargalos) && nchar(q$gargalos) > 0)
          div(style = "margin-bottom:10px",
              tags$span(style = "font-size:10px;font-weight:700;color:#A0A6B8;
                             text-transform:uppercase;letter-spacing:.05em;display:block;
                             margin-bottom:3px", "Gargalos"),
              tags$p(style = "font-size:13px;color:#1B1B1C;line-height:1.5", q$gargalos)
          ),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px",
            if (!is.na(q$acoes_realizadas) && nchar(q$acoes_realizadas) > 0)
              div(tags$span(style = "font-size:10px;font-weight:700;color:#A0A6B8;
                                 text-transform:uppercase;letter-spacing:.05em;display:block;
                                 margin-bottom:3px", "Ações Realizadas"),
                  tags$p(style = "font-size:13px;color:#1B1B1C;line-height:1.5",
                         q$acoes_realizadas)),
            if (!is.na(q$proximas_acoes) && nchar(q$proximas_acoes) > 0)
              div(tags$span(style = "font-size:10px;font-weight:700;color:#A0A6B8;
                                 text-transform:uppercase;letter-spacing:.05em;display:block;
                                 margin-bottom:3px", "Próximas Ações"),
                  tags$p(style = "font-size:13px;color:#1B1B1C;line-height:1.5",
                         q$proximas_acoes))
        )
    )
  })
  
}

app <- shinyApp(ui, server)