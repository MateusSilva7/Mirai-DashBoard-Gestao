# ============================================================
# MIRAI OKR - app.R  v2  (RAE Dashboard)
# Arquitetura: fluidPage + CSS inline + renderUI
# 3 camadas: Visão Geral → Objetivo → KR
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(plotly)
  library(DBI)
  library(RSQLite)
  library(DT)
})

source("modules/database.R")
init_database()

# ── Cores por objetivo ─────────────────────────────────────
OBJ_CORES <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
OBJ_LTRS  <- c("M","I","R","A","I")

# ── Ano de referência estratégico ──────────────────────────
ANO_REF <- 2026

# ── helpers gerais ─────────────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

status_cor <- function(st) {
  switch(st %||% "nd",
         verde    = "#18B884", amarelo = "#F5A623",
         vermelho = "#FF3B4E", "#A0A6B8")
}

status_label <- function(st) {
  switch(st %||% "nd",
         verde    = "No Alvo",
         amarelo  = "Em Risco",
         vermelho = "Critico",
         "Sem dados")
}

status_badge_cls <- function(st) {
  switch(st %||% "nd",
         verde    = "sbadge sb-verde",
         amarelo  = "sbadge sb-amarelo",
         vermelho = "sbadge sb-verm",
         "sbadge sb-nd")
}

prog_bar <- function(pct, st = "nd") {
  pct <- min(max(pct %||% 0, 0), 100)
  div(class = "pb-wrap",
      div(class = "pb-fill",
          style = paste0("width:", pct, "%;background:", status_cor(st), ";")))
}

# ── SVG Logo ───────────────────────────────────────────────
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

# ── CSS ────────────────────────────────────────────────────
APP_CSS <- "
*{box-sizing:border-box;margin:0;padding:0;}
body{font-family:'Manrope',sans-serif;background:#F5F6FA;color:#1B1B1C;font-size:14px;}
a{color:inherit;text-decoration:none;}
.form-group{margin-bottom:0!important;}
label{font-size:11px!important;font-weight:700!important;color:#6B7080!important;}

/* SIDEBAR */
.sidebar{width:224px;min-height:100vh;
  background:linear-gradient(175deg,#0A1E5E 0%,#0F2878 40%,#1742AD 100%);
  display:flex;flex-direction:column;position:fixed;left:0;top:0;bottom:0;z-index:100;overflow-y:auto;}
.sb-logo{padding:20px 20px 18px;border-bottom:1px solid rgba(255,255,255,.08);}
.sb-nav{padding:12px 10px;flex:1;}
.sb-label{font-size:10px;font-weight:700;letter-spacing:.12em;color:rgba(255,255,255,.35);
  padding:0 10px;margin:16px 0 4px;text-transform:uppercase;display:block;}
.sb-item{display:flex;align-items:center;gap:10px;padding:9px 10px;border-radius:9px;
  color:rgba(255,255,255,.6);font-size:13px;font-weight:500;cursor:pointer;
  transition:all .18s;margin-bottom:2px;border:1px solid transparent;
  background:none;width:100%;text-align:left;font-family:'Manrope',sans-serif;}
.sb-item:hover{background:rgba(255,255,255,.08);color:rgba(255,255,255,.9);}
.sb-item.active{background:rgba(50,113,254,.25);color:#fff;font-weight:600;
  border-color:rgba(50,113,254,.4);}
.sb-icon{width:34px;height:30px;border-radius:7px;display:flex;align-items:center;
  justify-content:center;font-size:9px;font-weight:800;letter-spacing:.04em;flex-shrink:0;color:#fff;}
.sb-item.active .sb-icon{background:#3271FE;}
.sb-item:not(.active) .sb-icon{background:rgba(255,255,255,.07);}
.sb-badge{margin-left:auto;background:#FF3B4E;color:#fff;font-size:9px;
  font-weight:800;padding:2px 6px;border-radius:99px;}
.sb-footer{padding:14px 14px 20px;border-top:1px solid rgba(255,255,255,.08);}
.sb-user{display:flex;align-items:center;gap:10px;padding:8px 6px;
  border-radius:9px;cursor:pointer;transition:background .15s;}
.sb-user:hover{background:rgba(255,255,255,.08);}
.sb-avatar{width:32px;height:32px;border-radius:50%;background:#3271FE;
  display:flex;align-items:center;justify-content:center;font-size:11px;font-weight:700;color:#fff;}
.sb-uname{font-size:12px;font-weight:600;color:#fff;}
.sb-urole{font-size:10px;color:rgba(255,255,255,.45);}

/* MAIN */
.main{margin-left:224px;min-height:100vh;display:flex;flex-direction:column;}

/* TOPBAR */
.topbar{background:#fff;border-bottom:1px solid #E8EBF5;padding:0 28px;height:60px;
  display:flex;align-items:center;gap:12px;position:sticky;top:0;z-index:50;flex-shrink:0;}
.tb-title{font-size:18px;font-weight:700;color:#1B1B1C;line-height:1.2;}
.tb-crumb{font-size:11px;color:#A0A6B8;}
.tb-crumb span{color:#1B1B1C;font-weight:600;}
.tb-spacer{flex:1;}
.tb-btn{background:#3271FE;color:#fff;border:none;border-radius:8px;padding:7px 16px;
  font-family:'Manrope',sans-serif;font-size:12px;font-weight:700;cursor:pointer;transition:background .15s;}
.tb-btn:hover{background:#1742AD;}
.tb-ghost{background:none;border:1px solid #E8EBF5;border-radius:8px;padding:7px 14px;
  font-family:'Manrope',sans-serif;font-size:12px;font-weight:600;color:#6B7080;cursor:pointer;transition:all .15s;}
.tb-ghost:hover{border-color:#3271FE;color:#3271FE;}

/* FILTER BAR */
.fbar{background:#fff;padding:10px 28px;border-bottom:2px solid #E8EBF5;
  display:flex;gap:14px;align-items:center;flex-wrap:wrap;}
.fbar-lbl{font-size:11px;color:#6B7080;font-weight:700;letter-spacing:.6px;white-space:nowrap;}

/* CONTENT */
.content{padding:22px 28px 56px;flex:1;}

/* SECTION */
.sec{font-size:10px;font-weight:800;color:#6B7080;letter-spacing:1.5px;
  text-transform:uppercase;margin:26px 0 10px;padding-bottom:6px;border-bottom:2px solid #E8EBF5;}
.sec:first-child{margin-top:0;}

/* KPI CARDS */
.kgrid{display:grid;grid-template-columns:1.5fr repeat(4,1fr) 1.1fr;gap:14px;margin-bottom:6px;}
@media(max-width:1400px){.kgrid{grid-template-columns:1.4fr repeat(3,1fr) 1.1fr;}}
@media(max-width:1100px){.kgrid{grid-template-columns:repeat(3,1fr);}}
.kcard{background:#fff;border-radius:12px;padding:18px 20px;border:1px solid #E8EBF5;
  box-shadow:0 1px 4px rgba(0,0,0,.04);transition:box-shadow .15s;position:relative;overflow:hidden;}
.kcard::before{content:'';position:absolute;top:0;left:0;right:0;height:3px;border-radius:12px 12px 0 0;}
.kcard.kc-blue::before{background:#3271FE;} .kcard.kc-green::before{background:#18B884;}
.kcard.kc-yellow::before{background:#F5A623;} .kcard.kc-red::before{background:#FF3B4E;}
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
.klbl{font-size:10px;font-weight:700;color:#6B7080;letter-spacing:.9px;text-transform:uppercase;margin-bottom:8px;}
.kval{font-size:24px;font-weight:800;color:#1B1B1C;line-height:1.1;}
.kval.g{color:#18B884;} .kval.y{color:#F5A623;} .kval.r{color:#FF3B4E;} .kval.bl{color:#3271FE;}
.kdelta{font-size:11px;margin-top:6px;font-weight:600;color:#6B7080;}
.kdelta.up{color:#18B884;} .kdelta.dn{color:#FF3B4E;}
.resumo-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:8px;padding:8px 0;}
.resumo-item{display:flex;flex-direction:column;align-items:center;gap:3px;}
.resumo-num{font-size:20px;font-weight:800;line-height:1;}
.resumo-lbl{font-size:9px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:#A0A6B8;}

/* CARDS / GRID */
.card{background:#fff;border-radius:12px;padding:20px;border:1px solid #E8EBF5;box-shadow:0 1px 5px rgba(0,0,0,.04);}
.cgrid{display:grid;grid-template-columns:1fr 320px;gap:16px;align-items:start;}
.cgrid-eq{display:grid;grid-template-columns:1fr 1fr;gap:14px;}
@media(max-width:1100px){.cgrid{grid-template-columns:1fr;}.cgrid-eq{grid-template-columns:1fr;}}
.card-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:14px;}
.card-ttl{font-size:13px;font-weight:700;color:#1B1B1C;}
.badge{background:#f0f4f8;color:#6B7080;font-size:10px;padding:3px 9px;border-radius:12px;font-weight:700;}
.badge-blue{background:#eff6ff;color:#2563eb;} .badge-green{background:#f0fdf4;color:#16a34a;}
.badge-yellow{background:#fff7ed;color:#d97706;} .badge-red{background:#fff1f0;color:#e03e3e;}

/* OBJ CARDS */
.obj-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px;}
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

/* BADGES SEMÁFORO */
.sbadge{display:inline-flex;align-items:center;gap:3px;padding:2px 8px;border-radius:99px;font-size:10px;font-weight:700;}
.sb-verde{background:rgba(24,184,132,.1);color:#18B884;}
.sb-amarelo{background:rgba(245,166,35,.1);color:#F5A623;}
.sb-verm{background:rgba(255,59,78,.1);color:#FF3B4E;}
.sb-nd{background:#f3f4f6;color:#9aa5b4;}

/* PROGRESS BAR */
.pb-wrap{height:5px;background:#F5F6FA;border-radius:99px;overflow:hidden;}
.pb-fill{height:100%;border-radius:99px;transition:width .5s ease;}

/* ALERTAS */
.al-section{margin-bottom:12px;}
.al-section-ttl{font-size:10px;font-weight:800;letter-spacing:.08em;text-transform:uppercase;
  margin-bottom:6px;display:flex;align-items:center;gap:6px;}
.al-item{display:flex;gap:10px;padding:10px 12px;border-radius:9px;margin-bottom:6px;border:1px solid transparent;}
.al-item:last-child{margin-bottom:0;}
.al-crit{background:rgba(255,59,78,.05);border-color:rgba(255,59,78,.15);}
.al-warn{background:rgba(245,166,35,.06);border-color:rgba(245,166,35,.18);}
.al-info{background:rgba(160,166,184,.06);border-color:rgba(160,166,184,.2);}
.al-dot{width:8px;height:8px;border-radius:50%;flex-shrink:0;margin-top:4px;}
.al-dot.red{background:#FF3B4E;} .al-dot.yellow{background:#F5A623;} .al-dot.grey{background:#A0A6B8;}
.al-body{flex:1;min-width:0;}
.al-kr{font-size:10px;font-weight:700;color:#A0A6B8;margin-bottom:1px;}
.al-title{font-size:11px;font-weight:700;margin-bottom:1px;}
.al-title.red{color:#FF3B4E;} .al-title.yellow{color:#F5A623;} .al-title.grey{color:#6B7080;}
.al-desc{font-size:11px;color:#6B7080;line-height:1.4;}

/* PENDÊNCIAS */
.pend-item{display:flex;align-items:center;gap:10px;padding:9px 0;border-bottom:1px solid #F5F6FA;}
.pend-item:last-child{border:none;}
.pend-kr{background:#f3f4f6;border-radius:6px;padding:3px 8px;font-size:10px;font-weight:700;color:#6B7080;flex-shrink:0;}
.pend-info{flex:1;min-width:0;}
.pend-nome{font-size:11px;font-weight:600;color:#1B1B1C;}
.pend-sub{font-size:10px;color:#A0A6B8;margin-top:1px;}
.pend-resp{font-size:10px;font-weight:700;color:#FF3B4E;}

/* KR ROWS (Camada 2/3) */
.kr-row{display:flex;flex-direction:column;gap:6px;padding:12px 14px;background:#fafbfc;
  border-radius:9px;border-left:3px solid #e5e9ef;margin-bottom:8px;cursor:pointer;transition:all .15s;}
.kr-row:hover{box-shadow:0 2px 8px rgba(0,0,0,.06);}
.kr-row.st-verde{border-left-color:#18B884;} .kr-row.st-amarelo{border-left-color:#F5A623;} .kr-row.st-vermelho{border-left-color:#FF3B4E;}
.kr-row-top{display:flex;justify-content:space-between;align-items:flex-start;gap:10px;}
.kr-nome{font-size:12.5px;font-weight:600;color:#1B1B1C;line-height:1.4;}
.kr-right{display:flex;align-items:center;gap:7px;flex-shrink:0;}
.kr-meta-txt{font-size:11px;color:#6B7080;}
.kr-pct{font-size:11px;font-weight:800;padding:2px 8px;border-radius:99px;color:#fff;}
.kr-pct.bg-verde{background:#18B884;} .kr-pct.bg-amarelo{background:#F5A623;} .kr-pct.bg-vermelho{background:#FF3B4E;} .kr-pct.bg-nd{background:#A0A6B8;}
.kr-bottom{display:flex;justify-content:space-between;align-items:center;}
.kr-data{font-size:10px;color:#A0A6B8;}
.kr-pill{font-size:10px;font-weight:700;padding:2px 7px;border-radius:4px;letter-spacing:.04em;}
.kr-pill.pill-verde{color:#18B884;background:rgba(24,184,132,.1);}
.kr-pill.pill-amarelo{color:#F5A623;background:rgba(245,166,35,.1);}
.kr-pill.pill-vermelho{color:#FF3B4E;background:rgba(255,59,78,.1);}
.kr-pill.pill-nd{color:#9aa5b4;background:#f3f4f6;}

/* CONF BARS */
.conf-row{margin-bottom:8px;} .conf-row:last-child{margin-bottom:0;}
.conf-hdr{display:flex;justify-content:space-between;margin-bottom:4px;}
.conf-lbl{font-size:11px;font-weight:600;color:#1B1B1C;}

/* DRILL-DOWN breadcrumb */
.drill-crumb{display:flex;align-items:center;gap:6px;margin-bottom:16px;flex-wrap:wrap;}
.drill-step{font-size:12px;font-weight:600;color:#6B7080;cursor:pointer;}
.drill-step:hover{color:#3271FE;}
.drill-step.active{color:#1B1B1C;cursor:default;}
.drill-sep{color:#A0A6B8;font-size:12px;}

/* FORM / INPUT */
.form-grid{display:grid;grid-template-columns:1fr 1fr;gap:20px;}
@media(max-width:900px){.form-grid{grid-template-columns:1fr;}}
.form-card{background:#fff;border-radius:12px;border:1px solid #E8EBF5;overflow:hidden;transition:box-shadow .18s;}
.form-card:hover{box-shadow:0 4px 16px rgba(50,113,254,.06);}
.form-card-accent{height:3px;}
.fa-blue{background:#3271FE;} .fa-dark{background:linear-gradient(90deg,#0A1E5E,#1742AD);}
.form-card-body{padding:22px;}
.form-card-ttl{font-size:14px;font-weight:700;color:#1B1B1C;margin-bottom:4px;}
.form-card-sub{font-size:12px;color:#6B7080;margin-bottom:18px;}
.fg{margin-bottom:13px;}
.fg-lbl{display:block;font-size:11px;font-weight:700;color:#6B7080;text-transform:uppercase;letter-spacing:.05em;margin-bottom:5px;}
.fg-2{display:grid;grid-template-columns:1fr 1fr;gap:12px;}
.fg-hint{font-size:11px;color:#A0A6B8;margin-top:4px;line-height:1.4;}
.form-control,.selectize-input{border:1px solid #E8EBF5!important;border-radius:8px!important;
  font-family:'Manrope',sans-serif!important;font-size:13px!important;background:#F5F6FA!important;
  color:#1B1B1C!important;box-shadow:none!important;}
.form-control{padding:8px 12px!important;}
.selectize-input{min-height:36px!important;padding:6px 12px!important;}
.form-control:focus,.selectize-input.focus{outline:none!important;border-color:#3271FE!important;
  box-shadow:0 0 0 3px rgba(50,113,254,.1)!important;background:#fff!important;}
textarea.form-control{resize:vertical!important;min-height:80px!important;}

/* KR info box (coleta) */
.kr-info-box{background:rgba(50,113,254,.05);border:1px solid rgba(50,113,254,.15);
  border-radius:8px;padding:10px 14px;display:flex;gap:18px;flex-wrap:wrap;margin-bottom:13px;}
.ki-item{display:flex;align-items:center;gap:5px;}
.ki-lbl{font-size:11px;color:#A0A6B8;font-weight:600;}
.ki-val{font-size:12px;font-weight:700;color:#3271FE;}

/* Preview atingimento */
.preview-box{border-radius:8px;padding:12px 14px;margin-bottom:13px;border:1px solid;}
.pv-verde{border-color:#18B884;background:rgba(24,184,132,.05);}
.pv-amarelo{border-color:#F5A623;background:rgba(245,166,35,.05);}
.pv-vermelho{border-color:#FF3B4E;background:rgba(255,59,78,.05);}
.pv-pct{font-size:26px;font-weight:800;line-height:1;margin-bottom:6px;letter-spacing:-.5px;}
.pv-verde .pv-pct{color:#18B884;} .pv-amarelo .pv-pct{color:#F5A623;} .pv-vermelho .pv-pct{color:#FF3B4E;}
.pv-status{font-size:11px;font-weight:700;margin-top:5px;}

/* Confiança 4 pontos */
.conf4-wrap{display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-top:4px;}
.conf4-btn{border:2px solid #E8EBF5;border-radius:8px;padding:8px 6px;text-align:center;
  cursor:pointer;transition:all .15s;background:#fff;font-family:'Manrope',sans-serif;}
.conf4-btn:hover{border-color:#3271FE;}
.conf4-btn.sel-1{border-color:#FF3B4E;background:rgba(255,59,78,.06);}
.conf4-btn.sel-2{border-color:#F5A623;background:rgba(245,166,35,.06);}
.conf4-btn.sel-3{border-color:#3271FE;background:rgba(50,113,254,.06);}
.conf4-btn.sel-4{border-color:#18B884;background:rgba(24,184,132,.06);}
.conf4-icon{font-size:18px;display:block;margin-bottom:3px;}
.conf4-lbl{font-size:10px;font-weight:700;color:#6B7080;}

/* Qualitativo display */
.qual-bloco{margin-bottom:14px;}
.qual-bloco:last-child{margin-bottom:0;}
.qual-bloco-lbl{font-size:10px;font-weight:700;color:#A0A6B8;text-transform:uppercase;
  letter-spacing:.05em;display:block;margin-bottom:4px;}
.qual-bloco-txt{font-size:13px;color:#1B1B1C;line-height:1.6;background:#fafbfc;
  border-radius:7px;padding:10px 12px;border:1px solid #E8EBF5;}

/* BOTÕES */
.btn-primary{width:100%;height:42px;border:none;border-radius:8px;background:#3271FE;color:#fff;
  font-family:'Manrope',sans-serif;font-size:13px;font-weight:700;cursor:pointer;margin-top:8px;transition:background .15s;}
.btn-primary:hover{background:#1742AD;}
.btn-dark{width:100%;height:42px;border:none;border-radius:8px;background:#1B1B1C;color:#fff;
  font-family:'Manrope',sans-serif;font-size:13px;font-weight:700;cursor:pointer;margin-top:8px;transition:background .15s;}
.btn-dark:hover{background:#333;}

/* TOAST */
.toast{padding:11px 16px;border-radius:9px;font-weight:600;font-size:13px;margin-top:13px;}
.toast-ok{background:rgba(24,184,132,.08);border:1px solid #18B884;color:#065f46;}
.toast-err{background:rgba(255,59,78,.08);border:1px solid #FF3B4E;color:#7f1d1d;}

/* MISC */
.chip{display:inline-flex;align-items:center;padding:3px 10px;border-radius:99px;
  font-size:11px;font-weight:600;background:rgba(50,113,254,.08);color:#3271FE;
  border:1px solid rgba(50,113,254,.15);}
.sem-dados{color:#A0A6B8;font-size:13px;padding:20px 0;text-align:center;}
::-webkit-scrollbar{width:5px;height:5px;}
::-webkit-scrollbar-track{background:transparent;}
::-webkit-scrollbar-thumb{background:#E8EBF5;border-radius:99px;}

/* DT override */
.dataTables_wrapper .dataTables_filter input{border:1px solid #E8EBF5;border-radius:6px;padding:4px 10px;font-size:12px;}
.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_paginate{font-size:12px;color:#6B7080;}
table.dataTable thead th{font-size:11px;font-weight:700;color:#6B7080;letter-spacing:.5px;text-transform:uppercase;}
table.dataTable tbody td{font-size:12px;}

/* EXPORTAÇÃO CSV */
.export-bar{display:flex;align-items:center;gap:10px;margin-bottom:14px;flex-wrap:wrap;}
.btn-export{display:inline-flex;align-items:center;gap:6px;padding:7px 16px;border-radius:8px;
  border:1px solid #18B884;background:rgba(24,184,132,.06);color:#18B884;
  font-family:'Manrope',sans-serif;font-size:12px;font-weight:700;cursor:pointer;
  text-decoration:none;transition:all .15s;}
.btn-export:hover{background:#18B884;color:#fff;}

/* PAINEL DE GESTÃO / EXCLUSÃO */
.gestao-row{display:flex;align-items:center;gap:10px;padding:10px 12px;
  border-radius:9px;border:1px solid #E8EBF5;margin-bottom:6px;background:#fff;
  transition:all .15s;}
.gestao-row:hover{border-color:#FF3B4E;background:rgba(255,59,78,.02);}
.gestao-info{flex:1;min-width:0;}
.gestao-kr{font-size:12px;font-weight:700;color:#1B1B1C;}
.gestao-meta{font-size:11px;color:#6B7080;margin-top:2px;}
.gestao-comp{font-size:11px;font-weight:600;color:#3271FE;white-space:nowrap;}
.gestao-val{font-size:12px;font-weight:700;text-align:right;white-space:nowrap;}
.btn-del{padding:5px 12px;border-radius:7px;border:1px solid #FF3B4E;
  background:rgba(255,59,78,.06);color:#FF3B4E;font-family:'Manrope',sans-serif;
  font-size:11px;font-weight:700;cursor:pointer;white-space:nowrap;transition:all .15s;}
.btn-del:hover{background:#FF3B4E;color:#fff;}
.modal-overlay{position:fixed;inset:0;background:rgba(0,0,0,.4);z-index:9000;
  display:flex;align-items:center;justify-content:center;}
.modal-box{background:#fff;border-radius:14px;padding:28px 32px;max-width:420px;
  width:90%;box-shadow:0 20px 60px rgba(0,0,0,.2);}
.modal-title{font-size:16px;font-weight:700;color:#1B1B1C;margin-bottom:8px;}
.modal-desc{font-size:13px;color:#6B7080;line-height:1.6;margin-bottom:20px;}
.modal-actions{display:flex;gap:10px;justify-content:flex-end;}
.btn-cancel{padding:8px 20px;border-radius:8px;border:1px solid #E8EBF5;background:#fff;
  color:#6B7080;font-family:'Manrope',sans-serif;font-size:13px;font-weight:600;cursor:pointer;}
.btn-cancel:hover{border-color:#6B7080;}
.btn-confirm-del{padding:8px 20px;border-radius:8px;border:none;background:#FF3B4E;
  color:#fff;font-family:'Manrope',sans-serif;font-size:13px;font-weight:700;cursor:pointer;}
.btn-confirm-del:hover{background:#e0293a;}
.gestao-filtro{display:flex;gap:8px;align-items:center;margin-bottom:14px;flex-wrap:wrap;}

/* TIPOLOGIA DE INDICADOR */
.tipo-chip{display:inline-flex;align-items:center;gap:4px;padding:2px 8px;
  border-radius:5px;font-size:9px;font-weight:800;letter-spacing:.06em;text-transform:uppercase;}
.tipo-acumulado{background:rgba(50,113,254,.1);color:#3271FE;}
.tipo-media{background:rgba(52,195,208,.1);color:#34C3D0;}
.tipo-taxa{background:rgba(255,158,98,.1);color:#FF9E62;}
.tipo-estoque{background:rgba(102,59,142,.1);color:#663B8E;}

/* KR ANUAL CARDS (visão consolidada) */
.anual-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(240px,1fr));gap:12px;margin-bottom:14px;}
.anual-card{background:#fff;border-radius:10px;padding:14px 16px;border:1px solid #E8EBF5;
  box-shadow:0 1px 3px rgba(0,0,0,.04);}
.anual-card-top{display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;}
.anual-kr-nome{font-size:11.5px;font-weight:600;color:#1B1B1C;line-height:1.4;flex:1;margin-right:8px;}
.anual-pct{font-size:20px;font-weight:800;line-height:1;}
.anual-vals{display:flex;gap:14px;margin-top:8px;}
.anual-val-item{display:flex;flex-direction:column;gap:2px;}
.anual-val-lbl{font-size:9px;font-weight:700;color:#A0A6B8;text-transform:uppercase;letter-spacing:.05em;}
.anual-val-num{font-size:12px;font-weight:700;color:#1B1B1C;}
.anual-meses{font-size:10px;color:#A0A6B8;margin-top:6px;}

/* CHART TABS */
.chart-tabs{display:flex;gap:2px;margin-bottom:10px;}
.chart-tab{padding:5px 14px;border-radius:7px;font-size:11px;font-weight:700;cursor:pointer;
  border:1px solid #E8EBF5;background:#fff;color:#6B7080;font-family:'Manrope',sans-serif;
  transition:all .15s;}
.chart-tab.active{background:#3271FE;color:#fff;border-color:#3271FE;}
.chart-tab:hover:not(.active){border-color:#3271FE;color:#3271FE;}

/* PROGRESSO ANUAL BARRA */
.anual-prog-wrap{height:6px;background:#F5F6FA;border-radius:99px;overflow:hidden;margin-top:6px;}
.anual-prog-fill{height:100%;border-radius:99px;}
"

# ── UI ─────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$title("MIRAI OKR - RAE Dashboard"),
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
                 tags$button(class = "sb-item active", id = "nav_geral",
                             div(class = "sb-icon", "VG"), "Visão Geral",
                             onclick = "Shiny.setInputValue('nav_page','geral',{priority:'event'})"),
                 tags$button(class = "sb-item", id = "nav_objetivos",
                             div(class = "sb-icon", "OBJ"), "Por Objetivo",
                             onclick = "Shiny.setInputValue('nav_page','objetivos',{priority:'event'})"),
                 tags$button(class = "sb-item", id = "nav_krs",
                             div(class = "sb-icon", "KR"), "KRs Detalhado",
                             onclick = "Shiny.setInputValue('nav_page','krs',{priority:'event'})"),
                 tags$span(class = "sb-label", "Gestão"),
                 tags$button(class = "sb-item", id = "nav_coleta",
                             div(class = "sb-icon", "COL"), "Coletar Dados",
                             onclick = "Shiny.setInputValue('nav_page','coleta',{priority:'event'})"),
                 tags$button(class = "sb-item", id = "nav_alertas",
                             div(class = "sb-icon", "ALT"), "Alertas",
                             tags$span(class = "sb-badge", id = "badge_n", ""),
                             onclick = "Shiny.setInputValue('nav_page','alertas',{priority:'event'})"),
                 tags$button(class = "sb-item", id = "nav_tabela",
                             div(class = "sb-icon", "TAB"), "Tabela Gerencial",
                             onclick = "Shiny.setInputValue('nav_page','tabela',{priority:'event'})"),
                 tags$button(class = "sb-item", id = "nav_gestao",
                             div(class = "sb-icon", "GES"), "Gestão de Dados",
                             onclick = "Shiny.setInputValue('nav_page','gestao',{priority:'event'})")
             ),
             div(class = "sb-footer",
                 div(class = "sb-user",
                     div(class = "sb-avatar", "MR"),
                     div(div(class = "sb-uname", "MIRAI Team"),
                         div(class = "sb-urole", "Admin | RAE Dashboard"))
                 )
             )
  ),
  
  div(class = "main",
      uiOutput("topbar_ui"),
      uiOutput("fbar_ui"),
      div(class = "content", uiOutput("body_ui"))
  ),
  
  tags$script(HTML("
    var NAV_MAP = {
      geral:'nav_geral', objetivos:'nav_objetivos', krs:'nav_krs',
      coleta:'nav_coleta', alertas:'nav_alertas', tabela:'nav_tabela', gestao:'nav_gestao'
    };
    Shiny.addCustomMessageHandler('setNav', function(page) {
      document.querySelectorAll('.sb-item').forEach(function(el) {
        el.classList.remove('active');
        var ic = el.querySelector('.sb-icon');
        if (ic) ic.style.background = 'rgba(255,255,255,.07)';
      });
      var id = NAV_MAP[page];
      if (id) {
        var el = document.getElementById(id);
        if (el) {
          el.classList.add('active');
          var ic = el.querySelector('.sb-icon');
          if (ic) ic.style.background = '#3271FE';
        }
      }
    });
    Shiny.addCustomMessageHandler('setBadge', function(n) {
      var b = document.getElementById('badge_n');
      if (b) b.textContent = n > 0 ? n : '';
    });
    Shiny.addCustomMessageHandler('clearDelKey', function(x) {
      Shiny.setInputValue('del_confirm_key', '', {priority: 'event'});
      Shiny.setInputValue('del_execute_key', '', {priority: 'event'});
    });
    Shiny.addCustomMessageHandler('setKrTab', function(tab) {
      document.querySelectorAll('.chart-tab').forEach(function(el) {
        el.classList.remove('active');
      });
      var tId = tab === 'anual' ? 'tab_anual' : 'tab_mensal';
      var el  = document.getElementById(tId);
      if (el) el.classList.add('active');
    });
  "))
)

# ── SERVER ──────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    page        = "geral",
    obj_sel     = NULL,   # id_objetivo para camada 2
    kr_sel      = NULL,   # id_kr para camada 3
    confianca   = 3L,
    toast       = NULL
  )
  
  refresh <- reactiveVal(Sys.time())
  observeEvent(input$btn_refresh, { refresh(Sys.time()) })
  observe({ invalidateLater(30000, session); refresh(Sys.time()) })
  
  # Navegação
  observeEvent(input$nav_page, {
    rv$page <- input$nav_page
    session$sendCustomMessage("setNav", input$nav_page)
  }, ignoreInit = TRUE)
  
  # Dados reativos
  competencias_disp <- reactive({
    refresh()
    get_competencias(ano = ANO_REF)  # sempre jan-dez do ano estratégico
  })
  
  comp_sel <- reactive({
    cs <- competencias_disp()
    if (length(cs) == 0) return(NULL)
    sel <- input$fil_comp
    if (!is.null(sel) && sel %in% cs) return(sel)
    # padrão: mês atual se dentro do ano, senão dez/ano_ref
    hc <- format(Sys.Date(), "%Y-%m")
    if (hc %in% cs) hc else cs[length(cs)]
  })
  
  df_status <- reactive({
    refresh()
    get_ultimo_status(comp_sel())
  })
  
  df_status_all <- reactive({
    refresh()
    get_ultimo_status(NULL)
  })
  
  # Resumo consolidado ATÉ o mês selecionado em fil_comp
  df_resumo_anual <- reactive({
    refresh()
    ate <- comp_sel() %||% ""
    get_resumo_anual_krs(ano = ANO_REF, ate_comp = ate)
  })
  
  # Consolidado de % por objetivo ATÉ o mês filtrado
  df_consolidado_obj <- reactive({
    refresh()
    ate <- comp_sel() %||% ""
    get_consolidado_objetivos(ano = ANO_REF, ate_comp = ate)
  })
  
  # Badge alertas
  observe({
    df <- df_status_all()
    n  <- sum(df$status %in% c("vermelho","amarelo"), na.rm = TRUE) +
      sum(is.na(df$status))
    session$sendCustomMessage("setBadge", n)
  })
  
  # ── TOPBAR ──────────────────────────────────────────────
  output$topbar_ui <- renderUI({
    info <- list(
      geral     = list(t="Visão Geral",      s="Performance consolidada da estratégia"),
      objetivos = list(t="Por Objetivo",      s="Camada 2 - Breakdown por objetivo estrategico"),
      krs       = list(t="KRs Detalhado",     s="Camada 3 - Serie temporal e analise qualitativa"),
      coleta    = list(t="Coletar Dados",      s="Registro de indicadores por competência"),
      alertas   = list(t="Central de Alertas",s="Desempenho | Risco | Ausencia de coleta"),
      tabela    = list(t="Tabela Gerencial",   s="Visão consolidada exportável"),
      gestao    = list(t="Gestão de Dados",    s="Excluir registros inseridos na plataforma")
    )[[rv$page]] %||% list(t="Dashboard", s="")
    
    div(class = "topbar",
        div(div(class = "tb-title", info$t),
            div(class = "tb-crumb", HTML(paste0("RAE | <span>", info$s, "</span>")))),
        div(class = "tb-spacer"),
        if (rv$page != "coleta")
          tags$button(class = "tb-ghost",
                      onclick = "Shiny.setInputValue('btn_refresh', Math.random())", "Atualizar")
    )
  })
  
  # ── FILTER BAR ─────────────────────────────────────────
  output$fbar_ui <- renderUI({
    if (rv$page == "coleta") return(NULL)
    
    cs <- competencias_disp()
    if (length(cs) == 0) return(NULL)
    
    # Meses que já possuem ao menos um dado registrado
    cs_com_dados <- tryCatch({
      con <- get_con()
      on.exit(dbDisconnect(con), add = TRUE)
      res <- dbGetQuery(con,
                        sprintf("SELECT DISTINCT competencia FROM fact_indicadores
                 WHERE substr(competencia,1,4)='%d'
                 ORDER BY competencia ASC", ANO_REF))
      res$competencia
    }, error = function(e) character(0))
    
    # Rótulos: meses com dado marcados com "*", futuros com "-"
    comp_labels <- sapply(cs, function(m) {
      lbl <- fmt_competencia(m)
      if (m %in% cs_com_dados) paste0(lbl, " *") else lbl
    })
    comp_choices <- setNames(cs, comp_labels)
    
    # Padrão: mês atual ou o mais recente com dados
    hoje_comp <- format(Sys.Date(), "%Y-%m")
    default_comp <- if (hoje_comp %in% cs) hoje_comp else {
      if (length(cs_com_dados) > 0) cs_com_dados[length(cs_com_dados)]
      else cs[length(cs)]
    }
    
    div(class = "fbar",
        div(class = "fbar-lbl", "ATÉ:"),
        selectInput("fil_comp", NULL, choices = comp_choices,
                    selected = default_comp, width = "175px"),
        div(class = "fbar-lbl", "DE:"),
        selectInput("fil_comp_de", NULL, choices = comp_choices,
                    selected = cs[1], width = "155px"),
        div(class = "fbar-lbl", "ATÉ (gráfico):"),
        selectInput("fil_comp_ate", NULL, choices = comp_choices,
                    selected = default_comp, width = "175px"),
        div(class = "fbar-lbl", "STATUS:"),
        selectInput("fil_status", NULL,
                    choices = c("Todos"="todos","No Alvo"="verde",
                                "Em Risco"="amarelo","Crítico"="vermelho","Sem dados"="nd"),
                    selected = "todos", width = "150px")
    )
  })
  
  # ── BODY PRINCIPAL ──────────────────────────────────────
  output$body_ui <- renderUI({
    switch(rv$page,
           geral     = render_geral(),
           objetivos = render_objetivos(),
           krs       = render_krs(),
           coleta    = render_coleta(),
           alertas   = render_alertas(),
           tabela    = render_tabela(),
           gestao    = render_gestao(),
           render_geral()
    )
  })
  
  # ══════════════════════════════════════════════════════
  # CAMADA 1 - VISÃO GERAL
  # ══════════════════════════════════════════════════════
  render_geral <- function() {
    # ── Fontes de dados ──────────────────────────────────────
    # da  = consolidado ATÉ o mês filtrado (regra tipologia por KR)
    # obj = consolidado por objetivo
    # df  = snapshot do último mês (para alertas e pendências)
    da   <- df_resumo_anual()
    obj  <- df_consolidado_obj()
    df   <- df_status()          # ainda usado para alertas/pendências
    objs <- get_objetivos()
    
    comp_lbl  <- fmt_competencia(comp_sel() %||% "")
    comp_atual <- comp_sel() %||% ""
    
    if (nrow(da) == 0)
      return(tagList(
        div(class="sec", "BEM-VINDO AO MIRAI OKR DASHBOARD"),
        div(class="card", style="max-width:560px;margin:40px auto;text-align:center;padding:40px 32px",
            tags$div(style="font-size:40px;margin-bottom:16px", "TAB"),
            tags$h3(style="font-size:18px;font-weight:700;color:#1B1B1C;margin-bottom:8px",
                    "Nenhum dado coletado ainda"),
            tags$p(style="font-size:13px;color:#6B7080;line-height:1.6;margin-bottom:20px",
                   "A base está pronta e os 10 KRs da MIRAI já estão configurados. ",
                   "Comece registrando os valores realizados na tela de Coleta de Dados."),
            tags$button(class="btn-primary", style="width:auto;padding:0 24px",
                        onclick="Shiny.setInputValue('nav_page','coleta',{priority:'event'})",
                        "Ir para Coletar Dados")
        )
      ))
    
    # ── KPIs a partir do CONSOLIDADO ATÉ o mês filtrado ──────
    verde_n   <- sum(da$status_anual == "verde",    na.rm = TRUE)
    amarelo_n <- sum(da$status_anual == "amarelo",  na.rm = TRUE)
    verm_n    <- sum(da$status_anual == "vermelho", na.rm = TRUE)
    sem_n     <- nrow(get_krs()) - nrow(da)     # KRs sem nenhum dado no período
    total     <- nrow(get_krs())
    avg_pct   <- round(mean(da$pct_anual, na.rm = TRUE), 1)
    if (is.nan(avg_pct)) avg_pct <- 0
    
    # Objetivos 100% saudáveis (todos KRs no alvo no consolidado)
    objs_saudaveis <- if (nrow(obj) > 0)
      sum(obj$status == "verde", na.rm = TRUE) else 0
    n_objs <- nrow(objs)
    
    # Cobertura: KRs com ao menos um registro no período
    cobertura_pct <- round(nrow(da) / max(total, 1) * 100)
    
    # Confiança = EXCLUSIVAMENTE do qualitativo (sem misturar com atingimento)
    qual_periodo <- get_qualitativo(competencia = comp_atual)
    conf_media <- if (nrow(qual_periodo) > 0)
      round(mean(qual_periodo$confianca, na.rm = TRUE), 1) else NA
    conf_lbl <- if (!is.na(conf_media)) confianca_label(round(conf_media)) else "Sem dados"
    conf_col <- if (!is.na(conf_media)) confianca_color(round(conf_media)) else "#A0A6B8"
    
    # Tendência: variação vs mês anterior no consolidado
    cs_ord <- get_serie_consolidada(ano = ANO_REF)
    cs_ord <- cs_ord[order(cs_ord$competencia), ]
    tendencia <- if (nrow(cs_ord) >= 2) {
      last2 <- tail(cs_ord[cs_ord$competencia <= comp_atual, ], 2)
      if (nrow(last2) == 2) {
        diff_pct <- round(last2$avg_pct[2] - last2$avg_pct[1], 1)
        if (diff_pct > 0) paste0("(+)", diff_pct, "% vs mês anterior")
        else if (diff_pct < 0) paste0("(-)", diff_pct, "% vs mês anterior")
        else "Estavel vs mes anterior"
      } else "Histórico insuficiente"
    } else "Histórico insuficiente"
    tend_pos <- !grepl("^\\(-\\)", tendencia)
    
    tagList(
      # ── KPIs ────────────────────────────────────────────
      div(class="sec", paste0("PERFORMANCE CONSOLIDADA - ATÉ ", comp_lbl)),
      div(class="kgrid",
          # Hero: KRs dentro da meta
          div(class="kcard hero",
              div(class="klbl","KRs DENTRO DA META"),
              div(style="display:flex;align-items:baseline;gap:6px",
                  div(class=if(verde_n>=total*0.8)"kval g"else"kval",
                      style="font-size:36px", verde_n),
                  div(style="color:rgba(255,255,255,.5);font-size:16px;font-weight:600",
                      paste0("de ", total))
              ),
              div(class=if(tend_pos)"kdelta up"else"kdelta dn", tendencia)
          ),
          # KPI: Objetivos 100% saudáveis
          div(class="kcard kc-blue",
              div(class="klbl","Objetivos Saudáveis"),
              div(class="kval bl", paste0(objs_saudaveis,"/",n_objs)),
              div(class="kdelta", "Todos os KRs no alvo")
          ),
          # KPI: Atingimento médio geral
          div(class="kcard kc-green",
              div(class="klbl","Atingimento Médio"),
              div(class=if(avg_pct>=80)"kval g"else if(avg_pct>=60)"kval y"else"kval r",
                  paste0(avg_pct,"%")),
              div(class="kdelta", "Média ponderada dos KRs")
          ),
          # KPI: Cobertura de coleta
          div(class=paste("kcard",if(cobertura_pct==100)"kc-green"else if(cobertura_pct>=70)"kc-yellow"else"kc-red"),
              div(class="klbl","Cobertura de Coleta"),
              div(class=if(cobertura_pct==100)"kval g"else if(cobertura_pct>=70)"kval y"else"kval r",
                  paste0(cobertura_pct,"%")),
              div(class="kdelta",
                  paste0(sum(!is.na(df$status))," de ",total," KRs preenchidos"))
          ),
          # KPI: Confiança média
          div(class="kcard kc-blue",
              div(class="klbl","Confiança nas Metas"),
              div(class="kval", style=paste0("color:",conf_col), conf_lbl),
              div(class="kdelta",
                  if(!is.na(conf_media)) paste0("Média: ",conf_media," / 4") else "Sem análises")
          ),
          # KPI: Resumo rápido
          div(class="kcard kc-grad",
              div(class="klbl","RESUMO"),
              div(class="resumo-grid",
                  div(class="resumo-item",div(class="resumo-num",style="color:#FF3B4E",verm_n),div(class="resumo-lbl","Critico")),
                  div(class="resumo-item",div(class="resumo-num",style="color:#F5A623",amarelo_n),div(class="resumo-lbl","Risco")),
                  div(class="resumo-item",div(class="resumo-num",style="color:#18B884",verde_n),div(class="resumo-lbl","Alvo")),
                  div(class="resumo-item",div(class="resumo-num",style="color:#A0A6B8",sem_n),div(class="resumo-lbl","S/Dados"))
              )
          )
      ),
      
      # ── ALERTAS (posição elevada - feedback Haag) ────────
      div(class="sec","ALERTAS ESTRATÉGICOS"),
      div(class="cgrid-eq",
          div(class="card",
              div(class="card-hdr",
                  div(class="card-ttl", paste0("[!] Resultado - Em Risco e Críticos (", comp_lbl, ")")),
                  uiOutput("chip_alertas", inline=TRUE)
              ),
              uiOutput("alertas_resumo")
          ),
          div(class="card",
              div(class="card-hdr",
                  div(class="card-ttl", paste0("[ ] Pendencias de Coleta - ", comp_lbl)),
                  tags$a(class="badge badge-blue", style="cursor:pointer",
                         onclick="Shiny.setInputValue(\'nav_page\',\'coleta\',{priority:\'event\'})",
                         "Preencher >")
              ),
              uiOutput("pendencias_resumo")
          )
      ),
      
      # ── OBJETIVOS + right panel (confiança) ─────────────
      # Cards usam CONSOLIDADO ATÉ o mês filtrado (não foto do mês)
      div(class="sec", paste0("OBJETIVOS ESTRATÉGICOS - Consolidado até ", comp_lbl)),
      div(class="cgrid",
          div(
            div(class="obj-grid",
                lapply(seq_len(nrow(objs)), function(i) {
                  ob   <- objs[i,]
                  # pega dados do consolidado de objetivos
                  obj_c <- if (nrow(obj) > 0) obj[obj$id_objetivo == ob$id_objetivo, ] else NULL
                  avg   <- if (!is.null(obj_c) && nrow(obj_c) > 0) obj_c$avg_pct else 0
                  v_n   <- if (!is.null(obj_c) && nrow(obj_c) > 0) obj_c$n_verde   else 0
                  a_n   <- if (!is.null(obj_c) && nrow(obj_c) > 0) obj_c$n_amarelo else 0
                  r_n   <- if (!is.null(obj_c) && nrow(obj_c) > 0) obj_c$n_vermelho else 0
                  n_kr  <- if (!is.null(obj_c) && nrow(obj_c) > 0) obj_c$n_krs     else 0
                  st    <- if (!is.null(obj_c) && nrow(obj_c) > 0) obj_c$status     else "nd"
                  col   <- OBJ_CORES[min(i, 5)]
                  ltr   <- OBJ_LTRS[min(i, 5)]
                  cls   <- if (i == 5) "obj-card full" else "obj-card"
                  
                  div(class = cls,
                      onclick = sprintf("Shiny.setInputValue('click_obj','%d',{priority:'event'})",
                                        ob$id_objetivo),
                      div(class = "obj-top",
                          div(class = "obj-letra", style = paste0("background:", col), ltr),
                          div(class = "obj-nome", substr(ob$nome, 1, 50)),
                          div(class = "obj-pct",
                              style = paste0("color:", status_cor(st)),
                              paste0(avg, "%"))
                      ),
                      prog_bar(avg, st),
                      div(class = "obj-footer",
                          div(class = "obj-sub",
                              paste0(n_kr, " KRs | OK:", v_n, " !", a_n, " X", r_n)),
                          tags$span(class = status_badge_cls(st), status_label(st))
                      )
                  )
                })
            )
          ),
          # Right panel - confiança nas metas
          div(
            div(class="card",
                div(class="card-hdr",
                    div(class="card-ttl","Confiança nas Metas"),
                    tags$span(class="badge",
                              paste0("Qualitativo | ", fmt_competencia(comp_sel() %||% "")))
                ),
                uiOutput("confianca_bars")
            )
          )
      ),
      
      # ── CONSOLIDADO ANUAL por KR ───────────────────────────
      div(class="sec",
          paste0("CONSOLIDADO ", ANO_REF, " - POSIÇÃO POR KEY RESULT (até ", comp_lbl, ")")),
      uiOutput("anual_krs_cards"),
      
      # ── GRÁFICO TEMPORAL - full width ──────────────────────
      div(class="sec","EVOLUÇÃO TEMPORAL"),
      div(class="card",
          div(class="card-hdr",
              div(class="card-ttl","Atingimento Médio vs Progresso Esperado - Série Histórica"),
              div(style="display:flex;align-items:center;gap:8px",
                  tags$span(class="badge badge-blue","Consolidado"),
                  tags$span(class="badge",
                            paste0(fmt_competencia(input$fil_comp_de %||% ""),
                                   " > ", fmt_competencia(input$fil_comp_ate %||% "")))
              )
          ),
          plotlyOutput("chart_consolidado", height="260px")
      )
    )
  }
  
  # ── Drill-down: clique em obj → vai para camada 2
  observeEvent(input$click_obj, {
    rv$obj_sel <- as.integer(input$click_obj)
    rv$page    <- "objetivos"
    session$sendCustomMessage("setNav","objetivos")
  }, ignoreInit=TRUE)
  
  # Chart consolidado
  output$chart_consolidado <- renderPlotly({
    refresh()
    df <- get_serie_consolidada()
    # Aplica filtro de intervalo de tempo
    comp_de  <- input$fil_comp_de  %||% ""
    comp_ate <- input$fil_comp_ate %||% ""
    if (nzchar(comp_de)  && nrow(df) > 0) df <- df[df$competencia >= comp_de, ]
    if (nzchar(comp_ate) && nrow(df) > 0) df <- df[df$competencia <= comp_ate, ]
    if (nrow(df) == 0)
      return(plotly_empty() %>%
               layout(
                 paper_bgcolor = "transparent", plot_bgcolor = "transparent",
                 annotations = list(list(
                   text = "Sem dados - registre indicadores na tela Coletar Dados",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE, font = list(size = 13, color = "#A0A6B8")
                 ))
               ))
    
    # Ordena cronologicamente
    df <- df[order(df$competencia), ]
    df$comp_lbl <- sapply(df$competencia, fmt_competencia)
    ordem <- df$comp_lbl
    n     <- nrow(df)
    
    # Progresso linear esperado: de 0% no inicio ate 100% no final do ciclo
    # Haag: "duas curvas - o que deveria progredir assim e o realizado"
    df$prog_linear <- round(seq(0, 100, length.out = max(n, 2))[seq_len(n)], 1)
    
    plot_ly(df, x = ~comp_lbl) %>%
      # Linha de progresso linear esperado
      add_lines(y = ~prog_linear, name = "Progresso Esperado (linear)",
                line = list(color = "rgba(160,166,184,.6)", width = 1.5, dash = "dash"),
                hovertemplate = "%{x}: %{y}% esperado<extra></extra>") %>%
      # Linha realizada
      add_trace(y = ~round(avg_pct, 1), name = "Atingimento Realizado",
                type = "scatter", mode = "lines+markers",
                line   = list(color = "#3271FE", width = 2.5),
                marker = list(color = "#3271FE", size = 7, line = list(color = "#fff", width = 2)),
                fill = "tozeroy", fillcolor = "rgba(50,113,254,.07)",
                hovertemplate = "%{x}: %{y}% realizado<extra></extra>") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font  = list(family = "Manrope,sans-serif", size = 11, color = "#A0A6B8"),
             xaxis = list(title = "", showgrid = FALSE, showline = FALSE, zeroline = FALSE,
                          categoryorder = "array", categoryarray = ordem),
             yaxis = list(title = "", showgrid = TRUE, gridcolor = "rgba(0,0,0,.04)",
                          showline = FALSE, zeroline = FALSE, range = c(0, 110),
                          ticksuffix = "%"),
             showlegend = TRUE,
             legend = list(orientation = "h", x = 0, y = 1.18, font = list(size = 10)),
             hovermode = "x unified",
             margin = list(l = 28, r = 8, t = 28, b = 28)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$chip_alertas <- renderUI({
    df <- df_status()
    if (nrow(df) == 0) return(tags$span(class="chip", "0 ativos"))
    n  <- sum(df$status %in% c("vermelho","amarelo"), na.rm=TRUE)
    tags$span(class="chip", paste(n, "ativos"))
  })
  
  output$alertas_resumo <- renderUI({
    df   <- df_status()
    crit <- head(df[!is.na(df$status) & df$status=="vermelho",], 3)
    warn <- head(df[!is.na(df$status) & df$status=="amarelo",],  2)
    if (nrow(crit)==0 && nrow(warn)==0)
      return(div(class="sem-dados","Nenhum alerta crítico"))
    tagList(
      lapply(seq_len(nrow(crit)), function(i) make_alert(crit[i,],"red")),
      lapply(seq_len(nrow(warn)), function(i) make_alert(warn[i,],"yellow"))
    )
  })
  
  output$pendencias_resumo <- renderUI({
    df  <- df_status()
    pnd <- head(df[is.na(df$status), ], 4)
    if (nrow(pnd) == 0)
      return(div(class="sem-dados",
                 "Todos os KRs preenchidos nesta competência"))
    lapply(seq_len(nrow(pnd)), function(i) {
      r <- pnd[i,]
      div(class="pend-item",
          div(class="pend-kr",paste0("KR",r$id_kr)),
          div(class="pend-info",
              div(class="pend-nome", substr(r$nome_kr,1,40)),
              div(class="pend-sub",
                  paste0("Resp.: ", r$responsavel_padrao %||% "-",
                         " | Meta: ", num_fmt(r$meta, unidade=r$unidade), " ", r$unidade))
          ),
          div(class="pend-resp","Sem coleta")
      )
    })
  })
  
  output$confianca_bars <- renderUI({
    # REGRA: esta seção exibe EXCLUSIVAMENTE a métrica qualitativa de confiança
    # Não usa percentual de atingimento nem mistura com performance
    comp <- comp_sel()
    qual <- get_qualitativo(competencia = comp)
    objs <- get_objetivos()
    
    if (nrow(objs) == 0) return(NULL)
    
    has_any_qual <- nrow(qual) > 0
    
    tagList(
      if (!has_any_qual)
        div(class = "sem-dados", style = "font-size:12px;padding:8px 0",
            "Sem análises qualitativas para ", fmt_competencia(comp), ".",
            tags$br(),
            tags$a(style="color:#3271FE;cursor:pointer;font-size:11px;font-weight:700",
                   onclick="Shiny.setInputValue('nav_page','coleta',{priority:'event'})",
                   "Registrar analise"))
      else
        lapply(seq_len(nrow(objs)), function(i) {
          ob    <- objs[i,]
          q_obj <- qual[qual$id_objetivo == ob$id_objetivo, ]
          conf_avg <- if (nrow(q_obj) > 0)
            round(mean(q_obj$confianca, na.rm = TRUE), 1) else NA
          conf_lbl  <- if (!is.na(conf_avg)) confianca_label(round(conf_avg)) else "Sem dados"
          conf_col  <- if (!is.na(conf_avg)) confianca_color(round(conf_avg)) else "#E8EBF5"
          conf_fill <- if (!is.na(conf_avg)) round((conf_avg / 4) * 100) else 0
          ltr <- OBJ_LTRS[min(i, 5)]
          
          div(class = "conf-row",
              div(class = "conf-hdr",
                  div(class = "conf-lbl",
                      paste0(ltr, " | ", substr(ob$nome, 1, 28), "...")),
                  # Mostra rótulo de confiança, não percentual de atingimento
                  tags$span(
                    style = paste0("font-size:10px;font-weight:800;padding:2px 7px;",
                                   "border-radius:5px;background:", conf_col, "22;color:", conf_col),
                    conf_lbl)
              ),
              # Barra de confiança (escala 0-4 → 0-100%)
              div(class="pb-wrap",
                  div(class="pb-fill",
                      style=paste0("width:",conf_fill,"%;background:",conf_col,";")))
          )
        })
    )
  })
  
  # helper alert item
  make_alert <- function(row, tipo) {
    cls  <- switch(tipo, red="al-crit", yellow="al-warn", "al-info")
    dot  <- switch(tipo, red="red", yellow="yellow", "grey")
    ttl  <- switch(tipo, red="Crítico", yellow="Em Risco", "Sem coleta")
    pct_s<- if(!is.na(row$percentual_atingimento))
      paste0(row$percentual_atingimento,"% atingido") else "Sem dados"
    div(class=paste("al-item",cls),
        div(class=paste("al-dot",dot)),
        div(class="al-body",
            div(class="al-kr", paste0("KR",row$id_kr," | ",substr(row$nome_kr,1,36))),
            div(class=paste("al-title",tipo), ttl),
            div(class="al-desc",
                paste0("Meta: ",num_fmt(row$meta, unidade=row$unidade)," ",row$unidade," - ",pct_s,
                       if(!is.na(row$responsavel))paste0(" | ",row$responsavel) else ""))
        )
    )
  }
  
  # ══════════════════════════════════════════════════════
  # CAMADA 2 - POR OBJETIVO
  # ══════════════════════════════════════════════════════
  render_objetivos <- function() {
    objs <- get_objetivos()
    df   <- df_status()
    comp <- comp_sel()
    
    # Tabs de objetivos
    obj_id <- rv$obj_sel %||% objs$id_objetivo[1]
    
    tagList(
      # Breadcrumb de navegação
      div(class="drill-crumb",
          tags$span(class="drill-step",
                    onclick="Shiny.setInputValue('nav_page','geral',{priority:'event'})",
                    "Visão Geral"),
          tags$span(class="drill-sep",">"),
          tags$span(class="drill-step active","Por Objetivo")
      ),
      
      # Seletor de objetivo (op-tabs)
      div(style="display:flex;gap:6px;margin-bottom:16px;flex-wrap:wrap",
          lapply(seq_len(nrow(objs)), function(i) {
            obj <- objs[i,]
            cls <- paste("badge", if(obj$id_objetivo==obj_id) "badge-blue" else "")
            tags$button(
              class=cls,
              style=paste0("cursor:pointer;padding:5px 14px;border:none;border-radius:99px;",
                           "font-family:'Manrope',sans-serif;font-size:12px;font-weight:700;",
                           if(obj$id_objetivo==obj_id)
                             paste0("background:",OBJ_CORES[min(i,5)],";color:#fff;")
                           else "background:#f0f4f8;color:#6B7080;"),
              onclick=sprintf("Shiny.setInputValue('sel_obj_tab','%d',{priority:'event'})",
                              obj$id_objetivo),
              paste0(OBJ_LTRS[min(i,5)]," - ", substr(obj$nome,1,20), "...")
            )
          })
      ),
      
      # Conteúdo do objetivo selecionado
      uiOutput("obj_detalhe"),
      
      # Gráfico de evolução do objetivo
      div(class="sec","EVOLUÇÃO TEMPORAL - ATINGIMENTO MÉDIO DO OBJETIVO"),
      div(class="card",
          plotlyOutput("chart_objetivo", height="200px")
      )
    )
  }
  
  observeEvent(input$sel_obj_tab, {
    rv$obj_sel <- as.integer(input$sel_obj_tab)
  }, ignoreInit=TRUE)
  
  output$obj_detalhe <- renderUI({
    obj_id <- rv$obj_sel %||% get_objetivos()$id_objetivo[1]
    df     <- df_status()
    objs   <- get_objetivos()
    obj    <- objs[objs$id_objetivo==obj_id,]
    krs    <- df[df$id_objetivo==obj_id,]
    i      <- match(obj_id, objs$id_objetivo)
    col    <- OBJ_CORES[min(i,5)]
    
    if (nrow(krs) == 0)
      return(div(class="card",
                 div(class="sem-dados", style="padding:32px 0",
                     tags$div(style="font-size:24px;margin-bottom:8px", "OBJ"),
                     tags$p("Nenhum dado coletado para este objetivo nesta competência."),
                     tags$a(style="color:#3271FE;cursor:pointer;font-weight:700;font-size:12px",
                            onclick="Shiny.setInputValue('nav_page','coleta',{priority:'event'})",
                            "Coletar agora"))))
    
    div(class="card",
        div(class="card-hdr",
            div(class="card-ttl",
                style=paste0("border-left:4px solid ",col,";padding-left:12px"),
                obj$nome),
            tags$span(class="badge",obj$categoria)
        ),
        lapply(seq_len(nrow(krs)), function(j) {
          kr  <- krs[j,]
          pct <- kr$percentual_atingimento %||% 0
          if(is.na(pct)) pct <- 0
          st  <- kr$status %||% "nd"
          div(class=paste0("kr-row st-",st),
              onclick=sprintf("Shiny.setInputValue('click_kr','%d',{priority:'event'})",kr$id_kr),
              div(class="kr-row-top",
                  div(class="kr-nome", kr$nome_kr),
                  div(class="kr-right",
                      div(class="kr-meta-txt",
                          if (!is.na(kr$valor_realizado))
                            paste0(num_fmt(kr$valor_realizado, unidade=kr$unidade),
                                   " / ", num_fmt(kr$meta, unidade=kr$unidade),
                                   " ", kr$unidade)
                          else paste0("Meta: ", num_fmt(kr$meta, unidade=kr$unidade),
                                      " ", kr$unidade)),
                      div(class=paste0("kr-pct bg-",st), paste0(pct,"%"))
                  )
              ),
              prog_bar(pct, st),
              div(class="kr-bottom",
                  div(class="kr-data",
                      paste0("Competência: ", fmt_competencia(kr$competencia %||% "-"),
                             " | Resp.: ", kr$responsavel %||% kr$responsavel_padrao %||% "-")),
                  div(class=paste0("kr-pill pill-",st), toupper(st))
              )
          )
        })
    )
  })
  
  observeEvent(input$click_kr, {
    rv$kr_sel  <- as.integer(input$click_kr)
    rv$page    <- "krs"
    session$sendCustomMessage("setNav","krs")
  }, ignoreInit=TRUE)
  
  output$chart_objetivo <- renderPlotly({
    obj_id <- rv$obj_sel %||% get_objetivos()$id_objetivo[1]
    df     <- get_serie_objetivo(obj_id)
    if (nrow(df) == 0)
      return(plotly_empty() %>% layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        annotations = list(list(
          text = "Sem dados históricos - registre indicadores na tela Coletar Dados",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 12, color = "#A0A6B8")
        ))))
    df <- df[order(df$competencia), ]
    df$comp_lbl <- sapply(df$competencia, fmt_competencia)
    ordem <- df$comp_lbl
    objs  <- get_objetivos()
    i     <- match(obj_id, objs$id_objetivo)
    col   <- OBJ_CORES[min(i, 5)]
    
    n_obj <- nrow(df)
    df$prog_linear <- round(seq(0, 100, length.out = max(n_obj, 2))[seq_len(n_obj)], 1)
    
    plot_ly(df, x = ~comp_lbl) %>%
      # Progresso linear esperado
      add_lines(y = ~prog_linear, name = "Progresso Esperado",
                line = list(color = "rgba(160,166,184,.6)", width = 1.5, dash = "dash"),
                hovertemplate = "%{x}: %{y}% esperado<extra></extra>") %>%
      # Realizado
      add_trace(y = ~round(avg_pct, 1), name = "Atingimento Realizado",
                type = "scatter", mode = "lines+markers",
                line   = list(color = col, width = 2.5),
                marker = list(color = col, size = 7, line = list(color = "#fff", width = 2)),
                hovertemplate = "%{x}: %{y:.1f}% realizado<extra></extra>") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font  = list(family = "Manrope,sans-serif", size = 11, color = "#A0A6B8"),
             xaxis = list(title = "", showgrid = FALSE, showline = FALSE, zeroline = FALSE,
                          categoryorder = "array", categoryarray = ordem),
             yaxis = list(title = "", showgrid = TRUE, gridcolor = "rgba(0,0,0,.04)",
                          showline = FALSE, zeroline = FALSE, range = c(0, 110),
                          ticksuffix = "%"),
             showlegend = TRUE,
             legend = list(orientation = "h", x = 0, y = 1.2, font = list(size = 10)),
             hovermode = "x unified",
             margin = list(l = 28, r = 8, t = 28, b = 28)) %>%
      config(displayModeBar = FALSE)
  })
  
  # ══════════════════════════════════════════════════════
  # CAMADA 3 - KR DETALHADO
  # ══════════════════════════════════════════════════════
  render_krs <- function() {
    krs  <- get_krs()
    kr_choices <- setNames(krs$id_kr,
                           paste0("[", krs$letra, "] ", krs$nome))
    
    tagList(
      div(class="drill-crumb",
          tags$span(class="drill-step",
                    onclick="Shiny.setInputValue('nav_page','geral',{priority:'event'})",
                    "Visão Geral"),
          tags$span(class="drill-sep",">"),
          tags$span(class="drill-step",
                    onclick="Shiny.setInputValue('nav_page','objetivos',{priority:'event'})",
                    "Por Objetivo"),
          tags$span(class="drill-sep",">"),
          tags$span(class="drill-step active","KR Detalhado")
      ),
      
      # ── Seletor + tabs de modo de visualização ───────────
      div(style="display:flex;gap:12px;align-items:flex-end;margin-bottom:16px;flex-wrap:wrap",
          div(class="fg", style="max-width:440px;margin-bottom:0",
              tags$label(class="fg-lbl","Selecionar Key Result"),
              selectInput("sel_kr_det", NULL, choices=kr_choices,
                          selected=rv$kr_sel, width="100%")
          ),
          div(class="chart-tabs",
              tags$button(class="chart-tab active", id="tab_mensal",
                          onclick="Shiny.setInputValue('kr_view_tab','mensal',{priority:'event'})",
                          "Visão Mensal"),
              tags$button(class="chart-tab", id="tab_anual",
                          onclick="Shiny.setInputValue('kr_view_tab','anual',{priority:'event'})",
                          paste0("Consolidado ", ANO_REF))
          )
      ),
      
      # ── Painéis de info + gráfico ─────────────────────
      div(class="cgrid-eq",
          div(class="card",
              div(class="card-hdr",
                  uiOutput("chart_kr_titulo"),
                  tags$span(class="badge badge-blue", uiOutput("kr_tipo_badge", inline=TRUE))
              ),
              plotlyOutput("chart_kr_serie", height="260px")
          ),
          div(class="card",
              div(class="card-hdr",
                  div(class="card-ttl","Informações do KR")),
              uiOutput("kr_info_det")
          )
      ),
      
      div(class="sec","ANÁLISE QUALITATIVA - COMPETÊNCIA SELECIONADA"),
      uiOutput("qual_display")
    )
  }
  
  observeEvent(input$sel_kr_det, {
    rv$kr_sel <- as.integer(input$sel_kr_det)
  }, ignoreInit=TRUE)
  
  # Controla o tab de visão (mensal vs anual)
  rv$kr_view <- "mensal"
  observeEvent(input$kr_view_tab, {
    rv$kr_view <- input$kr_view_tab
    # Atualiza estilo visual dos tabs via JS
    session$sendCustomMessage("setKrTab", input$kr_view_tab)
  }, ignoreInit=TRUE)
  
  output$chart_kr_titulo <- renderUI({
    if (is.null(rv$kr_view) || rv$kr_view == "mensal")
      div(class="card-ttl", "Realizado por Competência vs Meta")
    else
      div(class="card-ttl", paste0("Consolidado Anual ", ANO_REF, " - Evolução Acumulada"))
  })
  
  output$kr_tipo_badge <- renderUI({
    kr_id <- rv$kr_sel %||% get_krs()$id_kr[1]
    krs   <- get_krs()
    kr    <- krs[krs$id_kr == kr_id, ]
    if (nrow(kr) == 0) return(NULL)
    tipo  <- kr$tipo_consolidacao %||% "media"
    cls   <- paste0("tipo-chip tipo-", tipo)
    tags$span(class=cls, paste0(tipo_icon(tipo), " ", tipo_label(tipo)))
  })
  
  output$chart_kr_serie <- renderPlotly({
    kr_id <- rv$kr_sel %||% get_krs()$id_kr[1]
    req(kr_id)
    view  <- rv$kr_view %||% "mensal"
    empty_plot <- function(msg)
      plotly_empty() %>% layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        annotations  = list(list(text = msg, x = 0.5, y = 0.5,
                                 xref = "paper", yref = "paper", showarrow = FALSE,
                                 font = list(size = 12, color = "#A0A6B8"))))
    
    # ── VISÃO ANUAL CONSOLIDADA ──────────────────────────────
    if (view == "anual") {
      da <- get_serie_kr_anual(kr_id, ano = ANO_REF)
      if (nrow(da) == 0)
        return(empty_plot(paste0("Sem dados de ", ANO_REF, " - registre na tela Coletar Dados")))
      
      da <- da[order(da$competencia), ]
      da$comp_lbl <- sapply(da$competencia, fmt_competencia)
      ordem_a  <- da$comp_lbl
      tipo     <- da$tipo_consolidacao[1]
      cor_tipo <- switch(tipo,"acumulado"="#3271FE","media"="#34C3D0",
                         "taxa"="#FF9E62","estoque"="#663B8E","#3271FE")
      unid    <- da$unidade[1]
      meta_an <- da$meta_kr[1]
      
      # Rótulo dinâmico conforme tipo
      nome_serie <- switch(tipo,
                           "acumulado" = "Acumulado no Ano",
                           "media"     = paste0("Média do Período (", ANO_REF, ")"),
                           "taxa"      = "Taxa Média do Período",
                           "estoque"   = "Patamar Atual",
                           "Consolidado")
      
      p <- plot_ly(da, x = ~comp_lbl) %>%
        # Trajetória da meta (correta por tipo)
        add_lines(y = ~meta_trajetoria, name = "Meta / Trajetória",
                  line = list(color = "rgba(160,166,184,.7)", width = 1.5, dash = "dash"),
                  hovertemplate = paste0("%{x}: meta %{y} ", unid, "<extra></extra>")) %>%
        # Valor consolidado/evolutivo
        add_trace(y = ~valor_anual, name = nome_serie,
                  type = "scatter", mode = "lines+markers",
                  line   = list(color = cor_tipo, width = 3),
                  marker = list(color = cor_tipo, size = 9,
                                line = list(color = "#fff", width = 2)),
                  fill = "tozeroy", fillcolor = paste0(cor_tipo, "15"),
                  hovertemplate = paste0("%{x}<br>", nome_serie, ": %{y} ",
                                         unid, "<extra></extra>")) %>%
        layout(
          paper_bgcolor = "transparent", plot_bgcolor = "transparent",
          font   = list(family = "Manrope,sans-serif", size = 11, color = "#A0A6B8"),
          xaxis  = list(title = "", showgrid = FALSE, showline = FALSE, zeroline = FALSE,
                        categoryorder = "array", categoryarray = ordem_a),
          yaxis  = list(title = unid, showgrid = TRUE, gridcolor = "rgba(0,0,0,.04)",
                        showline = FALSE, zeroline = FALSE),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0, y = 1.2, font = list(size = 10)),
          hovermode = "x unified",
          margin = list(l = 44, r = 10, t = 28, b = 28)) %>%
        config(displayModeBar = FALSE)
      
      return(p)
    }
    
    # ── VISÃO MENSAL ─────────────────────────────────────────
    df <- get_serie_kr(kr_id)
    comp_de  <- input$fil_comp_de  %||% ""
    comp_ate <- input$fil_comp_ate %||% ""
    if (nzchar(comp_de)  && nrow(df) > 0) df <- df[df$competencia >= comp_de, ]
    if (nzchar(comp_ate) && nrow(df) > 0) df <- df[df$competencia <= comp_ate, ]
    if (nrow(df) == 0)
      return(empty_plot("Sem dados para este KR - registre na tela Coletar Dados"))
    
    df <- df[order(df$competencia), ]
    df$comp_lbl <- sapply(df$competencia, fmt_competencia)
    df$cores    <- sapply(df$status, status_cor)
    ordem <- df$comp_lbl
    n_kr  <- nrow(df)
    # Progresso esperado mensal (0 → meta linearmente)
    df$prog_esp <- round(seq(0, df$meta_kr[1],
                             length.out = max(n_kr, 2))[seq_len(n_kr)], 2)
    
    # VISÃO MENSAL = BARRAS comparativas por competência
    cores_barras <- sapply(df$status, status_cor)
    plot_ly(df, x = ~comp_lbl) %>%
      # Linha de meta anual para referência
      add_lines(y = ~meta_kr, name = "Meta Anual",
                line = list(color = "rgba(50,113,254,.3)", width = 1.5, dash = "dot"),
                hovertemplate = paste0("Meta: %{y} ", df$unidade[1], "<extra></extra>")) %>%
      # Barras do realizado mensal
      add_bars(y = ~valor_realizado, name = "Realizado no Mês",
               marker = list(color = cores_barras,
                             line = list(color = "#fff", width = 1.5)),
               hovertemplate = paste0("<b>%{x}</b><br>Realizado: %{y} ",
                                      df$unidade[1], "<extra></extra>")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font     = list(family = "Manrope,sans-serif", size = 11, color = "#A0A6B8"),
             barmode  = "group",
             xaxis    = list(title = "", showgrid = FALSE, showline = FALSE, zeroline = FALSE,
                             categoryorder = "array", categoryarray = ordem),
             yaxis    = list(title = df$unidade[1], showgrid = TRUE,
                             gridcolor = "rgba(0,0,0,.04)", showline = FALSE, zeroline = FALSE),
             showlegend = TRUE,
             legend   = list(orientation = "h", x = 0, y = 1.2, font = list(size = 10)),
             hovermode = "x unified",
             margin   = list(l = 44, r = 10, t = 28, b = 28)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$kr_info_det <- renderUI({
    kr_id <- rv$kr_sel %||% get_krs()$id_kr[1]
    req(kr_id)
    krs  <- get_krs()
    kr   <- krs[krs$id_kr == kr_id, ]
    if (nrow(kr) == 0) return(NULL)
    
    df_mensal <- get_serie_kr(kr_id)
    ult <- if (nrow(df_mensal) > 0) df_mensal[nrow(df_mensal), ] else NULL
    
    # Consolidado anual
    da  <- get_serie_kr_anual(kr_id, ano = ANO_REF)
    tipo <- kr$tipo_consolidacao %||% "media"
    meta_an <- kr$meta
    
    valor_an  <- if (nrow(da) > 0) da$valor_anual[nrow(da)] else NA
    pct_an    <- if (!is.na(valor_an)) round(valor_an / meta_an * 100, 1) else NA
    st_an     <- if (!is.na(pct_an))
      ifelse(pct_an >= 80, "verde", ifelse(pct_an >= 60, "amarelo", "vermelho")) else "nd"
    n_meses   <- nrow(da)
    
    tagList(
      # ── Objetivo e descrição
      div(class="fg",
          div(class="fg-lbl","Objetivo"),
          div(style="font-size:13px;font-weight:600;color:#1B1B1C", kr$nome_objetivo)
      ),
      div(class="fg",
          div(class="fg-lbl","Descrição"),
          div(style="font-size:12px;color:#6B7080;line-height:1.5", kr$descricao %||% "-")
      ),
      
      # ── Meta + tipo de consolidação
      div(class="fg-2",
          div(
            div(class="fg-lbl","Meta Anual"),
            div(style="font-size:15px;font-weight:800;color:#3271FE",
                paste(num_fmt(kr$meta, unidade=kr$unidade), kr$unidade))
          ),
          div(
            div(class="fg-lbl","Consolidação"),
            tags$span(class=paste0("tipo-chip tipo-",tipo),
                      paste0(tipo_icon(tipo), " ", tipo_label(tipo)))
          )
      ),
      
      # ── Responsável e frequência
      div(class="fg-2",
          div(div(class="fg-lbl","Responsável"),
              div(style="font-size:12px;font-weight:600", kr$responsavel_padrao %||% "-")),
          div(div(class="fg-lbl","Frequência"),
              div(style="font-size:12px;font-weight:600", kr$frequencia))
      ),
      
      # ── Consolidado anual em destaque
      div(style="border-top:1px solid #E8EBF5;padding-top:10px;margin-top:2px",
          div(class="fg-lbl", paste0("POSIÇÃO CONSOLIDADA - ", ANO_REF,
                                     " (", n_meses, " meses)")),
          if (!is.na(valor_an)) {
            tagList(
              div(class=paste0("preview-box pv-",st_an), style="margin-top:6px",
                  div(style="display:flex;justify-content:space-between;align-items:baseline",
                      div(class="pv-pct", paste0(pct_an, "%")),
                      div(style="font-size:12px;color:#6B7080;font-weight:600",
                          paste(num_fmt(valor_an, unidade=kr$unidade), kr$unidade))
                  ),
                  div(class="anual-prog-wrap",
                      div(class="anual-prog-fill",
                          style=paste0("width:", min(pct_an,100), "%;",
                                       "background:", status_cor(st_an), ";"))
                  ),
                  div(class="pv-status", style="margin-top:6px",
                      tags$span(class=status_badge_cls(st_an), status_label(st_an)))
              )
            )
          } else {
            div(class="sem-dados", style="padding:8px 0",
                "Sem dados registrados em ", ANO_REF)
          }
      ),
      
      # ── Último mês registrado
      if (!is.null(ult) && !is.na(ult$percentual_atingimento)) {
        div(style="border-top:1px solid #E8EBF5;padding-top:10px;margin-top:8px",
            div(class="fg-lbl",
                paste0("ÚLTIMO MÊS - ", fmt_competencia(ult$competencia))),
            div(style="display:flex;gap:12px;margin-top:6px",
                div(
                  div(class="fg-lbl","Realizado"),
                  div(style="font-size:14px;font-weight:800;color:#1B1B1C",
                      paste(num_fmt(ult$valor_realizado, unidade=kr$unidade), kr$unidade))
                ),
                div(
                  div(class="fg-lbl","Atingimento"),
                  div(class=paste0("kval ",
                                   if(ult$percentual_atingimento>=80)"g"
                                   else if(ult$percentual_atingimento>=60)"y" else "r"),
                      style="font-size:16px",
                      paste0(ult$percentual_atingimento, "%"))
                )
            )
        )
      }
    )
  })
  
  output$qual_display <- renderUI({
    kr_id <- rv$kr_sel %||% get_krs()$id_kr[1]
    req(kr_id)
    comp <- comp_sel()
    qual <- get_qualitativo(id_kr=kr_id, competencia=comp)
    if(nrow(qual)==0)
      return(div(class="card",div(class="sem-dados",
                                  "Sem análise qualitativa para esta competência.",
                                  tags$br(),
                                  tags$a(style="color:#3271FE;cursor:pointer;font-weight:700",
                                         onclick="Shiny.setInputValue('nav_page','coleta',{priority:'event'})",
                                         "Preencher agora"))))
    q <- qual[1,]
    conf_col <- confianca_color(q$confianca)
    conf_lbl <- confianca_label(q$confianca)
    
    div(class="card",
        div(class="card-hdr",
            div(class="card-ttl","Análise - ",fmt_competencia(comp)),
            div(style="display:flex;align-items:center;gap:10px",
                tags$span(style="font-size:11px;color:#A0A6B8",q$responsavel %||% "-"),
                tags$span(style=paste0("font-size:11px;font-weight:700;padding:3px 10px;",
                                       "border-radius:99px;background:",conf_col,"22;color:",conf_col),
                          paste0("Confiança: ", conf_lbl))
            )
        ),
        div(class="cgrid-eq",
            div(
              div(class="qual-bloco",
                  tags$span(class="qual-bloco-lbl","Contexto do Resultado"),
                  div(class="qual-bloco-txt",q$contexto_resultado %||% "-")
              ),
              div(class="qual-bloco",
                  tags$span(class="qual-bloco-lbl","Ações em Andamento"),
                  div(class="qual-bloco-txt",q$acoes_andamento %||% "-")
              )
            ),
            div(class="qual-bloco",
                tags$span(class="qual-bloco-lbl","Resultados Esperados"),
                div(class="qual-bloco-txt",q$resultados_esperados %||% "-")
            )
        )
    )
  })
  
  # ── Cards consolidados anuais (Visão Geral) ────────────
  output$anual_krs_cards <- renderUI({
    da <- df_resumo_anual()
    if (nrow(da) == 0)
      return(div(class="sem-dados",
                 "Sem dados de ", ANO_REF, " - registre indicadores em Coletar Dados."))
    
    div(class="anual-grid",
        lapply(seq_len(nrow(da)), function(i) {
          r      <- da[i, ]
          tipo   <- r$tipo_consolidacao
          col    <- r$cor
          pct    <- r$pct_anual
          st     <- r$status_anual
          vlbl   <- paste0(num_fmt(r$valor_anual, unidade=r$unidade),
                           if (tipo == "acumulado")
                             paste0(" / ", num_fmt(r$meta_anual, unidade=r$unidade), " ", r$unidade)
                           else
                             paste0(" ", r$unidade))
          
          div(class="anual-card",
              div(class="anual-card-top",
                  div(class="anual-kr-nome", r$nome_kr),
                  div(class="anual-pct", style=paste0("color:",status_cor(st)), paste0(pct,"%"))
              ),
              div(class="anual-prog-wrap",
                  div(class="anual-prog-fill",
                      style=paste0("width:",min(pct,100),"%;background:",status_cor(st),";"  ))
              ),
              div(class="anual-vals",
                  div(class="anual-val-item",
                      div(class="anual-val-lbl", tipo_label(tipo)),
                      div(class="anual-val-num", vlbl)
                  ),
                  div(class="anual-val-item",
                      div(class="anual-val-lbl","Objetivo"),
                      div(style=paste0("font-size:11px;font-weight:700;color:",col),
                          paste0(r$letra, " | ", substr(r$nome_objetivo,1,22), "..."))
                  )
              ),
              div(class="anual-meses",
                  paste0(r$n_meses_registrados, " meses registrados | Últ: ",
                         fmt_competencia(r$ultimo_mes)),
                  " | ",
                  tags$span(class=status_badge_cls(st), status_label(st))
              )
          )
        })
    )
  })
  
  # ══════════════════════════════════════════════════════
  # CENTRAL DE ALERTAS
  # ══════════════════════════════════════════════════════
  render_alertas <- function() {
    df   <- df_status()
    comp <- comp_sel()
    comp_lbl <- fmt_competencia(comp %||% "")
    
    crit <- df[!is.na(df$status) & df$status=="vermelho",]
    warn <- df[!is.na(df$status) & df$status=="amarelo",]
    sem  <- df[is.na(df$status),]
    
    tagList(
      div(class="sec",paste0("ALERTAS - ",comp_lbl)),
      div(class="cgrid-eq",
          # Desempenho crítico
          div(class="card",
              div(class="card-hdr",
                  div(class="card-ttl","[!] Desempenho Crítico"),
                  tags$span(class="badge badge-red",paste(nrow(crit),"KRs"))
              ),
              if(nrow(crit)==0)
                div(class="sem-dados","Nenhum KR crítico nesta competência")
              else
                tagList(lapply(seq_len(nrow(crit)), function(i) make_alert(crit[i,],"red")))
          ),
          # Em risco
          div(class="card",
              div(class="card-hdr",
                  div(class="card-ttl","Em Risco"),
                  tags$span(class="badge badge-yellow",paste(nrow(warn),"KRs"))
              ),
              if(nrow(warn)==0)
                div(class="sem-dados","Nenhum KR em risco nesta competência")
              else
                tagList(lapply(seq_len(nrow(warn)), function(i) make_alert(warn[i,],"yellow")))
          )
      ),
      div(class="sec","AUSÊNCIA DE COLETA"),
      div(class="card",
          div(class="card-hdr",
              div(class="card-ttl","[ ] KRs sem dados na competência selecionada"),
              tags$span(class="badge",paste(nrow(sem),"pendentes"))
          ),
          if(nrow(sem)==0)
            div(class="sem-dados","Todos os KRs preenchidos para esta competência")
          else {
            lapply(seq_len(nrow(sem)), function(i) {
              r <- sem[i,]
              div(class="al-item al-info",
                  div(class="al-dot grey"),
                  div(class="al-body",
                      div(class="al-kr",paste0("KR",r$id_kr)),
                      div(class="al-title grey",r$nome_kr),
                      div(class="al-desc",
                          paste0("Meta: ",num_fmt(r$meta)," ",r$unidade,
                                 " | Resp.: ",r$responsavel_padrao %||% "-"))
                  )
              )
            })
          }
      )
    )
  }
  
  # ══════════════════════════════════════════════════════
  # TABELA GERENCIAL
  # ══════════════════════════════════════════════════════
  render_tabela <- function() {
    tagList(
      div(class="sec","TABELA GERENCIAL - EXPORTÁVEL"),
      div(class="export-bar",
          downloadButton("btn_export_csv",
                         label = tagList(tags$span("[CSV]"), " Exportar CSV"),
                         class = "btn-export"),
          tags$span(style="font-size:11px;color:#A0A6B8",
                    "Exporta todos os registros visíveis com os filtros aplicados")
      ),
      div(class="card",
          div(class="card-hdr",
              div(class="card-ttl","Consolidado por Competência | KR | Objetivo"),
              tags$span(class="badge badge-blue","2026 - todos os meses")
          ),
          DTOutput("dt_gerencial")
      )
    )
  }
  
  output$dt_gerencial <- renderDT({
    comp   <- if (!is.null(input$fil_comp)   && nzchar(input$fil_comp))    input$fil_comp   else NULL
    obj_id <- if (!is.null(input$fil_obj)    && input$fil_obj != "0")      input$fil_obj    else NULL
    st_fil <- if (!is.null(input$fil_status) && input$fil_status != "todos") input$fil_status else NULL
    
    df <- get_tabela_gerencial(comp, obj_id, st_fil)
    if (nrow(df) == 0)
      return(datatable(
        data.frame(
          Mensagem = "Nenhum dado registrado. Utilize a tela Coletar Dados para iniciar o preenchimento."
        ),
        rownames = FALSE,
        options  = list(dom = "t", language = list(emptyTable = "Sem registros"))
      ))
    
    df_out <- data.frame(
      Objetivo             = df$objetivo,
      KR                   = df$kr,
      Competencia          = sapply(df$competencia, fmt_competencia),
      Unidade              = df$unidade,
      Realizado            = mapply(num_fmt, df$valor_realizado, unidade=df$unidade),
      Meta                 = mapply(num_fmt, df$meta,            unidade=df$unidade),
      Atingimento          = paste0(round(df$percentual_atingimento, 1), "%"),
      Status               = toupper(ifelse(is.na(df$status), "SEM DADOS", df$status)),
      Confianca            = sapply(df$confianca,
                                    function(x) if (is.na(x)) "---" else confianca_label(x)),
      Responsavel          = ifelse(is.na(df$responsavel), "---", df$responsavel),
      Contexto             = ifelse(is.na(df$contexto_resultado),   "---", df$contexto_resultado),
      Acoes                = ifelse(is.na(df$acoes_andamento),      "---", df$acoes_andamento),
      Resultados_Esperados = ifelse(is.na(df$resultados_esperados), "---", df$resultados_esperados),
      Preenchido_em        = substr(ifelse(is.na(df$timestamp_preenchimento),
                                           "---", df$timestamp_preenchimento), 1, 16),
      stringsAsFactors = FALSE
    )
    
    datatable(df_out,
              options = list(
                pageLength = 15,
                scrollX    = TRUE,
                dom        = "frtip",
                language   = list(
                  search   = "Buscar:",
                  info     = "Mostrando _START_ a _END_ de _TOTAL_ registros",
                  paginate = list(previous = "Anterior", `next` = "Proximo")
                )
              ),
              rownames = FALSE,
              class    = "compact stripe hover"
    )
  }, server = FALSE)
  
  # ── Download CSV da tabela gerencial ────────────────────
  output$btn_export_csv <- downloadHandler(
    filename = function() {
      comp_ate <- comp_sel() %||% format(Sys.Date(), "%Y-%m")
      paste0("MIRAI_OKR_", ANO_REF, "_ate_", comp_ate, ".csv")
    },
    content = function(file) {
      comp   <- comp_sel() %||% NULL
      st_fil <- if (!is.null(input$fil_status) && input$fil_status != "todos")
        input$fil_status else NULL
      df <- get_tabela_gerencial(NULL, NULL, st_fil, ano = ANO_REF)
      
      if (nrow(df) == 0) {
        write.csv(data.frame(Mensagem = "Sem dados"), file, row.names = FALSE)
        return()
      }
      
      df_exp <- data.frame(
        Objetivo             = df$objetivo,
        KR                   = df$kr,
        Tipo_Consolidacao    = df$tipo_consolidacao,
        Competencia          = df$competencia,
        Unidade              = df$unidade,
        Valor_Realizado      = df$valor_realizado,
        Meta                 = df$meta,
        Pct_Atingimento      = round(df$percentual_atingimento, 1),
        Status               = toupper(ifelse(is.na(df$status), "SEM DADOS", df$status)),
        Confianca_Qualitativa= sapply(df$confianca,
                                      function(x) if(is.na(x)) "" else confianca_label(x)),
        Responsavel          = ifelse(is.na(df$responsavel), "", df$responsavel),
        Contexto_Resultado   = ifelse(is.na(df$contexto_resultado),   "", df$contexto_resultado),
        Acoes_Andamento      = ifelse(is.na(df$acoes_andamento),      "", df$acoes_andamento),
        Resultados_Esperados = ifelse(is.na(df$resultados_esperados), "", df$resultados_esperados),
        Preenchido_em        = substr(ifelse(is.na(df$timestamp_preenchimento),
                                             "", df$timestamp_preenchimento), 1, 16),
        stringsAsFactors = FALSE
      )
      # UTF-8 com BOM via writeChar (compativel com shinyapps.io)
      bom <- "﻿"
      out <- capture.output(write.csv(df_exp, stdout(), row.names = FALSE))
      writeLines(c(bom, out), con = file, useBytes = FALSE)
    }
  )
  
  # ══════════════════════════════════════════════════════
  # COLETAR DADOS
  # ══════════════════════════════════════════════════════
  render_coleta <- function() {
    objs <- get_objetivos()
    obj_ch <- setNames(objs$id_objetivo,
                       paste0(OBJ_LTRS[seq_len(nrow(objs))], " - ", substr(objs$nome,1,45),"..."))
    
    # Todos os 12 meses do ano estratégico (jan-dez 2026)
    # Padrão: mês atual se dentro do ano, senão último mês do ano
    comp_opts  <- get_competencias(ano = ANO_REF)
    hoje_comp  <- format(Sys.Date(), "%Y-%m")
    sel_comp   <- if (hoje_comp %in% comp_opts) hoje_comp else comp_opts[length(comp_opts)]
    comp_lbls  <- setNames(comp_opts, sapply(comp_opts, fmt_competencia))
    
    tagList(
      div(class="sec","REGISTRO DE DADOS"),
      div(class="form-grid",
          # ── Card quantitativo ──────────────────────────
          div(class="form-card",
              div(class="form-card-accent fa-blue"),
              div(class="form-card-body",
                  div(class="form-card-ttl","Dados Quantitativos"),
                  div(class="form-card-sub",
                      "Registre o valor realizado por competência. O sistema sobrescreve automaticamente registros do mesmo período."),
                  
                  div(class="fg-2",
                      div(class="fg",
                          tags$label(class="fg-lbl","Objetivo"),
                          selectInput("col_obj",NULL,choices=obj_ch,width="100%")
                      ),
                      div(class="fg",
                          tags$label(class="fg-lbl","Competência"),
                          selectInput("col_comp",NULL,choices=comp_lbls,
                                      selected=sel_comp,width="100%")
                      )
                  ),
                  div(class="fg",
                      tags$label(class="fg-lbl","Key Result"),
                      selectInput("col_kr",NULL,choices=c("Selecione um objetivo..."=""),width="100%")
                  ),
                  uiOutput("col_kr_info"),
                  div(class="fg-2",
                      div(class="fg",
                          tags$label(class="fg-lbl","Valor Realizado"),
                          # Input dinâmico: step e placeholder variam por unidade
                          uiOutput("col_valor_input"),
                          uiOutput("col_unidade_hint")
                      ),
                      div(class="fg",
                          tags$label(class="fg-lbl","Responsável"),
                          textInput("col_resp",NULL,placeholder="Nome do responsável",width="100%")
                      )
                  ),
                  uiOutput("col_preview"),
                  tags$button(class="btn-primary","Salvar Registro",
                              onclick="Shiny.setInputValue('btn_salvar_quant',Math.random())")
              )
          ),
          
          # ── Card qualitativo ───────────────────────────
          div(class="form-card",
              div(class="form-card-accent fa-dark"),
              div(class="form-card-body",
                  div(class="form-card-ttl","Analise Qualitativa"),
                  div(class="form-card-sub",
                      "Estruture a análise em três blocos para sustentar a leitura estratégica na RAE."),
                  
                  div(class="fg-2",
                      div(class="fg",
                          tags$label(class="fg-lbl","Key Result"),
                          selectInput("col_kr_q",NULL,choices=c("Selecione..."=""),width="100%")
                      ),
                      div(class="fg",
                          tags$label(class="fg-lbl","Responsável"),
                          textInput("col_resp_q",NULL,placeholder="Nome",width="100%")
                      )
                  ),
                  div(class="fg",
                      tags$label(class="fg-lbl","1. Contexto do Resultado"),
                      textAreaInput("col_ctx",NULL,rows=3,width="100%",
                                    placeholder="Qual contexto explica o resultado desta competência? O que aconteceu?"),
                      div(class="fg-hint","Descreva os fatores internos/externos que influenciaram o resultado.")
                  ),
                  div(class="fg",
                      tags$label(class="fg-lbl","2. Ações em Andamento ou Planejadas"),
                      textAreaInput("col_acoes",NULL,rows=3,width="100%",
                                    placeholder="Quais ações estão em andamento ou planejadas para reverter/manter o resultado?"),
                      div(class="fg-hint","Seja específico: quem, o quê, quando.")
                  ),
                  div(class="fg",
                      tags$label(class="fg-lbl","3. Resultados Esperados"),
                      textAreaInput("col_expect",NULL,rows=3,width="100%",
                                    placeholder="Qual resultado você espera a partir dessas ações no próximo ciclo?"),
                      div(class="fg-hint","Conecte a expectativa ao nível de confiança selecionado abaixo.")
                  ),
                  div(class="fg",
                      tags$label(class="fg-lbl","Nível de Confiança no Atingimento da Meta"),
                      uiOutput("col_confianca_ui"),
                      div(class="fg-hint","Evite o neutro: escolha entre Baixa ou Alta.")
                  ),
                  tags$button(class="btn-dark","Salvar Análise",
                              onclick="Shiny.setInputValue('btn_salvar_qual',Math.random())")
              )
          )
      ),
      uiOutput("toast_ui")
    )
  }
  
  # Atualiza KRs ao mudar objetivo
  observeEvent(input$col_obj, {
    req(input$col_obj)
    krs <- get_krs(as.integer(input$col_obj))
    ch  <- setNames(krs$id_kr, krs$nome)
    updateSelectInput(session,"col_kr",  choices=ch)
    updateSelectInput(session,"col_kr_q",choices=ch)
  }, ignoreInit=TRUE)
  
  # Input de valor com step e placeholder adaptados à unidade do KR
  output$col_valor_input <- renderUI({
    kr_id <- input$col_kr
    if (is.null(kr_id) || !nzchar(kr_id)) {
      return(numericInput("col_valor", NULL, value = NULL, min = 0, width = "100%"))
    }
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(kr_id), ]
    if (nrow(kr) == 0) {
      return(numericInput("col_valor", NULL, value = NULL, min = 0, width = "100%"))
    }
    unid <- kr$unidade
    
    if (unid == "R$") {
      # Monetário: 2 casas, step 0.01, placeholder "ex: 850000,00"
      tagList(
        numericInput("col_valor", NULL, value = NULL, min = 0,
                     step = 0.01, width = "100%"),
        tags$script(HTML(
          "$(document).ready(function() {
             var el = document.getElementById('col_valor');
             if (el) { el.setAttribute('step','0.01');
                       el.setAttribute('placeholder','ex: 850000,00'); }
           });"))
      )
    } else {
      # Demais unidades: inteiros ou 1 casa decimal
      step_val <- switch(unid,
                         "pontos" = 0.1, "%" = 0.1, "dias" = 0.1, 1)
      numericInput("col_valor", NULL, value = NULL, min = 0,
                   step = step_val, width = "100%")
    }
  })
  
  # Info box do KR
  # Hint de unidade de medida abaixo do campo Valor Realizado
  output$col_unidade_hint <- renderUI({
    req(input$col_kr)
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(input$col_kr), ]
    if (nrow(kr) == 0) return(NULL)
    unid <- kr$unidade
    
    # Instrução de preenchimento adaptada à unidade
    instrucao <- if (unid == "R$") {
      "Use ponto como separador decimal - ex: 850000.50 (o R Shiny lê ponto)"
    } else if (unid %in% c("pontos", "%", "dias")) {
      "Use ponto para decimais se necessário - ex: 78.5"
    } else {
      "Informe um valor inteiro."
    }
    
    meta_fmt <- num_fmt(kr$meta, unidade = unid)
    
    div(class = "fg-hint", style = "margin-top:4px;line-height:1.5",
        tags$span(style = "display:block",
                  tags$strong("Unidade: "), unid,
                  tags$span(style = "margin-left:10px;color:#1B1B1C",
                            tags$strong("Meta: "), meta_fmt, " ", unid)),
        tags$span(style = paste0("display:block;margin-top:3px;",
                                 if (unid == "R$") "color:#FF9E62;font-weight:700;" else "color:#A0A6B8;"),
                  "", instrucao)
    )
  })
  
  output$col_kr_info <- renderUI({
    req(input$col_kr)
    krs <- get_krs()
    kr  <- krs[krs$id_kr==as.integer(input$col_kr),]
    if(nrow(kr)==0) return(NULL)
    div(class="kr-info-box",
        div(class="ki-item",
            tags$span(class="ki-lbl","Objetivo:"),
            tags$span(class="ki-val",substr(kr$nome_objetivo,1,30))
        ),
        div(class="ki-item",
            tags$span(class="ki-lbl","Unidade:"),
            tags$span(class="ki-val",kr$unidade)
        ),
        div(class="ki-item",
            tags$span(class="ki-lbl","Meta:"),
            tags$span(class="ki-val",
                      paste(num_fmt(kr$meta, unidade = kr$unidade), kr$unidade))
        ),
        div(class="ki-item",
            tags$span(class="ki-lbl","Frequência:"),
            tags$span(class="ki-val",kr$frequencia)
        )
    )
  })
  
  # Preview atingimento
  output$col_preview <- renderUI({
    req(input$col_kr, input$col_valor)
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(input$col_kr), ]
    if (nrow(kr) == 0 || is.na(input$col_valor)) return(NULL)
    unid <- kr$unidade
    pct  <- round((input$col_valor / kr$meta) * 100, 1)
    st   <- if (pct >= 80) "verde" else if (pct >= 60) "amarelo" else "vermelho"
    lbl  <- switch(st, verde="No Alvo", amarelo="Em Risco", "Critico")
    v_fmt  <- num_fmt(input$col_valor, unidade = unid)
    m_fmt  <- num_fmt(kr$meta,         unidade = unid)
    div(class = paste0("preview-box pv-", st),
        div(class = "pv-pct", paste0(pct, "%")),
        div(style = "font-size:11px;color:#6B7080;margin:4px 0",
            paste0(v_fmt, " ", unid, " de ", m_fmt, " ", unid)),
        prog_bar(pct, st),
        div(class = "pv-status", lbl)
    )
  })
  
  # Confiança 4 pontos
  output$col_confianca_ui <- renderUI({
    opts <- list(
      list(v=1,icon="(1)",lbl="Muito Baixa",cls="sel-1"),
      list(v=2,icon="(2)",lbl="Baixa",      cls="sel-2"),
      list(v=3,icon="(3)",lbl="Alta",        cls="sel-3"),
      list(v=4,icon="(4)",lbl="Muito Alta",  cls="sel-4")
    )
    div(class="conf4-wrap",
        lapply(opts, function(o) {
          sel <- identical(as.integer(rv$confianca), as.integer(o$v))
          tags$button(
            class=paste("conf4-btn", if(sel) o$cls else ""),
            onclick=sprintf("Shiny.setInputValue('col_conf_sel','%d',{priority:'event'})",o$v),
            tags$span(class="conf4-icon",o$icon),
            tags$span(class="conf4-lbl",o$lbl)
          )
        })
    )
  })
  
  observeEvent(input$col_conf_sel, {
    rv$confianca <- as.integer(input$col_conf_sel)
  }, ignoreInit=TRUE)
  
  # Toast
  toast_msg <- reactiveVal(NULL)
  output$toast_ui <- renderUI({
    msg <- toast_msg()
    if(is.null(msg)) return(NULL)
    div(class=paste0("toast toast-",msg$cls),msg$txt)
  })
  observe({
    req(toast_msg())
    invalidateLater(4000,session)
    toast_msg(NULL)
  })
  
  # Salvar quantitativo
  observeEvent(input$btn_salvar_quant, {
    req(input$col_kr, input$col_valor, input$col_comp)
    krs <- get_krs()
    kr  <- krs[krs$id_kr==as.integer(input$col_kr),]
    tryCatch({
      res <- upsert_indicador(
        id_kr           = as.integer(input$col_kr),
        competencia     = input$col_comp,
        valor_realizado = input$col_valor,
        meta            = kr$meta,
        responsavel     = input$col_resp %||% ""
      )
      toast_msg(list(cls="ok",
                     txt=paste0("Salvo! Competência: ",fmt_competencia(input$col_comp),
                                " | Atingimento: ",res$pct,"% - ",toupper(res$status))))
      refresh(Sys.time())
    }, error=function(e)
      toast_msg(list(cls="err",txt=paste("Erro:",e$message)))
    )
  }, ignoreInit=TRUE)
  
  # Salvar qualitativo
  observeEvent(input$btn_salvar_qual, {
    req(input$col_kr_q, input$col_ctx, input$col_comp)
    tryCatch({
      upsert_qualitativo(
        id_kr               = as.integer(input$col_kr_q),
        competencia         = input$col_comp,
        responsavel         = input$col_resp_q %||% "",
        contexto_resultado  = input$col_ctx    %||% "",
        acoes_andamento     = input$col_acoes  %||% "",
        resultados_esperados= input$col_expect %||% "",
        confianca           = rv$confianca
      )
      toast_msg(list(cls="ok",txt="Analise qualitativa salva com sucesso!"))
      refresh(Sys.time())
    }, error=function(e)
      toast_msg(list(cls="err",txt=paste("Erro:",e$message)))
    )
  }, ignoreInit=TRUE)
  
  
  # ══════════════════════════════════════════════════════
  # GESTÃO DE DADOS - Excluir registros
  # ══════════════════════════════════════════════════════
  render_gestao <- function() {
    krs  <- get_krs()
    kr_del_ch <- c("Todos os KRs" = "0",
                   setNames(krs$id_kr, paste0("[", krs$letra, "] ", krs$nome)))
    comp_all  <- get_competencias(ano = ANO_REF)
    comp_lbls <- c("Todas as competências" = "0",
                   setNames(comp_all, sapply(comp_all, fmt_competencia)))
    
    tagList(
      div(class="sec", "GESTÃO DE DADOS - EXCLUSÃO DE REGISTROS"),
      div(class="card",
          div(class="card-hdr",
              div(class="card-ttl", "Atencao: a exclusão é permanente e irreversível"),
              tags$span(class="badge badge-red", "Ação destrutiva")
          ),
          div(style="font-size:12px;color:#6B7080;margin-bottom:16px;line-height:1.6",
              "Selecione o KR e a competência que deseja apagar. ",
              "O sistema remove o registro quantitativo ", tags$strong("e"), " a análise qualitativa associada."
          ),
          div(class="gestao-filtro",
              div(style="flex:1;min-width:200px",
                  tags$label(class="fg-lbl", "Filtrar por KR"),
                  selectInput("gestao_kr_fil", NULL, choices = kr_del_ch,
                              selected = "0", width = "100%")
              ),
              div(style="flex:0 0 200px",
                  tags$label(class="fg-lbl", "Filtrar por Competência"),
                  selectInput("gestao_comp_fil", NULL, choices = comp_lbls,
                              selected = "0", width = "100%")
              )
          ),
          uiOutput("gestao_lista")
      ),
      uiOutput("gestao_modal")
    )
  }
  
  output$gestao_lista <- renderUI({
    refresh()
    registros <- get_registros_para_exclusao(ano = ANO_REF)
    
    # Aplica filtros do painel
    kr_f    <- input$gestao_kr_fil   %||% "0"
    comp_f  <- input$gestao_comp_fil %||% "0"
    if (!is.null(kr_f)   && kr_f   != "0")
      registros <- registros[registros$id_kr == as.integer(kr_f), ]
    if (!is.null(comp_f) && comp_f != "0")
      registros <- registros[registros$competencia == comp_f, ]
    
    if (nrow(registros) == 0)
      return(div(class = "sem-dados",
                 "Nenhum registro encontrado para os filtros selecionados."))
    
    lapply(seq_len(nrow(registros)), function(i) {
      r     <- registros[i, ]
      pct   <- round(r$percentual_atingimento, 1)
      st    <- r$status %||% "nd"
      key   <- paste0(r$id_kr, "__", r$competencia)
      
      div(class = "gestao-row",
          div(class = "gestao-comp", fmt_competencia(r$competencia)),
          div(class = "gestao-info",
              div(class = "gestao-kr",
                  paste0("[", r$letra, "] ", r$nome_kr)),
              div(class = "gestao-meta",
                  paste0(r$nome_objetivo,
                         if (!is.na(r$responsavel) && nzchar(r$responsavel %||% ""))
                           paste0(" | ", r$responsavel) else "",
                         if (r$tem_qualitativo == 1) " | com analise" else ""))
          ),
          div(class = "gestao-val",
              div(style = paste0("color:", status_cor(st), ";font-size:13px;font-weight:800"),
                  paste0(pct, "%")),
              div(style = "font-size:10px;color:#A0A6B8",
                  paste0(num_fmt(r$valor_realizado, unidade=r$unidade),
                         " ", r$unidade))
          ),
          tags$button(class = "btn-del",
                      onclick = sprintf(
                        "Shiny.setInputValue('del_confirm_key','%s',{priority:'event'})", key),
                      "Excluir")
      )
    })
  })
  
  # Modal de confirmação
  output$gestao_modal <- renderUI({
    key <- input$del_confirm_key %||% ""
    if (!nzchar(key)) return(NULL)
    parts <- strsplit(key, "__")[[1]]
    if (length(parts) != 2) return(NULL)
    id_kr_m <- parts[1]; comp_m <- parts[2]
    
    krs <- get_krs()
    kr  <- krs[krs$id_kr == as.integer(id_kr_m), ]
    kr_nome <- if (nrow(kr) > 0) kr$nome[1] else paste0("KR ", id_kr_m)
    
    div(class = "modal-overlay",
        onclick = "Shiny.setInputValue('del_confirm_key','',{priority:'event'})",
        div(class = "modal-box",
            onclick = "event.stopPropagation()",
            div(class = "modal-title", "Confirmar exclusao"),
            div(class = "modal-desc",
                tags$strong("KR: "), kr_nome, tags$br(),
                tags$strong("Competência: "), fmt_competencia(comp_m), tags$br(), tags$br(),
                "Esta ação removerá ", tags$strong("permanentemente"), " o registro quantitativo ",
                "e a análise qualitativa associada. Não é possível desfazer."
            ),
            div(class = "modal-actions",
                tags$button(class = "btn-cancel",
                            onclick = "Shiny.setInputValue('del_confirm_key','',{priority:'event'})",
                            "Cancelar"),
                tags$button(class = "btn-confirm-del",
                            onclick = sprintf(
                              "Shiny.setInputValue('del_execute_key','%s',{priority:'event'})", key),
                            "Sim, excluir")
            )
        )
    )
  })
  
  # Executar exclusão após confirmação
  observeEvent(input$del_execute_key, {
    key <- input$del_execute_key %||% ""
    if (!nzchar(key)) return()
    parts <- strsplit(key, "__")[[1]]
    if (length(parts) != 2) return()
    
    tryCatch({
      delete_registro_completo(
        id_kr       = as.integer(parts[1]),
        competencia = parts[2]
      )
      # Limpa modal e atualiza
      session$sendCustomMessage("clearDelKey", list())
      refresh(Sys.time())
      toast_msg(list(cls = "ok",
                     txt = paste0("Registro de ", fmt_competencia(parts[2]),
                                  " excluído com sucesso.")))
    }, error = function(e) {
      toast_msg(list(cls = "err", txt = paste("Erro ao excluir:", e$message)))
    })
  }, ignoreInit = TRUE)
  
  
}
app <- shinyApp(ui, server)