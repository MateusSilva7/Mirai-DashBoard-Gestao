# ============================================================
# MIRAI OKR — dashboard_module.R v2
# Layout fiel ao HTML: KPI grid 4col, main-grid 2col,
# obj cards 2x2, chart-card, right-col com alertas/pendências/confiança
# ============================================================

# ── UI ────────────────────────────────────────────────────
dashboard_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # KPI GRID
    uiOutput(ns("kpi_grid")),

    # MAIN GRID (left col + right col)
    div(class = "main-grid",

      # ── LEFT COLUMN ──────────────────────────────────────
      div(
        # Section header
        div(class = "section-header",
          div(class = "section-title", "Objetivos Estratégicos"),
          tags$a(class = "see-all", "Ver todos →")
        ),

        # Objective cards
        uiOutput(ns("obj_grid")),

        # Chart card
        div(class = "chart-card",
          div(class = "chart-header",
            div(class = "chart-title", "Progresso por Objetivo — Percentual de Atingimento"),
            div(style = "display:flex;align-items:center;gap:10px",
              div(class = "legend-row",
                div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#3271FE"), "Realizado"
                ),
                div(class = "legend-item",
                  div(class = "legend-dot", style = "background:rgba(50,113,254,0.2)"), "Meta"
                )
              ),
              div(class = "period-tabs",
                tags$button(class = "period-tab", "6m"),
                tags$button(class = "period-tab active", "12m"),
                tags$button(class = "period-tab", "YTD")
              )
            )
          ),
          # Obj legend badges
          div(class = "legend-row", style = "margin-bottom:12px;gap:8px",
            tags$span(class = "obj-badge-wrap", style = "background:rgba(50,113,254,0.08);color:#3271FE;border-color:rgba(50,113,254,0.2)",
              tags$span(style = "width:8px;height:8px;background:#3271FE;border-radius:99px;display:inline-block"), "M"),
            tags$span(class = "obj-badge-wrap", style = "background:rgba(255,84,96,0.08);color:#FF5460;border-color:rgba(255,84,96,0.2)",
              tags$span(style = "width:8px;height:8px;background:#FF5460;border-radius:99px;display:inline-block"), "I"),
            tags$span(class = "obj-badge-wrap", style = "background:rgba(102,59,142,0.08);color:#663B8E;border-color:rgba(102,59,142,0.2)",
              tags$span(style = "width:8px;height:8px;background:#663B8E;border-radius:99px;display:inline-block"), "R"),
            tags$span(class = "obj-badge-wrap", style = "background:rgba(255,158,98,0.08);color:#FF9E62;border-color:rgba(255,158,98,0.2)",
              tags$span(style = "width:8px;height:8px;background:#FF9E62;border-radius:99px;display:inline-block"), "A"),
            tags$span(class = "obj-badge-wrap", style = "background:rgba(52,195,208,0.08);color:#34C3D0;border-color:rgba(52,195,208,0.2)",
              tags$span(style = "width:8px;height:8px;background:#34C3D0;border-radius:99px;display:inline-block"), "I2")
          ),
          plotly::plotlyOutput(ns("chart_progress"), height = "200px")
        )
      ),

      # ── RIGHT COLUMN ──────────────────────────────────────
      div(class = "right-col",

        # Alertas
        div(class = "panel",
          div(class = "panel-title",
            "Alertas Principais",
            uiOutput(ns("alert_chip"), inline = TRUE)
          ),
          uiOutput(ns("alertas_list"))
        ),

        # Pendências
        div(class = "panel",
          div(class = "panel-title",
            "Pendências de Coleta",
            tags$a(class = "see-all", style = "font-size:11px", "Ver todas →")
          ),
          uiOutput(ns("pendencias_list"))
        ),

        # Confiança
        div(class = "panel",
          div(class = "panel-title", "Confiança nas Metas Anuais"),
          uiOutput(ns("confianca_bars"))
        )
      )
    )
  )
}

# ── SERVER ────────────────────────────────────────────────
dashboard_module_server <- function(id, refresh_trigger) {
  moduleServer(id, function(input, output, session) {

    # Reactive data
    data_status <- reactive({
      refresh_trigger()
      get_ultimo_status()
    })

    data_indicadores <- reactive({
      refresh_trigger()
      get_indicadores()
    })

    # ── KPI GRID ──────────────────────────────────────────
    output$kpi_grid <- renderUI({
      df <- data_status()
      if (nrow(df) == 0) return(div(class = "loading-msg", "Carregando..."))

      total  <- nrow(df)
      verde  <- sum(df$status == "verde",    na.rm = TRUE)
      amarelo<- sum(df$status == "amarelo",  na.rm = TRUE)
      vermelho<-sum(df$status == "vermelho", na.rm = TRUE)
      sem    <- sum(is.na(df$status))
      avg    <- round(mean(df$percentual_atingimento, na.rm = TRUE), 1)

      div(class = "kpi-grid",
        # Card 1 — Percentual médio
        div(class = "kpi-card kpi-blue",
          div(class = "kpi-icon-wrap blue", "📊"),
          div(class = "kpi-label", "Percentual Médio"),
          div(class = "kpi-value col-blue", paste0(avg, "%")),
          div(class = "kpi-delta pos", "▲ Média dos KRs ativos")
        ),
        # Card 2 — No Alvo
        div(class = "kpi-card kpi-green",
          div(class = "kpi-icon-wrap green", "✓"),
          div(class = "kpi-label", "KRs no Alvo"),
          div(class = "kpi-value col-green", verde),
          div(class = "kpi-delta pos", paste0("▲ de ", total, " KRs totais"))
        ),
        # Card 3 — Em Risco
        div(class = "kpi-card kpi-orange",
          div(class = "kpi-icon-wrap orange", "⚠"),
          div(class = "kpi-label", "KRs em Risco"),
          div(class = "kpi-value col-yellow", amarelo),
          div(class = "kpi-delta warn", "Requerem atenção")
        ),
        # Card 4 — Resumo Geral
        div(class = "kpi-card kpi-grad",
          div(class = "kpi-label", "Resumo Geral"),
          div(class = "resumo-grid",
            div(class = "resumo-item",
              div(class = "resumo-num", style = "color:var(--red)", vermelho),
              div(class = "resumo-lbl", "Crítico")
            ),
            div(class = "resumo-item",
              div(class = "resumo-num", style = "color:var(--yellow)", amarelo),
              div(class = "resumo-lbl", "Revisão")
            ),
            div(class = "resumo-item",
              div(class = "resumo-num", style = "color:var(--green)", verde),
              div(class = "resumo-lbl", "No Alvo")
            ),
            div(class = "resumo-item",
              div(class = "resumo-num", style = "color:var(--text-muted)", sem),
              div(class = "resumo-lbl", "TBD")
            )
          )
        )
      )
    })

    # ── OBJ CARDS GRID ────────────────────────────────────
    output$obj_grid <- renderUI({
      df    <- data_status()
      objs  <- get_objetivos()
      if (nrow(df) == 0) return(NULL)

      # Colors and badge colors per objective
      obj_colors <- c("#3271FE", "#FF5460", "#663B8E", "#FF9E62", "#34C3D0")
      obj_labels <- c("M", "I", "R", "A", "I")

      div(class = "obj-grid",
        lapply(seq_len(nrow(objs)), function(i) {
          obj  <- objs[i, ]
          krs  <- df[df$id_objetivo == obj$id_objetivo, ]
          if (nrow(krs) == 0) return(NULL)

          avg  <- round(mean(krs$percentual_atingimento, na.rm = TRUE), 1)
          if (is.nan(avg)) avg <- 0
          verde_n   <- sum(krs$status == "verde",    na.rm = TRUE)
          amarelo_n <- sum(krs$status == "amarelo",  na.rm = TRUE)
          vermelho_n<- sum(krs$status == "vermelho", na.rm = TRUE)

          # Overall status
          status <- if (vermelho_n > 0) "vermelho" else if (amarelo_n > 0) "amarelo" else "verde"
          badge_class <- if (status == "verde") "green" else if (status == "amarelo") "yellow" else "red"
          badge_label <- if (status == "verde") "● No Alvo" else if (status == "amarelo") "◐ Em Risco" else "● Crítico"
          pct_color   <- if (status == "verde") "var(--green)" else if (status == "amarelo") "var(--yellow)" else "var(--red)"
          bar_color   <- if (status == "verde") "var(--green)" else if (status == "amarelo") "var(--yellow)" else "var(--red)"

          # KR summary text
          kr_summary <- paste0(nrow(krs), " KRs")

          # Full width for last card (O5)
          card_class <- if (i == 5) "obj-card full-width" else "obj-card"
          col  <- obj_colors[min(i, length(obj_colors))]
          ltr  <- obj_labels[min(i, length(obj_labels))]

          div(class = card_class,
            div(class = "obj-header",
              div(class = "obj-badge", style = paste0("background:", col), ltr),
              div(class = "obj-name", substr(obj$nome, 1, 55)),
              div(class = "obj-pct", style = paste0("color:", pct_color), paste0(avg, "%"))
            ),
            div(class = "progress-bar",
              div(class = "progress-fill",
                  style = paste0("width:", min(avg,100), "%;background:", bar_color))
            ),
            div(class = "obj-meta",
              div(class = "obj-krs", paste0(kr_summary, " · ", verde_n, " verde · ", amarelo_n, " amarelo")),
              div(class = paste("status-badge", badge_class), badge_label)
            )
          )
        })
      )
    })

    # ── PROGRESS CHART ────────────────────────────────────
    output$chart_progress <- plotly::renderPlotly({
      df <- data_indicadores()
      if (nrow(df) == 0) return(plotly::plotly_empty())

      # Aggregate avg pct per month across all KRs
      df$data <- as.Date(df$data)
      df$mes  <- format(df$data, "%b")
      df$mes_n<- as.integer(format(df$data, "%m"))

      agg <- df %>%
        group_by(data) %>%
        summarise(avg_pct = mean(percentual_atingimento, na.rm = TRUE)) %>%
        arrange(data)

      plotly::plot_ly() %>%
        plotly::add_lines(
          data = agg, x = ~data, y = ~avg_pct,
          name = "Realizado",
          line = list(color = "#3271FE", width = 2.5),
          fill = "tozeroy",
          fillcolor = "rgba(50,113,254,0.08)",
          hovertemplate = "%{x|%b %Y}: %{y:.1f}%<extra></extra>"
        ) %>%
        plotly::add_lines(
          x = c(min(agg$data), max(agg$data)),
          y = c(80, 80),
          name = "Meta",
          line = list(color = "rgba(50,113,254,0.25)", width = 1.5, dash = "dot"),
          showlegend = FALSE,
          hoverinfo = "none"
        ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font  = list(family = "Manrope, sans-serif", size = 11, color = "#A0A6B8"),
          xaxis = list(
            title = "", showgrid = FALSE,
            showline = FALSE, zeroline = FALSE,
            tickformat = "%b"
          ),
          yaxis = list(
            title = "", showgrid = TRUE,
            gridcolor = "rgba(0,0,0,0.04)",
            showline = FALSE, zeroline = FALSE,
            range = c(0, 110),
            ticksuffix = "%"
          ),
          showlegend = FALSE,
          hovermode  = "x unified",
          margin = list(l = 30, r = 10, t = 4, b = 30)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ── ALERTAS ───────────────────────────────────────────
    output$alert_chip <- renderUI({
      df        <- data_status()
      n_critico <- sum(df$status %in% c("vermelho","amarelo"), na.rm = TRUE)
      div(class = "chip", paste(n_critico, "ativos"))
    })

    output$alertas_list <- renderUI({
      df <- data_status()

      criticos <- df[!is.na(df$status) & df$status == "vermelho", ]
      atencao  <- df[!is.na(df$status) & df$status == "amarelo",  ]
      bons     <- head(df[!is.na(df$status) & df$status == "verde", ], 1)

      make_alert <- function(row, tipo) {
        cls  <- if (tipo == "red") "critical" else if (tipo == "yellow") "warn" else "ok"
        dot  <- if (tipo == "red") "red"      else if (tipo == "yellow") "yellow" else "green"
        ttl  <- if (tipo == "red") "Crítico"  else if (tipo == "yellow") "Em Risco" else "No Alvo"
        pct  <- if (!is.na(row$percentual_atingimento)) paste0(row$percentual_atingimento, "% atingido") else "Sem dados"

        div(class = paste("alert-item", cls),
          div(class = paste("alert-dot", dot)),
          div(class = "alert-content",
            div(class = "alert-kr", paste0("KR", row$id_kr, " · ", substr(row$nome_kr, 1, 35))),
            div(class = paste("alert-title-row", tipo), ttl),
            div(class = "alert-desc",
              paste0("Meta: ", formatC(row$meta, format="f", digits=0, big.mark="."),
                     " ", row$unidade, " — ", pct))
          )
        )
      }

      tagList(
        lapply(seq_len(min(nrow(criticos), 2)), function(i) make_alert(criticos[i,], "red")),
        lapply(seq_len(min(nrow(atencao),  2)), function(i) make_alert(atencao[i,],  "yellow")),
        lapply(seq_len(min(nrow(bons),     1)), function(i) make_alert(bons[i,],     "green"))
      )
    })

    # ── PENDÊNCIAS ────────────────────────────────────────
    output$pendencias_list <- renderUI({
      df <- data_status()
      krs_sem <- df[is.na(df$status), ]
      krs_vel  <- df[!is.na(df$status) & df$status != "verde", ]
      pendentes <- if (nrow(krs_sem) > 0) head(krs_sem, 3) else head(krs_vel, 3)

      if (nrow(pendentes) == 0) {
        return(div(style = "font-size:12px;color:var(--text-muted);padding:8px 0",
                   "Sem pendências identificadas."))
      }

      tagList(
        lapply(seq_len(nrow(pendentes)), function(i) {
          kr <- pendentes[i, ]
          div(class = "pending-item",
            div(class = "pending-kr", paste0("KR", kr$id_kr)),
            div(class = "pending-info",
              div(class = "pending-title", substr(kr$nome_kr, 1, 40)),
              div(class = "pending-sub",
                paste0("Meta: ", formatC(kr$meta, format="f", digits=0, big.mark="."),
                       " ", kr$unidade))
            ),
            div(class = "overdue-badge", "→")
          )
        })
      )
    })

    # ── CONFIANÇA BARS ────────────────────────────────────
    output$confianca_bars <- renderUI({
      df   <- data_status()
      objs <- get_objetivos()
      obj_colors <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
      obj_labels <- c("O1 · Impacto","O2 · Faturamento","O3 · Carteira","O4 · Pessoas","O5 · Assets")

      div(style = "display:flex;flex-direction:column;gap:9px",
        lapply(seq_len(min(nrow(objs), 5)), function(i) {
          obj  <- objs[i, ]
          krs  <- df[df$id_objetivo == obj$id_objetivo, ]
          avg  <- round(mean(krs$percentual_atingimento, na.rm = TRUE), 1)
          if (is.nan(avg)) avg <- 0

          col  <- if (avg >= 80) "var(--green)" else if (avg >= 60) "var(--mirai-blue)" else if (avg >= 40) "var(--yellow)" else "var(--red)"
          bar  <- obj_colors[min(i, length(obj_colors))]

          div(class = "conf-row",
            div(class = "conf-header",
              div(class = "conf-lbl", obj_labels[i]),
              div(class = "conf-val", style = paste0("color:", col), paste0(avg, "%"))
            ),
            div(class = "progress-bar",
              div(class = "progress-fill",
                  style = paste0("width:", min(avg,100), "%;background:", col))
            )
          )
        })
      )
    })

  })
}

# ── Detail page (série temporal + qualitativo) ────────────
detail_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "section-header",
      div(class = "section-title", "Análise por Key Result"),
      div(class = "inline-select",
        tags$label("KR:", class = "form-label"),
        selectInput(ns("sel_kr"), NULL, choices = c("Carregando..." = ""), width = "350px")
      )
    ),

    div(class = "obj-tab-row", uiOutput(ns("obj_tabs"))),
    uiOutput(ns("kr_breakdown")),

    div(class = "chart-card",
      div(class = "chart-header",
        div(class = "chart-title", "Meta vs Realizado"),
        div(class = "period-tabs",
          tags$button(class = "period-tab", "3m"),
          tags$button(class = "period-tab active", "6m"),
          tags$button(class = "period-tab", "12m")
        )
      ),
      plotly::plotlyOutput(ns("chart_kr"), height = "280px")
    ),

    uiOutput(ns("qual_card"))
  )
}

detail_module_server <- function(id, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_obj <- reactiveVal(1L)

    observe({
      krs <- get_krs()
      ch  <- setNames(krs$id_kr, paste0("[", krs$letra, "] ", krs$nome))
      updateSelectInput(session, "sel_kr", choices = ch)
    })

    # Obj tabs
    output$obj_tabs <- renderUI({
      objs <- get_objetivos()
      obj_colors <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
      obj_labels <- c("M","I","R","A","I")
      div(class = "obj-tab-row",
        lapply(seq_len(nrow(objs)), function(i) {
          obj <- objs[i, ]
          cls <- if (selected_obj() == obj$id_objetivo) "obj-tab-btn tab-active" else "obj-tab-btn"
          actionButton(ns(paste0("tab_", obj$id_objetivo)), obj_labels[i], class = cls)
        })
      )
    })

    observe({
      objs <- get_objetivos()
      lapply(objs$id_objetivo, function(oid) {
        observeEvent(input[[paste0("tab_", oid)]], { selected_obj(oid) }, ignoreInit = TRUE)
      })
    })

    # KR breakdown
    output$kr_breakdown <- renderUI({
      refresh_trigger()
      df   <- get_ultimo_status()
      objs <- get_objetivos()
      oid  <- selected_obj()
      krs  <- df[df$id_objetivo == oid, ]
      obj  <- objs[objs$id_objetivo == oid, ]
      obj_colors <- c("#3271FE","#FF5460","#663B8E","#FF9E62","#34C3D0")
      i    <- match(oid, objs$id_objetivo)
      col  <- obj_colors[min(i, length(obj_colors))]

      if (nrow(krs) == 0) return(div(class = "empty-state", "Selecione um objetivo acima."))

      div(class = "breakdown-panel",
        div(class = "breakdown-obj-header", style = paste0("border-left-color:", col),
          div(class = "breakdown-obj-title", obj$nome)
        ),
        div(class = "kr-list",
          lapply(seq_len(nrow(krs)), function(j) {
            kr   <- krs[j, ]
            pct  <- if (!is.na(kr$percentual_atingimento)) kr$percentual_atingimento else 0
            st   <- if (!is.na(kr$status)) kr$status else "cinza"
            item_cls  <- paste0("kr-row-item status-", st)
            badge_cls <- paste0("kr-pct-badge badge-", st)
            fill_cls  <- paste0("kr-row-fill fill-", st)
            pill_cls  <- paste0("kr-status-pill pill-", st)
            pill_lbl  <- toupper(st)

            div(class = item_cls,
              div(class = "kr-row-top",
                div(class = "kr-row-name", kr$nome_kr),
                div(class = "kr-row-right",
                  div(class = "kr-val",
                    if (!is.na(kr$valor_realizado))
                      paste0(formatC(kr$valor_realizado, format="f",digits=0,big.mark="."),
                             "/", formatC(kr$meta, format="f",digits=0,big.mark="."), kr$unidade)
                    else "—"
                  ),
                  div(class = badge_cls, paste0(pct, "%"))
                )
              ),
              div(class = "kr-row-bar",
                div(class = fill_cls, style = paste0("width:", min(pct,100), "%"))
              ),
              div(class = "kr-row-bottom",
                div(class = "kr-row-date", if (!is.na(kr$data)) paste("Atualizado:", kr$data) else "Sem registros"),
                div(class = pill_cls, pill_lbl)
              )
            )
          })
        )
      )
    })

    # Chart KR
    output$chart_kr <- plotly::renderPlotly({
      req(input$sel_kr)
      df  <- get_indicadores(as.integer(input$sel_kr))
      krs <- get_krs()
      kr  <- krs[krs$id_kr == as.integer(input$sel_kr), ]

      if (nrow(df) == 0) return(plotly::plotly_empty() %>%
        plotly::layout(title = list(text = "Sem dados para este KR", font = list(size = 13, color = "#A0A6B8"))))

      df$data <- as.Date(df$data)
      df <- df[order(df$data), ]

      plotly::plot_ly() %>%
        plotly::add_lines(data = df, x = ~data, y = ~meta,
          name = "Meta",
          line = list(color = "rgba(50,113,254,0.25)", width = 1.5, dash = "dot"),
          hoverinfo = "none"
        ) %>%
        plotly::add_lines(data = df, x = ~data, y = ~valor_realizado,
          name = "Realizado",
          line = list(color = "#3271FE", width = 2.5),
          fill = "tozeroy", fillcolor = "rgba(50,113,254,0.08)",
          hovertemplate = paste0("%{x|%d %b}: %{y} ", kr$unidade, "<extra></extra>")
        ) %>%
        plotly::add_markers(data = df, x = ~data, y = ~valor_realizado,
          marker = list(
            color = ifelse(df$status == "verde",    "#18B884",
                    ifelse(df$status == "amarelo",  "#F5A623", "#FF3B4E")),
            size = 8, line = list(color = "white", width = 2)
          ),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font   = list(family = "Manrope, sans-serif", size = 11, color = "#A0A6B8"),
          xaxis  = list(title = "", showgrid = FALSE, showline = FALSE, zeroline = FALSE),
          yaxis  = list(title = paste0(kr$unidade), showgrid = TRUE,
                        gridcolor = "rgba(0,0,0,0.04)", showline = FALSE, zeroline = FALSE),
          legend = list(orientation = "h", x = 0, y = 1.15, font = list(size = 11)),
          hovermode = "x unified",
          margin = list(l = 40, r = 10, t = 10, b = 40)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # Qualitative card
    output$qual_card <- renderUI({
      req(input$sel_kr)
      qual <- get_qualitativo(as.integer(input$sel_kr))
      if (nrow(qual) == 0) return(NULL)
      q    <- qual[1, ]
      stars <- paste(rep("★", q$confianca), collapse = "")

      div(class = "qual-card",
        div(class = "qual-header",
          tags$span(class = "qual-title", "💬 Análise mais recente"),
          tags$span(class = "qual-date",  q$data),
          tags$span(class = "qual-resp",  paste("por", q$responsavel)),
          tags$span(class = "conf-stars", stars)
        ),
        div(class = "qual-body",
          if (!is.na(q$diagnostico) && nchar(q$diagnostico) > 0)
            div(tags$span(class="qual-sec-label","Diagnóstico"), p(class="qual-text", q$diagnostico)),
          if (!is.na(q$gargalos) && nchar(q$gargalos) > 0)
            div(tags$span(class="qual-sec-label","Gargalos"), p(class="qual-text", q$gargalos)),
          div(class = "qual-row-2",
            if (!is.na(q$acoes_realizadas) && nchar(q$acoes_realizadas) > 0)
              div(tags$span(class="qual-sec-label","Ações Realizadas"), p(class="qual-text", q$acoes_realizadas)),
            if (!is.na(q$proximas_acoes) && nchar(q$proximas_acoes) > 0)
              div(tags$span(class="qual-sec-label","Próximas Ações"), p(class="qual-text", q$proximas_acoes))
          )
        )
      )
    })
  })
}
