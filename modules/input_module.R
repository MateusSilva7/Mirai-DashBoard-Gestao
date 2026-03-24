# ============================================================
# MIRAI OKR — input_module.R v2
# Design fiel ao HTML: cards com accent top 3px, border-radius 14px
# ============================================================

input_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "section-header", style = "margin-top:0",
      div(class = "section-title", "Registro de Dados"),
      div(style = "font-size:12px;color:var(--text-secondary)", "Atualize os KRs semanalmente")
    ),

    div(class = "input-grid",

      # ── Card: Dados Quantitativos ──────────────────────
      div(class = "input-card",
        div(class = "input-card-accent accent-blue"),
        div(class = "input-card-body",
          div(class = "input-card-title", "📊 Dados Quantitativos"),
          div(class = "input-card-desc", "Registre o valor realizado para o KR selecionado"),

          div(class = "form-group",
            tags$label("Objetivo", class = "form-label"),
            selectInput(ns("sel_objetivo"), NULL, choices = c("Carregando..." = ""), width = "100%")
          ),

          div(class = "form-group",
            tags$label("Key Result", class = "form-label"),
            selectInput(ns("sel_kr"), NULL, choices = c("Selecione um objetivo..." = ""), width = "100%")
          ),

          uiOutput(ns("kr_info")),

          div(class = "form-row-2",
            div(class = "form-group",
              tags$label("Data de Referência", class = "form-label"),
              dateInput(ns("data_ref"), NULL, value = Sys.Date(),
                        language = "pt-BR", format = "dd/mm/yyyy", width = "100%")
            ),
            div(class = "form-group",
              tags$label("Valor Realizado", class = "form-label"),
              numericInput(ns("valor"), NULL, value = NULL, min = 0, width = "100%")
            )
          ),

          uiOutput(ns("preview")),

          actionButton(ns("btn_quant"), "Salvar Registro Quantitativo",
                       class = "btn-primary-mirai")
        )
      ),

      # ── Card: Análise Qualitativa ──────────────────────
      div(class = "input-card",
        div(class = "input-card-accent accent-dark"),
        div(class = "input-card-body",
          div(class = "input-card-title", "💬 Análise Qualitativa"),
          div(class = "input-card-desc", "Diagnóstico, gargalos e próximas ações do período"),

          div(class = "form-row-2",
            div(class = "form-group",
              tags$label("Key Result", class = "form-label"),
              selectInput(ns("sel_kr_qual"), NULL,
                          choices = c("Selecione..." = ""), width = "100%")
            ),
            div(class = "form-group",
              tags$label("Responsável", class = "form-label"),
              textInput(ns("responsavel"), NULL,
                        placeholder = "Nome do responsável", width = "100%")
            )
          ),

          div(class = "form-group",
            tags$label("Diagnóstico do Período", class = "form-label"),
            textAreaInput(ns("diagnostico"), NULL,
                          placeholder = "Como foi o período? O que aconteceu de relevante?",
                          rows = 3, width = "100%")
          ),

          div(class = "form-group",
            tags$label("Gargalos Identificados", class = "form-label"),
            textAreaInput(ns("gargalos"), NULL,
                          placeholder = "O que está travando o avanço deste KR?",
                          rows = 2, width = "100%")
          ),

          div(class = "form-row-2",
            div(class = "form-group",
              tags$label("Ações Realizadas", class = "form-label"),
              textAreaInput(ns("acoes"), NULL,
                            placeholder = "O que foi executado?",
                            rows = 3, width = "100%")
            ),
            div(class = "form-group",
              tags$label("Próximas Ações", class = "form-label"),
              textAreaInput(ns("proximas"), NULL,
                            placeholder = "O que será feito até a próxima coleta?",
                            rows = 3, width = "100%")
            )
          ),

          div(class = "form-group",
            tags$label("Nível de Confiança (1-5)", class = "form-label"),
            div(class = "confidence-selector",
              radioButtons(ns("confianca"), NULL,
                choices = c("1 ☁️" = 1,"2 🌤️" = 2,"3 ⛅" = 3,"4 🌞" = 4,"5 🚀" = 5),
                selected = 3, inline = TRUE)
            )
          ),

          div(class = "form-group",
            tags$label("Data", class = "form-label"),
            dateInput(ns("data_qual"), NULL, value = Sys.Date(),
                      language = "pt-BR", format = "dd/mm/yyyy", width = "100%")
          ),

          actionButton(ns("btn_qual"), "Salvar Análise Qualitativa",
                       class = "btn-secondary-mirai")
        )
      )
    ),

    uiOutput(ns("feedback"))
  )
}

input_module_server <- function(id, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load objetivos
    observe({
      objs <- get_objetivos()
      ch   <- setNames(objs$id_objetivo,
                       paste0(objs$letra, " — ", substr(objs$nome, 1, 50), "..."))
      updateSelectInput(session, "sel_objetivo", choices = ch)
    })

    # Load KRs on objetivo change
    observeEvent(input$sel_objetivo, {
      req(input$sel_objetivo)
      krs <- get_krs(as.integer(input$sel_objetivo))
      ch  <- setNames(krs$id_kr, krs$nome)
      updateSelectInput(session, "sel_kr", choices = ch)
      updateSelectInput(session, "sel_kr_qual", choices = ch)
    })

    # KR info box
    output$kr_info <- renderUI({
      req(input$sel_kr)
      krs <- get_krs()
      kr  <- krs[krs$id_kr == as.integer(input$sel_kr), ]
      if (nrow(kr) == 0) return(NULL)
      div(class = "kr-info-box",
        div(class = "kr-info-item",
          tags$span(class = "kr-info-label", "Meta:"),
          tags$span(class = "kr-info-value",
                    paste(formatC(kr$meta, format="f", digits=0, big.mark="."), kr$unidade))
        ),
        div(class = "kr-info-item",
          tags$span(class = "kr-info-label", "Frequência:"),
          tags$span(class = "kr-info-value", kr$frequencia)
        )
      )
    })

    # Preview atingimento
    output$preview <- renderUI({
      req(input$sel_kr, input$valor)
      krs <- get_krs()
      kr  <- krs[krs$id_kr == as.integer(input$sel_kr), ]
      if (nrow(kr) == 0 || is.na(input$valor)) return(NULL)

      pct    <- round((input$valor / kr$meta) * 100, 1)
      status <- if (pct >= 80) "verde" else if (pct >= 60) "amarelo" else "vermelho"
      label  <- if (status == "verde") "✅ No Alvo" else if (status == "amarelo") "⚠️ Em Risco" else "🔴 Crítico"

      div(class = paste0("preview-box preview-", status),
        div(class = "preview-pct", paste0(pct, "%")),
        div(class = "preview-bar-bg",
          div(class = paste0("preview-bar bar-", status),
              style = paste0("width:", min(pct, 100), "%"))
        ),
        div(class = "preview-status", label)
      )
    })

    # Feedback
    fb <- reactiveVal(NULL)

    observeEvent(input$btn_quant, {
      req(input$sel_kr, input$valor, input$data_ref)
      krs <- get_krs()
      kr  <- krs[krs$id_kr == as.integer(input$sel_kr), ]
      tryCatch({
        res <- insert_indicador(as.integer(input$sel_kr), input$data_ref, input$valor, kr$meta)
        fb(list(type = "success",
                msg = paste0("✅ Registro salvo! Atingimento: ", res$pct, "% — ", toupper(res$status))))
        refresh_trigger(Sys.time())
      }, error = function(e) fb(list(type = "error", msg = paste("Erro:", e$message))))
    })

    observeEvent(input$btn_qual, {
      req(input$sel_kr_qual, input$responsavel, input$diagnostico)
      tryCatch({
        insert_qualitativo(
          id_kr          = as.integer(input$sel_kr_qual),
          data           = input$data_qual,
          responsavel    = input$responsavel,
          diagnostico    = input$diagnostico,
          gargalos       = input$gargalos %||% "",
          acoes_realizadas = input$acoes %||% "",
          proximas_acoes = input$proximas %||% "",
          confianca      = as.integer(input$confianca)
        )
        fb(list(type = "success", msg = "✅ Análise qualitativa salva com sucesso!"))
        refresh_trigger(Sys.time())
      }, error = function(e) fb(list(type = "error", msg = paste("Erro:", e$message))))
    })

    output$feedback <- renderUI({
      msg <- fb()
      if (is.null(msg)) return(NULL)
      div(class = paste0("feedback-toast toast-", msg$type), msg$msg)
    })

    # Clear toast
    observe({
      req(fb())
      invalidateLater(4000, session)
      fb(NULL)
    })
  })
}

`%||%` <- function(a, b) if (!is.null(a) && nchar(a) > 0) a else b
