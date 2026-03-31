# ============================================================
# MIRAI OKR — database.R  v2
# Melhorias:
#  - Competência (YYYY-MM) substitui data livre
#  - timestamp_preenchimento registra quando foi salvo
#  - Upsert automático: mesmo KR + mesma competência → sobrescreve
#  - Descrição do KR adicionada em dim_krs
#  - Confiança em escala 4 pontos (1=Muito Baixa … 4=Muito Alta)
#  - Responsável vinculado ao registro quantitativo
#  - Qualitativo estruturado em 3 campos orientados
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)

DB_PATH <- "data/mirai_okr.sqlite"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

get_con <- function() {
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  dbConnect(SQLite(), DB_PATH)
}

# ── Detecta schema v1 (sem coluna competencia) ─────────────
.schema_v1 <- function(con) {
  tabs <- dbListTables(con)
  if (!"fact_indicadores" %in% tabs) return(FALSE)
  !"competencia" %in% dbListFields(con, "fact_indicadores")
}

# ── Dropa tudo para recriar com schema v2 ──────────────────
.reset_db <- function(con) {
  message("[MIRAI OKR] Schema desatualizado — recriando banco com v2...")
  for (t in dbListTables(con))
    dbExecute(con, paste0("DROP TABLE IF EXISTS [", t, "]"))
}

# ============================================================
# INICIALIZAÇÃO
# ============================================================
init_database <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Migração automática: se banco v1 existe, recria do zero
  if (.schema_v1(con)) .reset_db(con)
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_objetivos (
      id_objetivo INTEGER PRIMARY KEY AUTOINCREMENT,
      nome        TEXT NOT NULL,
      descricao   TEXT,
      letra       TEXT,
      cor         TEXT DEFAULT '#3271FE',
      categoria   TEXT DEFAULT 'Resultado'
    )")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_krs (
      id_kr              INTEGER PRIMARY KEY AUTOINCREMENT,
      id_objetivo        INTEGER NOT NULL,
      nome               TEXT NOT NULL,
      descricao          TEXT,
      meta               REAL NOT NULL,
      unidade            TEXT DEFAULT '%',
      frequencia         TEXT DEFAULT 'Mensal',
      responsavel_padrao TEXT,
      FOREIGN KEY (id_objetivo) REFERENCES dim_objetivos(id_objetivo)
    )")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS fact_indicadores (
      id_registro            INTEGER PRIMARY KEY AUTOINCREMENT,
      id_kr                  INTEGER NOT NULL,
      competencia            TEXT NOT NULL,
      valor_realizado        REAL NOT NULL,
      meta                   REAL NOT NULL,
      percentual_atingimento REAL,
      status                 TEXT,
      responsavel            TEXT,
      timestamp_preenchimento TEXT,
      UNIQUE(id_kr, competencia),
      FOREIGN KEY (id_kr) REFERENCES dim_krs(id_kr)
    )")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS fact_qualitativo (
      id_registro             INTEGER PRIMARY KEY AUTOINCREMENT,
      id_kr                   INTEGER NOT NULL,
      competencia             TEXT NOT NULL,
      responsavel             TEXT,
      contexto_resultado      TEXT,
      acoes_andamento         TEXT,
      resultados_esperados    TEXT,
      confianca               INTEGER DEFAULT 3,
      timestamp_preenchimento TEXT,
      UNIQUE(id_kr, competencia),
      FOREIGN KEY (id_kr) REFERENCES dim_krs(id_kr)
    )")
  
  seed_data(con)
}

# ============================================================
# SEED
# ============================================================
seed_data <- function(con) {
  if (dbGetQuery(con, "SELECT COUNT(*) as n FROM dim_objetivos")$n > 0)
    return(invisible(NULL))
  
  obj_cores <- c("#3271FE", "#FF5460", "#663B8E", "#FF9E62", "#34C3D0")
  
  objetivos <- data.frame(
    nome = c(
      "Maximizar a capacidade de gerar grande impacto nos nossos Clientes",
      "Impulsionar o faturamento recorrente, sem nos endividar e nem deteriorar a margem",
      "Robustecer nossa carteira com novos Clientes na oferta de SqaaS",
      "Alavancar a experiência e a capacidade técnica dos talentos da MIRAI",
      "Incorporar assets que suportam as ofertas e a produtividade do time MIRAI"
    ),
    descricao = c(
      "Propósito — Geração de impacto real nos clientes através de SQaaS",
      "Resultado — Crescimento financeiro sustentável e saudável",
      "Resultado — Expansão da carteira de clientes SqaaS",
      "Fundações — Desenvolvimento e retenção de talentos",
      "Fundações — Construção de ativos estratégicos e produtividade"
    ),
    letra     = c("M", "I", "R", "A", "I"),
    cor       = obj_cores,
    categoria = c("Propósito", "Resultado", "Resultado", "Fundações", "Fundações"),
    stringsAsFactors = FALSE
  )
  dbAppendTable(con, "dim_objetivos", objetivos)
  
  krs <- data.frame(
    id_objetivo = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5),
    nome = c(
      "Retorno ao Cliente em até 12 meses (SQaaS)",
      "NPS nos Clientes",
      "Faturamento bruto anual com EBITDA de 40%",
      "PMR médio",
      "Novos clientes de SqaaS",
      "Renovação de contratos SqaaS",
      "e-NPS do time",
      "Turnover voluntário",
      "Solução agêntica suportando etapas do método",
      "Ofertas comerciais suportadas por soluções",
      "Índice de produtividade do time"
    ),
    descricao = c(
      "% de clientes SQaaS com retorno mensurável em até 12 meses de contrato",
      "Net Promoter Score medido junto à base de clientes ativos",
      "Receita bruta acumulada no ano com EBITDA mínimo de 40%",
      "Prazo médio de recebimento das faturas em dias corridos",
      "Qtd. de novos clientes fechados na oferta SqaaS no período",
      "% de contratos SqaaS renovados no vencimento",
      "Employee NPS — pesquisa interna de satisfação e engajamento",
      "Taxa de saída voluntária do time em relação ao headcount médio",
      "Número de soluções agênticas operacionais suportando o método MIRAI",
      "Qtd. de ofertas comerciais sustentadas por soluções proprietárias",
      "Índice composto de produtividade operacional do time MIRAI"
    ),
    meta = c(30, 80, 12000000, 45, 4, 100, 70, 10, 1, 3, 80),
    unidade = c("%", "pontos", "R$", "dias", "clientes", "%", "pontos", "%",
                "unidade", "unidades", "%"),
    frequencia = c("Trimestral", "Mensal", "Anual", "Mensal", "Trimestral",
                   "Semestral", "Trimestral", "Semestral", "Semestral", "Anual", "Mensal"),
    responsavel_padrao = c("CS Lead", "CS Lead", "CFO", "CFO",
                           "Head Comercial", "Head Comercial",
                           "People Lead", "People Lead",
                           "Head Produto", "Head Produto", "COO"),
    stringsAsFactors = FALSE
  )
  dbAppendTable(con, "dim_krs", krs)
  
  set.seed(42)
  hoje <- Sys.Date()
  comps <- format(seq(as.Date(format(hoje - 180, "%Y-%m-01")),
                      as.Date(format(hoje, "%Y-%m-01")),
                      by = "month"), "%Y-%m")
  
  registros <- lapply(1:11, function(kid) {
    meta_val <- krs$meta[kid]
    do.call(rbind, lapply(seq_along(comps), function(ci) {
      # Tendência ligeiramente crescente com ruído
      base  <- 0.50 + ci * 0.06
      valor <- round(meta_val * runif(1, max(0.3, base - 0.10), min(1.1, base + 0.15)), 2)
      pct   <- round((valor / meta_val) * 100, 1)
      st    <- ifelse(pct >= 80, "verde", ifelse(pct >= 60, "amarelo", "vermelho"))
      data.frame(
        id_kr                  = kid,
        competencia            = comps[ci],
        valor_realizado        = valor,
        meta                   = meta_val,
        percentual_atingimento = pct,
        status                 = st,
        responsavel            = krs$responsavel_padrao[kid],
        timestamp_preenchimento = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
    }))
  })
  
  dbAppendTable(con, "fact_indicadores", do.call(rbind, registros))
  
  qual_ex <- data.frame(
    id_kr = c(1, 2, 3, 5, 7),
    competencia = format(hoje, "%Y-%m"),
    responsavel = c("CS Lead","CS Lead","CFO","Head Comercial","People Lead"),
    contexto_resultado = c(
      "Clientes SQaaS ativos com retorno médio de 24%. Dois casos de sucesso documentados.",
      "NPS +6 pts vs período anterior. Detratores concentrados em tempo de resposta.",
      "Faturamento em linha com projeção. EBITDA em 38%, ligeiramente abaixo da meta.",
      "1 cliente fechado no trimestre de 4 previstos. Pipeline com 5 oportunidades qualificadas.",
      "e-NPS caiu 4 pts. Driver principal: sobrecarga de projetos simultâneos."
    ),
    acoes_andamento = c(
      "Programa de success stories em andamento para 3 clientes-âncora.",
      "SLA de resposta 4h para chamados prioritários em implantação.",
      "Revisão do mix de contratos para aumentar margem. Corte de custos em análise.",
      "Prospecção ativa via LinkedIn. 3 demos agendadas para o próximo mês.",
      "Revisão de alocação: máximo 2 projetos simultâneos por analista."
    ),
    resultados_esperados = c(
      "28% de retorno médio até fim do trimestre.",
      "NPS acima de 72 pts no próximo ciclo.",
      "EBITDA de 40% a partir do próximo mês.",
      "Fechar mais 2 clientes. Atingir 3/4 da meta trimestral.",
      "Recuperar e-NPS para 65+ no próximo trimestre."
    ),
    confianca = c(3L, 3L, 2L, 2L, 2L),
    timestamp_preenchimento = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  dbAppendTable(con, "fact_qualitativo", qual_ex)
}

# ============================================================
# QUERIES DE LEITURA
# ============================================================

get_objetivos <- function() {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, "SELECT * FROM dim_objetivos ORDER BY id_objetivo")
}

get_krs <- function(id_objetivo = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  cond <- if (!is.null(id_objetivo))
    sprintf("WHERE k.id_objetivo = %d", as.integer(id_objetivo)) else ""
  dbGetQuery(con, sprintf("
    SELECT k.*, o.nome AS nome_objetivo, o.letra, o.cor, o.categoria
    FROM dim_krs k
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    %s ORDER BY k.id_objetivo, k.id_kr", cond))
}

get_ultimo_status <- function(competencia = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  comp_cond <- if (!is.null(competencia))
    sprintf("AND f.competencia = '%s'", competencia)
  else
    "AND f.competencia = (SELECT MAX(f2.competencia) FROM fact_indicadores f2 WHERE f2.id_kr = k.id_kr)"
  dbGetQuery(con, sprintf("
    SELECT k.id_kr, k.nome AS nome_kr, k.meta, k.unidade, k.descricao,
           k.responsavel_padrao, k.id_objetivo,
           o.nome AS nome_objetivo, o.letra, o.cor, o.categoria,
           f.valor_realizado, f.percentual_atingimento, f.status,
           f.competencia, f.responsavel, f.timestamp_preenchimento
    FROM dim_krs k
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    LEFT JOIN fact_indicadores f ON f.id_kr = k.id_kr %s
    ORDER BY k.id_objetivo, k.id_kr", comp_cond))
}

get_serie_kr <- function(id_kr) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf("
    SELECT f.*, k.nome AS nome_kr, k.unidade, k.meta AS meta_kr,
           o.nome AS nome_objetivo, o.cor
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    WHERE f.id_kr = %d ORDER BY f.competencia ASC", as.integer(id_kr)))
}

get_serie_objetivo <- function(id_objetivo) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf("
    SELECT f.competencia,
           AVG(f.percentual_atingimento) AS avg_pct,
           SUM(CASE WHEN f.status='verde'    THEN 1 ELSE 0 END) AS n_verde,
           SUM(CASE WHEN f.status='amarelo'  THEN 1 ELSE 0 END) AS n_amarelo,
           SUM(CASE WHEN f.status='vermelho' THEN 1 ELSE 0 END) AS n_vermelho
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    WHERE k.id_objetivo = %d
    GROUP BY f.competencia ORDER BY f.competencia ASC", as.integer(id_objetivo)))
}

get_serie_consolidada <- function() {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, "
    SELECT f.competencia,
           AVG(f.percentual_atingimento) AS avg_pct,
           SUM(CASE WHEN f.status='verde'    THEN 1 ELSE 0 END) AS n_verde,
           SUM(CASE WHEN f.status='amarelo'  THEN 1 ELSE 0 END) AS n_amarelo,
           SUM(CASE WHEN f.status='vermelho' THEN 1 ELSE 0 END) AS n_vermelho
    FROM fact_indicadores f
    GROUP BY f.competencia ORDER BY f.competencia ASC")
}

get_competencias <- function() {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  res <- dbGetQuery(con,
                    "SELECT DISTINCT competencia FROM fact_indicadores ORDER BY competencia DESC")
  res$competencia
}

get_qualitativo <- function(id_kr = NULL, competencia = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  conds <- c()
  if (!is.null(id_kr))       conds <- c(conds, sprintf("q.id_kr = %d", as.integer(id_kr)))
  if (!is.null(competencia)) conds <- c(conds, sprintf("q.competencia = '%s'", competencia))
  where <- if (length(conds)) paste("WHERE", paste(conds, collapse = " AND ")) else ""
  dbGetQuery(con, sprintf("
    SELECT q.*, k.nome AS nome_kr, k.id_objetivo,
           o.nome AS nome_objetivo
    FROM fact_qualitativo q
    JOIN dim_krs k ON q.id_kr = k.id_kr
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    %s ORDER BY q.competencia DESC", where))
}

get_tabela_gerencial <- function(competencia = NULL, id_objetivo = NULL, status_fil = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  conds <- c()
  if (!is.null(competencia) && competencia != "")
    conds <- c(conds, sprintf("f.competencia = '%s'", competencia))
  if (!is.null(id_objetivo) && id_objetivo != "0")
    conds <- c(conds, sprintf("k.id_objetivo = %d", as.integer(id_objetivo)))
  if (!is.null(status_fil) && status_fil != "todos")
    conds <- c(conds, sprintf("f.status = '%s'", status_fil))
  where <- if (length(conds)) paste("WHERE", paste(conds, collapse = " AND ")) else ""
  dbGetQuery(con, sprintf("
    SELECT o.letra, o.nome AS objetivo, k.nome AS kr, k.unidade,
           f.competencia, f.valor_realizado, k.meta,
           f.percentual_atingimento, f.status,
           q.confianca, COALESCE(f.responsavel, k.responsavel_padrao) AS responsavel,
           q.contexto_resultado, q.acoes_andamento, q.resultados_esperados,
           f.timestamp_preenchimento
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    LEFT JOIN fact_qualitativo q ON q.id_kr = f.id_kr AND q.competencia = f.competencia
    %s
    ORDER BY o.id_objetivo, k.id_kr, f.competencia DESC", where))
}

# ============================================================
# ESCRITA — UPSERT
# ============================================================

upsert_indicador <- function(id_kr, competencia, valor_realizado, meta, responsavel = "") {
  pct    <- round((valor_realizado / meta) * 100, 1)
  status <- ifelse(pct >= 80, "verde", ifelse(pct >= 60, "amarelo", "vermelho"))
  ts     <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  resp   <- gsub("'", "''", responsavel %||% "")
  
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, sprintf("
    INSERT INTO fact_indicadores
      (id_kr, competencia, valor_realizado, meta, percentual_atingimento,
       status, responsavel, timestamp_preenchimento)
    VALUES (%d, '%s', %f, %f, %f, '%s', '%s', '%s')
    ON CONFLICT(id_kr, competencia) DO UPDATE SET
      valor_realizado        = excluded.valor_realizado,
      meta                   = excluded.meta,
      percentual_atingimento = excluded.percentual_atingimento,
      status                 = excluded.status,
      responsavel            = excluded.responsavel,
      timestamp_preenchimento = excluded.timestamp_preenchimento",
                         as.integer(id_kr), competencia, valor_realizado, meta, pct, status, resp, ts))
  
  list(pct = pct, status = status)
}

upsert_qualitativo <- function(id_kr, competencia, responsavel,
                               contexto_resultado, acoes_andamento,
                               resultados_esperados, confianca) {
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  esc <- function(x) gsub("'", "''", x %||% "")
  
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, sprintf("
    INSERT INTO fact_qualitativo
      (id_kr, competencia, responsavel, contexto_resultado, acoes_andamento,
       resultados_esperados, confianca, timestamp_preenchimento)
    VALUES (%d, '%s', '%s', '%s', '%s', '%s', %d, '%s')
    ON CONFLICT(id_kr, competencia) DO UPDATE SET
      responsavel          = excluded.responsavel,
      contexto_resultado   = excluded.contexto_resultado,
      acoes_andamento      = excluded.acoes_andamento,
      resultados_esperados = excluded.resultados_esperados,
      confianca            = excluded.confianca,
      timestamp_preenchimento = excluded.timestamp_preenchimento",
                         as.integer(id_kr), competencia, esc(responsavel),
                         esc(contexto_resultado), esc(acoes_andamento),
                         esc(resultados_esperados), as.integer(confianca), ts))
}

# ── Helpers de exibição ────────────────────────────────────
confianca_label <- function(n) {
  switch(as.character(n),
         "1" = "Muito Baixa", "2" = "Baixa",
         "3" = "Alta",        "4" = "Muito Alta", "—")
}

confianca_color <- function(n) {
  switch(as.character(n),
         "1" = "#FF3B4E", "2" = "#F5A623",
         "3" = "#3271FE", "4" = "#18B884", "#A0A6B8")
}

fmt_competencia <- function(comp) {
  tryCatch({
    format(as.Date(paste0(comp, "-01")), "%b/%Y")
  }, error = function(e) comp)
}

num_fmt <- function(x, digits = 0) {
  vapply(x, function(xi) {
    v <- suppressWarnings(as.numeric(xi))
    if (is.na(v)) return("—")
    formatC(v, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  }, character(1))
}