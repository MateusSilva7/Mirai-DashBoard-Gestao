# ============================================================
# MIRAI OKR — modules/database.R  v3
# Tipologia de indicadores + consolidação anual 2026
# ============================================================
#
# TIPOLOGIA DE INDICADORES (regra de consolidação):
#
#  "acumulado"  — soma dos meses no ano (ex: Faturamento, Novos
#                 clientes, Retorno ao cliente acumulado)
#                 → valor_anual = SUM(valores mensais)
#                 → meta anual comparada ao acumulado
#
#  "media"      — média dos meses registrados (ex: NPS, e-NPS,
#                 PMR — refletem estado pontual, não acumulam)
#                 → valor_anual = AVG(valores mensais)
#                 → cada mês pode ser comparado à meta
#
#  "taxa"       — taxa/percentual de período (ex: Turnover,
#                 Renovação — calculada sobre um universo)
#                 → valor_anual = último valor ou média ponderada
#                 → comparado à meta pontual
#
#  "estoque"    — estado de estoque no momento (ex: Solução
#                 agêntica, Ofertas suportadas — contagem de
#                 ativos existentes)
#                 → valor_anual = MAX(valor mais recente)
#                 → meta é o patamar a atingir
#
# ============================================================

library(DBI)
library(RSQLite)

DB_PATH <- "data/mirai_okr.sqlite"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

get_con <- function() {
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  dbConnect(SQLite(), DB_PATH)
}

# ── Detecção de schema legado ───────────────────────────────
.schema_precisa_migrar <- function(con) {
  tabs <- dbListTables(con)
  if (!"dim_krs" %in% tabs) return(FALSE)
  cols_krs  <- dbListFields(con, "dim_krs")
  cols_fact <- if ("fact_indicadores" %in% tabs) dbListFields(con, "fact_indicadores") else c()
  !("tipo_consolidacao" %in% cols_krs) || !("competencia" %in% cols_fact)
}

.reset_db <- function(con) {
  message("[MIRAI OKR] Schema desatualizado — recriando banco v3...")
  for (t in dbListTables(con))
    dbExecute(con, paste0("DROP TABLE IF EXISTS [", t, "]"))
}

# ============================================================
# INICIALIZAÇÃO
# ============================================================
init_database <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  if (.schema_precisa_migrar(con)) .reset_db(con)
  
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
      meta               REAL NOT NULL,       -- meta anual do indicador
      unidade            TEXT DEFAULT '%',
      frequencia         TEXT DEFAULT 'Mensal',
      responsavel_padrao TEXT,
      tipo_consolidacao  TEXT DEFAULT 'media', -- acumulado | media | taxa | estoque
      FOREIGN KEY (id_objetivo) REFERENCES dim_objetivos(id_objetivo)
    )")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS fact_indicadores (
      id_registro             INTEGER PRIMARY KEY AUTOINCREMENT,
      id_kr                   INTEGER NOT NULL,
      competencia             TEXT NOT NULL,   -- YYYY-MM
      valor_realizado         REAL NOT NULL,   -- valor bruto do mês
      meta                    REAL NOT NULL,   -- meta mensal de referência
      percentual_atingimento  REAL,            -- vs meta mensal
      status                  TEXT,            -- verde | amarelo | vermelho
      responsavel             TEXT,
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
  
  seed_dimensoes(con)
}

# ============================================================
# SEED — dimensões estratégicas (sem dados históricos)
# ============================================================
seed_dimensoes <- function(con) {
  if (dbGetQuery(con, "SELECT COUNT(*) AS n FROM dim_objetivos")$n > 0)
    return(invisible(NULL))
  
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
    cor       = c("#3271FE", "#FF5460", "#663B8E", "#FF9E62", "#34C3D0"),
    categoria = c("Propósito", "Resultado", "Resultado", "Fundações", "Fundações"),
    stringsAsFactors = FALSE
  )
  dbAppendTable(con, "dim_objetivos", objetivos)
  
  # ── Tipologia:
  #  acumulado → soma no ano (Faturamento, Novos clientes, Retorno ao cliente)
  #  media     → média dos meses (NPS, e-NPS, PMR, Renovação)
  #  taxa      → último valor registrado ou média (Turnover)
  #  estoque   → valor mais recente / patamar (Solução agêntica, Ofertas)
  
  krs <- data.frame(
    id_objetivo = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L),
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
      "Ofertas comerciais suportadas por soluções"
    ),
    descricao = c(
      "% acumulado de clientes SQaaS com retorno mensurável em até 12 meses",
      "Net Promoter Score medido junto à base de clientes ativos",
      "Receita bruta acumulada no ano com EBITDA mínimo de 40%",
      "Prazo médio de recebimento das faturas em dias corridos",
      "Qtd. acumulada de novos clientes SqaaS fechados no ano",
      "% de contratos SqaaS renovados no vencimento",
      "Employee NPS — pesquisa interna de satisfação e engajamento",
      "Taxa de saída voluntária do time em relação ao headcount médio",
      "Número de soluções agênticas operacionais suportando o método MIRAI",
      "Qtd. de ofertas comerciais sustentadas por soluções proprietárias"
    ),
    meta = c(
      30,         # Retorno: 30% ao final do ano
      80,         # NPS: manter >= 80 pontos
      12000000,   # Faturamento: R$ 12M acumulado no ano
      45,         # PMR: <= 45 dias (meta pontual)
      4,          # Novos clientes: 4 no ano (acumulado)
      100,        # Renovação: 100% (taxa por período)
      70,         # e-NPS: >= 70 pontos
      10,         # Turnover: <= 10% (taxa)
      1,          # Solução agêntica: 1 unidade operacional
      3           # Ofertas: 3 unidades
    ),
    unidade = c(
      "%", "pontos", "R$", "dias",
      "clientes", "%", "pontos", "%",
      "unidade", "unidades"
    ),
    frequencia = c(
      "Trimestral", "Mensal", "Mensal", "Mensal",
      "Trimestral", "Semestral", "Trimestral", "Semestral",
      "Semestral", "Anual"
    ),
    responsavel_padrao = c(
      "CS Lead", "CS Lead",
      "CFO", "CFO",
      "Head Comercial", "Head Comercial",
      "People Lead", "People Lead",
      "Head Produto", "Head Produto"
    ),
    tipo_consolidacao = c(
      "acumulado",  # Retorno ao cliente: % acumulada de clientes com retorno
      "media",      # NPS: média dos meses (estado da percepção)
      "acumulado",  # Faturamento: soma no ano
      "media",      # PMR: média dos meses (prazo médio recorrente)
      "acumulado",  # Novos clientes: contagem acumulada no ano
      "media",      # Renovação: média do percentual de renovação por período
      "media",      # e-NPS: média dos períodos
      "taxa",       # Turnover: último valor ou média (taxa semestral)
      "estoque",    # Solução agêntica: valor atual (patamar)
      "estoque"     # Ofertas: valor atual (patamar)
    ),
    stringsAsFactors = FALSE
  )
  dbAppendTable(con, "dim_krs", krs)
}

# ============================================================
# QUERIES — LEITURA
# ============================================================

get_objetivos <- function() {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, "SELECT * FROM dim_objetivos ORDER BY id_objetivo")
}

get_krs <- function(id_objetivo = NULL) {
  con  <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  cond <- if (!is.null(id_objetivo))
    sprintf("WHERE k.id_objetivo = %d", as.integer(id_objetivo)) else ""
  dbGetQuery(con, sprintf("
    SELECT k.*, o.nome AS nome_objetivo, o.letra, o.cor, o.categoria
    FROM dim_krs k
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    %s ORDER BY k.id_objetivo, k.id_kr", cond))
}

# Status do mês (ou mais recente) por KR
get_ultimo_status <- function(competencia = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  comp_cond <- if (!is.null(competencia))
    sprintf("AND f.competencia = '%s'", competencia)
  else
    "AND f.competencia = (SELECT MAX(f2.competencia) FROM fact_indicadores f2 WHERE f2.id_kr = k.id_kr)"
  dbGetQuery(con, sprintf("
    SELECT k.id_kr, k.nome AS nome_kr, k.meta, k.unidade, k.descricao,
           k.responsavel_padrao, k.tipo_consolidacao, k.id_objetivo,
           o.nome AS nome_objetivo, o.letra, o.cor, o.categoria,
           f.valor_realizado, f.percentual_atingimento, f.status,
           f.competencia, f.responsavel, f.timestamp_preenchimento
    FROM dim_krs k
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    LEFT JOIN fact_indicadores f ON f.id_kr = k.id_kr %s
    ORDER BY k.id_objetivo, k.id_kr", comp_cond))
}

# Série mensal bruta de um KR (todos os meses registrados)
get_serie_kr <- function(id_kr) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf("
    SELECT f.competencia, f.valor_realizado, f.meta AS meta_mensal,
           f.percentual_atingimento, f.status,
           k.nome AS nome_kr, k.unidade, k.meta AS meta_kr,
           k.tipo_consolidacao, o.nome AS nome_objetivo, o.cor
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    WHERE f.id_kr = %d
    ORDER BY f.competencia ASC", as.integer(id_kr)))
}

# Série com valor consolidado anual calculado por tipo
# Retorna mês a mês + coluna valor_anual (acumulado/media/taxa/estoque)
get_serie_kr_anual <- function(id_kr, ano = 2026) {
  df <- get_serie_kr(id_kr)
  if (nrow(df) == 0) return(df)
  
  # Filtra o ano de interesse
  df <- df[substr(df$competencia, 1, 4) == as.character(ano), ]
  if (nrow(df) == 0) return(df)
  
  tipo <- df$tipo_consolidacao[1]
  meta_anual <- df$meta_kr[1]
  
  # Calcula valor consolidado conforme tipologia
  df$valor_anual <- switch(tipo,
                           "acumulado" = cumsum(df$valor_realizado),
                           "media"     = {
                             vapply(seq_len(nrow(df)), function(i)
                               mean(df$valor_realizado[1:i]), numeric(1))
                           },
                           "taxa"      = {
                             # Média móvel simples (taxa recorrente)
                             vapply(seq_len(nrow(df)), function(i)
                               mean(df$valor_realizado[1:i]), numeric(1))
                           },
                           "estoque"   = {
                             # Patamar máximo atingido (estoque cresce ou se mantém)
                             cummax(df$valor_realizado)
                           },
                           df$valor_realizado  # fallback: valor bruto
  )
  
  # % de atingimento anual (vs meta anual)
  df$pct_anual <- switch(tipo,
                         "acumulado" = round(df$valor_anual / meta_anual * 100, 1),
                         "media"     = round(df$valor_anual / meta_anual * 100, 1),
                         "taxa"      = round(df$valor_anual / meta_anual * 100, 1),
                         "estoque"   = round(df$valor_anual / meta_anual * 100, 1),
                         round(df$valor_anual / meta_anual * 100, 1)
  )
  
  # Trajetória esperada da meta no ano (linear ou distribuída)
  # Para acumulado: distribui a meta anual igualmente por 12 meses
  # Para media/taxa/estoque: a meta já é o alvo pontual
  n_meses <- nrow(df)
  df$meta_trajetoria <- switch(tipo,
                               "acumulado" = round(seq(meta_anual / 12, meta_anual,
                                                       length.out = 12)[seq_len(n_meses)], 2),
                               # Para os demais: meta horizontal (alvo constante)
                               rep(meta_anual, n_meses)
  )
  
  df
}

# Série por objetivo (percentual médio mensal + consolidado anual)
get_serie_objetivo <- function(id_objetivo, ano = 2026) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf("
    SELECT f.competencia,
           AVG(f.percentual_atingimento)                          AS avg_pct,
           SUM(CASE WHEN f.status='verde'    THEN 1 ELSE 0 END)  AS n_verde,
           SUM(CASE WHEN f.status='amarelo'  THEN 1 ELSE 0 END)  AS n_amarelo,
           SUM(CASE WHEN f.status='vermelho' THEN 1 ELSE 0 END)  AS n_vermelho
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    WHERE k.id_objetivo = %d
      AND substr(f.competencia, 1, 4) = '%s'
    GROUP BY f.competencia
    ORDER BY f.competencia ASC",
                          as.integer(id_objetivo), as.character(ano)))
}

# Série consolidada de todos os KRs (percentual médio)
get_serie_consolidada <- function(ano = 2026) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf("
    SELECT f.competencia,
           AVG(f.percentual_atingimento)                          AS avg_pct,
           SUM(CASE WHEN f.status='verde'    THEN 1 ELSE 0 END)  AS n_verde,
           SUM(CASE WHEN f.status='amarelo'  THEN 1 ELSE 0 END)  AS n_amarelo,
           SUM(CASE WHEN f.status='vermelho' THEN 1 ELSE 0 END)  AS n_vermelho
    FROM fact_indicadores f
    WHERE substr(f.competencia, 1, 4) = '%s'
    GROUP BY f.competencia
    ORDER BY f.competencia ASC", as.character(ano)))
}

# Resumo anual consolidado por KR (usado nos cards de visão geral)
# ate_comp: filtrar consolidado ATÉ aquele mês (ex: "2026-03" = jan+fev+mar)
get_resumo_anual_krs <- function(ano = 2026, ate_comp = NULL) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  comp_cond <- if (!is.null(ate_comp) && nzchar(ate_comp))
    sprintf("AND f.competencia <= '%s'", ate_comp) else ""
  raw <- dbGetQuery(con, sprintf("
    SELECT f.id_kr, f.competencia, f.valor_realizado,
           k.meta AS meta_kr, k.tipo_consolidacao, k.nome AS nome_kr,
           k.unidade, k.id_objetivo,
           o.nome AS nome_objetivo, o.letra, o.cor, o.categoria,
           k.responsavel_padrao
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    WHERE substr(f.competencia, 1, 4) = '%s' %s
    ORDER BY f.id_kr, f.competencia ASC", as.character(ano), comp_cond))
  
  if (nrow(raw) == 0) return(raw)
  
  do.call(rbind, lapply(unique(raw$id_kr), function(kid) {
    d     <- raw[raw$id_kr == kid, ]
    tipo  <- d$tipo_consolidacao[1]
    meta  <- d$meta_kr[1]
    
    valor_consol <- switch(tipo,
                           "acumulado" = sum(d$valor_realizado),
                           "media"     = mean(d$valor_realizado),
                           "taxa"      = mean(d$valor_realizado),
                           "estoque"   = max(d$valor_realizado),
                           mean(d$valor_realizado)
    )
    
    pct <- round(valor_consol / meta * 100, 1)
    st  <- ifelse(pct >= 80, "verde", ifelse(pct >= 60, "amarelo", "vermelho"))
    
    data.frame(
      id_kr              = kid,
      nome_kr            = d$nome_kr[1],
      unidade            = d$unidade[1],
      tipo_consolidacao  = tipo,
      meta_anual         = meta,
      valor_anual        = round(valor_consol, 2),
      pct_anual          = pct,
      status_anual       = st,
      n_meses_registrados= nrow(d),
      ultimo_mes         = max(d$competencia),
      id_objetivo        = d$id_objetivo[1],
      nome_objetivo      = d$nome_objetivo[1],
      letra              = d$letra[1],
      cor                = d$cor[1],
      categoria          = d$categoria[1],
      responsavel_padrao = d$responsavel_padrao[1],
      stringsAsFactors   = FALSE
    )
  }))
}

# Consolidado de % atingimento por objetivo até ate_comp
# Usa get_resumo_anual_krs internamente, agrupa por objetivo
get_consolidado_objetivos <- function(ano = 2026, ate_comp = NULL) {
  da <- get_resumo_anual_krs(ano = ano, ate_comp = ate_comp)
  if (nrow(da) == 0) return(da)
  do.call(rbind, lapply(unique(da$id_objetivo), function(oid) {
    d   <- da[da$id_objetivo == oid, ]
    avg <- round(mean(d$pct_anual, na.rm = TRUE), 1)
    if (is.nan(avg)) avg <- 0
    v_n <- sum(d$status_anual == "verde",    na.rm = TRUE)
    a_n <- sum(d$status_anual == "amarelo",  na.rm = TRUE)
    r_n <- sum(d$status_anual == "vermelho", na.rm = TRUE)
    st  <- if (r_n > 0) "vermelho" else if (a_n > 0) "amarelo" else "verde"
    data.frame(
      id_objetivo  = oid,
      nome_objetivo= d$nome_objetivo[1],
      letra        = d$letra[1],
      cor          = d$cor[1],
      categoria    = d$categoria[1],
      avg_pct      = avg,
      n_verde      = v_n,
      n_amarelo    = a_n,
      n_vermelho   = r_n,
      n_krs        = nrow(d),
      status       = st,
      stringsAsFactors = FALSE
    )
  }))
}

# Competências disponíveis
# Retorna todos os 12 meses do ano de referência (jan→dez)
# independente de haver dados — garante que os filtros sempre
# mostram o calendário completo do ano estratégico
get_competencias <- function(ano = 2026) {
  sprintf("%d-%02d", ano, 1:12)
}

# Alias mantido para compatibilidade
get_competencias_ano <- function(ano = 2026) {
  get_competencias(ano)
}

# Qualitativo
get_qualitativo <- function(id_kr = NULL, competencia = NULL) {
  con   <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
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

# Tabela gerencial exportável
get_tabela_gerencial <- function(competencia = NULL, id_objetivo = NULL,
                                 status_fil = NULL, ano = 2026) {
  con   <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  conds <- c(sprintf("substr(f.competencia,1,4) = '%s'", as.character(ano)))
  if (!is.null(competencia) && nzchar(competencia))
    conds <- c(conds, sprintf("f.competencia = '%s'", competencia))
  if (!is.null(id_objetivo) && id_objetivo != "0")
    conds <- c(conds, sprintf("k.id_objetivo = %d", as.integer(id_objetivo)))
  if (!is.null(status_fil) && status_fil != "todos")
    conds <- c(conds, sprintf("f.status = '%s'", status_fil))
  where <- paste("WHERE", paste(conds, collapse = " AND "))
  
  dbGetQuery(con, sprintf("
    SELECT o.letra, o.nome AS objetivo, k.nome AS kr, k.unidade,
           k.tipo_consolidacao,
           f.competencia, f.valor_realizado, k.meta,
           f.percentual_atingimento, f.status,
           q.confianca, COALESCE(f.responsavel, k.responsavel_padrao) AS responsavel,
           q.contexto_resultado, q.acoes_andamento, q.resultados_esperados,
           f.timestamp_preenchimento
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    LEFT JOIN fact_qualitativo q
      ON q.id_kr = f.id_kr AND q.competencia = f.competencia
    %s
    ORDER BY o.id_objetivo, k.id_kr, f.competencia ASC", where))
}

# ============================================================
# ESCRITA — UPSERT
# ============================================================

upsert_indicador <- function(id_kr, competencia, valor_realizado,
                             meta, responsavel = "") {
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
      valor_realizado         = excluded.valor_realizado,
      meta                    = excluded.meta,
      percentual_atingimento  = excluded.percentual_atingimento,
      status                  = excluded.status,
      responsavel             = excluded.responsavel,
      timestamp_preenchimento = excluded.timestamp_preenchimento",
                         as.integer(id_kr), competencia,
                         valor_realizado, meta, pct, status, resp, ts))
  
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

# ============================================================
# EXCLUSÃO DE REGISTROS
# ============================================================

# Apaga registro quantitativo (id_kr + competencia)
delete_indicador <- function(id_kr, competencia) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, sprintf(
    "DELETE FROM fact_indicadores WHERE id_kr = %d AND competencia = '%s'",
    as.integer(id_kr), competencia))
}

# Apaga registro qualitativo (id_kr + competencia)
delete_qualitativo <- function(id_kr, competencia) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, sprintf(
    "DELETE FROM fact_qualitativo WHERE id_kr = %d AND competencia = '%s'",
    as.integer(id_kr), competencia))
}

# Apaga AMBOS (quantitativo + qualitativo) de um KR+competencia
delete_registro_completo <- function(id_kr, competencia) {
  delete_indicador(id_kr, competencia)
  delete_qualitativo(id_kr, competencia)
  invisible(TRUE)
}

# Lista todos os registros do ano para o painel de gestão
get_registros_para_exclusao <- function(ano = 2026) {
  con <- get_con(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf("
    SELECT f.id_kr, f.competencia,
           k.nome AS nome_kr, k.unidade,
           o.nome AS nome_objetivo, o.letra,
           f.valor_realizado, f.meta,
           f.percentual_atingimento, f.status,
           f.responsavel,
           f.timestamp_preenchimento,
           CASE WHEN q.id_registro IS NOT NULL THEN 1 ELSE 0 END AS tem_qualitativo
    FROM fact_indicadores f
    JOIN dim_krs k ON f.id_kr = k.id_kr
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    LEFT JOIN fact_qualitativo q
      ON q.id_kr = f.id_kr AND q.competencia = f.competencia
    WHERE substr(f.competencia, 1, 4) = '%s'
    ORDER BY o.id_objetivo, k.id_kr, f.competencia ASC",
                          as.character(ano)))
}

# ============================================================
# HELPERS
# ============================================================

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

tipo_label <- function(tipo) {
  switch(tipo %||% "media",
         "acumulado" = "Acumulado no ano",
         "media"     = "Média do período",
         "taxa"      = "Taxa do período",
         "estoque"   = "Patamar atual",
         "—")
}

tipo_icon <- function(tipo) {
  switch(tipo %||% "media",
         "acumulado" = "∑",
         "media"     = "x̄",
         "taxa"      = "%",
         "estoque"   = "◼",
         "")
}

fmt_competencia <- function(comp) {
  tryCatch(
    format(as.Date(paste0(comp, "-01")), "%b/%Y"),
    error = function(e) comp
  )
}

# Retorna quantidade de casas decimais adequada para cada unidade
digits_para_unidade <- function(unidade) {
  if (is.null(unidade) || is.na(unidade)) return(0L)
  if (trimws(unidade) == "R$") return(2L)
  return(0L)
}

# Formata número pt-BR com casas decimais automáticas por unidade
# Uso: num_fmt(valor) → sem casas | num_fmt(valor, unidade="R$") → 2 casas
num_fmt <- function(x, digits = NULL, unidade = NULL) {
  d <- if (!is.null(digits)) digits else digits_para_unidade(unidade)
  vapply(x, function(xi) {
    v <- suppressWarnings(as.numeric(xi))
    if (is.na(v)) return("—")
    formatC(v, format = "f", digits = d,
            big.mark = ".", decimal.mark = ",")
  }, character(1))
}