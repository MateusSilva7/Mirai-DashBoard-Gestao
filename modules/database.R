# ============================================================
# MIRAI OKR SYSTEM — Database Module
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)

DB_PATH <- "data/mirai_okr.sqlite"

# ============================================================
# INITIALIZE DATABASE
# ============================================================
init_database <- function() {
  # Garante que o diretório data/ existe antes de conectar
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  con <- dbConnect(SQLite(), DB_PATH)
  
  # dim_objetivos
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_objetivos (
      id_objetivo INTEGER PRIMARY KEY AUTOINCREMENT,
      nome TEXT NOT NULL,
      descricao TEXT,
      letra TEXT,
      cor TEXT DEFAULT '#3271FE'
    )
  ")
  
  # dim_krs
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_krs (
      id_kr INTEGER PRIMARY KEY AUTOINCREMENT,
      id_objetivo INTEGER NOT NULL,
      nome TEXT NOT NULL,
      meta REAL NOT NULL,
      unidade TEXT DEFAULT '%',
      frequencia TEXT DEFAULT 'Mensal',
      FOREIGN KEY (id_objetivo) REFERENCES dim_objetivos(id_objetivo)
    )
  ")
  
  # fact_indicadores
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS fact_indicadores (
      id_registro INTEGER PRIMARY KEY AUTOINCREMENT,
      id_kr INTEGER NOT NULL,
      data TEXT NOT NULL,
      valor_realizado REAL NOT NULL,
      meta REAL NOT NULL,
      percentual_atingimento REAL,
      status TEXT,
      FOREIGN KEY (id_kr) REFERENCES dim_krs(id_kr)
    )
  ")
  
  # fact_inputs_qualitativos
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS fact_inputs_qualitativos (
      id_registro INTEGER PRIMARY KEY AUTOINCREMENT,
      id_kr INTEGER NOT NULL,
      data TEXT NOT NULL,
      responsavel TEXT,
      diagnostico TEXT,
      gargalos TEXT,
      acoes_realizadas TEXT,
      proximas_acoes TEXT,
      confianca INTEGER DEFAULT 3,
      FOREIGN KEY (id_kr) REFERENCES dim_krs(id_kr)
    )
  ")
  
  # Seed data — OKRs extraídos da imagem
  seed_data(con)
  
  dbDisconnect(con)
}

seed_data <- function(con) {
  count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM dim_objetivos")$n
  if (count > 0) return(invisible(NULL))
  
  # Objetivos MIRAI (extraídos da imagem)
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
    letra = c("M", "I", "R", "A", "I2"),
    cor = c("#3271FE", "#1742AD", "#3271FE", "#34C3D0", "#663B8E"),
    stringsAsFactors = FALSE
  )
  
  dbAppendTable(con, "dim_objetivos", objetivos)
  
  # KRs por objetivo
  krs <- data.frame(
    id_objetivo = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5),
    nome = c(
      "Retorno ao Cliente (SQaaS) em até 12 meses",
      "NPS nos Clientes",
      "Faturamento bruto anual com EBITDA de 40%",
      "PMR médio",
      "Novos clientes de SqaaS",
      "Renovação de contratos SqaaS",
      "e-NPS do time",
      "Turnover voluntário",
      "Solução agnética suportando etapas do método",
      "Ofertas comerciais suportadas por soluções",
      "Produtividade do time (índice composto)"
    ),
    meta = c(30, 80, 12000000, 45, 4, 100, 70, 10, 1, 3, 80),
    unidade = c("%", "%", "R$", "dias", "clientes", "%", "pontos", "%", "solução", "ofertas", "%"),
    frequencia = c("Trimestral", "Mensal", "Anual", "Mensal", "Trimestral", "Semestral",
                   "Trimestral", "Semestral", "Semestral", "Anual", "Mensal"),
    stringsAsFactors = FALSE
  )
  
  dbAppendTable(con, "dim_krs", krs)
  
  # Dados de exemplo para demonstração
  set.seed(42)
  today <- Sys.Date()
  
  sample_data <- lapply(1:11, function(kr_id) {
    meta_val <- krs$meta[kr_id]
    dates <- seq(today - 90, today, by = 30)
    
    data.frame(
      id_kr = kr_id,
      data = as.character(dates),
      valor_realizado = round(meta_val * runif(length(dates), 0.5, 1.1), 2),
      meta = meta_val,
      stringsAsFactors = FALSE
    )
  })
  
  sample_df <- do.call(rbind, sample_data)
  sample_df$percentual_atingimento <- round((sample_df$valor_realizado / sample_df$meta) * 100, 1)
  sample_df$status <- ifelse(sample_df$percentual_atingimento >= 80, "verde",
                              ifelse(sample_df$percentual_atingimento >= 60, "amarelo", "vermelho"))
  
  dbAppendTable(con, "fact_indicadores", sample_df)
}

# ============================================================
# HELPERS
# ============================================================
get_con <- function() dbConnect(SQLite(), DB_PATH)

get_objetivos <- function() {
  con <- get_con()
  res <- dbGetQuery(con, "SELECT * FROM dim_objetivos ORDER BY id_objetivo")
  dbDisconnect(con)
  res
}

get_krs <- function(id_objetivo = NULL) {
  con <- get_con()
  if (is.null(id_objetivo)) {
    res <- dbGetQuery(con, "SELECT k.*, o.nome as nome_objetivo, o.letra, o.cor 
                            FROM dim_krs k 
                            JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
                            ORDER BY k.id_objetivo, k.id_kr")
  } else {
    res <- dbGetQuery(con, sprintf("SELECT k.*, o.nome as nome_objetivo, o.letra, o.cor 
                                    FROM dim_krs k 
                                    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
                                    WHERE k.id_objetivo = %d
                                    ORDER BY k.id_kr", id_objetivo))
  }
  dbDisconnect(con)
  res
}

get_indicadores <- function(id_kr = NULL, limit = 100) {
  con <- get_con()
  where <- if (!is.null(id_kr)) sprintf("WHERE f.id_kr = %d", id_kr) else ""
  res <- dbGetQuery(con, sprintf(
    "SELECT f.*, k.nome as nome_kr, k.unidade, k.id_objetivo,
            o.nome as nome_objetivo, o.letra, o.cor
     FROM fact_indicadores f
     JOIN dim_krs k ON f.id_kr = k.id_kr
     JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
     %s
     ORDER BY f.data DESC
     LIMIT %d", where, limit
  ))
  dbDisconnect(con)
  res
}

get_ultimo_status <- function() {
  con <- get_con()
  res <- dbGetQuery(con, "
    SELECT k.id_kr, k.nome as nome_kr, k.meta, k.unidade, k.id_objetivo,
           o.nome as nome_objetivo, o.letra, o.cor,
           f.valor_realizado, f.percentual_atingimento, f.status, f.data
    FROM dim_krs k
    JOIN dim_objetivos o ON k.id_objetivo = o.id_objetivo
    LEFT JOIN fact_indicadores f ON f.id_kr = k.id_kr
    WHERE f.id_registro = (
      SELECT MAX(f2.id_registro) FROM fact_indicadores f2 WHERE f2.id_kr = k.id_kr
    )
    ORDER BY k.id_objetivo, k.id_kr
  ")
  dbDisconnect(con)
  res
}

get_qualitativo <- function(id_kr = NULL) {
  con <- get_con()
  where <- if (!is.null(id_kr)) sprintf("WHERE q.id_kr = %d", id_kr) else ""
  res <- dbGetQuery(con, sprintf(
    "SELECT q.*, k.nome as nome_kr
     FROM fact_inputs_qualitativos q
     JOIN dim_krs k ON q.id_kr = k.id_kr
     %s
     ORDER BY q.data DESC
     LIMIT 50", where
  ))
  dbDisconnect(con)
  res
}

insert_indicador <- function(id_kr, data, valor_realizado, meta) {
  pct <- round((valor_realizado / meta) * 100, 1)
  status <- ifelse(pct >= 80, "verde", ifelse(pct >= 60, "amarelo", "vermelho"))
  
  con <- get_con()
  dbExecute(con, sprintf(
    "INSERT INTO fact_indicadores (id_kr, data, valor_realizado, meta, percentual_atingimento, status)
     VALUES (%d, '%s', %f, %f, %f, '%s')",
    id_kr, as.character(data), valor_realizado, meta, pct, status
  ))
  dbDisconnect(con)
  list(pct = pct, status = status)
}

insert_qualitativo <- function(id_kr, data, responsavel, diagnostico, gargalos, 
                                acoes_realizadas, proximas_acoes, confianca) {
  con <- get_con()
  dbExecute(con, sprintf(
    "INSERT INTO fact_inputs_qualitativos 
     (id_kr, data, responsavel, diagnostico, gargalos, acoes_realizadas, proximas_acoes, confianca)
     VALUES (%d, '%s', '%s', '%s', '%s', '%s', '%s', %d)",
    id_kr, as.character(data),
    gsub("'", "''", responsavel),
    gsub("'", "''", diagnostico),
    gsub("'", "''", gargalos),
    gsub("'", "''", acoes_realizadas),
    gsub("'", "''", proximas_acoes),
    confianca
  ))
  dbDisconnect(con)
}
