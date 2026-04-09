# ============================================================
# MIRAI OKR — Deploy no shinyapps.io
# Execute este script no console do RStudio com o projeto aberto
# ============================================================

# 1. Instalar dependências (se necessário)
pkgs <- c("shiny", "dplyr", "plotly", "DBI", "RSQLite", "DT", "rsconnect")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

# 2. Configurar conta shinyapps.io

rsconnect::setAccountInfo(
  name   = "MateusSilva7",   
  token  = "A00591A26BE19346C38F25B9E2F27062",           
  secret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"         
)

# 3. Deploy
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rsconnect::deployApp(
  appName  = "mirai-okr",
  appTitle = "MIRAI · OKR Dashboard",
  appFiles = c("app.R", "modules/database.R", "www/Mirai_logos.svg"),
  forceUpdate = TRUE
)

# ── Notas importantes ──────────────────────────────────────
# O banco SQLite é recriado automaticamente a cada deploy.
# Dados inseridos serão perdidos se a instância for reiniciada.
#
# Para persistência real, migre para PostgreSQL externo:
#   1. Crie um banco no Neon (neon.tech) ou Supabase (supabase.com)
#   2. Configure as variáveis de ambiente no painel shinyapps.io:
#      Settings > Environment Variables:
#        DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASS
#   3. Substitua get_con() em modules/database.R:
#
#   get_con <- function() {
#     DBI::dbConnect(
#       RPostgres::Postgres(),
#       host     = Sys.getenv("DB_HOST"),
#       port     = as.integer(Sys.getenv("DB_PORT", "5432")),
#       dbname   = Sys.getenv("DB_NAME"),
#       user     = Sys.getenv("DB_USER"),
#       password = Sys.getenv("DB_PASS")
#     )
#   }
