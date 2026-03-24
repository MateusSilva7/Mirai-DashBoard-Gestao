# MIRAI OKR System
## Sistema Operacional de Gestão Estratégica

---

## 🚀 Como Rodar Localmente

### Pré-requisitos
- R 4.2+
- RStudio (recomendado)

### 1. Instalar Dependências

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "DBI",
  "RSQLite",
  "plotly"
))
```

### 2. Estrutura de Arquivos

```
mirai_okr/
├── app.R                    ← Ponto de entrada principal
├── modules/
│   ├── database.R           ← Banco de dados SQLite + queries
│   ├── input_module.R       ← Formulário de coleta de dados
│   └── dashboard_module.R   ← Dashboard e visualizações
├── www/
│   └── styles.css           ← Identidade visual MIRAI
└── data/
    └── mirai_okr.sqlite     ← Criado automaticamente no 1º run
```

### 3. Rodar o App

**Opção A — Via RStudio:**
Abra `app.R` e clique em "Run App"

**Opção B — Via Console:**
```r
setwd("caminho/para/mirai_okr")
shiny::runApp()
```

**Opção C — Via Terminal:**
```bash
Rscript -e "shiny::runApp('mirai_okr', port=3838)"
```

---

## 🗄️ Banco de Dados

O SQLite é criado automaticamente em `data/mirai_okr.sqlite` com dados de exemplo.

### Tabelas

| Tabela | Descrição |
|--------|-----------|
| `dim_objetivos` | 5 objetivos estratégicos MIRAI |
| `dim_krs` | 11 Key Results extraídos da imagem |
| `fact_indicadores` | Histórico quantitativo (valor vs meta) |
| `fact_inputs_qualitativos` | Diagnósticos, gargalos, ações |

### Reset do Banco

```r
file.remove("data/mirai_okr.sqlite")
# Reinicie o app — o banco será recriado
```

---

## 📊 Funcionalidades

### Dashboard
- **Visão Executiva:** KPIs consolidados + semáforos por objetivo
- **Breakdown por Objetivo:** Detalhamento de KRs com barra de progresso
- **Série Temporal:** Gráfico Meta vs Realizado com marcadores coloridos
- **Alertas:** Lista de KRs críticos e em atenção
- **Auto-refresh:** 30 segundos (configurável)

### Registro de Dados
- **Formulário Quantitativo:** Seleção de KR + valor realizado → cálculo automático de %
- **Formulário Qualitativo:** Diagnóstico, gargalos, ações, nível de confiança
- **Preview em tempo real:** Veja o status antes de salvar

---

## ☁️ Deploy (Produção)

### Opção 1 — shinyapps.io (mais fácil)

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name="SEU_USUARIO", token="SEU_TOKEN", secret="SEU_SECRET")
rsconnect::deployApp("mirai_okr")
```

**Atenção:** O SQLite não persiste entre deploys no shinyapps.io.
Para persistência, migre para PostgreSQL (ver abaixo).

### Opção 2 — Shiny Server (VPS/Docker)

```bash
# Instalar Shiny Server
sudo apt-get install shiny-server

# Copiar app
sudo cp -r mirai_okr /srv/shiny-server/

# Acessar em: http://seu-ip:3838/mirai_okr
```

### Opção 3 — Docker

```dockerfile
FROM rocker/shiny:latest
RUN R -e "install.packages(c('bslib','dplyr','DBI','RSQLite','plotly'))"
COPY mirai_okr /srv/shiny-server/mirai_okr
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
```

### Migrar para PostgreSQL (produção escalável)

No `modules/database.R`, substitua:

```r
# SQLite (local)
con <- dbConnect(SQLite(), "data/mirai_okr.sqlite")

# PostgreSQL (produção)
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host   = Sys.getenv("DB_HOST"),
  port   = Sys.getenv("DB_PORT"),
  user   = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)
```

---

## 🎨 Identidade Visual

Baseada no MIRAI Brand Guidelines 1.0:

| Token | Cor | Uso |
|-------|-----|-----|
| `--blue` | `#3271FE` | Primária, botões, KRs ativos |
| `--support-blue` | `#1742AD` | Hover, elementos secundários |
| `--black` | `#1B1B1C` | Sidebar, textos primários |
| `--aqua` | `#34C3D0` | Destaques, gradientes |
| `--coral` | `#FF5460` | Alertas críticos |

**Tipografia:** Manrope (Google Fonts)

---

## 📋 OKRs Mapeados

| Letra | Objetivo | KRs |
|-------|----------|-----|
| M | Maximizar capacidade de gerar grande impacto | Retorno 30%, NPS 80% |
| I | Impulsionar faturamento recorrente | Faturamento R$12M, PMR 45 dias |
| R | Robustecer carteira com novos Clientes SqaaS | 4 novos clientes, Renovação 100% |
| A | Alavancar experiência e capacidade técnica | e-NPS 70, Turnover <10% |
| I | Incorporar assets para ofertas e produtividade | Solução agnética, 3 ofertas |

---

## 🔄 Fluxo do Sistema

```
INPUT (Formulário Shiny)
    ↓
BANCO SQLite (fact_indicadores / fact_inputs_qualitativos)
    ↓
PROCESSAMENTO (cálculo %, classificação status)
    ↓
OUTPUT (Dashboard, Gráficos, Alertas)
    ↓
[Auto-refresh 30s → volta ao início]
```
