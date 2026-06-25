# app.R
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(sf)
  library(DT); library(plotly); library(leaflet)
  library(bslib); library(shinyWidgets); library(networkD3)
  library(ggplot2); library(scales); library(readr); library(writexl); library(digest); library(stringi)
})

options(scipen = 999)
options(shiny.maxRequestSize = 50 * 1024^2)

res_dir <- normalizePath(".", winslash = "/")

data_atualizacao <- format(file.info(file.path(res_dir, "cfem.rds"))$mtime, "%d %B %Y")
if (is.na(data_atualizacao)) data_atualizacao <- "Data não disponível"

.read_rds <- function(name) readRDS(file.path(res_dir, name))
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Dados tabulares ----
cfem        <- .read_rds("cfem.rds")
cfem_anual  <- .read_rds("cfem_anual.rds")
cfem_mensal <- .read_rds("cfem_mensal.rds")
pma_simpl   <- .read_rds("pma_simpl.rds")

# ---- Geometrias ----
ti  <- .read_rds("ti_simpl.rds")
uc  <- .read_rds("uc_simpl.rds")
qui <- .read_rds("qui_simpl.rds")

# ---- Choices iniciais ----
anos_all          <- sort(unique(cfem$ANO))
subs_all_grupo    <- sort(unique(cfem$SUBSarrSIM))
subs_all_original <- sort(unique(cfem$SUBSarr))
ufs_all           <- sort(na.omit(unique(cfem$abbrev_state)))
muns_all          <- sort(na.omit(unique(cfem$name_muni)))
fases_all         <- sort(na.omit(unique(cfem$FASE)))
procs_all         <- sort(na.omit(unique(cfem$PROCESSO)))
tits_all          <- sort(na.omit(unique(cfem$TITULAR)))
decl_all          <- sort(na.omit(unique(cfem$NOME_arr)))
map_subs          <- cfem |> dplyr::distinct(SUBSarrSIM, SUBSarr)

# ---- Lookups para filtros encadeados ----
lk_mun      <- .read_rds("lk_mun_tab1.rds")
lk_tit_proc <- .read_rds("lk_tit_proc_tab1.rds")
lk_decl     <- .read_rds("lk_decl_tab1.rds")

lk_mun_tab2      <- .read_rds("lk_mun_tab2.rds")
lk_tit_proc_tab2 <- .read_rds("lk_tit_proc_tab2.rds")
lk_decl_tab2     <- .read_rds("lk_decl_tab2.rds")

# ---- Microdados SCM (aba Consulta) ----
.read_rds_opt <- function(name) {
  p <- file.path(res_dir, name)
  if (file.exists(p)) readRDS(p) else NULL
}
micro_processos     <- .read_rds_opt("micro_processos.rds")
micro_eventos       <- .read_rds_opt("micro_eventos.rds")
micro_pessoas       <- .read_rds_opt("micro_pessoas.rds")
micro_pessoa_resumo <- .read_rds_opt("micro_pessoa_resumo.rds")
micro_substancias   <- .read_rds_opt("micro_substancias.rds")
micro_titulos       <- .read_rds_opt("micro_titulos.rds")
micro_municipios    <- .read_rds_opt("micro_municipios.rds")
micro_documentacao  <- .read_rds_opt("micro_documentacao.rds")
micro_associacoes   <- .read_rds_opt("micro_associacoes.rds")
micro_propsolo      <- .read_rds_opt("micro_propsolo.rds")
micro_ok            <- !is.null(micro_processos)
micro_proc_choices  <- if (micro_ok) sort(unique(micro_processos$processo)) else character(0)

# ---- PLGs impedidas ----
plg_imp     <- .read_rds_opt("plg_impedidas.rds")
plg_imp_geo <- .read_rds_opt("plg_impedidas_geo.rds")
plg_ok      <- !is.null(plg_imp)
plg_uf_choices <- if (plg_ok && "uf" %in% names(plg_imp))
  sort(unique(na.omit(plg_imp$uf))) else character(0)
plg_motivo_choices <- if (plg_ok && "motivo" %in% names(plg_imp))
  sort(unique(na.omit(plg_imp$motivo))) else character(0)

cp_motivo_choices <- plg_motivo_choices
cp_uf_choices     <- plg_uf_choices
cp_anos <- if (plg_ok && "dt_impedimento" %in% names(plg_imp))
  suppressWarnings(as.integer(format(plg_imp$dt_impedimento, "%Y"))) else integer(0)
cp_anos <- cp_anos[!is.na(cp_anos)]
cp_ano_min <- if (length(cp_anos)) min(cp_anos) else 2000L
cp_ano_max <- if (length(cp_anos)) max(cp_anos) else as.integer(format(Sys.Date(), "%Y"))

# ---- Colunas + rótulos (aba Tabela) ----
cols_visible <- c(
  "SUBSarrSIM", "SUBSarr", "PROCESSO", "AREA_HA", "ANO", "MES",
  "abbrev_state", "name_muni", "TITULAR", "CPF_CNPJcm", "NOME_arr",
  "CPF_CNPJarr", "VALORarr", "VALORtot", "PESO_G", "PESO_KG",
  "preco_g_orig", "corr", "PESO_G_final", "PESO_KG_final", "preco_g_final",
  "FASE", "ULT_EV_DES", "ULT_EV_DAT", "UCname", "TIname", "QUIname"
)
cols_labels <- c(
  SUBSarrSIM = "Grupo", SUBSarr = "Substância", PROCESSO = "Processo",
  AREA_HA = "Área proc.(ha)", ANO = "Ano", MES = "Mês",
  abbrev_state = "UF", name_muni = "Município", TITULAR = "Titular",
  CPF_CNPJcm = "CPF-CNPJ (titular)", NOME_arr = "Parte declarante",
  CPF_CNPJarr = "CPF-CNPJ (declarante)", VALORarr = "Valor Recolhido (R$)",
  VALORtot = "Valor Total (R$)", PESO_G = "Peso orig (g)", PESO_KG = "Peso orig (Kg)",
  preco_g_orig = "R$/g (orig)", corr = "Peso corrigido?",
  PESO_G_final = "Peso final (g)", PESO_KG_final = "Peso final (kg)",
  preco_g_final = "R$/g (final)", FASE = "Fase Processo",
  ULT_EV_DES = "Último evento", ULT_EV_DAT = "Data último evento",
  UCname = "UC", TIname = "TI", QUIname = "QUI"
)

# ---- Tema ----
primary_color <- "#1B4332"
accent_color  <- "#2D6A4F"
theme <- bs_theme(
  version = 5,
  base_font = font_google("Inter"), heading_font = font_google("Inter"),
  primary = primary_color, info = accent_color, bg = "#ffffff", fg = "#212529"
)

picker_opts <- list(
  `actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE,
  `noneSelectedText` = "Todos", `selectedTextFormat` = "count > 2"
)

# ---- Relatório de seleção ----
relatorio_selecao <- function(df, mensal = TRUE, list_cap = 10) {
  if (is.null(df) || nrow(df) == 0) return("Nenhum dado encontrado com os filtros aplicados.")
  stopifnot("PESO_KG_final" %in% names(df))
  peso_total <- sum(df$PESO_KG_final, na.rm = TRUE)
  anos <- range(na.omit(df$ANO))
  fmt_num_br <- function(x) format(round(x, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  fmt_cur_br <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
  showv <- function(v) paste(c(utils::head(v, list_cap), if (length(v) > list_cap) "…"), collapse = ", ")
  subs_u <- sort(unique(na.omit(df$SUBSarr)))
  grps_u <- sort(unique(na.omit(df$SUBSarrSIM)))

  linhas <- c(
    paste0("Período: ", anos[1], "–", anos[2]),
    paste0("UF (", dplyr::n_distinct(df$abbrev_state), "): ",
           paste(sort(unique(na.omit(df$abbrev_state))), collapse = ", ")),
    paste0("Município (", dplyr::n_distinct(df$name_muni), "): ",
           paste(utils::head(sort(unique(na.omit(df$name_muni))), 20), collapse = ", "),
           if (dplyr::n_distinct(df$name_muni) > 20) ", …" else ""),
    paste0("Substância - Grupo (", length(grps_u), "): ", showv(grps_u)),
    paste0("Substância - Detalhe (", length(subs_u), "): ", showv(subs_u)),
    paste0("Fase (", dplyr::n_distinct(df$FASE), "): ",
           paste(sort(unique(na.omit(df$FASE))), collapse = ", "))
  )

  proc_u <- sort(unique(na.omit(df$PROCESSO)))
  tit_u  <- sort(unique(na.omit(df$TITULAR)))
  dec_u  <- sort(unique(na.omit(df$NOME_arr)))
  linhas_listas <- c(
    paste0("Processos únicos (", length(proc_u), "):\n  ", showv(proc_u)),
    paste0("Titulares únicos (", length(tit_u), "):\n  ", showv(tit_u)),
    paste0("Partes declarantes únicas (", length(dec_u), "):\n  ", showv(dec_u))
  )

  area_total <- NA_real_; n_area_ok <- 0L; linha_area <- NULL
  if ("AREA_HA" %in% names(df)) {
    area_info <- df |>
      dplyr::select(PROCESSO, AREA_HA) |>
      dplyr::filter(!is.na(AREA_HA)) |>
      dplyr::group_by(PROCESSO) |>
      dplyr::summarise(area_ha = dplyr::first(AREA_HA), .groups = "drop")
    area_total <- sum(area_info$area_ha, na.rm = TRUE)
    n_area_ok  <- nrow(area_info)
    linha_area <- if (n_area_ok > 0) {
      paste0("Área total (ha): ", fmt_num_br(area_total), " [", n_area_ok, " processos]")
    } else {
      "Área total (ha): não disponível (sem valores de área na seleção)."
    }
  }

  linha_ratio <- NULL
  if (is.finite(area_total) && !is.na(area_total) && area_total > 0) {
    kg_ha <- peso_total / area_total
    linha_ratio <- paste0("Relação (Kg/ha): ", fmt_num_br(kg_ha))
  }

  linhas2 <- c(
    paste0("Total Declarações CFEM: ", format(nrow(df), big.mark = ".", decimal.mark = ",")),
    paste0("Total Valor Recolhido: ", fmt_cur_br(sum(df$VALORarr, na.rm = TRUE))),
    paste0("Total Peso declarado (Kg): ", fmt_num_br(peso_total)),
    linha_area, linha_ratio
  )

  add_ov <- function(flag, namecol, rotulo) {
    if (flag %in% names(df) && any(df[[flag]] == 1, na.rm = TRUE)) {
      nomes <- sort(unique(na.omit(df[[namecol]][ df[[flag]] == 1 ])))
      paste0("- ", rotulo, " (", length(nomes), "): ",
             paste(utils::head(nomes, 15), collapse = ", "),
             if (length(nomes) > 15) ", …" else "")
    } else NULL
  }
  bloco_ov <- c(
    "Sobreposição com Territórios Protegidos:",
    add_ov("TIov",  "TIname",  "Terras Indígenas"),
    add_ov("UCov",  "UCname",  "Unidades de Conservação"),
    add_ov("QUIov", "QUIname", "Comunidades Quilombolas")
  )
  if (identical(bloco_ov[-1], list(NULL, NULL, NULL))) bloco_ov <- "Sobreposição com Territórios Protegidos: Nenhuma."
  bloco_buf <- c(
    "Proximidade (10 km):",
    add_ov("TIov10km",  "TIname",  "Terras Indígenas"),
    add_ov("UCov10km",  "UCname",  "Unidades de Conservação"),
    add_ov("QUIov10km", "QUIname", "Comunidades Quilombolas")
  )
  if (identical(bloco_buf[-1], list(NULL, NULL, NULL))) bloco_buf <- "Proximidade (10 km): Não."

  paste(c(
    linhas, "",
    linhas_listas[1], "", linhas_listas[2], "", linhas_listas[3], "",
    linhas2, "", bloco_ov, "", bloco_buf
  ), collapse = "\n")
}

# ---- UI ----
ui <- page_navbar(
  title = "Arrecadação de CFEM (2010-2026)",
  theme = theme,
  header = tags$head(
    tags$style(HTML("
      body { font-size: 12px; color: #212529; }
      h1, h2 { color: #2C3E50; font-weight: 600; }
      h3 { font-size: 16px; font-weight: 600; margin-top: 10px; margin-bottom: 8px; color: #2C3E50; }
      h4 { font-size: 14px; font-weight: 600; margin-top: 14px; margin-bottom: 6px; color: #2C3E50; }
      .app-subtitle { font-size: 14px; color: #6c757d; line-height: 1.5; margin-bottom: 8px; }
      .note-text {font-size: 14px; color: #6c757d; font-style: italic; margin-top: -4px; margin-bottom: 10px;}
      .filters-card { background: #F8F9FA; border: 1px solid #E1E5EB; border-radius: 6px; padding: 10px; }
      .filters-card .form-control, .filters-card .selectpicker, .filters-card .form-select { font-size: 10px; height: calc(1.8em + 0.75rem + 2px); }
      .filters-card .shiny-input-container { margin-bottom: 10px; width: 100%; }
      .filters-card .btn { width: 100%; font-size: 11px; }
      .bootstrap-select .bs-actionsbox { padding: 4px 8px !important; }
      .bootstrap-select .bs-actionsbox .btn-group { display: flex !important; width: auto !important; gap: 6px; }
      .bootstrap-select .bs-actionsbox .btn-group .btn { flex: 0 0 auto !important; width: auto !important; padding: 2px 6px !important; font-size: 10px !important; line-height: 1.2 !important; }
      .bootstrap-select .dropdown-menu li a span.text { font-size: 12px !important; }
      .bootstrap-select .dropdown-menu { max-height: 70vh !important; z-index: 3000 !important; }
      .bootstrap-select .dropdown-menu .inner { max-height: 64vh !important; }
      .summary-box { background: #F8F9FA; border: 1px solid #D6D8DB; border-radius: 8px; padding: 12px; margin-bottom: 10px; }
      .summary-title { font-weight: 600; font-size: 14px; color: #2C3E50; margin-bottom: 6px; }
      .btn-light { border: 1px solid #ced4da; color: #2C3E50; }
      .dt-buttons .dt-button { font-size: 8px !important; padding: 1px 8px !important; border-radius: 4px !important; background-color: #343a40 !important; color: white !important; border: none !important; margin-right: 5px; }
      .dataTables_wrapper .dataTables_paginate { float: left; }
      .dt-buttons .btn:hover { background-color: #495057 !important; color: white !important; }
      .dataTables_filter { display: none !important; }
      #sankeyPlot { height: 1300px !important; }
      #relatorio_tab1, #relatorio_tab2, #relatorio_tab3 { white-space: pre-wrap; font-size: 12px; }
      #tabela_dt { width: 100% !important; margin: 0 auto; }
      .dataTables_wrapper { width: 100% !important; overflow-x: auto !important; position: relative; }
      .dataTables_scrollBody { overflow-x: auto !important; max-width: 100% !important; }
      .dataTables_scrollHead { overflow: hidden !important; }
      table.dataTable { width: auto !important; margin-bottom: 0 !important; }
      table.dataTable td, table.dataTable th { white-space: nowrap !important; vertical-align: middle !important; padding: 8px 12px !important; }
      table.dataTable thead th { position: sticky !important; top: 0 !important; background-color: #f8f9fa !important; z-index: 10 !important; }
      table.dataTable td:not(.dt-wrap), table.dataTable th:not(.dt-wrap) { white-space: nowrap !important; }
      table.dataTable td.dt-wrap, table.dataTable th.dt-wrap { white-space: normal !important; word-break: break-word; overflow-wrap: break-word; line-height: 1.25; min-width: 200px; max-width: 320px; }
      .dataTables_paginate { margin-top: 10px !important; }
      ::-webkit-scrollbar { height: 8px; width: 8px; }
      ::-webkit-scrollbar-track { background: #f1f1f1; }
      ::-webkit-scrollbar-thumb { background: #888; border-radius: 4px; }
      ::-webkit-scrollbar-thumb:hover { background: #555; }
      @media screen and (max-width: 767px) {
        .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate { float: none !important; text-align: center !important; }
        .dataTables_wrapper .dataTables_paginate { margin-top: 0.5em !important; }
      }
      .tab-pane { height: calc(100vh - 120px) !important; display: flex; flex-direction: column; }
      .fluid-row { display: flex; flex: 1; min-height: 0; }
      .col-sm-3 { overflow: visible; padding-bottom: 20px; }
      .col-sm-9 { height: 100%; display: flex; flex-direction: column; }
      .dataTables_wrapper { flex: 1; display: flex; flex-direction: column; min-height: 0; }
      .dataTables_scrollBody { flex: 1; min-height: 0; }
      div.dt-buttons { display: inline-flex !important; gap: 6px; margin: 0 0 8px 0; }
      div.dt-buttons .dt-button, div.dt-buttons .btn { font-size: 8px !important; line-height: 1.2 !important; padding: 4px 10px !important; border-radius: 4px !important; width: auto !important; flex: 0 0 auto !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button { font-size: 6px !important; padding: 2px 6px !important; min-width: 10px !important; margin: 0 1px !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current { font-size: 6px !important; padding: 2px 6px !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.previous, .dataTables_wrapper .dataTables_paginate .paginate_button.next { font-size: 6px !important; padding: 2px 6px !important; }
      .btn-group > .btn { margin: 2px 3px; }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background: #e9ecef !important; border: 1px solid #dee2e6 !important; }
      #ov_flags_tab1 .btn, #ov_flags_tab2 .btn, #ov_flags_tab3 .btn { font-size: 11px !important; padding: 2px 10px !important; }
    "))
  ),

  # ---- Aba 1 – Tabela ----
  nav_panel("Tabela de Dados",
    tags$p(class = "app-subtitle",
      "Explore os registros mensais da arrecadação da Compensação Financeira pela Exploração Mineral (CFEM) vinculados a processos minerários ativos do SIGMINE/ANM. ",
      "Filtre por substância (grupo/detalhe), fase, UF, município, processo, titular e parte declarante. ",
      "Os valores estão em R$ e as quantidades em kg e g. ",
      "Dados: ",
      tags$a("dados.gov.br/sistema-arrecadacao", href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao", target = "_blank"), ". ",
      "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "),
    tags$p(class = "note-text",
      "Nota - Alíquota CFEM utilizada para obtenção da coluna 'Valor Final': até 31/10/2017 (Lei 8.001/1990), adotamos: ouro em PLG = 0,2%; ouro fora de PLG = 2%; diamante em PLG = 0,2%; nióbio = 3%; e 2% para as demais substâncias aqui analisadas. A partir de 01/11/2017 (Lei 13.540/2017), as alíquotas passam a ouro = 1,5%, diamante = 2%, nióbio = 3% e 2% para todas as demais. O valor total é então calculado por 'Valor Arrecadado' ÷ alíquota vigente, por competência (ANO/MÊS) e por substância."),
    fluidRow(
      column(width = 3,
        div(class = "filters-card",
          tags$div(class = "mb-2", tags$strong("Filtros")),
          pickerInput("subs_tab1", "Substância(s) (grupo):",
                      choices = subs_all_grupo, selected = "OURO", multiple = TRUE, options = picker_opts),
          pickerInput("subs_det_tab1", "Substância(s) (detalhadas):",
                      choices = subs_all_original, selected = c("OURO", "MINÉRIO DE OURO", "OURO NATIVO"),
                      multiple = TRUE, options = picker_opts),
          pickerInput("fases_tab1", "Fase(s):",
                      choices = fases_all, selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"),
                      multiple = TRUE, options = picker_opts),
          checkboxGroupButtons("ov_flags_tab1", "Territórios Protegidos:",
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (10 km)" = "UCov10km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
                      selected = c(), direction = "horizontal",
                      checkIcon = list(yes = icon("check"), no = icon("minus")), size = "sm", status = "light"),
          sliderInput("periodo_tab1", "Período (anos):",
                      min = min(anos_all), max = max(anos_all), value = c(2018, 2026), step = 1, sep = "", ticks = FALSE),
          pickerInput("ufs_tab1", "UF(s):", choices = ufs_all, selected = ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("muns_tab1", "Município(s):", choices = muns_all, multiple = TRUE, options = picker_opts),
          pickerInput("procs_tab1", "Processo(s):", choices = procs_all, multiple = TRUE, options = picker_opts),
          pickerInput("tits_tab1", "Titular(es):", choices = tits_all, multiple = TRUE, options = picker_opts),
          pickerInput("decl_tab1", "Parte(s) Declarante(s):", choices = decl_all, multiple = TRUE, options = picker_opts),
          tags$hr(),
          div(class = "d-grid gap-2 mt-1", actionButton("reset_tab1", "Resetar filtros", class = "btn btn-light btn-sm")),
          tags$hr(),
          div(class = "mb-0 d-flex gap-0", downloadButton("baixar_csv", "CSV"), downloadButton("baixar_xlsx", "Excel")),
          tags$hr(),
          downloadButton("baixar_pma_sel_tab1", "PMAs (seleção) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_titular_tab1", "PMAs (mesmo titular) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_declarante_tab1", "PMAs (mesma declarante) .shp", class = "btn btn-light")
        )
      ),
      column(width = 9,
        div(style = "overflow-x: auto;", DTOutput("tabela_dt", height = "100%")),
        br(),
        div(class = "summary-box",
          div(class = "summary-title", "Resumo da seleção"),
          verbatimTextOutput("relatorio_tab1", placeholder = TRUE))
      )
    )
  ),

  # ---- Aba 2 – Fluxo Sankey ----
  nav_panel("Fluxo Anual de Arrecadação",
    tags$p(class = "app-subtitle",
      "Fluxo anual da CFEM (R$ ou kg) entre os níveis: UF → Município → Titular → Processo → Parte declarante. ",
      "Ajuste “Máx. de nós por nível” para manter a legibilidade e refine com os filtros laterais. ",
      "Dados: ",
      tags$a("dados.gov.br/sistema-arrecadacao", href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao", target = "_blank"), ".",
      "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "),
    fluidRow(
      column(width = 3,
        div(class = "filters-card", tags$div(class = "mb-2", tags$strong("Filtros")),
          numericInput("max_nodes_sankey", "Máx. de nós por nível:", value = 10, min = 5, max = 200, step = 5),
          radioButtons("variavel_fluxo_tab2", "Métrica do fluxo:",
                       choices = c("Valor Recolhido (R$)" = "VALORarr", "Quantidade (Kg líquido)" = "PESO_KG_final"),
                       selected = "VALORarr"),
          pickerInput("subs_tab2", "Substância(s) (grupo):",
                      choices = subs_all_grupo, selected = "OURO", multiple = TRUE, options = picker_opts),
          pickerInput("subs_det_tab2", "Substância(s) (detalhadas):",
                      choices = subs_all_original, selected = c("OURO", "MINÉRIO DE OURO", "OURO NATIVO"),
                      multiple = TRUE, options = picker_opts),
          pickerInput("fases_tab2", "Fase(s):",
                      choices = fases_all, multiple = TRUE,
                      selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"), options = picker_opts),
          checkboxGroupButtons("ov_flags_tab2", "Territórios Protegidos:",
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (10 km)" = "UCov10km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
                      selected = c(), direction = "horizontal",
                      checkIcon = list(yes = icon("check"), no = icon("minus")), size = "sm", status = "light"),
          sliderInput("periodo_tab2", "Período (anos):",
                      min = min(anos_all), max = max(anos_all), value = c(2018, 2026), step = 1, sep = "", ticks = FALSE),
          pickerInput("ufs_tab2", "UF(s):", choices = ufs_all, selected = ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("muns_tab2", "Município(s):", choices = muns_all, multiple = TRUE, options = picker_opts),
          pickerInput("procs_tab2", "Processo(s):", choices = procs_all, multiple = TRUE, options = picker_opts),
          pickerInput("tits_tab2", "Titular(es):", choices = tits_all, multiple = TRUE, options = picker_opts),
          pickerInput("decl_tab2", "Parte(s) Declarante(s):", choices = decl_all, multiple = TRUE, options = picker_opts),
          tags$hr(),
          div(class = "d-grid gap-2 mt-1", actionButton("reset_tab2", "Resetar filtros", class = "btn btn-light btn-sm")),
          tags$hr(),
          div(class = "mb-0 d-flex gap-0", downloadButton("baixar_csv_tab2", "CSV"), downloadButton("baixar_xlsx_tab2", "Excel")),
          tags$hr(),
          downloadButton("baixar_pma_sel_tab2", "PMAs (seleção) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_titular_tab2", "PMAs (mesmo titular) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_declarante_tab2", "PMAs (mesma declarante) .shp", class = "btn btn-light")
        )
      ),
      column(width = 9, sankeyNetworkOutput("sankeyPlot", height = "800px"),
        br(),
        div(class = "summary-box",
          div(class = "summary-title", "Resumo da seleção"),
          verbatimTextOutput("relatorio_tab2", placeholder = TRUE)))
    )
  ),

  # ---- Aba 3 – Série Temporal e Mapa ----
  nav_panel("Série Temporal e Mapa Processos Minerários",
    tags$p(class = "app-subtitle",
      "Série mensal da CFEM (R$ ou kg) conforme os filtros. ",
      "Veja a curva geral ou separe por Processo, Titular, Parte Declarante, Substância, Grupo ou Fase. ",
      "Defina o intervalo de anos e meses; pontos acima de 1,5×IQR são destacados como outliers. ",
      "Dados: ",
      tags$a("dados.gov.br/sistema-arrecadacao", href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao", target = "_blank"), ".",
      "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "),
    tags$p(class = "note-text",
      "Nota: os pontos destacados como outliers são calculados por grupo (Processo, Titular, etc.) ",
      "com base no critério do boxplot: valores acima de Q3 + 1,5 × IQR são considerados atípicos. ",
      "Quando o intervalo interquartílico (IQR) é nulo, aplica-se um ajuste usando o desvio padrão da série."),
    fluidRow(
      column(width = 3,
        div(class = "filters-card", tags$div(class = "mb-2", tags$strong("Filtros")),
          radioButtons("variavel_fluxo_tab3", "Métrica do fluxo:",
                       choices = c("Valor Recolhido (R$)" = "VALORarr", "Quantidade (Kg líquido)" = "PESO_KG_final"),
                       selected = "VALORarr"),
          pickerInput("subs_tab3", "Substância(s) (grupo):",
                      choices = subs_all_grupo, selected = "OURO", multiple = TRUE, options = picker_opts),
          pickerInput("subs_det_tab3", "Substância(s) (detalhadas):",
                      choices = subs_all_original, selected = c("OURO", "OURO NATIVO", "MINÉRIO DE OURO"),
                      multiple = TRUE, options = picker_opts),
          pickerInput("fases_tab3", "Fase(s):",
                      choices = fases_all, selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"),
                      multiple = TRUE, options = picker_opts),
          checkboxGroupButtons("ov_flags_tab3", "Territórios Protegidos:",
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (10 km)" = "UCov10km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
                      selected = c(), direction = "horizontal",
                      checkIcon = list(yes = icon("check"), no = icon("minus")), size = "sm", status = "light"),
          selectInput("agrupamento_tab3", "Visualiza linhas por:",
                      choices = c("Geral" = "geral", "Processo" = "PROCESSO", "Titular" = "TITULAR",
                                  "Parte Declarante" = "NOME_arr", "Substância" = "SUBSarr",
                                  "Grupo (subs)" = "SUBSarrSIM", "Fase" = "FASE")),
          sliderInput("periodo_tab3", "Período (anos):",
                      min = min(anos_all), max = max(anos_all), value = c(2018, 2026), step = 1, sep = "", ticks = FALSE),
          sliderInput("meses_tab3", "Meses:", min = 1, max = 12, value = c(1, 12), step = 1, sep = "", ticks = FALSE),
          pickerInput("ufs_tab3", "UF(s):", choices = ufs_all, selected = ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("muns_tab3", "Município(s):", choices = muns_all, multiple = TRUE, options = picker_opts),
          pickerInput("procs_tab3", "Processo(s):", choices = procs_all, multiple = TRUE, options = picker_opts),
          pickerInput("tits_tab3", "Titular(es):", choices = tits_all, multiple = TRUE, options = picker_opts),
          pickerInput("decl_tab3", "Parte(s) Declarante(s):", choices = decl_all, multiple = TRUE, options = picker_opts),
          tags$hr(),
          div(class = "d-grid gap-2 mt-1", actionButton("reset_tab3", "Resetar filtros", class = "btn btn-light btn-sm")),
          tags$hr(),
          div(class = "mt-2 mb-2 d-flex gap-0", downloadButton("baixar_csv_tab3", "CSV"), downloadButton("baixar_xlsx_tab3", "Excel")),
          tags$hr(),
          downloadButton("baixar_pma_sel_tab3", "Download PMAs (seleção) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_titular_tab3", "Download PMAs (mesmo titular) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_declarante_tab3", "Download PMAs (mesma declarante) .shp", class = "btn btn-light")
        )
      ),
      column(width = 9,
        leafletOutput("mapa_cfem_pma_tab3", height = "525px"),
        br(),
        plotlyOutput("serie_temporal", height = "400px"),
        br(),
        plotlyOutput("grafico_outliers", height = "400px"),
        br(),
        div(class = "summary-box",
          div(class = "summary-title", "Resumo da seleção"),
          verbatimTextOutput("relatorio_tab3", placeholder = TRUE))
      )
    )
  ),

  # ---- Aba 4 – Consulta de Processos (Dossiê + PLGs Impedidas) ----
  nav_panel("Consulta de Processos",
    tags$p(class = "app-subtitle",
      "Consulta de processos minerários(Amazônia Legal). ",
      "Fonte: SIGMINE e Microdados/ANM (download em ", data_atualizacao, ")."),
    fluidRow(
      column(width = 4,
        div(class = "filters-card",
          tags$div(class = "mb-2", tags$strong("Filtros")),
          radioGroupButtons("cp_modo", label = NULL,
                      choices = c("Todos os processos" = "todos", "Processos impedidos" = "impedidas"),
                      selected = "impedidas", justified = TRUE, size = "sm", status = "light"),
          textInput("cp_busca", "Buscar (processo ou titular):", placeholder = "ex.: 850123/2016 ou nome..."),
          conditionalPanel(
            condition = "input.cp_modo == 'impedidas'",
            checkboxGroupButtons("cp_tipo", "Tipo de impedimento:",
                      choices = c("Definitivo" = "Definitivo", "Temporário" = "Temporario"),
                      selected = c("Definitivo", "Temporario"),
                      justified = TRUE, size = "sm", status = "light", checkIcon = list(yes = icon("check"))),
            pickerInput("cp_motivo", "Motivo:",
                      choices = cp_motivo_choices, selected = cp_motivo_choices, multiple = TRUE, options = picker_opts),
            sliderInput("cp_ano", "Ano do impedimento:",
                      min = cp_ano_min, max = cp_ano_max, value = c(cp_ano_min, cp_ano_max), step = 1, sep = "", ticks = FALSE)
          ),
          tags$hr(),
          uiOutput("cp_info"),
          tags$hr(),
          div(class = "mb-0 d-flex gap-0", downloadButton("cp_csv", "CSV"), downloadButton("cp_xlsx", "Excel")),
          tags$hr(),
          div(style = "max-height: 460px; overflow-y: auto;", DTOutput("cp_lista", height = "auto"))
        )
      ),
      column(width = 8,
        leafletOutput("cp_mapa", height = "400px"),
        br(),
        uiOutput("cp_dossie_box"),
        uiOutput("cp_dossie_cabecalho"),
        uiOutput("cp_dossie_corpo")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  # ---- Helpers ----
  filter_in <- function(df, col, sel) {
    if (is.null(sel) || length(sel) == 0) return(df)
    df[df[[col]] %in% sel, , drop = FALSE]
  }

  sync_pair <- function(session, id_group, id_detail, map_df, col_group, col_detail) {
    lock <- reactiveVal(FALSE)
    observeEvent(input[[id_group]], {
      if (lock()) return()
      lock(TRUE); on.exit(lock(FALSE), add = TRUE)
      g_sel <- input[[id_group]]
      valid_choices <- map_df |>
        dplyr::filter(.data[[col_group]] %in% g_sel) |>
        dplyr::pull(.data[[col_detail]]) |> unique() |> sort()
      updatePickerInput(session, id_detail, choices = valid_choices,
                        selected = intersect(isolate(input[[id_detail]]), valid_choices))
    }, ignoreInit = TRUE)
    observeEvent(input[[id_detail]], {
      if (lock()) return()
      lock(TRUE); on.exit(lock(FALSE), add = TRUE)
      d_sel <- input[[id_detail]]
      if (!length(d_sel)) return()
      parent_choices <- map_df |>
        dplyr::filter(.data[[col_detail]] %in% d_sel) |>
        dplyr::pull(.data[[col_group]]) |> unique() |> sort()
      updatePickerInput(session, id_group,
                        choices = sort(unique(map_df[[col_group]])), selected = parent_choices)
    }, ignoreInit = TRUE)
  }

  filtra_sobrepos <- function(df, flags) {
    if (length(flags) == 0) return(df)
    cols_ok <- intersect(flags, names(df))
    if (length(cols_ok) == 0) return(df)
    df |> dplyr::filter(rowSums(dplyr::across(dplyr::all_of(cols_ok), ~ dplyr::coalesce(.x, 0))) >= 1)
  }

  exportar_shapefile <- function(sf_obj, nome_base, temp_dir) {
    stopifnot(inherits(sf_obj, "sf"))
    if (nrow(sf_obj) == 0) stop("Sem feições para exportar.")
    path_base <- file.path(temp_dir, nome_base)
    if (is.na(sf::st_crs(sf_obj))) warning("Objeto sf sem CRS definido; o .prj pode sair vazio.")
    sf::st_write(sf_obj, paste0(path_base, ".shp"), delete_layer = TRUE, quiet = TRUE)
    arquivos <- list.files(temp_dir,
                           pattern = paste0("^", nome_base, "\\.(shp|shx|dbf|prj|cpg|qml|qpj)$"),
                           full.names = TRUE)
    zipfile <- file.path(temp_dir, paste0(nome_base, ".zip"))
    zip::zip(zipfile, files = arquivos, mode = "cherry-pick")
    zipfile
  }

  # ==========================================================================
  # ABA 4 — Consulta de Processos
  # ==========================================================================
  cp_lista_df <- reactive({
    req(micro_ok)
    if (identical(input$cp_modo, "impedidas")) {
      req(plg_ok)
      df <- plg_imp
      if (!is.null(input$cp_tipo) && length(input$cp_tipo) > 0)
        df <- df[df$tipo_impedimento %in% input$cp_tipo, , drop = FALSE]
      if (!is.null(input$cp_motivo) && length(input$cp_motivo) > 0)
        df <- df[df$motivo %in% input$cp_motivo, , drop = FALSE]
      if (!is.null(input$cp_uf) && length(input$cp_uf) > 0 && "uf" %in% names(df))
        df <- df[df$uf %in% input$cp_uf, , drop = FALSE]
      if (!is.null(input$cp_ano) && "dt_impedimento" %in% names(df)) {
        anos <- as.integer(format(df$dt_impedimento, "%Y"))
        df <- df[!is.na(anos) & anos >= input$cp_ano[1] & anos <= input$cp_ano[2], , drop = FALSE]
      }
      termo <- trimws(input$cp_busca %||% "")
      if (nchar(termo) > 0) {
        cols <- intersect(c("processo", "titular"), names(df))
        chave <- do.call(paste, c(lapply(df[cols], as.character), sep = " | "))
        df <- df[grepl(tolower(termo), tolower(chave), fixed = TRUE), , drop = FALSE]
      }
      cols <- intersect(c("processo", "uf", "motivo", "dt_impedimento", "tipo_impedimento"), names(df))
      df[, cols, drop = FALSE]
    } else {
      termo <- trimws(input$cp_busca %||% "")
      if (nchar(termo) == 0) return(NULL)
      df <- micro_processos
      hit <- grepl(tolower(termo), tolower(df$processo), fixed = TRUE)
      if (!is.null(micro_pessoas)) {
        tit <- micro_pessoas[grepl("titular", micro_pessoas$relacao, ignore.case = TRUE), , drop = FALSE]
        proc_tit <- unique(tit$processo[grepl(tolower(termo), tolower(tit$nome %||% ""), fixed = TRUE)])
        hit <- hit | (df$processo %in% proc_tit)
      }
      df <- df[hit, , drop = FALSE]
      cols <- intersect(c("processo", "uf", "fase", "municipios"), names(df))
      df[, cols, drop = FALSE]
    }
  }) |> debounce(300)

  output$cp_info <- renderUI({
    if (!micro_ok) return(tags$div(style = "font-size:12px;color:#b02a37;",
      "Microdados não encontrados (rode os scripts 06 e plg_impedidas)."))
    df <- cp_lista_df()
    if (is.null(df)) return(tags$div(style = "font-size:12px;color:#6c757d;",
      "Digite um processo ou titular para listar."))
    tags$div(style = "font-size:12px;color:#2C3E50;",
      tags$strong("Resultados: "), format(nrow(df), big.mark = ".", decimal.mark = ","))
  })

  output$cp_lista <- renderDT({
    validate(need(micro_ok, "Dados indisponíveis."))
    df <- cp_lista_df()
    validate(need(!is.null(df), "Use a busca ou os filtros para listar processos."))
    validate(need(nrow(df) > 0, "Nenhum processo encontrado."))
    nm <- names(df); cn <- nm
    cn[nm == "processo"] <- "Processo"; cn[nm == "uf"] <- "UF"
    cn[nm == "motivo"] <- "Motivo"; cn[nm == "dt_impedimento"] <- "Data imped."
    cn[nm == "tipo_impedimento"] <- "Tipo"; cn[nm == "fase"] <- "Fase"
    cn[nm == "municipios"] <- "Município"
    DT::datatable(df, rownames = FALSE, class = "compact", colnames = cn,
      selection = "single", extensions = "Scroller",
      options = list(scrollX = TRUE, dom = "tip", pageLength = 12, deferRender = TRUE,
                     columnDefs = list(list(targets = "_all", className = "dt-left"))))
  }, server = TRUE)

  cp_proc_sel <- reactive({
    s <- input$cp_lista_rows_selected
    df <- cp_lista_df()
    if (is.null(s) || length(s) == 0 || is.null(df) || nrow(df) == 0) return(NULL)
    as.character(df$processo[s[1]])
  })

  output$cp_dossie_box <- renderUI({
    p <- cp_proc_sel(); if (is.null(p) || !plg_ok) return(NULL)
    row <- plg_imp[plg_imp$processo == p, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row <- row[1, ]
    is_def <- identical(row$tipo_impedimento, "Definitivo")
    cor_bg <- if (is_def) "#FCEBEB" else "#FAEEDA"
    cor_bd <- if (is_def) "#A32D2D" else "#854F0B"
    cor_tx <- if (is_def) "#501313" else "#412402"
    rotulo <- if (is_def) "IMPEDIMENTO DEFINITIVO" else "IMPEDIMENTO TEMPORÁRIO"
    linha <- function(rot, val) if (!is.null(val) && !is.na(val) && val != "")
      tags$div(tags$strong(paste0(rot, ": ")), val) else NULL
    tags$div(
      style = paste0("background:", cor_bg, "; border-left:5px solid ", cor_bd,
                     "; color:", cor_tx, "; padding:12px 16px; border-radius:6px; margin-bottom:14px;"),
      tags$div(style = "font-weight:600; margin-bottom:6px;", rotulo),
      linha("Motivo", row$motivo),
      linha("Data do impedimento", as.character(row$dt_impedimento)),
      if (isTRUE(row$foi_revertido)) tags$div(tags$em("Atenção: houve reversão posterior — verifique o histórico.")) else NULL,
      linha("Evento original (auditoria)", row$evento_original)
    )
  })

  output$cp_dossie_cabecalho <- renderUI({
    p <- cp_proc_sel()
    if (is.null(p)) return(tags$div(class = "summary-box", "Selecione um processo."))
    ph <- micro_processos[micro_processos$processo == p, , drop = FALSE]
    if (nrow(ph) == 0) return(tags$div("Processo não encontrado nos microdados."))
    ph <- ph[1, ]
    linha <- function(rot, val) if (!is.null(val) && !is.na(val) && val != "")
      tags$div(tags$strong(paste0(rot, ": ")), val) else NULL
    tags$div(
      tags$h3(paste0("Processo ", p)),
      tags$div(style = "font-size:13px; color:#2C3E50; margin-bottom:14px;",
        linha("NUP", ph$nup),
        linha("Ativo", ph$ativo),
        linha("Tipo requerimento", ph$tipo_requerimento),
        linha("Fase", ph$fase),
        linha("Área (ha)", if (!is.na(ph$area_ha)) format(round(ph$area_ha, 2), big.mark = ".", decimal.mark = ",") else NA),
        linha("UF", if ("uf" %in% names(ph)) ph$uf else NA),
        linha("Município(s)", if ("municipios" %in% names(ph)) ph$municipios else NA),
        linha("Protocolo", as.character(ph$dt_protocolo)),
        linha("Prioridade", as.character(ph$dt_prioridade))
      )
    )
  })

  cp_bloco <- function(tbl, p, drop = "processo") {
    if (is.null(tbl)) return(NULL)
    d <- tbl[tbl$processo == p, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    d[, setdiff(names(d), drop), drop = FALSE]
  }

  output$cp_dossie_corpo <- renderUI({
    p <- cp_proc_sel(); if (is.null(p)) return(NULL)
    sec <- function(titulo, id) tagList(tags$h4(titulo), DTOutput(id, height = "auto"))
    tagList(
      sec("Substâncias", "cp_sub"),
      sec("Pessoas relacionadas", "cp_pes"),
      sec("Histórico de eventos", "cp_eventos"),
      sec("Processos associados", "cp_assoc"),
      sec("Documentação", "cp_doc"),
      
    )
  })

  .cp_dt <- function(df) {
    if (is.null(df)) df <- data.frame(aviso = "Nenhum registro.")
    DT::datatable(df, rownames = FALSE, class = "compact",
      options = list(scrollX = TRUE, dom = "tp", pageLength = 5,
                     columnDefs = list(list(targets = "_all", className = "dt-left"))),
      selection = "none")
  }
  output$cp_sub       <- renderDT(.cp_dt(cp_bloco(micro_substancias,  cp_proc_sel())), server = FALSE)
  output$cp_pes       <- renderDT(.cp_dt(cp_bloco(micro_pessoas,      cp_proc_sel())), server = FALSE)
  output$cp_tit       <- renderDT(.cp_dt(cp_bloco(micro_titulos,      cp_proc_sel())), server = FALSE)
  output$cp_mun_bloco <- renderDT(.cp_dt(cp_bloco(micro_municipios,   cp_proc_sel())), server = FALSE)
  output$cp_solo      <- renderDT(.cp_dt(cp_bloco(micro_propsolo,     cp_proc_sel())), server = FALSE)
  output$cp_assoc     <- renderDT(.cp_dt(cp_bloco(micro_associacoes,  cp_proc_sel())), server = FALSE)
  output$cp_doc       <- renderDT(.cp_dt(cp_bloco(micro_documentacao, cp_proc_sel())), server = FALSE)

  output$cp_eventos <- renderDT({
    p <- cp_proc_sel()
    df <- cp_bloco(micro_eventos, p)
    if (is.null(df)) df <- data.frame(aviso = "Nenhum evento.")
    if ("data" %in% names(df)) df <- df[order(df$data, decreasing = TRUE), , drop = FALSE]
    nm <- names(df); cn <- nm
    cn[nm == "data"] <- "Data"; cn[nm == "evento"] <- "Evento"
    cn[nm == "observacao"] <- "Observação"; cn[nm == "publicacao"] <- "Publicação (DOU)"
    DT::datatable(df, rownames = FALSE, class = "compact", colnames = cn,
      extensions = "Scroller",
      options = list(scrollX = TRUE, dom = "tip", pageLength = 10, deferRender = TRUE,
                     columnDefs = list(
                       list(targets = "_all", className = "dt-left"),
                       list(targets = which(nm %in% c("observacao", "publicacao")) - 1, className = "dt-wrap"))))
  }, server = FALSE)

  cp_prep <- function() {
    df <- cp_lista_df(); validate(need(!is.null(df) && nrow(df) > 0, "Nada para exportar.")); df
  }
  output$cp_csv <- downloadHandler(
    filename = function() paste0("consulta_processos_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(cp_prep(), file))
  output$cp_xlsx <- downloadHandler(
    filename = function() paste0("consulta_processos_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(cp_prep(), path = file))

  output$cp_mapa <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
      leaflet::addLayersControl(baseGroups = c("Satélite", "CartoDB"),
                                options = leaflet::layersControlOptions(collapsed = FALSE)) |>
      leaflet::setView(lng = -55, lat = -5, zoom = 4)
  })

  observeEvent(cp_proc_sel(), {
    proxy <- leaflet::leafletProxy("cp_mapa")
    proxy |> leaflet::clearGroup("proc")
    p <- cp_proc_sel()
    if (is.null(p)) return()
    geo <- NULL
    if (!is.null(plg_imp_geo) && "processo" %in% names(plg_imp_geo)) {
      g <- plg_imp_geo[plg_imp_geo$processo == p, , drop = FALSE]
      if (nrow(g) > 0) geo <- g
    }
    if (is.null(geo) && exists("pma_simpl") && "PROCESSO" %in% names(pma_simpl)) {
      g <- pma_simpl[pma_simpl$PROCESSO == p, , drop = FALSE]
      if (nrow(g) > 0) geo <- sf::st_transform(g, 4326)
    }
    if (is.null(geo) || nrow(geo) == 0) return()
    bb <- sf::st_bbox(geo)
    proxy |>
      leaflet::addPolygons(data = geo, group = "proc",
                           color = "#FF3D00", weight = 2, opacity = 1,
                           fillOpacity = 0.35, smoothFactor = 0.2) |>
      leaflet::fitBounds(lng1 = as.numeric(bb["xmin"]), lat1 = as.numeric(bb["ymin"]),
                         lng2 = as.numeric(bb["xmax"]), lat2 = as.numeric(bb["ymax"]))
  })

  # ==========================================================================
  # ABA 1 — Tabela
  # ==========================================================================
  sync_pair(session, "subs_tab1", "subs_det_tab1", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab1, input$subs_det_tab1, input$ufs_tab1, input$fases_tab1, input$periodo_tab1), {
    df_temp <- lk_mun |>
      dplyr::filter(ANO >= input$periodo_tab1[1], ANO <= input$periodo_tab1[2],
                    FASE %in% input$fases_tab1, abbrev_state %in% input$ufs_tab1)
    if (length(input$subs_det_tab1)) {
      df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab1)
    } else {
      df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab1)
    }
    muns_ok <- sort(unique(df_temp$name_muni))
    updatePickerInput(session, "muns_tab1", choices = muns_ok,
                      selected = intersect(isolate(input$muns_tab1), muns_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$muns_tab1, input$ufs_tab1), {
    df_temp <- lk_tit_proc |> dplyr::filter(abbrev_state %in% input$ufs_tab1, name_muni %in% input$muns_tab1)
    tits_ok  <- sort(unique(df_temp$TITULAR))
    procs_ok <- sort(unique(df_temp$PROCESSO))
    updatePickerInput(session, "tits_tab1", choices = tits_ok,
                      selected = intersect(isolate(input$tits_tab1), tits_ok))
    updatePickerInput(session, "procs_tab1", choices = procs_ok,
                      selected = intersect(isolate(input$procs_tab1), procs_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$procs_tab1, input$tits_tab1), {
    df_temp <- lk_decl |>
      dplyr::filter(abbrev_state %in% input$ufs_tab1, name_muni %in% input$muns_tab1,
                    TITULAR %in% input$tits_tab1, PROCESSO %in% input$procs_tab1)
    decl_ok <- sort(unique(df_temp$NOME_arr))
    updatePickerInput(session, "decl_tab1", choices = decl_ok,
                      selected = intersect(isolate(input$decl_tab1), decl_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  dados_filtrados <- reactive({
    showNotification("Filtrando dados...", duration = 1, type = "default")
    df <- cfem |>
      dplyr::filter(ANO >= input$periodo_tab1[1], ANO <= input$periodo_tab1[2],
                    FASE %in% input$fases_tab1, abbrev_state %in% input$ufs_tab1)
    if (length(input$subs_det_tab1)) {
      df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab1)
    } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab1)
    }
    if (length(input$muns_tab1)) df  <- df |> dplyr::filter(name_muni %in% input$muns_tab1)
    if (length(input$procs_tab1)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab1)
    if (length(input$tits_tab1)) df  <- df |> dplyr::filter(TITULAR %in% input$tits_tab1)
    if (length(input$decl_tab1)) df  <- df |> dplyr::filter(NOME_arr %in% input$decl_tab1)
    df <- filtra_sobrepos(df, flags = input$ov_flags_tab1)
    df
  }) |> bindCache(
    input$periodo_tab1, input$subs_tab1, input$subs_det_tab1, input$ufs_tab1, input$muns_tab1, input$fases_tab1,
    input$procs_tab1, input$tits_tab1, input$decl_tab1, input$ov_flags_tab1
  ) |> debounce(250)

  observeEvent(input$reset_tab1, {
    updatePickerInput(session, "subs_tab1", choices = subs_all_grupo, selected = subs_all_grupo)
    updateSliderInput(session, "periodo_tab1", value = c(min(anos_all), max(anos_all)))
    updatePickerInput(session, "ufs_tab1", choices = ufs_all, selected = ufs_all)
    updatePickerInput(session, "fases_tab1", choices = fases_all, selected = fases_all)
    updateCheckboxGroupButtons(session, "ov_flags_tab1", selected = c())
    updatePickerInput(session, "subs_det_tab1", choices = subs_all_original, selected = subs_all_original)
    updatePickerInput(session, "muns_tab1", choices = muns_all, selected = muns_all)
    updatePickerInput(session, "tits_tab1", choices = tits_all, selected = tits_all)
    updatePickerInput(session, "procs_tab1", choices = procs_all, selected = character(0))
    updatePickerInput(session, "decl_tab1", choices = decl_all, selected = decl_all)
  })

  output$tabela_dt <- renderDT({
    df <- dados_filtrados()
    validate(need(nrow(df) > 0, "Nenhum dado encontrado com os filtros aplicados."))
    cols_keep  <- intersect(cols_visible, names(df))
    df_display <- df[, cols_keep, drop = FALSE]
    names(df_display) <- unname(cols_labels[cols_keep])
    if ("Peso corrigido?" %in% names(df_display)) {
      df_display[["Peso corrigido?"]] <- tolower(as.character(df_display[["Peso corrigido?"]]))
    } else {
      df_display[["Peso corrigido?"]] <- NA_character_
    }
    num_cols <- intersect(
      c("Peso orig (g)", "Peso orig (Kg)", "Peso final (g)", "Peso final (kg)", "Valor Recolhido (R$)", "Valor Total (R$)"),
      names(df_display))
    totals_raw <- vapply(num_cols, function(nm) sum(df_display[[nm]], na.rm = TRUE), numeric(1))
    fmt_num <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    fmt_cur <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
    totals_fmt <- setNames(
      ifelse(grepl("\\(R\\$\\)", names(totals_raw)), fmt_cur(totals_raw), fmt_num(totals_raw)),
      names(totals_raw))
    first_label <- sprintf("TOTAL (linhas: %s)", nrow(df_display))
    sketch <- htmltools::withTags(table(
      thead(
        tr(lapply(seq_along(df_display), function(i) {
          nm <- names(df_display)[i]
          val <- if (i == 1) first_label else if (nm %in% names(totals_fmt)) totals_fmt[[nm]] else ""
          th(style = "background:#F8F9FA;font-weight:700;", val)
        })),
        tr(lapply(names(df_display), th))
      )
    ))
    wrap_cols <- c("Titular", "Parte declarante", "UC", "TI")
    wrap_idx  <- which(names(df_display) %in% wrap_cols) - 1
    dt_obj <- datatable(
      df_display, container = sketch, extensions = c("Scroller"),
      rownames = FALSE, class = "compact",
      options = list(
        scrollX = TRUE, dom = 'ftip', pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Tudo')),
        columnDefs = list(
          list(targets = "_all", className = "dt-left"),
          list(targets = wrap_idx, className = "dt-wrap"),
          list(targets = wrap_idx, width = "260px")),
        autoWidth = TRUE, deferRender = TRUE)
    ) |>
      formatCurrency("Valor Recolhido (R$)", currency = "R$ ", digits = 2) |>
      formatCurrency("Valor Total (R$)", currency = "R$ ", digits = 2) |>
      formatRound("Peso orig (g)", digits = 2) |> formatRound("Peso orig (Kg)", digits = 2) |>
      formatRound("Peso final (g)", digits = 2) |> formatRound("Peso final (kg)", digits = 2) |>
      formatRound("R$/g (orig)", digits = 1) |> formatRound("R$/g (final)", digits = 1)
    lv <- setdiff(unique(df_display[["Peso corrigido?"]]), "original")
    dt_obj |> formatStyle(columns = names(df_display), valueColumns = "Peso corrigido?",
      backgroundColor = styleEqual(lv, rep("rgba(255,250,205,0.9)", length(lv))))
  }, server = TRUE)

  output$relatorio_tab1 <- renderText({ relatorio_selecao(dados_filtrados(), mensal = TRUE) })

  proxy_tabela <- DT::dataTableProxy("tabela_dt")
  observeEvent(dados_filtrados(), { DT::reloadData(proxy_tabela, resetPaging = TRUE) }, ignoreInit = TRUE)

  prep_export <- function() {
    df <- dados_filtrados()
    cols_keep <- intersect(cols_visible, names(df))
    df_export <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }
  prep_export_tab2 <- function() {
    df <- dados_selecionados_sankey()
    validate(need(nrow(df) > 0, "Nenhum dado para exportar (aba 2)."))
    cols_keep <- intersect(cols_visible, names(df))
    df_export <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }
  prep_export_tab3 <- function() {
    df <- dados_mensal()
    validate(need(nrow(df) > 0, "Nenhum dado para exportar (aba 3)."))
    cols_keep <- intersect(cols_visible, names(df))
    df_export <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }

  output$baixar_csv  <- downloadHandler(filename = function() paste0("cfem_filtrado_", Sys.Date(), ".csv"),
                                        content = function(file) readr::write_csv(prep_export(), file))
  output$baixar_xlsx <- downloadHandler(filename = function() paste0("cfem_filtrado_", Sys.Date(), ".xlsx"),
                                        content = function(file) write_xlsx(prep_export(), path = file))
  output$baixar_csv_tab2  <- downloadHandler(filename = function() paste0("cfem_anual_filtrado_", Sys.Date(), ".csv"),
                                             content = function(file) readr::write_csv(prep_export_tab2(), file))
  output$baixar_xlsx_tab2 <- downloadHandler(filename = function() paste0("cfem_anual_filtrado_", Sys.Date(), ".xlsx"),
                                             content = function(file) writexl::write_xlsx(prep_export_tab2(), path = file))
  output$baixar_csv_tab3  <- downloadHandler(filename = function() paste0("cfem_mensal_filtrado_", Sys.Date(), ".csv"),
                                             content = function(file) readr::write_csv(prep_export_tab3(), file))
  output$baixar_xlsx_tab3 <- downloadHandler(filename = function() paste0("cfem_mensal_filtrado_", Sys.Date(), ".xlsx"),
                                             content = function(file) writexl::write_xlsx(prep_export_tab3(), path = file))

  procs_sel_tab1 <- reactive({ unique(dados_filtrados()$PROCESSO) }) |> bindCache(dados_filtrados()$PROCESSO)
  tits_sel_tab1  <- reactive({ unique(dados_filtrados()$TITULAR) }) |> bindCache(dados_filtrados()$TITULAR)
  decl_sel_tab1  <- reactive({ unique(dados_filtrados()$NOME_arr) }) |> bindCache(dados_filtrados()$NOME_arr)

  pma_sel_tab1 <- reactive({
    procs <- procs_sel_tab1()
    src <- pma_simpl
    if (!length(procs)) return(src[0, ])
    dplyr::filter(src, PROCESSO %in% procs)
  }) |> bindCache(procs_sel_tab1())

  pma_titular_tab1 <- reactive({
    tits  <- tits_sel_tab1()
    procs <- procs_sel_tab1()
    src <- pma_simpl
    if (!length(tits)) return(src[0, ])
    dplyr::filter(src, TITULAR %in% tits, !(PROCESSO %in% procs))
  }) |> bindCache(tits_sel_tab1(), procs_sel_tab1())

  pma_declarante_tab1 <- reactive({
    declarantes <- decl_sel_tab1()
    if (!length(declarantes)) { src <- pma_simpl; return(src[0, ]) }
    procs_declarantes <- cfem |> dplyr::filter(NOME_arr %in% declarantes) |> dplyr::pull(PROCESSO) |> unique()
    src <- pma_simpl
    dplyr::filter(src, PROCESSO %in% procs_declarantes)
  }) |> bindCache(decl_sel_tab1())

  output$baixar_pma_sel_tab1 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab1_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_sel_tab1(), "pmas_selecao_tab1", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_titular_tab1 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab1_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_titular_tab1(), "pmas_titular_tab1", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_declarante_tab1 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab1_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_declarante_tab1(), "pmas_declarante_tab1", temp_dir), file, overwrite = TRUE)
    })

  # ==========================================================================
  # ABA 2 — Fluxo Sankey
  # ==========================================================================
  sync_pair(session, "subs_tab2", "subs_det_tab2", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab2, input$subs_det_tab2, input$ufs_tab2, input$fases_tab2, input$periodo_tab2), {
    df_temp <- lk_mun_tab2 |>
      dplyr::filter(ANO >= input$periodo_tab2[1], ANO <= input$periodo_tab2[2],
                    FASE %in% input$fases_tab2, abbrev_state %in% input$ufs_tab2)
    if (length(input$subs_det_tab2)) {
      df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab2)
    } else {
      df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab2)
    }
    muns_ok <- sort(unique(df_temp$name_muni))
    updatePickerInput(session, "muns_tab2", choices = muns_ok,
                      selected = intersect(isolate(input$muns_tab2), muns_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$muns_tab2, input$tits_tab2, input$ufs_tab2), {
    df_temp <- lk_tit_proc_tab2 |> dplyr::filter(abbrev_state %in% input$ufs_tab2, name_muni %in% input$muns_tab2)
    tits_ok <- sort(unique(df_temp$TITULAR)); procs_ok <- sort(unique(df_temp$PROCESSO))
    updatePickerInput(session, "tits_tab2", choices = tits_ok,
                      selected = intersect(isolate(input$tits_tab2), tits_ok))
    updatePickerInput(session, "procs_tab2", choices = procs_ok,
                      selected = intersect(isolate(input$procs_tab2), procs_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$procs_tab2, input$tits_tab2), {
    df_temp <- lk_decl_tab2 |> dplyr::filter(abbrev_state %in% input$ufs_tab2, name_muni %in% input$muns_tab2,
                                             TITULAR %in% input$tits_tab2, PROCESSO %in% input$procs_tab2)
    decl_ok <- sort(unique(df_temp$NOME_arr))
    updatePickerInput(session, "decl_tab2", choices = decl_ok,
                      selected = intersect(isolate(input$decl_tab2), decl_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  dados_selecionados_sankey <- reactive({
    showNotification("Atualizando Sankey...", duration = 1, type = "default")
    df <- cfem_anual |>
      dplyr::filter(ANO >= input$periodo_tab2[1], ANO <= input$periodo_tab2[2],
                    FASE %in% input$fases_tab2, abbrev_state %in% input$ufs_tab2)
    if (length(input$subs_det_tab2)) {
      df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab2)
    } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab2)
    }
    if (length(input$muns_tab2)) df <- df |> dplyr::filter(name_muni %in% input$muns_tab2)
    if (length(input$tits_tab2)) df <- df |> dplyr::filter(TITULAR %in% input$tits_tab2)
    if (length(input$procs_tab2)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab2)
    if (length(input$decl_tab2)) df <- df |> dplyr::filter(NOME_arr %in% input$decl_tab2)
    df <- filtra_sobrepos(df, flags = input$ov_flags_tab2)
    df
  }) |> bindCache(
    input$periodo_tab2, input$ufs_tab2, input$fases_tab2, input$subs_tab2, input$subs_det_tab2,
    input$muns_tab2, input$tits_tab2, input$procs_tab2, input$decl_tab2, input$ov_flags_tab2
  ) |> debounce(250)

  collapse_level <- function(df, col, top_n, label_outros) {
    tot <- df |> dplyr::group_by(.data[[col]]) |>
      dplyr::summarise(total = sum(valor_usado, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))
    keep <- head(tot[[col]], top_n)
    df[[col]] <- ifelse(df[[col]] %in% keep, df[[col]], label_outros)
    df
  }

  output$sankeyPlot <- renderSankeyNetwork({
    dados <- dados_selecionados_sankey()
    if (nrow(dados) == 0) { showNotification("Nenhum fluxo encontrado com os filtros aplicados.", type = "warning"); return(NULL) }
    dados <- dados |>
      dplyr::group_by(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr) |>
      dplyr::summarise(valor_usado = sum(.data[[input$variavel_fluxo_tab2]], na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(!is.na(NOME_arr), valor_usado > 0)
    top_n <- req(input$max_nodes_sankey)
    dados <- dados |>
      collapse_level("abbrev_state", top_n, "Outros — UFs") |>
      collapse_level("name_muni", top_n, "Outros — Municípios") |>
      collapse_level("TITULAR", top_n, "Outros — Titulares") |>
      collapse_level("PROCESSO", top_n, "Outros — Processos") |>
      collapse_level("NOME_arr", top_n, "Outros — Partes")
    dados2 <- dados |>
      mutate(UF = paste0(abbrev_state), MUN = paste0(name_muni), TIT = paste0(TITULAR),
             PROC = paste0(PROCESSO), DEC = paste0(NOME_arr))
    nodes <- data.frame(name = c(
      sort(unique(dados2$UF)), sort(unique(dados2$MUN)), sort(unique(dados2$TIT)),
      sort(unique(dados2$PROC)), sort(unique(dados2$DEC))))
    criar_links <- function(df, a, b) df |>
      dplyr::group_by(.data[[a]], .data[[b]]) |>
      dplyr::summarise(value = sum(valor_usado, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(source = match(.data[[a]], nodes$name) - 1, target = match(.data[[b]], nodes$name) - 1)
    links <- dplyr::bind_rows(
      criar_links(dados2, "UF", "MUN"), criar_links(dados2, "MUN", "TIT"),
      criar_links(dados2, "TIT", "PROC"), criar_links(dados2, "PROC", "DEC"))
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value",
      NodeID = "name", fontSize = 14, nodeWidth = 50, nodePadding = 50, sinksRight = FALSE)
  })

  observeEvent(input$reset_tab2, {
    updateRadioButtons(session, "variavel_fluxo_tab2", selected = "VALORarr")
    updateNumericInput(session, "max_nodes_sankey", value = 10)
    updatePickerInput(session, "subs_tab2", choices = subs_all_grupo, selected = subs_all_grupo)
    updatePickerInput(session, "fases_tab2", choices = fases_all, selected = fases_all)
    updateCheckboxGroupButtons(session, "ov_flags_tab2", selected = c())
    updateSliderInput(session, "periodo_tab2", value = c(min(anos_all), max(anos_all)))
    updatePickerInput(session, "subs_det_tab2", choices = subs_all_original, selected = subs_all_original)
    updatePickerInput(session, "ufs_tab2", choices = ufs_all, selected = ufs_all)
    updatePickerInput(session, "muns_tab2", choices = muns_all, selected = character(0))
    updatePickerInput(session, "tits_tab2", choices = tits_all, selected = character(0))
    updatePickerInput(session, "procs_tab2", choices = procs_all, selected = character(0))
    updatePickerInput(session, "decl_tab2", choices = decl_all, selected = character(0))
  })

  procs_sel_tab2 <- reactive({ unique(dados_selecionados_sankey()$PROCESSO) }) |> bindCache(dados_selecionados_sankey()$PROCESSO)
  tits_sel_tab2  <- reactive({ unique(dados_selecionados_sankey()$TITULAR) })  |> bindCache(dados_selecionados_sankey()$TITULAR)
  decl_sel_tab2  <- reactive({ unique(dados_selecionados_sankey()$NOME_arr) }) |> bindCache(dados_selecionados_sankey()$NOME_arr)

  pma_sel_tab2 <- reactive({
    procs <- procs_sel_tab2()
    src <- pma_simpl
    if (!length(procs)) return(src[0, ])
    dplyr::filter(src, PROCESSO %in% procs)
  }) |> bindCache(procs_sel_tab2())

  pma_titular_tab2 <- reactive({
    tits  <- tits_sel_tab2()
    procs <- procs_sel_tab2()
    src <- pma_simpl
    if (!length(tits)) return(src[0, ])
    dplyr::filter(src, TITULAR %in% tits, !(PROCESSO %in% procs))
  }) |> bindCache(tits_sel_tab2(), procs_sel_tab2())

  pma_declarante_tab2 <- reactive({
    declarantes <- decl_sel_tab2()
    if (!length(declarantes)) { src <- pma_simpl; return(src[0, ]) }
    procs_declarantes <- cfem |> dplyr::filter(NOME_arr %in% declarantes) |> dplyr::pull(PROCESSO) |> unique()
    src <- pma_simpl
    dplyr::filter(src, PROCESSO %in% procs_declarantes)
  }) |> bindCache(decl_sel_tab2())

  output$baixar_pma_sel_tab2 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab2_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_sel_tab2(), "pmas_selecao_tab2", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_titular_tab2 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab2_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_titular_tab2(), "pmas_titular_tab2", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_declarante_tab2 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab2_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_declarante_tab2(), "pmas_declarante_tab2", temp_dir), file, overwrite = TRUE)
    })

  output$relatorio_tab2 <- renderText({
    base <- relatorio_selecao(dados_selecionados_sankey(), mensal = FALSE)
    metrica <- if (input$variavel_fluxo_tab2 == "VALORarr") "Valor Recolhido (R$)" else "Quantidade (Kg líquido)"
    paste0(base, "\n\nMétrica no Sankey: ", metrica)
  })

  # ==========================================================================
  # ABA 3 — Série Temporal e Mapa
  # ==========================================================================
  sync_pair(session, "subs_tab3", "subs_det_tab3", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab3, input$subs_det_tab3, input$ufs_tab3, input$fases_tab3, input$periodo_tab3, input$meses_tab3), {
    df_temp <- cfem_mensal |>
      dplyr::filter(ANO >= input$periodo_tab3[1], ANO <= input$periodo_tab3[2],
                    FASE %in% input$fases_tab3, MES >= input$meses_tab3[1], MES <= input$meses_tab3[2],
                    abbrev_state %in% input$ufs_tab3)
    if (length(input$subs_det_tab3)) {
      df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab3)
    } else {
      df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab3)
    }
    updatePickerInput(session, "muns_tab3", choices = sort(unique(df_temp$name_muni)),
                      selected = intersect(isolate(input$muns_tab3), sort(unique(df_temp$name_muni))))
    rm(df_temp); gc()
  }, ignoreInit = FALSE)

  observeEvent(list(input$muns_tab3, input$ufs_tab3), {
    df_temp <- cfem_mensal |> filter_in("abbrev_state", input$ufs_tab3) |> filter_in("name_muni", input$muns_tab3)
    updatePickerInput(session, "tits_tab3", choices = sort(unique(df_temp$TITULAR)),
                      selected = intersect(isolate(input$tits_tab3), sort(unique(df_temp$TITULAR))))
    updatePickerInput(session, "procs_tab3", choices = sort(unique(df_temp$PROCESSO)),
                      selected = intersect(isolate(input$procs_tab3), sort(unique(df_temp$PROCESSO))))
    rm(df_temp); gc()
  }, ignoreInit = FALSE)

  observeEvent(list(input$procs_tab3, input$tits_tab3, input$ufs_tab3, input$muns_tab3), {
    df_temp <- cfem_mensal |>
      filter_in("abbrev_state", input$ufs_tab3) |> filter_in("name_muni", input$muns_tab3) |>
      filter_in("TITULAR", input$tits_tab3) |> filter_in("PROCESSO", input$procs_tab3)
    updatePickerInput(session, "decl_tab3", choices = sort(unique(df_temp$NOME_arr)),
                      selected = intersect(isolate(input$decl_tab3), sort(unique(df_temp$NOME_arr))))
    rm(df_temp); gc()
  }, ignoreInit = FALSE)

  dados_mensal <- reactive({
    df <- cfem_mensal |>
      dplyr::filter(ANO >= input$periodo_tab3[1], ANO <= input$periodo_tab3[2],
                    FASE %in% input$fases_tab3, MES >= input$meses_tab3[1], MES <= input$meses_tab3[2],
                    abbrev_state %in% input$ufs_tab3)
    if (length(input$subs_det_tab3)) {
      df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab3)
    } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab3)
    }
    if (length(input$muns_tab3)) df <- df |> dplyr::filter(name_muni %in% input$muns_tab3)
    if (length(input$tits_tab3)) df <- df |> dplyr::filter(TITULAR %in% input$tits_tab3)
    if (length(input$procs_tab3)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab3)
    if (length(input$decl_tab3)) df <- df |> dplyr::filter(NOME_arr %in% input$decl_tab3)
    df <- filtra_sobrepos(df, flags = input$ov_flags_tab3)
    df
  }) |> bindCache(input$periodo_tab3, input$meses_tab3, input$fases_tab3, input$ufs_tab3,
                  input$subs_tab3, input$subs_det_tab3, input$muns_tab3, input$tits_tab3, input$procs_tab3,
                  input$decl_tab3, input$ov_flags_tab3) |> debounce(450)

  observeEvent(input$reset_tab3, {
    updateRadioButtons(session, "variavel_fluxo_tab3", selected = "VALORarr")
    updateSelectInput(session, "agrupamento_tab3", selected = "geral")
    updatePickerInput(session, "subs_tab3", choices = subs_all_grupo, selected = subs_all_grupo)
    updatePickerInput(session, "fases_tab3", choices = fases_all, selected = fases_all)
    updateCheckboxGroupButtons(session, "ov_flags_tab3", selected = c())
    updateSliderInput(session, "periodo_tab3", value = c(min(anos_all), max(anos_all)))
    updateSliderInput(session, "meses_tab3", value = c(1, 12))
    updatePickerInput(session, "subs_det_tab3", choices = subs_all_original, selected = subs_all_original)
    updatePickerInput(session, "ufs_tab3", choices = ufs_all, selected = ufs_all)
    updatePickerInput(session, "muns_tab3", choices = muns_all, selected = muns_all)
    updatePickerInput(session, "tits_tab3", choices = tits_all, selected = tits_all)
    updatePickerInput(session, "procs_tab3", choices = procs_all, selected = procs_all)
    updatePickerInput(session, "decl_tab3", choices = decl_all, selected = decl_all)
  })

  output$serie_temporal <- renderPlotly({
    df <- dados_mensal(); req(nrow(df) > 0)
    variavel <- input$variavel_fluxo_tab3; agrup <- input$agrupamento_tab3
    if (agrup == "geral") {
      df_plot <- df |> group_by(data) |> summarise(valor = sum(.data[[variavel]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_plot, aes(x = data, y = valor)) +
        geom_line(color = "tomato", linewidth = 1) + geom_point(color = "tomato", size = 1.5) +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
        labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado (R$)", "Peso declarado (Kg)"), x = "Data") +
        theme_minimal(base_size = 13) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
    } else {
      df_plot <- df |> group_by(data, grupo = .data[[agrup]]) |> summarise(valor = sum(.data[[variavel]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_plot, aes(x = data, y = valor, group = grupo, color = grupo,
                               text = paste0("<b>", grupo, "</b><br>", "Data: ", format(data, "%b/%Y"), "<br>", "valor:", comma(valor)))) +
        geom_line(linewidth = 1) + geom_point(size = 1.5) +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
        labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado (R$)", "Peso declarado (Kg)"), x = "Data") +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "none")
      ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
    }
  })

  output$grafico_outliers <- renderPlotly({
    df <- dados_mensal(); req(nrow(df) > 0)
    variavel <- input$variavel_fluxo_tab3; agrup <- input$agrupamento_tab3
    df_plot <- df |>
      mutate(grupo = if (agrup == "geral") "geral" else .data[[agrup]], valor = as.numeric(.data[[variavel]])) |>
      group_by(data, grupo) |> summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    df_plot <- df_plot |>
      group_by(grupo) |>
      mutate(Q1 = quantile(valor, 0.25, na.rm = TRUE), Q3 = quantile(valor, 0.75, na.rm = TRUE),
             IQR = Q3 - Q1, sdv = sd(valor, na.rm = TRUE),
             limite_sup = ifelse(is.finite(IQR) & IQR > 0, Q3 + 1.5 * IQR,
                                 Q3 + 3 * ifelse(is.finite(sdv) & !is.na(sdv), sdv, 0)),
             outlier = valor > limite_sup) |> ungroup()
    message("Outliers totais: ", sum(df_plot$outlier, na.rm = TRUE))
    df_plot <- df_plot |> mutate(outlier = factor(outlier, levels = c(FALSE, TRUE), labels = c("Não", "Sim")))
    dummy_legend <- data.frame(data = as.Date(c(NA, NA)), valor = c(NA_real_, NA_real_),
                               outlier = factor(c("Não", "Sim"), levels = c("Não", "Sim")))
    p <- ggplot(df_plot, aes(x = data, y = valor)) +
      geom_line(aes(group = grupo), alpha = 0.2, color = "gray50", linewidth = 0.6) +
      geom_point(aes(color = outlier), size = 1.6) +
      geom_point(data = dummy_legend, aes(color = outlier), alpha = 0) +
      scale_color_manual(values = c("#212529", "tomato"), breaks = c("Não", "Sim"), drop = FALSE, name = "Outlier") +
      scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado por mês (R$)", "Peso declarado por mês (Kg)"), x = "Data") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", legend.title = element_text(size = 10), legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 70, hjust = 1, size = 9), axis.text.y = element_text(size = 10))
    ggplotly(p, tooltip = c("x", "y", "color")) |>
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2)) |> config(displayModeBar = FALSE)
  })

  pma_src_tab3 <- reactive(pma_simpl)
  dados_mapa_cfem_tab3 <- reactive({ dados_mensal() })

  pma_filtrado_tab3 <- reactive({
    procs <- unique(dados_mapa_cfem_tab3()$PROCESSO)
    src <- pma_src_tab3()
    if (length(procs) == 0) return(src[0, ])
    src |> dplyr::filter(PROCESSO %in% procs)
  }) |> bindCache(dados_mapa_cfem_tab3()$PROCESSO)

  pma_titular_tab3 <- reactive({
    procs_sel <- unique(dados_mapa_cfem_tab3()$PROCESSO)
    tits <- unique(dados_mapa_cfem_tab3()$TITULAR)
    src <- pma_src_tab3()
    if (length(tits) == 0) return(src[0, ])
    src |> dplyr::filter(TITULAR %in% tits, !(PROCESSO %in% procs_sel))
  }) |> bindCache(dados_mapa_cfem_tab3()$TITULAR, dados_mapa_cfem_tab3()$PROCESSO)

  pma_declarante_tab3 <- reactive({
    declarantes <- unique(dados_mapa_cfem_tab3()$NOME_arr)
    src <- pma_src_tab3()
    if (!length(declarantes)) return(src[0, ])
    processos_declarantes <- cfem |> dplyr::filter(NOME_arr %in% declarantes) |> dplyr::pull(PROCESSO) |> unique()
    src |> dplyr::filter(PROCESSO %in% processos_declarantes)
  }) |> bindCache(dados_mapa_cfem_tab3()$NOME_arr)

  output$mapa_cfem_pma_tab3 <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2, maxZoom = 18, preferCanvas = TRUE)) |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
      leaflet::addPolygons(data = uc, group = "Unidades de Conservação",
                           color = "#78c679", weight = 0.5, opacity = 0.8, fillOpacity = 0.5,
                           popup = ~paste0("<b>UC:</b> ", nome_uc)) |>
      leaflet::addPolygons(data = ti, group = "Terras Indígenas",
                           color = "#006837", weight = 0.5, opacity = 0.8, fillOpacity = 0.5,
                           popup = ~paste0("<b>TI:</b> ", terrai_nom,
                                           if ("fase_ti" %in% names(ti)) paste0("<br><b>Fase:</b> ", fase_ti) else "")) |>
      leaflet::addPolygons(data = qui, group = "Comunidades Quilombolas",
                           color = "#dfc27d", weight = 0.5, opacity = 0.85, fillOpacity = 0.45,
                           popup = ~paste0("<b>Comunidade:</b> ", nm_comunid,
                                           if ("fase" %in% names(qui)) paste0("<br><b>Fase:</b> ", fase) else "")) |>
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "Satélite"),
        overlayGroups = c("Processos Minerários", "PMAs do mesmo Titular", "PMAs da mesma Parte Declarante",
                          "Unidades de Conservação", "Terras Indígenas", "Comunidades Quilombolas"),
        options = leaflet::layersControlOptions(collapsed = FALSE)) |>
      leaflet::hideGroup(c("PMAs do mesmo Titular", "PMAs da mesma Parte Declarante",
                           "Unidades de Conservação", "Terras Indígenas", "Comunidades Quilombolas"))
  })

  observeEvent(input$ov_flags_tab3, {
    proxy <- leaflet::leafletProxy("mapa_cfem_pma_tab3")
    groups <- c("Unidades de Conservação" = "UCov", "Terras Indígenas" = "TIov", "Comunidades Quilombolas" = "QUIov")
    lapply(names(groups), function(g) proxy |> leaflet::hideGroup(g))
    sel <- input$ov_flags_tab3
    lapply(names(groups)[groups %in% sel], function(g) proxy |> leaflet::showGroup(g))
  })

  prev_hash_tab3 <- reactiveVal(NULL)
  observeEvent(pma_filtrado_tab3(), {
    pm <- pma_filtrado_tab3()
    validate(need(nrow(pm) > 0, "Nenhum processo minerário encontrado com os filtros."))
    h <- digest::digest(list(proc = sort(pm$PROCESSO), src = "simpl"))
    if (is.null(prev_hash_tab3()) || h != prev_hash_tab3()) {
      prev_hash_tab3(h)
      bb <- sf::st_bbox(sf::st_transform(pm, 4326))
      leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
        leaflet::clearGroup("Processos Minerários") |>
        leaflet::addPolygons(data = pm, group = "Processos Minerários",
          color = "#FF3D00", weight = 2, opacity = 1, fillOpacity = 0.4, smoothFactor = 0.2,
          popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>", "<b>Substância:</b> ", SUBS, "<br>",
                          "<b>Fase:</b> ", FASE, "<br>", "<b>Titular:</b> ", TITULAR)) |>
        leaflet::fitBounds(lng1 = as.numeric(bb["xmin"]), lat1 = as.numeric(bb["ymin"]),
                           lng2 = as.numeric(bb["xmax"]), lat2 = as.numeric(bb["ymax"]))
    }
  })

  observeEvent(pma_titular_tab3(), {
    d <- pma_titular_tab3()
    leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
      leaflet::clearGroup("PMAs do mesmo Titular") |>
      leaflet::addPolygons(data = d, group = "PMAs do mesmo Titular",
        color = "#0078FF", weight = 1, opacity = 0.8, fillOpacity = 0.35, smoothFactor = 0.2,
        popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>", "<b>Titular:</b> ", TITULAR))
  })

  observeEvent(pma_declarante_tab3(), {
    d <- pma_declarante_tab3()
    leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
      leaflet::clearGroup("PMAs da mesma Parte Declarante") |>
      leaflet::addPolygons(data = d, group = "PMAs da mesma Parte Declarante",
        color = "#6a3d9a", weight = 1, opacity = 0.8, fillOpacity = 0.35, smoothFactor = 0.2,
        popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>", "<b>Titular:</b> ", TITULAR))
  })

  output$baixar_pma_sel_tab3 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_filtrado_tab3(), "pmas_selecao_tab3", temp_dir), file, overwrite = TRUE); gc()
    })
  output$baixar_pma_titular_tab3 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_titular_tab3(), "pmas_titular_tab3", temp_dir), file, overwrite = TRUE); gc()
    })
  output$baixar_pma_declarante_tab3 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_declarante_tab3(), "pmas_declarante_tab3", temp_dir), file, overwrite = TRUE); gc()
    })

  outputOptions(output, "mapa_cfem_pma_tab3", suspendWhenHidden = FALSE)
  outputOptions(output, "cp_mapa", suspendWhenHidden = FALSE)

  output$relatorio_tab3 <- renderText({
    base <- relatorio_selecao(dados_mensal(), mensal = TRUE)
    metrica <- if (input$variavel_fluxo_tab3 == "VALORarr") "Valor Recolhido (R$)" else "Quantidade (Kg líquido)"
    agr_labels <- c(geral = "Geral", PROCESSO = "Processo", TITULAR = "Titular", NOME_arr = "Parte Declarante",
                    SUBSarr = "Substância", SUBSarrSIM = "Grupo (subs)", FASE = "Fase")
    agr <- agr_labels[[input$agrupamento_tab3]] %||% input$agrupamento_tab3
    df <- dados_mensal()
    if (!nrow(df)) return(paste0(base, "\n\nMétrica nos gráficos: ", metrica, " | Linhas por: ", agr))
    if (!"data" %in% names(df)) df$data <- as.Date(sprintf("%s-%02d-01", df$ANO, df$MES))
    paste0(base, "\n\nMétrica nos gráficos: ", metrica, " | Linhas por: ", agr, "\n")
  })
}

shinyApp(ui, server)