# app.R
#setwd("C:/GP/anm-geo")

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(sf)
  library(DT); library(plotly); library(leaflet)
  library(bslib); library(shinyWidgets); library(networkD3)
  library(ggplot2); library(scales); library(readr); library(writexl); library(digest); library(stringi)
})

options(scipen = 999)
options(shiny.maxRequestSize = 50 * 1024^2)
#options(shiny.useragg = TRUE)


res_dir <- normalizePath(".", winslash = "/")

# Dynamic update date based on file modification
data_atualizacao <- format(file.info(file.path(res_dir, "cfem.rds"))$mtime, "%d %B %Y")
if(is.na(data_atualizacao)) data_atualizacao <- "Data não disponível"

# ---- helpers ----
.read_rds <- function(name) readRDS(file.path(res_dir, name))

# ---- dados tabulares ----
cfem        <- .read_rds("cfem.rds")
cfem_anual  <- .read_rds("cfem_anual.rds")
cfem_mensal <- .read_rds("cfem_mensal.rds")

# versões FULL e SIMPL do PMA
#pma_full  <- .read_rds("pma.rds")
pma_simpl <- .read_rds("pma_simpl.rds")

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- geometrias (preferir simplificadas, senão cai no bruto) ----
ti  <- .read_rds("ti_simpl.rds")
uc  <- .read_rds("uc_simpl.rds")
qui <- .read_rds("qui_simpl.rds")

# ---- Choices - escolhas iniciais ----
anos_all          <- sort(unique(cfem$ANO))
subs_all_grupo    <- sort(unique(cfem$SUBSarrSIM))
subs_all_original <- sort(unique(cfem$SUBSarr))
ufs_all           <- sort(na.omit(unique(cfem$abbrev_state)))
muns_all          <- sort(na.omit(unique(cfem$name_muni)))
fases_all         <- sort(na.omit(unique(cfem$FASE)))
procs_all         <- sort(na.omit(unique(cfem$PROCESSO)))
tits_all          <- sort(na.omit(unique(cfem$TITULAR)))
decl_all          <- sort(na.omit(unique(cfem$NOME_arr)))

fases_anual  <- fases_all
fases_mensal <- fases_all
map_subs     <- cfem |> dplyr::distinct(SUBSarrSIM, SUBSarr)

# ---- Lookups enxutos para filtros encadeados (pré-calculados no 05) ----
# Lidos prontos do disco, em vez de calcular distinct() sobre o cfem inteiro na
# inicialização — mantém a abertura do app rápida.
lk_mun      <- .read_rds("lk_mun_tab1.rds")
lk_tit_proc <- .read_rds("lk_tit_proc_tab1.rds")
lk_decl     <- .read_rds("lk_decl_tab1.rds")

lk_mun_tab2      <- .read_rds("lk_mun_tab2.rds")
lk_tit_proc_tab2 <- .read_rds("lk_tit_proc_tab2.rds")
lk_decl_tab2     <- .read_rds("lk_decl_tab2.rds")

# ---- Primeira aba ordem colunas + rótulos amigáveis ----
cols_visible <- c(
  "SUBSarrSIM",
  "SUBSarr",
  "PROCESSO",
  "AREA_HA",
  "ANO", "MES",
  "abbrev_state",
  "name_muni",
  "TITULAR",
  "CPF_CNPJcm",
  "NOME_arr",
  "CPF_CNPJarr",
  "VALORarr",
  "VALORtot",
  "PESO_G",
  "PESO_KG",
  "preco_g_orig",
  "corr",
  "PESO_G_final",
  "PESO_KG_final",
  "preco_g_final",
  "FASE",
  "ULT_EV_DES",
  "ULT_EV_DAT",
  "UCname",
  "TIname",
  "QUIname"
)

cols_labels <- c(
  SUBSarrSIM    = "Grupo",
  SUBSarr       = "Substância",
  PROCESSO      = "Processo",
  AREA_HA       = "Área proc.(ha)",
  ANO           = "Ano",
  MES           = "Mês",
  abbrev_state  = "UF",
  name_muni     = "Município",
  TITULAR     = "Titular",
  CPF_CNPJcm    = "CPF-CNPJ (titular)",
  NOME_arr      = "Parte declarante",
  CPF_CNPJarr   = "CPF-CNPJ (declarante)",
  VALORarr      = "Valor Recolhido (R$)",
  VALORtot      = "Valor Total (R$)",
  PESO_G        = "Peso orig (g)",
  PESO_KG       = "Peso orig (Kg)",
  preco_g_orig  = "R$/g (orig)",
  corr          = "Peso corrigido?",
  PESO_G_final  = "Peso final (g)",
  PESO_KG_final = "Peso final (kg)",
  preco_g_final = "R$/g (final)",
  FASE          = "Fase Processo",
  ULT_EV_DES    = "Último evento",
  ULT_EV_DAT    = "Data último evento",
  UCname        = "UC",
  TIname        = "TI",
  QUIname       = "QUI"
)

# ---- Tema ----
primary_color <- "#1B4332"
accent_color  <- "#2D6A4F"
bg_light      <- "#F8F9FA"

theme <- bs_theme(
  version      = 5,
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  primary      = primary_color,
  info         = accent_color,
  bg           = "#ffffff",
  fg           = "#212529"
)

# ---- dropdown ----
picker_opts <- list(
  `actions-box`      = TRUE,
  `live-search`      = TRUE,
  `dropup-auto`      = FALSE,
  `noneSelectedText`   = "Todos",
  `selectedTextFormat` = "count > 2"
)

safe_picker <- function(id, label, choices, select_all_cap = 100,
                        preselect = c("auto","none","all"),
                        selected_keep = NULL) {
  preselect <- match.arg(preselect)
  ch <- as.character(choices)
  ch <- sort(unique(ch[!is.na(ch)]))

  sel_keep <- intersect(as.character(selected_keep %||% character()), ch)

  sel <- switch(
    preselect,
    "none" = NULL,
    "all"  = ch,
    "auto" = if (length(ch) <= select_all_cap) ch else NULL
  )

  if (length(sel_keep)) sel <- sel_keep

  shinyWidgets::pickerInput(
    inputId = id, label = label,
    choices = ch, selected = sel, multiple = TRUE,
    options = picker_opts
  )
}

# ---- Helpers p/ formatação e relatório ----
relatorio_selecao <- function(df, mensal = TRUE, list_cap = 10) {
  if (is.null(df) || nrow(df) == 0) return("Nenhum dado encontrado com os filtros aplicados.")

  # peso: usar SOMENTE o final (sem fallback)
  stopifnot("PESO_KG_final" %in% names(df))
  peso_total <- sum(df$PESO_KG_final, na.rm = TRUE)

  anos <- range(na.omit(df$ANO))

  fmt_num_br <- function(x) format(round(x, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  fmt_cur_br <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
  showv <- function(v) paste(c(utils::head(v, list_cap), if (length(v) > list_cap) "…"), collapse = ", ")

  subs_u <- sort(unique(na.omit(df$SUBSarr)))
  grps_u <- sort(unique(na.omit(df$SUBSarrSIM)))

  # 1) Cabeçalho (sem "Meses")
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

  # 2) Contagem + lista: Processos / Titulares / Declarantes
  proc_u <- sort(unique(na.omit(df$PROCESSO)))
  tit_u  <- sort(unique(na.omit(df$TITULAR)))
  dec_u  <- sort(unique(na.omit(df$NOME_arr)))

  # cada bloco vira um “título” + quebra de linha + itens
  linhas_listas <- c(
    paste0("Processos únicos (", length(proc_u), "):\n  ", showv(proc_u)),
    paste0("Titulares únicos (", length(tit_u), "):\n  ", showv(tit_u)),
    paste0("Partes declarantes únicas (", length(dec_u), "):\n  ", showv(dec_u))
  )


  # 3) Área total (ha): 1 valor por PROCESSO
  area_total <- NA_real_
  n_area_ok  <- 0L
  linha_area <- NULL
  if ("AREA_HA" %in% names(df)) {
    area_info <- df |>
      dplyr::select(PROCESSO, AREA_HA) |>
      dplyr::filter(!is.na(AREA_HA)) |>
      dplyr::group_by(PROCESSO) |>
      dplyr::summarise(area_ha = dplyr::first(AREA_HA), .groups = "drop")  # 1 valor por processo
    area_total <- sum(area_info$area_ha, na.rm = TRUE)
    n_area_ok  <- nrow(area_info)
    linha_area <- if (n_area_ok > 0) {
      paste0("Área total (ha): ", fmt_num_br(area_total), " [", n_area_ok, " processos]")
    } else {
      "Área total (ha): não disponível (sem valores de área na seleção)."
    }
  }

  # 3.1) Relação Kg/ha = (Σ PESO_KG_final) ÷ (Σ área única por PROCESSO)
  linha_ratio <- NULL
  if (is.finite(area_total) && !is.na(area_total) && area_total > 0) {
    kg_ha <- peso_total / area_total
    linha_ratio <- paste0("Relação (Kg/ha): ", fmt_num_br(kg_ha))
  }

  # 4) Totais gerais
  linhas2 <- c(
    paste0("Total Declarações CFEM: ", format(nrow(df), big.mark = ".", decimal.mark = ",")),
    paste0("Total Valor Recolhido: ", fmt_cur_br(sum(df$VALORarr, na.rm = TRUE))),
    paste0("Total Peso declarado (Kg): ", fmt_num_br(peso_total)),
    linha_area,
    linha_ratio
  )

  # 5) Sobreposição / Proximidade
  add_ov <- function(flag, namecol, rotulo) {
    #if (flag %in% names(df) && any(df[[flag]], na.rm = TRUE)) {
    #nomes <- sort(unique(na.omit(df[[namecol]][ df[[flag]] %in% TRUE ])))
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

  # 6) Montagem
  paste(c(
    linhas, "",
    linhas_listas[1], "",
    linhas_listas[2], "",
    linhas_listas[3], "",
    linhas2, "",
    bloco_ov, "",
    bloco_buf
  ), collapse = "\n")

}

# ---- UI ----
ui <- page_navbar(
  title = "Arrecadação de CFEM (2010-2026)",
  theme = theme,
  # CSS adicional ----
  header = tags$head(
    tags$style(HTML("
      /* ... seus estilos CSS existentes ... */
      body { font-size: 12px; color: #212529; }
      h1, h2 { color: #2C3E50; font-weight: 600; }
      h3 { font-size: 16px; font-weight: 600; margin-top: 10px; margin-bottom: 8px; color: #2C3E50; }
      .app-subtitle { font-size: 14px; color: #6c757d; line-height: 1.5; margin-bottom: 8px; }
      .note-text {font-size: 14px;/* aumenta um pouco */color: #6c757d;/* cinza Bootstrap */font-style: italic;/* itálico */margin-top: -4px;margin-bottom: 10px;}
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
      #ov_flags_tab1 .btn, #ov_flags_tab2 .btn, #ov_flags_tab3 .btn, #ov_flags_tab4 .btn { font-size: 11px !important; padding: 2px 10px !important; }
      #mapa_cfem_pma { height: 600px !important; border-radius: 8px; margin-top: 20px; border: 1px solid #dee2e6; }
    "))
  ),

  # ---- Primeira aba - Tabela (mantém) ----
  nav_panel("Tabela de Dados",
            tags$p(
              class = "app-subtitle",
              "Explore os registros mensais da arrecadação da Compensação Financeira pela Exploração Mineral (CFEM) vinculados a processos minerários ativos do SIGMINE/ANM. ",
              "Filtre por substância (grupo/detalhe), fase, UF, município, processo, titular e parte declarante. ",
              "Os valores estão em R$ e as quantidades em kg e g. ",
              "Dados: ",
              tags$a("dados.gov.br/sistema-arrecadacao",
                     href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao",
                     target = "_blank"), ". ",
              "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "
            ),
            tags$p(
              class = "note-text",
              "Nota - Alíquota CFEM utilizada para obtenção da coluna 'Valor Final': até 31/10/2017 (Lei 8.001/1990), adotamos: ouro em PLG = 0,2%; ouro fora de PLG = 2%; diamante em PLG = 0,2%; nióbio = 3%; e 2% para as demais substâncias aqui analisadas. A partir de 01/11/2017 (Lei 13.540/2017), as alíquotas passam a ouro = 1,5%, diamante = 2%, nióbio = 3% e 2% para todas as demais. O valor total é então calculado por 'Valor Arrecadado' ÷ alíquota vigente, por competência (ANO/MÊS) e por substância."
            ),
            fluidRow(
              column(
                width = 3,
                div(
                  class = "filters-card",
                  tags$div(class = "mb-2", tags$strong("Filtros")),

                  pickerInput("subs_tab1",
                              "Substância(s) (grupo):",
                              choices  = subs_all_grupo,
                              selected = "OURO",
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "subs_det_tab1",
                              label    = "Substância(s) (detalhadas):",
                              choices  = subs_all_original,
                              selected = c("OURO", "MINÉRIO DE OURO", "OURO NATIVO"),
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput("fases_tab1",
                              "Fase(s):",
                              choices  = fases_all,
                              selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"),
                              multiple = TRUE,
                              options  = picker_opts),

                  checkboxGroupButtons(inputId = "ov_flags_tab1",
                                       label   = "Territórios Protegidos:",
                                       choices = c("UC"  = "UCov",
                                                   "TI"  = "TIov",
                                                   "QUI" = "QUIov",
                                                   "UC (10 km)"  = "UCov10km",
                                                   "TI (10 km)"  = "TIov10km",
                                                   "QUI (10 km)" = "QUIov10km"),
                                       selected  = c(),
                                       direction = "horizontal",
                                       checkIcon = list(yes = icon("check"),
                                                        no  = icon("minus")),
                                       size   = "sm",
                                       status = "light"),

                  sliderInput("periodo_tab1",
                              "Período (anos):",
                              min   = min(anos_all),
                              max   = max(anos_all),
                              # value = c(min(anos_all),
                              #           max(anos_all)),
                              value = c(2018, 2026),
                              step  = 1,
                              sep = "",
                              ticks = FALSE),

                  pickerInput("ufs_tab1",
                              "UF(s):",
                              choices  = ufs_all,
                              selected = ufs_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "muns_tab1",
                              label    = "Município(s):",
                              choices  = muns_all,
                              multiple = TRUE,
                              options  = picker_opts),
                  pickerInput(inputId  = "procs_tab1",
                              label    = "Processo(s):",
                              choices  = procs_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "tits_tab1",
                              label    = "Titular(es):",
                              choices  = tits_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "decl_tab1",
                              label    = "Parte(s) Declarante(s):",
                              choices  = decl_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  tags$hr(),
                  div(class = "d-grid gap-2 mt-1", actionButton("reset_tab1", "Resetar filtros", class = "btn btn-light btn-sm")),
                  tags$hr(),
                  div(class = "mb-0 d-flex gap-0", downloadButton("baixar_csv", "CSV"), downloadButton("baixar_xlsx", "Excel")),
                  tags$hr(),
                  downloadButton("baixar_pma_sel_tab1",        "PMAs (seleção) .shp",           class = "btn btn-light"),
                  downloadButton("baixar_pma_titular_tab1",    "PMAs (mesmo titular) .shp",     class = "btn btn-light"),
                  downloadButton("baixar_pma_declarante_tab1", "PMAs (mesma declarante) .shp",  class = "btn btn-light")

                )
              ),
              column(width = 9,
                     div(style = "overflow-x: auto;", DTOutput("tabela_dt", height = "100%")),
                     br(),
                     div(
                       class = "summary-box",
                       div(class = "summary-title", "Resumo da seleção"),
                       verbatimTextOutput("relatorio_tab1", placeholder = TRUE)
                     )
              )
            )
  ),

  # ---- Segunda aba - Fluxo Sankey (mantém) ----
  nav_panel("Fluxo Anual de Arrecadação",
            tags$p(
              class = "app-subtitle",
              "Fluxo anual da CFEM (R$ ou kg) entre os níveis: UF → Município → Titular → Processo → Parte declarante. ",
              "Ajuste “Máx. de nós por nível” para manter a legibilidade e refine com os filtros laterais. ",
              "Dados: ",
              tags$a("dados.gov.br/sistema-arrecadacao",
                     href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao",
                     target = "_blank"), ".",
              "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "
            ),
            fluidRow(
              column(
                width = 3,
                div(
                  class = "filters-card", tags$div(class = "mb-2", tags$strong("Filtros")),

                  numericInput("max_nodes_sankey",
                               "Máx. de nós por nível:",
                               value = 10,
                               min   = 5,
                               max   = 200,
                               step  = 5),

                  radioButtons("variavel_fluxo_tab2",
                               "Métrica do fluxo:",
                               choices = c("Valor Recolhido (R$)"    = "VALORarr",
                                           "Quantidade (Kg líquido)" = "PESO_KG_final"),
                               selected = "VALORarr"),

                  pickerInput("subs_tab2",
                              "Substância(s) (grupo):",
                              choices  = subs_all_grupo,
                              selected = "OURO",
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "subs_det_tab2",
                              label    = "Substância(s) (detalhadas):",
                              choices  = subs_all_original,
                              selected = c("OURO", "MINÉRIO DE OURO", "OURO NATIVO"),
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput("fases_tab2",
                              "Fase(s):",
                              choices  = fases_all,
                              multiple = TRUE,
                              selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"),
                              options  = picker_opts),

                  checkboxGroupButtons(inputId = "ov_flags_tab2",
                                       label   = "Territórios Protegidos:",
                                       choices = c("UC"  = "UCov",
                                                   "TI"  = "TIov",
                                                   "QUI" = "QUIov",
                                                   "UC (10 km)"  = "UCov10km",
                                                   "TI (10 km)"  = "TIov10km",
                                                   "QUI (10 km)" = "QUIov10km"),
                                       selected  = c(),
                                       direction = "horizontal",
                                       checkIcon = list(yes = icon("check"),
                                                        no = icon("minus")),
                                       size   = "sm",
                                       status = "light"),

                  sliderInput("periodo_tab2", "Período (anos):",
                              min   = min(anos_all),
                              max   = max(anos_all),
                              value = c(2018, 2026),
                              # value = c(min(anos_all),
                              #           max(anos_all)),
                              step  = 1,
                              sep   = "",
                              ticks = FALSE),

                  pickerInput("ufs_tab2",
                              "UF(s):",
                              choices  = ufs_all,
                              selected = ufs_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "muns_tab2",
                              label    = "Município(s):",
                              choices  = muns_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "procs_tab2",
                              label    = "Processo(s):",
                              choices  = procs_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "tits_tab2",
                              label    = "Titular(es):",
                              choices  = tits_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  pickerInput(inputId  = "decl_tab2",
                              label    = "Parte(s) Declarante(s):",
                              choices  = decl_all,
                              multiple = TRUE,
                              options  = picker_opts),

                  tags$hr(),
                  div(class = "d-grid gap-2 mt-1", actionButton("reset_tab2", "Resetar filtros", class = "btn btn-light btn-sm")),
                  tags$hr(),
                  div(class = "mb-0 d-flex gap-0",
                      downloadButton("baixar_csv_tab2",  "CSV"),
                      downloadButton("baixar_xlsx_tab2", "Excel")
                  ),
                  tags$hr(),
                  downloadButton("baixar_pma_sel_tab2",        "PMAs (seleção) .shp",           class = "btn btn-light"),
                  downloadButton("baixar_pma_titular_tab2",    "PMAs (mesmo titular) .shp",     class = "btn btn-light"),
                  downloadButton("baixar_pma_declarante_tab2", "PMAs (mesma declarante) .shp",  class = "btn btn-light")

                )
              ),
              column(width = 9, sankeyNetworkOutput("sankeyPlot", height = "800px"),
                     br(),
                     div(
                       class = "summary-box",
                       div(class = "summary-title", "Resumo da seleção"),
                       verbatimTextOutput("relatorio_tab2", placeholder = TRUE)
                     ))
            )
  ),

  # ---- Terceira aba – Série Temporal (agora só com os gráficos) ----
  nav_panel(
    "Série Temporal e Mapa Processos Minerários",
    tags$p(
      class = "app-subtitle",
      "Série mensal da CFEM (R$ ou kg) conforme os filtros. ",
      "Veja a curva geral ou separe por Processo, Titular, Parte Declarante, Substância, Grupo ou Fase. ",
      "Defina o intervalo de anos e meses; pontos acima de 1,5×IQR são destacados como outliers. ",
      "Dados: ",
      tags$a("dados.gov.br/sistema-arrecadacao",
             href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao",
             target = "_blank"), ".",
      "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "
    ),
    tags$p(
      class = "note-text",
      "Nota: os pontos destacados como outliers são calculados por grupo (Processo, Titular, etc.) ",
      "com base no critério do boxplot: valores acima de Q3 + 1,5 × IQR são considerados atípicos. ",
      "Quando o intervalo interquartílico (IQR) é nulo, aplica-se um ajuste usando o desvio padrão da série."
    ),
    fluidRow(
      column(
        width = 3,
        div(
          class = "filters-card", tags$div(class = "mb-2", tags$strong("Filtros")),

          radioButtons("variavel_fluxo_tab3",
                       "Métrica do fluxo:",
                       choices  = c("Valor Recolhido (R$)"   = "VALORarr",
                                    "Quantidade (Kg líquido)" = "PESO_KG_final"),
                       selected = "VALORarr"),

          pickerInput("subs_tab3",
                      "Substância(s) (grupo):",
                      choices  = subs_all_grupo,
                      selected = "OURO",
                      multiple = TRUE,
                      options  = picker_opts),

          pickerInput("subs_det_tab3",
                      "Substância(s) (detalhadas):",
                      choices  = subs_all_original,
                      selected = c("OURO", "OURO NATIVO", "MINÉRIO DE OURO"),
                      multiple = TRUE,
                      options  = picker_opts),

          pickerInput("fases_tab3",
                      "Fase(s):",
                      choices  = fases_all,
                      selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"),
                      multiple = TRUE,
                      options  = picker_opts),

          checkboxGroupButtons(inputId = "ov_flags_tab3",
                               label   = "Territórios Protegidos:",
                               choices = c("UC"  = "UCov",
                                           "TI"  = "TIov",
                                           "QUI" = "QUIov",
                                           "UC (10 km)"   = "UCov10km",
                                           "TI (10 km)"   = "TIov10km",
                                           "QUI (10 km)"  = "QUIov10km"),
                               selected  = c(), direction = "horizontal",
                               checkIcon = list(yes = icon("check"),
                                                no  = icon("minus")),
                               size   = "sm",
                               status = "light"),

          selectInput("agrupamento_tab3",
                      "Visualiza linhas por:",
                      choices = c("Geral"            = "geral",
                                  "Processo"         = "PROCESSO",
                                  "Titular"          = "TITULAR",
                                  "Parte Declarante" = "NOME_arr",
                                  "Substância"       = "SUBSarr",
                                  "Grupo (subs)"     = "SUBSarrSIM",
                                  "Fase"             = "FASE")),

          sliderInput("periodo_tab3",
                      "Período (anos):",
                      min = min(anos_all),
                      max = max(anos_all),
                      value = c(2018, 2026),
                      #value = c(min(anos_all),
                      #          max(anos_all)),
                      step = 1,
                      sep = "",
                      ticks = FALSE),

          sliderInput("meses_tab3",
                      "Meses:",
                      min   = 1,
                      max   = 12,
                      value = c(1, 12),
                      step  = 1,
                      sep   = "",
                      ticks = FALSE),

          pickerInput("ufs_tab3",
                      "UF(s):",
                      choices  = ufs_all,
                      selected = ufs_all,
                      multiple = TRUE,
                      options  = picker_opts),

          pickerInput(inputId  = "muns_tab3",
                      label    = "Município(s):",
                      choices  = muns_all,
                      multiple = TRUE,
                      options  = picker_opts),

          pickerInput(inputId  = "procs_tab3",
                      label    = "Processo(s):",
                      choices  = procs_all,
                      multiple = TRUE,
                      options  = picker_opts),

          pickerInput(inputId  = "tits_tab3",
                      label    = "Titular(es):",
                      choices  = tits_all,
                      multiple = TRUE,
                      options  = picker_opts),

          pickerInput(inputId  = "decl_tab3",
                      label    = "Parte(s) Declarante(s):",
                      choices  = decl_all,
                      multiple = TRUE,
                      options  = picker_opts),

          tags$hr(),
          div(class = "d-grid gap-2 mt-1", actionButton("reset_tab3", "Resetar filtros", class = "btn btn-light btn-sm")),
          tags$hr(),
          div(class = "mt-2 mb-2 d-flex gap-0",
              downloadButton("baixar_csv_tab3",  "CSV"),
              downloadButton("baixar_xlsx_tab3", "Excel")
          ),
          tags$hr(),
          downloadButton("baixar_pma_sel_tab3",        "Download PMAs (seleção) .shp",  class = "btn btn-light"),
          downloadButton("baixar_pma_titular_tab3",    "Download PMAs (mesmo titular) .shp",  class = "btn btn-light"),
          downloadButton("baixar_pma_declarante_tab3", "Download PMAs (mesma declarante) .shp", class = "btn btn-light")
        )
      ),
      column(width = 9,
             leafletOutput("mapa_cfem_pma_tab3", height = "525px"),
             br(),
             plotlyOutput("serie_temporal",   height = "400px"),
             br(),
             plotlyOutput("grafico_outliers", height = "400px"),
             br(),
             div(
               class = "summary-box",
               div(class = "summary-title", "Resumo da seleção"),
               verbatimTextOutput("relatorio_tab3", placeholder = TRUE)
             )
      )
    )
  )
)
#,
#   # ---- Quarta aba – Mapa (NOVA) ----
#   nav_panel(
#     "Mapa de Processos",
#     tags$p(
#       class = "app-subtitle",
#       "Mapa dos processos minerários compatíveis com os filtros da CFEM. ",
#       "Use o controle do mapa para exibir camadas adicionais (Unidades de Conservação, Terras Indígenas e Comunidades Quilombolas). ",
#       "Clique em um polígono para ver detalhes; a visão ajusta automaticamente aos resultados. ",
#       "Base: CartoDB / Satélite. Fonte: SIGMINE/ANM e Sistema de Arrecadação (download em ",
#       data_atualizacao, ")."
#     ),
#     fluidRow(
#       column(
#         width = 3,
#         div(
#           class = "filters-card", tags$div(class = "mb-2", tags$strong("Filtros")),
#
#           pickerInput("subs_tab4",
#                       "Substância(s) (grupo):",
#                       choices  = subs_all_grupo,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           pickerInput("subs_det_tab4",
#                       "Substância(s) (detalhadas):",
#                       choices  = subs_all_original,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           pickerInput("fases_tab4",
#                       "Fase(s):",
#                       choices  = fases_all,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           sliderInput("periodo_tab4",
#                       "Período (anos):",
#                       min   = min(anos_all),
#                       max   = max(anos_all),
#                       value = c(min(anos_all),
#                                 max(anos_all)),
#                       step  = 1,
#                       sep   = "",
#                       ticks = FALSE),
#
#           pickerInput("ufs_tab4",
#                       "UF(s):",
#                       choices  = ufs_all,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           pickerInput(inputId  = "muns_tab4",
#                       label    = "Município(s):",
#                       choices  = muns_all,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           checkboxGroupButtons(inputId = "ov_flags_tab4",
#                                label   = "Territórios Protegidos:",
#                                choices = c("UC"  = "UCov",
#                                            "TI"  = "TIov",
#                                            "QUI" = "QUIov",
#                                            "UC (10 km)"  = "UCov10km",
#                                            "TI (10 km)"  = "TIov10km",
#                                            "QUI (10 km)" = "QUIov10km"),
#                                selected  = c(),
#                                direction = "horizontal",
#                                checkIcon = list(yes = icon("check"),
#                                                 no  = icon("minus")),
#                                size   = "sm",
#                                status = "light"),
#
#           pickerInput(inputId  = "procs_tab4",
#                       label    = "Processo(s):",
#                       choices  = procs_all,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           pickerInput(inputId  = "tits_tab4",
#                       label    = "Titular(es):",
#                       choices  = tits_all,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           pickerInput(inputId  = "decl_tab4",
#                       label    = "Parte(s) Declarante(s):",
#                       choices  = decl_all,
#                       multiple = TRUE,
#                       options  = picker_opts),
#
#           tags$hr(),
#           actionButton("reset_tab4", "Resetar filtros", class = "btn btn-outline-dark w-100 mt-2")
#         )
#       ),
#       column(width = 9,
#              leafletOutput("mapa_cfem_pma", height = "900px")
#       )
#     )
#   )
# ---- SERVER ----
server <- function(input, output, session) {

  # ---- helpers ----
  filter_in <- function(df, col, sel) {
    # se o picker estiver vazio (NULL ou length 0), não filtra
    if (is.null(sel) || length(sel) == 0) return(df)
    # caso contrário, aplica o filtro
    df[df[[col]] %in% sel, , drop = FALSE]
  }

  # ---- Utilitário para sincronizar par grupo <-> detalhe, com trava ----
  sync_pair <- function(session, id_group, id_detail, map_df, col_group, col_detail) {
    lock <- reactiveVal(FALSE)

    # group -> detail
    observeEvent(input[[id_group]], {
      if (lock()) return()
      lock(TRUE); on.exit(lock(FALSE), add = TRUE)
      g_sel <- input[[id_group]]
      valid_choices <- map_df |>
        dplyr::filter(.data[[col_group]] %in% g_sel) |>
        dplyr::pull(.data[[col_detail]]) |>
        unique() |>
        sort()
      updatePickerInput(session, id_detail,
                        choices = valid_choices,
                        selected = intersect(isolate(input[[id_detail]]), valid_choices))
    }, ignoreInit = TRUE)

    # detail -> group
    observeEvent(input[[id_detail]], {
      if (lock()) return()
      lock(TRUE); on.exit(lock(FALSE), add = TRUE)
      d_sel <- input[[id_detail]]
      if (!length(d_sel)) return()

      parent_choices <- map_df |>
        dplyr::filter(.data[[col_detail]] %in% d_sel) |>
        dplyr::pull(.data[[col_group]]) |>
        unique() |>
        sort()
      updatePickerInput(session, id_group,
                        choices = sort(unique(map_df[[col_group]])),
                        selected = parent_choices)
    }, ignoreInit = TRUE)
  }

  filtra_sobrepos <- function(df, flags) {
    if (length(flags) == 0) return(df)
    cols_ok <- intersect(flags, names(df))
    if (length(cols_ok) == 0) return(df)
    df |> dplyr::filter(rowSums(dplyr::across(dplyr::all_of(cols_ok), ~ dplyr::coalesce(.x, 0))) >= 1)
  }

  # ---- Exportar shp
  exportar_shapefile <- function(sf_obj, nome_base, temp_dir) {
    stopifnot(inherits(sf_obj, "sf"))
    if (nrow(sf_obj) == 0) stop("Sem feições para exportar.")

    path_base <- file.path(temp_dir, nome_base)

    # opcional: garantir CRS no .prj
    if (is.na(sf::st_crs(sf_obj))) {
      warning("Objeto sf sem CRS definido; o .prj pode sair vazio.")
    }

    # sf::st_write(sf_obj, paste0(path_base, ".shp"),
    #              delete_dsn = TRUE, quiet = TRUE)

    sf::st_write(sf_obj, paste0(path_base, ".shp"),
                 delete_layer = TRUE, quiet = TRUE)


    arquivos <- list.files(temp_dir,
                           pattern = paste0("^", nome_base, "\\.(shp|shx|dbf|prj|cpg|qml|qpj)$"),
                           full.names = TRUE)
    zipfile  <- file.path(temp_dir, paste0(nome_base, ".zip"))
    zip::zip(zipfile, files = arquivos, mode = "cherry-pick")
    zipfile
  }

  # ---- Lógica da primeira aba (Tabela) ----
  sync_pair(session, "subs_tab1", "subs_det_tab1", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab1, input$subs_det_tab1, input$ufs_tab1, input$fases_tab1, input$periodo_tab1), {
    df_temp <- lk_mun |>
      dplyr::filter(
        ANO >= input$periodo_tab1[1], ANO <= input$periodo_tab1[2],
        FASE %in% input$fases_tab1, abbrev_state %in% input$ufs_tab1
      )
    if (length(input$subs_det_tab1)) { df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab1) } else {
      df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab1) }
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
      dplyr::filter(
        abbrev_state %in% input$ufs_tab1, name_muni %in% input$muns_tab1,
        TITULAR %in% input$tits_tab1, PROCESSO %in% input$procs_tab1
      )
    decl_ok <- sort(unique(df_temp$NOME_arr))
    updatePickerInput(session, "decl_tab1", choices = decl_ok,
                      selected = intersect(isolate(input$decl_tab1), decl_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  dados_filtrados <- reactive({
    showNotification("Filtrando dados...", duration = 1, type = "default")
    df <- cfem |>
      dplyr::filter(
        ANO >= input$periodo_tab1[1], ANO <= input$periodo_tab1[2],
        FASE %in% input$fases_tab1, abbrev_state %in% input$ufs_tab1
      )
    if (length(input$subs_det_tab1)) { df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab1) } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab1) }
    if (length(input$muns_tab1)) df  <- df |> dplyr::filter(name_muni %in% input$muns_tab1)
    if (length(input$procs_tab1)) df <- df |> dplyr::filter(PROCESSO  %in% input$procs_tab1)
    if (length(input$tits_tab1)) df  <- df |> dplyr::filter(TITULAR %in% input$tits_tab1)
    if (length(input$decl_tab1)) df  <- df |> dplyr::filter(NOME_arr  %in% input$decl_tab1)
    df <- filtra_sobrepos(df, flags  = input$ov_flags_tab1)
    df
  }) |> bindCache(
    input$periodo_tab1, input$subs_tab1, input$subs_det_tab1, input$ufs_tab1,input$muns_tab1, input$fases_tab1,
    input$procs_tab1, input$tits_tab1, input$decl_tab1, input$ov_flags_tab1
  ) |> debounce(250)

  observeEvent(input$reset_tab1, {
    updatePickerInput(session, "subs_tab1", choices  = subs_all_grupo, selected = subs_all_grupo)
    updateSliderInput(session, "periodo_tab1", value = c(min(anos_all), max(anos_all)))
    updatePickerInput(session, "ufs_tab1", choices   = ufs_all, selected   = ufs_all)
    updatePickerInput(session, "fases_tab1", choices = fases_all, selected = fases_all)
    updateCheckboxGroupButtons(session, "ov_flags_tab1", selected = c())
    updatePickerInput(session, "subs_det_tab1", choices = subs_all_original, selected = subs_all_original)
    updatePickerInput(session, "muns_tab1", choices     = muns_all, selected  = muns_all)
    updatePickerInput(session, "tits_tab1", choices     = tits_all, selected  = tits_all)
    updatePickerInput(session, "procs_tab1", choices    = procs_all, selected = character(0))
    updatePickerInput(session, "decl_tab1", choices     = decl_all, selected  = decl_all)
  })

  output$tabela_dt <- renderDT({
    df <- dados_filtrados()
    validate(need(nrow(df) > 0, "Nenhum dado encontrado com os filtros aplicados."))

    cols_keep   <- intersect(cols_visible, names(df))
    df_display  <- df[, cols_keep, drop = FALSE]
    names(df_display) <- unname(cols_labels[cols_keep])

    if ("Peso corrigido?" %in% names(df_display)) {
      df_display[["Peso corrigido?"]] <- tolower(as.character(df_display[["Peso corrigido?"]]))
    } else {
      df_display[["Peso corrigido?"]] <- NA_character_
    }

    # colunas p somar
    num_cols <- intersect(
      c("Peso orig (g)", "Peso orig (Kg)", "Peso final (g)", "Peso final (kg)", "Valor Recolhido (R$)", "Valor Total (R$)"),
      names(df_display)
    )

    # Totais (NA -> 0)
    totals_raw <- vapply(num_cols, function(nm) sum(df_display[[nm]], na.rm = TRUE), numeric(1))

    # Formatação BR
    fmt_num <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    fmt_cur <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))

    totals_fmt <- setNames(
      ifelse(grepl("\\(R\\$\\)", names(totals_raw)), fmt_cur(totals_raw), fmt_num(totals_raw)),
      names(totals_raw)
    )

    # Texto da primeira célula
    first_label <- sprintf("TOTAL (linhas: %s)", nrow(df_display))

    # Cabeçalho com 2 linhas: 1) totais, 2) nomes
    sketch <- htmltools::withTags(table(
      thead(
        tr(
          lapply(seq_along(df_display), function(i) {
            nm <- names(df_display)[i]
            val <- if (i == 1) first_label else if (nm %in% names(totals_fmt)) totals_fmt[[nm]] else ""
            th(style = "background:#F8F9FA;font-weight:700;", val)
          })
        ),
        tr( lapply(names(df_display), th) )
      )
    ))

    wrap_cols <- c("Titular", "Parte declarante", "UC", "TI")
    wrap_idx  <- which(names(df_display) %in% wrap_cols) - 1

    # cria o datatable
    dt_obj <- datatable(
      df_display,
      container    = sketch,
      extensions   = c("Scroller"),
      rownames     = FALSE,
      class        = "compact",
      options      = list(
        scrollX    = TRUE,
        dom        = 'ftip',
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25','50','100','Tudo')),
        columnDefs = list(
          list(targets = "_all", className = "dt-left"),
          list(targets = wrap_idx, className = "dt-wrap"),
          list(targets = wrap_idx, width = "260px")
        ),
        autoWidth   = TRUE,
        deferRender = TRUE
      )
    ) |>
      formatCurrency("Valor Recolhido (R$)", currency = "R$ ", digits = 2) |>
      formatCurrency("Valor Total (R$)",     currency = "R$ ", digits = 2) |>
      formatRound("Peso orig (g)", digits = 2) |>
      formatRound("Peso orig (Kg)", digits = 2) |>
      formatRound("Peso final (g)", digits = 2) |>
      formatRound("Peso final (kg)", digits = 2) |>
      formatRound("R$/g (orig)", digits = 1) |>
      formatRound("R$/g (final)", digits = 1)

    lv <- setdiff(unique(df_display[["Peso corrigido?"]]), "original")
    dt_obj <- dt_obj |> formatStyle(
      columns = names(df_display),
      valueColumns = "Peso corrigido?",
      backgroundColor = styleEqual(lv, rep("rgba(255,250,205,0.9)", length(lv)))
    )
    dt_obj
  }, server = TRUE)

  # --- [ADD] Relatório de seleção (Aba 1) ---
  output$relatorio_tab1 <- renderText({
    relatorio_selecao(dados_filtrados(), mensal = TRUE)
  })

  proxy_tabela <- DT::dataTableProxy("tabela_dt")
  observeEvent(dados_filtrados(), {
    DT::reloadData(proxy_tabela, resetPaging = TRUE)
  }, ignoreInit = TRUE)


  # --------- Export Helpers para exportar ---------

  prep_export <- function() {
    df <- dados_filtrados()
    cols_keep <- intersect(cols_visible, names(df))
    df_export <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }

  prep_export_tab2 <- function() {
    # usa o df FILTRADO da aba 2 (anual)
    df <- dados_selecionados_sankey()
    validate(need(nrow(df) > 0, "Nenhum dado para exportar (aba 2)."))

    cols_keep   <- intersect(cols_visible, names(df))
    df_export   <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }

  prep_export_tab3 <- function() {
    # usa o df FILTRADO da aba 3 (mensal)
    df <- dados_mensal()
    validate(need(nrow(df) > 0, "Nenhum dado para exportar (aba 3)."))

    cols_keep   <- intersect(cols_visible, names(df))
    df_export   <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }

  # ---------- Downloads CSV/XLSX (Aba 1 - Original)
  output$baixar_csv <- downloadHandler(filename  = function() paste0("cfem_filtrado_", Sys.Date(), ".csv"),
                                       content   = function(file) { readr::write_csv(prep_export(), file) })
  output$baixar_xlsx <- downloadHandler(filename = function() paste0("cfem_filtrado_", Sys.Date(), ".xlsx"),
                                        content  = function(file) { write_xlsx(prep_export(), path = file) })
  # ---------- Downloads CSV/XLSX (Aba 2 - Anual)
  output$baixar_csv_tab2 <- downloadHandler(
    filename = function() paste0("cfem_anual_filtrado_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(prep_export_tab2(), file)
  )

  output$baixar_xlsx_tab2 <- downloadHandler(
    filename = function() paste0("cfem_anual_filtrado_", Sys.Date(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(prep_export_tab2(), path = file)
  )

  # ---------- Downloads CSV/XLSX (Aba 3 - Mensal)
  output$baixar_csv_tab3 <- downloadHandler(
    filename = function() paste0("cfem_mensal_filtrado_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(prep_export_tab3(), file)
  )

  output$baixar_xlsx_tab3 <- downloadHandler(
    filename = function() paste0("cfem_mensal_filtrado_", Sys.Date(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(prep_export_tab3(), path = file)
  )


  # ---------- PMA para a ABA 1
  # .pick_pma_src_tab1 <- function(n) {
  #   MAX_FULL <- 2000L
  #   if (is.na(n) || n <= MAX_FULL) pma_full else pma_simpl
  # }

  # processos selecionados pelos filtros da Aba 1
  procs_sel_tab1 <- reactive({
    unique(dados_filtrados()$PROCESSO)
  }) |> bindCache(dados_filtrados()$PROCESSO)

  # titulares e declarantes correntes (pelos filtros da Aba 1)
  tits_sel_tab1  <- reactive({ unique(dados_filtrados()$TITULAR) }) |> bindCache(dados_filtrados()$TITULAR)
  decl_sel_tab1  <- reactive({ unique(dados_filtrados()$NOME_arr)   }) |> bindCache(dados_filtrados()$NOME_arr)

  # PMAs da seleção (PROCESSO ∈ procs_sel_tab1)
  pma_sel_tab1 <- reactive({
    procs <- procs_sel_tab1()
    # src   <- .pick_pma_src_tab1(length(procs))
    # if (!length(procs)) return(src[0, ])
    # dplyr::filter(src, PROCESSO %in% procs)
    src <- pma_simpl
    if (!length(procs)) return(src[0, ])
    dplyr::filter(src, PROCESSO %in% procs)
  }) |> bindCache(procs_sel_tab1())

  # PMAs do mesmo titular (exclui os já na seleção para não duplicar)
  pma_titular_tab1 <- reactive({
    tits  <- tits_sel_tab1()
    procs <- procs_sel_tab1()
    # usa a mesma heurística de fonte
    # src   <- .pick_pma_src_tab1(length(tits))
    # if (!length(tits)) return(src[0, ])
    # dplyr::filter(src, TITULARcm %in% tits, !(PROCESSO %in% procs))
    src <- pma_simpl
    if (!length(tits)) return(src[0, ])
    dplyr::filter(src, TITULAR %in% tits, !(PROCESSO %in% procs))
  }) |> bindCache(tits_sel_tab1(), procs_sel_tab1())

  # PMAs vinculados às mesmas partes declarantes
  pma_declarante_tab1 <- reactive({
    declarantes <- decl_sel_tab1()
    if (!length(declarantes)) {
      src <- pma_simpl
      return(src[0, ])
    }

    procs_declarantes <- cfem |>
      dplyr::filter(NOME_arr %in% declarantes) |>
      dplyr::pull(PROCESSO) |>
      unique()
    # src <- .pick_pma_src_tab1(length(procs_declarantes))
    # dplyr::filter(src, PROCESSO %in% procs_declarantes)
    src <- pma_simpl
    dplyr::filter(src, PROCESSO %in% procs_declarantes)
  }) |> bindCache(decl_sel_tab1())

  # ---------- Downloads .shp (Aba 1)
  output$baixar_pma_sel_tab1 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab1_", Sys.Date(), ".zip"),
    content  = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_sel_tab1(), "pmas_selecao_tab1", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
    }
  )

  output$baixar_pma_titular_tab1 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab1_", Sys.Date(), ".zip"),
    content  = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_titular_tab1(), "pmas_titular_tab1", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
    }
  )

  output$baixar_pma_declarante_tab1 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab1_", Sys.Date(), ".zip"),
    content  = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_declarante_tab1(), "pmas_declarante_tab1", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
    }
  )


  # ---- Lógica da segunda aba (Fluxo Sankey) ----
  sync_pair(session, "subs_tab2", "subs_det_tab2", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab2, input$subs_det_tab2, input$ufs_tab2, input$fases_tab2, input$periodo_tab2), {
    df_temp <- lk_mun_tab2 |>
      dplyr::filter(ANO >= input$periodo_tab2[1], ANO <= input$periodo_tab2[2], FASE %in% input$fases_tab2, abbrev_state %in% input$ufs_tab2)
    if (length(input$subs_det_tab2)) { df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab2) } else { df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab2) }
    muns_ok <- sort(unique(df_temp$name_muni))
    updatePickerInput(session, "muns_tab2", choices = muns_ok, selected = intersect(isolate(input$muns_tab2), muns_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$muns_tab2, input$tits_tab2, input$ufs_tab2), {
    df_temp <- lk_tit_proc_tab2 |> dplyr::filter(abbrev_state %in% input$ufs_tab2, name_muni %in% input$muns_tab2)
    tits_ok <- sort(unique(df_temp$TITULAR)); procs_ok <- sort(unique(df_temp$PROCESSO))
    updatePickerInput(session, "tits_tab2", choices = tits_ok, selected = intersect(isolate(input$tits_tab2), tits_ok))
    updatePickerInput(session, "procs_tab2", choices = procs_ok, selected = intersect(isolate(input$procs_tab2), procs_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$procs_tab2, input$tits_tab2), {
    df_temp <- lk_decl_tab2 |> dplyr::filter(abbrev_state %in% input$ufs_tab2, name_muni %in% input$muns_tab2, TITULAR %in% input$tits_tab2, PROCESSO %in% input$procs_tab2)
    decl_ok <- sort(unique(df_temp$NOME_arr))
    updatePickerInput(session, "decl_tab2", choices = decl_ok, selected = intersect(isolate(input$decl_tab2), decl_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  dados_selecionados_sankey <- reactive({
    showNotification("Atualizando Sankey...", duration = 1, type = "default")
    df <- cfem_anual |>
      dplyr::filter(
        ANO >= input$periodo_tab2[1], ANO <= input$periodo_tab2[2], FASE %in% input$fases_tab2, abbrev_state %in% input$ufs_tab2
      )
    if (length(input$subs_det_tab2)) { df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab2) } else { df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab2) }
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
    tot <- df |> dplyr::group_by(.data[[col]]) |> dplyr::summarise(total = sum(valor_usado, na.rm = TRUE), .groups = "drop") |> dplyr::arrange(dplyr::desc(total))
    keep <- head(tot[[col]], top_n)
    df[[col]] <- ifelse(df[[col]] %in% keep, df[[col]], label_outros)
    df
  }

  output$sankeyPlot <- renderSankeyNetwork({
    dados <- dados_selecionados_sankey()
    if (nrow(dados) == 0) { showNotification("Nenhum fluxo encontrado com os filtros aplicados.", type = "warning"); return(NULL) }
    dados <- dados |> dplyr::group_by(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr) |> dplyr::summarise(valor_usado = sum(.data[[input$variavel_fluxo_tab2]], na.rm = TRUE), .groups = "drop") |> dplyr::filter(!is.na(NOME_arr), valor_usado > 0)
    top_n <- req(input$max_nodes_sankey)
    dados <- dados |>
      collapse_level("abbrev_state", top_n, "Outros — UFs") |> collapse_level("name_muni", top_n, "Outros — Municípios") |>
      collapse_level("TITULAR", top_n, "Outros — Titulares") |> collapse_level("PROCESSO", top_n, "Outros — Processos") |>
      collapse_level("NOME_arr", top_n, "Outros — Partes")

    dados2 <- dados |>
      mutate(UF  = paste0(abbrev_state),
             MUN = paste0(name_muni),
             TIT = paste0(TITULAR),
             PROC= paste0(PROCESSO),
             DEC = paste0(NOME_arr))

    nodes <- data.frame(name = c(
      sort(unique(dados2$UF)),
      sort(unique(dados2$MUN)),
      sort(unique(dados2$TIT)),
      sort(unique(dados2$PROC)),
      sort(unique(dados2$DEC))
    ))

    criar_links <- function(df, a, b) df |>
      dplyr::group_by(.data[[a]], .data[[b]]) |>
      dplyr::summarise(value = sum(valor_usado, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(source = match(.data[[a]], nodes$name) - 1,
                    target = match(.data[[b]], nodes$name) - 1)

    links <- dplyr::bind_rows(
      criar_links(dados2, "UF","MUN"),
      criar_links(dados2, "MUN","TIT"),
      criar_links(dados2, "TIT","PROC"),
      criar_links(dados2, "PROC","DEC")
    )

    sankeyNetwork(
      Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value",
      NodeID = "name", fontSize = 14, nodeWidth = 50, nodePadding = 50, sinksRight = FALSE
    )
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

  # ---------- PMA para a ABA 2

  # mesma heurística da aba 1 p/ escolher fonte FULL/SIMPL
  # .pick_pma_src_tab2 <- function(n) {
  #   MAX_FULL <- 2000L
  #   if (is.na(n) || n <= MAX_FULL) pma_full else pma_simpl
  # }

  # Seleções atuais vindas do filtro anual da Aba 2
  procs_sel_tab2 <- reactive({
    unique(dados_selecionados_sankey()$PROCESSO)
  }) |> bindCache(dados_selecionados_sankey()$PROCESSO)

  tits_sel_tab2 <- reactive({
    unique(dados_selecionados_sankey()$TITULAR)
  }) |> bindCache(dados_selecionados_sankey()$TITULAR)

  decl_sel_tab2 <- reactive({
    unique(dados_selecionados_sankey()$NOME_arr)
  }) |> bindCache(dados_selecionados_sankey()$NOME_arr)

  # PMAs da seleção (PROCESSO ∈ procs_sel_tab2)
  pma_sel_tab2 <- reactive({
    procs <- procs_sel_tab2()
    src   <- .pick_pma_src_tab2(length(procs))
    if (!length(procs)) return(src[0, ])
    dplyr::filter(src, PROCESSO %in% procs)
  }) |> bindCache(procs_sel_tab2())

  # PMAs do mesmo titular (exclui os já selecionados)
  pma_titular_tab2 <- reactive({
    tits  <- tits_sel_tab2()
    procs <- procs_sel_tab2()
    src   <- .pick_pma_src_tab2(length(tits))
    if (!length(tits)) return(src[0, ])
    dplyr::filter(src, TITULAR %in% tits, !(PROCESSO %in% procs))
  }) |> bindCache(tits_sel_tab2(), procs_sel_tab2())

  # PMAs relacionados às mesmas partes declarantes
  pma_declarante_tab2 <- reactive({
    declarantes <- decl_sel_tab2()
    if (!length(declarantes)) {
      src <- pma_simpl
      return(src[0, ])
    }
    # usa a base CFEM completa p/ pegar todos os processos dos declarantes
    procs_declarantes <- cfem |>
      dplyr::filter(NOME_arr %in% declarantes) |>
      dplyr::pull(PROCESSO) |>
      unique()
    src <- .pick_pma_src_tab2(length(procs_declarantes))
    dplyr::filter(src, PROCESSO %in% procs_declarantes)
  }) |> bindCache(decl_sel_tab2())

  # ---------- Downloads .shp (Aba 2)
  output$baixar_pma_sel_tab2 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab2_", Sys.Date(), ".zip"),
    content  = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_sel_tab2(), "pmas_selecao_tab2", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
    }
  )

  output$baixar_pma_titular_tab2 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab2_", Sys.Date(), ".zip"),
    content  = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_titular_tab2(), "pmas_titular_tab2", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
    }
  )

  output$baixar_pma_declarante_tab2 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab2_", Sys.Date(), ".zip"),
    content  = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_declarante_tab2(), "pmas_declarante_tab2", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
    }
  )

  # --- [ADD] Relatório de seleção (Aba 2 - anual) ---
  output$relatorio_tab2 <- renderText({
    base <- relatorio_selecao(dados_selecionados_sankey(), mensal = FALSE)
    metrica <- if (input$variavel_fluxo_tab2 == "VALORarr") "Valor Recolhido (R$)" else "Quantidade (Kg líquido)"
    paste0(base, "\n\nMétrica no Sankey: ", metrica)
  })

  # ---- Lógica da Terceira Aba: Série Temporal ----
  sync_pair(session, "subs_tab3", "subs_det_tab3", map_subs, "SUBSarrSIM", "SUBSarr")

  # Lógica de filtros encadeados para a aba 3
  observeEvent(list(input$subs_tab3, input$subs_det_tab3, input$ufs_tab3, input$fases_tab3, input$periodo_tab3, input$meses_tab3), {
    df_temp <- cfem_mensal |>
      dplyr::filter(
        ANO >= input$periodo_tab3[1], ANO <= input$periodo_tab3[2],
        FASE %in% input$fases_tab3, MES >= input$meses_tab3[1], MES <= input$meses_tab3[2],
        abbrev_state %in% input$ufs_tab3
      )
    if (length(input$subs_det_tab3)) {
      df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab3) } else {
        df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab3) }

    updatePickerInput(session, "muns_tab3",
                      choices = sort(unique(df_temp$name_muni)),
                      selected = intersect(isolate(input$muns_tab3),
                                           sort(unique(df_temp$name_muni))))
    rm(df_temp)
    gc()
  }, ignoreInit = FALSE)

  observeEvent(list(input$muns_tab3, input$ufs_tab3), {
    df_temp <- cfem_mensal |>
      filter_in("abbrev_state", input$ufs_tab3) |>
      filter_in("name_muni",    input$muns_tab3)

    updatePickerInput(session, "tits_tab3",
                      choices  = sort(unique(df_temp$TITULAR)),
                      selected = intersect(isolate(input$tits_tab3), sort(unique(df_temp$TITULAR)))
    )
    updatePickerInput(session, "procs_tab3",
                      choices  = sort(unique(df_temp$PROCESSO)),
                      selected = intersect(isolate(input$procs_tab3), sort(unique(df_temp$PROCESSO)))
    )
    rm(df_temp)
    gc()
  }, ignoreInit = FALSE)

  observeEvent(list(input$procs_tab3, input$tits_tab3, input$ufs_tab3, input$muns_tab3), {
    df_temp <- cfem_mensal |>
      filter_in("abbrev_state", input$ufs_tab3) |>
      filter_in("name_muni",    input$muns_tab3) |>
      filter_in("TITULAR",    input$tits_tab3) |>
      filter_in("PROCESSO",     input$procs_tab3)

    updatePickerInput(session, "decl_tab3",
                      choices  = sort(unique(df_temp$NOME_arr)),
                      selected = intersect(isolate(input$decl_tab3), sort(unique(df_temp$NOME_arr)))
    )
    rm(df_temp)
    gc()
  }, ignoreInit = FALSE)

  # Reactive principal para os gráficos
  dados_mensal <- reactive({
    df <- cfem_mensal |>
      dplyr::filter(
        ANO >= input$periodo_tab3[1], ANO <= input$periodo_tab3[2],
        FASE %in% input$fases_tab3,
        MES >= input$meses_tab3[1], MES <= input$meses_tab3[2],
        abbrev_state %in% input$ufs_tab3
      )
    if (length(input$subs_det_tab3)) { df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab3) } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab3) }
    if (length(input$muns_tab3)) df <- df |> dplyr::filter(name_muni %in% input$muns_tab3)
    if (length(input$tits_tab3)) df <- df |> dplyr::filter(TITULAR %in% input$tits_tab3)
    if (length(input$procs_tab3)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab3)
    if (length(input$decl_tab3)) df <- df |> dplyr::filter(NOME_arr %in% input$decl_tab3)
    df <- filtra_sobrepos(df, flags = input$ov_flags_tab3)
    df
  }) |> bindCache(input$periodo_tab3, input$meses_tab3, input$fases_tab3, input$ufs_tab3,
                  input$subs_tab3, input$subs_det_tab3, input$muns_tab3, input$tits_tab3, input$procs_tab3,
                  input$decl_tab3, input$ov_flags_tab3) |> debounce(450)

  # Lógica de reset
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

  # Gráfico Principal (Série Temporal)
  output$serie_temporal <- renderPlotly({
    df <- dados_mensal()
    req(nrow(df) > 0)
    variavel <- input$variavel_fluxo_tab3
    agrup <- input$agrupamento_tab3
    if (agrup == "geral") {
      df_plot <- df |> group_by(data) |> summarise(valor = sum(.data[[variavel]], na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_plot, aes(x = data, y = valor)) + geom_line(color = "tomato", linewidth = 1) + geom_point(color = "tomato", size = 1.5) +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
        labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado (R$)", "Peso declarado (Kg)"), x = "Data") +
        theme_minimal(base_size = 13) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
    } else {

      df_plot <- df |> group_by(data, grupo = .data[[agrup]]) |> summarise(valor = sum(.data[[variavel]], na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_plot, aes(x = data, y = valor, group = grupo, color = grupo,
                               text = paste0("<b>", grupo, "</b><br>", "Data: ", format(data, "%b/%Y"),"<br>", "valor:", comma(valor)))) +
        geom_line(linewidth = 1) +
        geom_point(size = 1.5) +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
        labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado (R$)", "Peso declarado (Kg)"), x = "Data") +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "none")
      ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
    }
  })

  # Gráfico de Outliers
  output$grafico_outliers <- renderPlotly({
    df <- dados_mensal()
    req(nrow(df) > 0)

    variavel <- input$variavel_fluxo_tab3
    agrup <- input$agrupamento_tab3

    df_plot <- df |>
      mutate(
        grupo = if (agrup == "geral") "geral" else .data[[agrup]],
        valor = as.numeric(.data[[variavel]])
      ) |>
      group_by(data, grupo) |>
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

    df_plot <- df_plot |>
      group_by(grupo) |>
      mutate(
        Q1  = quantile(valor, 0.25, na.rm = TRUE),
        Q3  = quantile(valor, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        sdv = sd(valor, na.rm = TRUE),
        limite_sup = ifelse(
          is.finite(IQR) & IQR > 0,
          Q3 + 1.5 * IQR,
          # fallback quando IQR = 0 (todos iguais ou quase):
          Q3 + 3 * ifelse(is.finite(sdv) & !is.na(sdv), sdv, 0)
        ),
        outlier = valor > limite_sup
      ) |>
      ungroup()

    # diagnósticos no console (opcional)
    message("Outliers totais: ", sum(df_plot$outlier, na.rm = TRUE))

    df_plot <- df_plot |>
      mutate(outlier = factor(outlier, levels = c(FALSE, TRUE),
                              labels = c("Não", "Sim")))

    # camada fantasma para manter legenda mesmo sem 'Sim'
    dummy_legend <- data.frame(
      data = as.Date(c(NA, NA)),
      valor = c(NA_real_, NA_real_),
      outlier = factor(c("Não", "Sim"), levels = c("Não", "Sim"))
    )

    p <- ggplot(df_plot, aes(x = data, y = valor)) +
      geom_line(aes(group = grupo), alpha = 0.2, color = "gray50", linewidth = 0.6) +
      geom_point(aes(color = outlier), size = 1.6) +
      geom_point(data = dummy_legend, aes(color = outlier), alpha = 0) +
      scale_color_manual(values = c("#212529", "tomato"),
                         breaks = c("Não", "Sim"), drop = FALSE, name = "Outlier") +
      scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado por mês (R$)", "Peso declarado por mês (Kg)"),
           x = "Data") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 10),
            legend.text  = element_text(size = 10),
            axis.text.x  = element_text(angle = 70, hjust = 1, size = 9),
            axis.text.y  = element_text(size = 10))

    ggplotly(p, tooltip = c("x", "y", "color")) |>
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2)) |>
      config(displayModeBar = FALSE)
  })

  # Mapa

  # # zoom atual do mapa da aba 3
  # map_zoom_tab3 <- reactive(input$mapa_cfem_pma_tab3_zoom %||% 8) |> debounce(250)
  #
  # # estado da fonte atual (simpl/full)
  # src_state_tab3 <- reactiveVal("simpl")
  #
  # observeEvent(map_zoom_tab3(), {
  #   z <- map_zoom_tab3()
  #   cur <- src_state_tab3()
  #   # histerese: só troca em 10 pra full e 8 pra simpl
  #   if (z >= 10 && cur != "full")  src_state_tab3("full")
  #   if (z <= 8  && cur != "simpl") src_state_tab3("simpl")
  # }, ignoreInit = TRUE)
  #
  # # fonte agora depende do estado
  # pma_src_tab3 <- reactive({
  #   if (src_state_tab3() == "full") pma_full else pma_simpl
  # })
  pma_src_tab3 <- reactive(pma_simpl)

  # dados filtrados para o mapa da aba 3 (usando os inputs da aba 3)
  dados_mapa_cfem_tab3 <- reactive({
    # Reusa dados_mensal (mesmos filtros; cfem_mensal = cfem + coluna 'data').
    # Evita uma segunda varredura idêntica dos dados pesados a cada interação.
    dados_mensal()
  })

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
    processos_declarantes <- cfem |>
      dplyr::filter(NOME_arr %in% declarantes) |>
      dplyr::pull(PROCESSO) |>
      unique()
    src |> dplyr::filter(PROCESSO %in% processos_declarantes)
  }) |> bindCache(dados_mapa_cfem_tab3()$NOME_arr)


  # render base do mapa (aba 3)
  output$mapa_cfem_pma_tab3 <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(
      minZoom = 2, maxZoom = 18, preferCanvas = TRUE
    )) |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
      
      # leaflet::addMiniMap(
      #   tiles = providers$CartoDB.Positron,
      #   toggleDisplay = TRUE,
      #   position = "bottomright",
      #   width = 150, height = 150
      # ) |>
      leaflet::addPolygons(data = uc,  group = "Unidades de Conservação",
                           color = "#78c679", weight = 0.5, opacity = 0.8, fillOpacity = 0.5,
                           popup = ~paste0("<b>UC:</b> ", nome_uc)) |>
      leaflet::addPolygons(data = ti,  group = "Terras Indígenas",
                           color = "#006837", weight = 0.5, opacity = 0.8, fillOpacity = 0.5,
                           popup = ~paste0("<b>TI:</b> ", terrai_nom,
                                           if ("fase_ti" %in% names(ti)) paste0("<br><b>Fase:</b> ", fase_ti) else "")) |>
      leaflet::addPolygons(data = qui, group = "Comunidades Quilombolas",
                           color = "#dfc27d", weight = 0.5, opacity = 0.85, fillOpacity = 0.45,
                           popup = ~paste0("<b>Comunidade:</b> ", nm_comunid,
                                           if ("fase" %in% names(qui)) paste0("<br><b>Fase:</b> ", fase) else "")) |>
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "Satélite"),
        overlayGroups = c(
          "Processos Minerários",
          "PMAs do mesmo Titular",
          "PMAs da mesma Parte Declarante",
          "Unidades de Conservação",
          "Terras Indígenas",
          "Comunidades Quilombolas"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) |>
      leaflet::hideGroup(c(
        "PMAs do mesmo Titular",
        "PMAs da mesma Parte Declarante",
        "Unidades de Conservação",
        "Terras Indígenas",
        "Comunidades Quilombolas"
      ))
  })


  # overlays UC/TI/QUI controladas pelos botões da aba 3
  observeEvent(input$ov_flags_tab3, {
    proxy <- leaflet::leafletProxy("mapa_cfem_pma_tab3")
    groups <- c("Unidades de Conservação" = "UCov",
                "Terras Indígenas"        = "TIov",
                "Comunidades Quilombolas" = "QUIov")
    # esconde todas
    lapply(names(groups), function(g) proxy |> leaflet::hideGroup(g))
    # mostra só as selecionadas
    sel <- input$ov_flags_tab3
    lapply(names(groups)[groups %in% sel], function(g) proxy |> leaflet::showGroup(g))
  })


  # camada principal: PMAs filtrados (aba 3)
  prev_hash_tab3 <- reactiveVal(NULL)

  # observeEvent(pma_filtrado_tab3(), {
  #   pm <- pma_filtrado_tab3()
  #   validate(need(nrow(pm) > 0, "Nenhum processo minerário encontrado com os filtros."))

  #   # redesenha só se mudou (opcional, por performance)
  #   #h <- digest::digest(list(proc = sort(pm$PROCESSO), src = src_state_tab3()))
  #   h <- digest::digest(list(proc = sort(pm$PROCESSO), src = "simpl"))
  #   if (is.null(prev_hash_tab3()) || h != prev_hash_tab3()) {
  #     prev_hash_tab3(h)
  #     leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
  #       leaflet::clearGroup("Processos Minerários") |>
  #       leaflet::addPolygons(
  #         data = pm, group = "Processos Minerários",
  #         color = "#FF3D00", weight = 2, opacity = 1,
  #         fillOpacity = 0.4, smoothFactor = 0.2,
  #         popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>",
  #                         "<b>Substância:</b> ", SUBS, "<br>",
  #                         "<b>Fase:</b> ", FASE, "<br>",
  #                         "<b>Titular:</b> ", TITULAR)
  #       )
  #   }
  # })
  observeEvent(pma_filtrado_tab3(), {
    pm <- pma_filtrado_tab3()
    validate(need(nrow(pm) > 0, "Nenhum processo minerário encontrado com os filtros."))

    h <- digest::digest(list(proc = sort(pm$PROCESSO), src = "simpl"))
    if (is.null(prev_hash_tab3()) || h != prev_hash_tab3()) {
      prev_hash_tab3(h)
      
      # [CÓDIGO NOVO] 1. Calcula a "caixa" (bounding box) que engloba os polígonos
      bb <- sf::st_bbox(sf::st_transform(pm, 4326))
      
      leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
        leaflet::clearGroup("Processos Minerários") |>
        leaflet::addPolygons(
          data = pm, group = "Processos Minerários",
          color = "#FF3D00", weight = 2, opacity = 1,
          fillOpacity = 0.4, smoothFactor = 0.2,
          popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>",
                          "<b>Substância:</b> ", SUBS, "<br>",
                          "<b>Fase:</b> ", FASE, "<br>",
                          "<b>Titular:</b> ", TITULAR)
        ) |>
        # [CÓDIGO NOVO] 2. Ajusta o zoom do mapa para focar nesses polígonos
        leaflet::fitBounds(
          lng1 = as.numeric(bb["xmin"]), lat1 = as.numeric(bb["ymin"]),
          lng2 = as.numeric(bb["xmax"]), lat2 = as.numeric(bb["ymax"])
        )
    }
  })


  # camadas complementares (aba 3)
  observeEvent(pma_titular_tab3(), {
    d <- pma_titular_tab3()
    leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
      leaflet::clearGroup("PMAs do mesmo Titular") |>
      leaflet::addPolygons(
        data = d, group = "PMAs do mesmo Titular",
        color = "#0078FF", weight = 1, opacity = 0.8,
        fillOpacity = 0.35, smoothFactor = 0.2,
        popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>",
                        "<b>Titular:</b> ", TITULAR)
      )
  })

  observeEvent(pma_declarante_tab3(), {
    d <- pma_declarante_tab3()
    leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
      leaflet::clearGroup("PMAs da mesma Parte Declarante") |>
      leaflet::addPolygons(
        data = d, group = "PMAs da mesma Parte Declarante",
        color = "#6a3d9a", weight = 1, opacity = 0.8,
        fillOpacity = 0.35, smoothFactor = 0.2,
        popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>",
                        "<b>Titular:</b> ", TITULAR)
      )
  })

  # Seleção atual mostrada no mapa (Aba 3)
  output$baixar_pma_sel_tab3 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_filtrado_tab3(), "pmas_selecao_tab3", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
      gc()
    }
  )

  # PMAs do mesmo titular (excluindo os já na seleção)
  output$baixar_pma_titular_tab3 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_titular_tab3(), "pmas_titular_tab3", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
      gc()
    }
  )

  # PMAs da mesma parte declarante
  output$baixar_pma_declarante_tab3 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      shp_zip  <- exportar_shapefile(pma_declarante_tab3(), "pmas_declarante_tab3", temp_dir)
      file.copy(shp_zip, file, overwrite = TRUE)
      gc()
    }
  )

  outputOptions(output, "mapa_cfem_pma_tab3", suspendWhenHidden = FALSE)

  # --- [ADD] Resumo da seleção (Aba 3 - mensal) ---
  output$relatorio_tab3 <- renderText({
    base <- relatorio_selecao(dados_mensal(), mensal = TRUE)

    # Métrica atual e rótulo do agrupamento (só pra informar no cabeçalho)
    metrica <- if (input$variavel_fluxo_tab3 == "VALORarr") "Valor Recolhido (R$)" else "Quantidade (Kg líquido)"
    agr_labels <- c(
      geral      = "Geral",
      PROCESSO   = "Processo",
      TITULAR  = "Titular",
      NOME_arr   = "Parte Declarante",
      SUBSarr    = "Substância",
      SUBSarrSIM = "Grupo (subs)",
      FASE       = "Fase"
    )
    agr <- agr_labels[[input$agrupamento_tab3]] %||% input$agrupamento_tab3

    # Helpers de formatação BR
    fmt_num_br <- function(x) format(round(x, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
    fmt_cur_br <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
    fmt_val <- function(x) if (input$variavel_fluxo_tab3 == "VALORarr") fmt_cur_br(x) else paste0(fmt_num_br(x), " kg")

    # Base mensal atual
    df <- dados_mensal()
    if (!nrow(df)) {
      return(paste0(base, "\n\nMétrica nos gráficos: ", metrica, " | Linhas por: ", agr,
                    "\nOutliers por Processo (boxplot por processo): Nenhum."))
    }
    if (!"data" %in% names(df)) df$data <- as.Date(sprintf("%s-%02d-01", df$ANO, df$MES))

    variavel <- input$variavel_fluxo_tab3

    paste0(
      base,
      "\n\nMétrica nos gráficos: ", metrica,
      " | Linhas por: ", agr,
      "\n"
    )
  })

  # # ---- Lógica da Quarta Aba: Mapa ----
  # sync_pair(session, "subs_tab4", "subs_det_tab4", map_subs, "SUBSarrSIM", "SUBSarr")
  #
  # # Lógica de atualização dos filtros encadeados para a aba 4
  # observeEvent(list(input$subs_tab4, input$subs_det_tab4, input$fases_tab4, input$periodo_tab4), {
  #   if (is.null(input$subs_tab4) || length(input$subs_tab4) == 0) return()
  #
  #   df_temp <- cfem |>
  #     dplyr::filter(
  #       ANO >= input$periodo_tab4[1], ANO <= input$periodo_tab4[2],
  #       FASE %in% input$fases_tab4
  #     )
  #   if (length(input$subs_det_tab4)) { df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab4) } else { df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab4) }
  #   updatePickerInput(session, "ufs_tab4", choices = sort(unique(df_temp$abbrev_state)), selected = intersect(isolate(input$ufs_tab4), sort(unique(df_temp$abbrev_state))))
  # }, ignoreInit = FALSE)
  #
  # observeEvent(list(input$ufs_tab4), {
  #   df_temp <- cfem |> dplyr::filter(abbrev_state %in% input$ufs_tab4)
  #   updatePickerInput(session, "muns_tab4", choices = sort(unique(df_temp$name_muni)), selected = intersect(isolate(input$muns_tab4), sort(unique(df_temp$name_muni))))
  # }, ignoreInit = FALSE)
  #
  # observeEvent(list(input$muns_tab4, input$ufs_tab4), {
  #   df_temp <- cfem |> dplyr::filter(abbrev_state %in% input$ufs_tab4, name_muni %in% input$muns_tab4)
  #   updatePickerInput(session, "tits_tab4", choices = sort(unique(df_temp$TITULARcm)), selected = intersect(isolate(input$tits_tab4), sort(unique(df_temp$TITULARcm))))
  #   updatePickerInput(session, "procs_tab4", choices = sort(unique(df_temp$PROCESSO)), selected = intersect(isolate(input$procs_tab4), sort(unique(df_temp$PROCESSO))))
  # }, ignoreInit = FALSE)
  #
  # observeEvent(list(input$procs_tab4, input$tits_tab4), {
  #   df_temp <- cfem |> dplyr::filter(abbrev_state %in% input$ufs_tab4, name_muni %in% input$muns_tab4, TITULARcm %in% input$tits_tab4, PROCESSO %in% input$procs_tab4)
  #   updatePickerInput(session, "decl_tab4", choices = sort(unique(df_temp$NOME_arr)), selected = intersect(isolate(input$decl_tab4), sort(unique(df_temp$NOME_arr))))
  # }, ignoreInit = FALSE)
  #
  # # Reactive principal para os dados do mapa
  # dados_mapa_cfem <- reactive({
  #   df <- cfem |>
  #     dplyr::filter(
  #       ANO >= input$periodo_tab4[1], ANO <= input$periodo_tab4[2],
  #       FASE %in% input$fases_tab4
  #     )
  #   if (length(input$subs_det_tab4)) { df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab4) } else { df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab4) }
  #   if (length(input$ufs_tab4)) df <- df |> dplyr::filter(abbrev_state %in% input$ufs_tab4)
  #   if (length(input$muns_tab4)) df <- df |> dplyr::filter(name_muni %in% input$muns_tab4)
  #   if (length(input$procs_tab4)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab4)
  #   if (length(input$tits_tab4)) df <- df |> dplyr::filter(TITULARcm %in% input$tits_tab4)
  #   if (length(input$decl_tab4)) df <- df |> dplyr::filter(NOME_arr %in% input$decl_tab4)
  #   df <- filtra_sobrepos(df, flags = input$ov_flags_tab4)
  #   df
  # }) |> bindCache(
  #   input$periodo_tab4, input$subs_tab4, input$subs_det_tab4, input$ufs_tab4, input$muns_tab4, input$fases_tab4,
  #   input$procs_tab4, input$tits_tab4, input$decl_tab4, input$ov_flags_tab4
  # ) |> debounce(250)
  #
  # # Lógica de reset
  # observeEvent(input$reset_tab4, {
  #   updatePickerInput(session, "subs_tab4", choices = subs_all_grupo)
  #   updatePickerInput(session, "subs_det_tab4", choices = subs_all_original)
  #   updatePickerInput(session, "fases_tab4", choices = fases_all)
  #   updateSliderInput(session, "periodo_tab4", value = c(min(anos_all), max(anos_all)))
  #   updatePickerInput(session, "ufs_tab4", choices = ufs_all)
  #   updatePickerInput(session, "muns_tab4", choices = muns_all)
  #   updatePickerInput(session, "procs_tab4", choices = procs_all)
  #   updatePickerInput(session, "tits_tab4", choices = tits_all)
  #   updatePickerInput(session, "decl_tab4", choices = decl_all)
  #   updateCheckboxGroupButtons(session, "ov_flags_tab4", selected = c())
  # })
  #
  # # zoom atual do mapa
  # map_zoom <- reactive(input$mapa_cfem_pma_zoom %||% 8) |> debounce(100)
  #
  # # escolhe a fonte conforme zoom (simpl < 9, full >= 9)
  # pma_src <- reactive({
  #   z <- map_zoom()
  #   if (is.null(z) || z < 9) pma_simpl else pma_full
  # })
  #
  # # ---- Mapa
  # # PMAs filtrados pelos processos selecionados
  # pma_filtrado <- reactive({
  #   procs <- unique(dados_mapa_cfem()$PROCESSO)
  #   src <- pma_src()
  #   if (length(procs) == 0) return(src[0, ])
  #   src |>
  #     dplyr::filter(PROCESSO %in% procs)
  # }) |> bindCache(dados_mapa_cfem()$PROCESSO, map_zoom())
  #
  # # PMAs do mesmo titular, exceto os já incluídos em pma_filtrado
  # pma_titular_tab4 <- reactive({
  #   procs_sel <- unique(dados_mapa_cfem()$PROCESSO)
  #   tits <- unique(dados_mapa_cfem()$TITULARcm)
  #   src <- pma_src()
  #   if (length(tits) == 0) return(src[0, ])
  #
  #   out <- src |>
  #     dplyr::filter(TITULARcm %in% tits, !(PROCESSO %in% procs_sel))
  #   out
  # }) |> bindCache(dados_mapa_cfem()$TITULARcm, map_zoom())
  #
  # # PMAs vinculados às mesmas partes declarantes
  # pma_declarante_tab4 <- reactive({
  #   declarantes <- unique(dados_mapa_cfem()$NOME_arr)
  #   src <- pma_src()
  #   if (!length(declarantes)) return(src[0, ])
  #
  #   processos_declarantes <- cfem |>
  #     dplyr::filter(NOME_arr %in% declarantes) |>
  #     dplyr::pull(PROCESSO) |>
  #     unique()
  #
  #   src |>
  #     dplyr::filter(PROCESSO %in% processos_declarantes)
  # }) |> bindCache(dados_mapa_cfem()$NOME_arr, map_zoom())
  #
  # # helper para ajustar a visão do mapa
  # .fit_to <- function(proxy_id, sfobj) {
  #   if (!inherits(sfobj, "sf") || nrow(sfobj) == 0) return(invisible())
  #   bb <- sf::st_bbox(sf::st_transform(sfobj, 4326))
  #   leaflet::leafletProxy(proxy_id) |>
  #     leaflet::fitBounds(lng1 = bb["xmin"], lat1 = bb["ymin"],
  #                        lng2 = bb["xmax"], lat2 = bb["ymax"])
  #   invisible()
  # }
  #
  # # Renderização base do mapa
  # output$mapa_cfem_pma <- leaflet::renderLeaflet({
  #   leaflet::leaflet(options = leaflet::leafletOptions(
  #     minZoom = 2, maxZoom = 18, preferCanvas = TRUE
  #   )) |>
  #     leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
  #     leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
  #     leaflet::addLayersControl(
  #       baseGroups = c("CartoDB", "Satélite"),
  #       overlayGroups = c(
  #         "Processos Minerários",
  #         "PMAs do mesmo Titular",
  #         "PMAs da mesma Parte Declarante",
  #         "Unidades de Conservação",
  #         "Terras Indígenas",
  #         "Comunidades Quilombolas"
  #       ),
  #       options = leaflet::layersControlOptions(collapsed = FALSE)
  #     ) |>
  #     leaflet::hideGroup(c(
  #       "PMAs do mesmo Titular", "PMAs da mesma Parte Declarante",
  #       "Unidades de Conservação", "Terras Indígenas", "Comunidades Quilombolas"
  #     ))
  # })
  #
  # # Evento que gerencia TI, UC, QUI
  # observeEvent(input$ov_flags_tab4, {
  #   s <- input$ov_flags_tab4
  #   proxy <- leaflet::leafletProxy("mapa_cfem_pma")
  #
  #   # Limpa as camadas existentes de UC, TI, QUI
  #   proxy |>
  #     leaflet::clearGroup("Unidades de Conservação") |>
  #     leaflet::clearGroup("Terras Indígenas") |>
  #     leaflet::clearGroup("Comunidades Quilombolas")
  #
  #   # Adiciona as camadas selecionadas
  #   if ("UCov" %in% s) {
  #     proxy |> leaflet::addPolygons(data = uc, group = "Unidades de Conservação", color = "#78c679", weight = 0.5, opacity = 0.8, fillOpacity = 0.5, popup = ~paste0("<b>UC:</b> ", nome_uc))
  #   }
  #   if ("TIov" %in% s) {
  #     proxy |> leaflet::addPolygons(data = ti, group = "Terras Indígenas", color = "#006837", weight = 0.5, opacity = 0.8, fillOpacity = 0.5, popup = ~paste0("<b>TI:</b> ", terrai_nom, if ("fase_ti" %in% names(ti)) paste0("<br><b>Fase:</b> ", fase_ti) else ""))
  #   }
  #   if ("QUIov" %in% s) {
  #     proxy |> leaflet::addPolygons(data = qui, group = "Comunidades Quilombolas", color = "#dfc27d", weight = 0.5, opacity = 0.85, fillOpacity = 0.45, popup = ~paste0("<b>Comunidade:</b> ", nm_comunid, if ("fase" %in% names(qui)) paste0("<br><b>Fase:</b> ", fase) else ""))
  #   }
  # })
  #
  # # Evento que gerencia a camada "Processos Minerários" (filtrados)
  # observeEvent(pma_filtrado(), {
  #   pm <- pma_filtrado()
  #   validate(need(nrow(pm) > 0, "Nenhum processo minerário encontrado com os filtros."))
  #
  #   proxy <- leaflet::leafletProxy("mapa_cfem_pma")
  #   proxy |>
  #     leaflet::clearGroup("Processos Minerários") |>
  #     leaflet::addPolygons(
  #       data = pm, group = "Processos Minerários",
  #       color = "#FF3D00", weight = 2, opacity = 1,
  #       fillOpacity = 0.4, smoothFactor = 0.2,
  #       popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>",
  #                       "<b>Substância:</b> ", SUBScm, "<br>",
  #                       "<b>Fase:</b> ", FASEcm, "<br>",
  #                       "<b>Titular:</b> ", TITULARcm)
  #     )
  #   # Centraliza o mapa na extensão dos PMs filtrados
  #   .fit_to("mapa_cfem_pma", pm)
  # })
  #
  # # Evento que gerencia a camada "PMAs do mesmo Titular"
  # observeEvent(pma_titular_tab4(), {
  #   d <- pma_titular_tab4()
  #   leaflet::leafletProxy("mapa_cfem_pma") |>
  #     leaflet::clearGroup("PMAs do mesmo Titular") |>
  #     leaflet::addPolygons(
  #       data = d, group = "PMAs do mesmo Titular",
  #       color = "#0078FF", weight = 1, opacity = 0.8,
  #       fillOpacity = 0.35, smoothFactor = 0.2,
  #       popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>",
  #                       "<b>Titular:</b> ", TITULARcm)
  #     )
  # })
  #
  # # Evento que gerencia a camada "PMAs da mesma Parte Declarante"
  # observeEvent(pma_declarante_tab4(), {
  #   d <- pma_declarante_tab4()
  #   leaflet::leafletProxy("mapa_cfem_pma") |>
  #     leaflet::clearGroup("PMAs da mesma Parte Declarante") |>
  #     leaflet::addPolygons(
  #       data = d, group = "PMAs da mesma Parte Declarante",
  #       color = "#6a3d9a",
  #       weight = 1, opacity = 0.8,
  #       fillOpacity = 0.35, smoothFactor = 0.2,
  #       popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>",
  #                       "<b>Titular:</b> ", TITULARcm)
  #     )
  # })
}

shinyApp(ui, server)
