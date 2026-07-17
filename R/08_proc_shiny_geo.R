################################################################################
# 08_proc_shiny_geo.R
#
# Prepara os objetos "gerais" do Shiny (abas 1-3: Tabela / Anual / Mensal):
# cfem.rds, cfem_anual.rds, cfem_mensal.rds, pma_simpl.rds, ti/uc/qui_simpl.rds
# e os lookups de filtro encadeado (lk_*_tab1/2/3.rds).
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(sf)
  library(rmapshaper)
  library(here)
})

# --- Caminhos -----------------------------------------------------------------
INPUT_DIR  <- here::here("data", "result_shiny")
OUTPUT_DIR <- here::here("shiny_dashboard")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cfem_csv_path <- file.path(INPUT_DIR, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv")
pma_shp_path  <- file.path(INPUT_DIR, "pma_amzl_ALLminerals_final.shp")

if (!file.exists(cfem_csv_path)) stop("[08] CFEM nao encontrado: ", cfem_csv_path, " — rode o 05_integracao_final.R primeiro.")
if (!file.exists(pma_shp_path))  stop("[08] PMA nao encontrado: ",  pma_shp_path,  " — rode o 05_integracao_final.R primeiro.")

# =============================================================================
# 1) CFEM base (declaracao a declaracao, sem agregacao)
# =============================================================================
message("[08] lendo CFEM base...")

cfem <- readr::read_csv(cfem_csv_path, show_col_types = FALSE) |>
  dplyr::mutate(
    ANO           = as.integer(ANO),
    MES           = as.integer(MES),
    VALORarr      = as.numeric(VALORarr),
    PESO_KG       = as.numeric(PESO_KG),
    PESO_G        = as.numeric(PESO_G),
    preco_g_orig  = as.numeric(preco_g_orig),
    PESO_G_final  = as.numeric(PESO_G_final),
    PESO_KG_final = as.numeric(PESO_KG_final),
    preco_g_final = as.numeric(preco_g_final),
    proc_ano      = paste0(trimws(PROCESSO), "/", ANO)
  )

saveRDS(cfem, file.path(OUTPUT_DIR, "cfem.rds"))
message(sprintf("[08] cfem.rds: %d declaracoes", nrow(cfem)))

# --- Lookups enxutos para filtros encadeados do app (tab1 / fonte cfem) -----
lk_mun_tab1      <- cfem |> dplyr::distinct(SUBSarrSIM, SUBSarr, FASE, abbrev_state, ANO, name_muni)
lk_tit_proc_tab1 <- cfem |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO)
lk_decl_tab1     <- cfem |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr)
saveRDS(lk_mun_tab1,      file.path(OUTPUT_DIR, "lk_mun_tab1.rds"))
saveRDS(lk_tit_proc_tab1, file.path(OUTPUT_DIR, "lk_tit_proc_tab1.rds"))
saveRDS(lk_decl_tab1,     file.path(OUTPUT_DIR, "lk_decl_tab1.rds"))

# =============================================================================
# 2) CFEM anual (agregacao POR ANO — legitima aqui: e o proposito da aba
#    "Anual" do app, nao esconde nada da granularidade original que continua
#    disponivel em cfem.rds/aba Tabela)
# =============================================================================
message("[08] agregando CFEM anual...")

cfem_anual <- cfem |>
  dplyr::group_by(ANO, abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr, SUBSarrSIM, SUBSarr) |>
  dplyr::summarise(
    VALORarr      = sum(VALORarr,      na.rm = TRUE),
    VALORtot      = sum(VALORtot,      na.rm = TRUE),
    PESO_KG       = sum(PESO_KG,       na.rm = TRUE),
    PESO_G        = sum(PESO_G,        na.rm = TRUE),
    PESO_G_final  = sum(PESO_G_final,  na.rm = TRUE),
    PESO_KG_final = sum(PESO_KG_final, na.rm = TRUE),
    .groups       = "drop"
  )

pma_ocd_attr <- sf::st_read(pma_shp_path, quiet = TRUE) |>
  dplyr::select(
    PROCESSO, AREA_HA, FASE, ULT_EV_DAT, ULT_EV_DES,
    TIov, UCov, QUIov, TIov10km, UCov2_10km, QUIov10km,
    UCtype, UCname, TIname, QUIname
  ) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(dplyr::across(c(TIov, UCov, QUIov, TIov10km, UCov2_10km, QUIov10km), ~ as.logical(.x)))

cfem_anual <- dplyr::inner_join(cfem_anual, pma_ocd_attr, by = "PROCESSO")
saveRDS(cfem_anual, file.path(OUTPUT_DIR, "cfem_anual.rds"))
message(sprintf("[08] cfem_anual.rds: %d linhas (ano x processo x substancia)", nrow(cfem_anual)))

lk_mun_tab2      <- cfem_anual |> dplyr::distinct(SUBSarrSIM, SUBSarr, FASE, abbrev_state, ANO, name_muni)
lk_tit_proc_tab2 <- cfem_anual |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO)
lk_decl_tab2     <- cfem_anual |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr)
saveRDS(lk_mun_tab2,      file.path(OUTPUT_DIR, "lk_mun_tab2.rds"))
saveRDS(lk_tit_proc_tab2, file.path(OUTPUT_DIR, "lk_tit_proc_tab2.rds"))
saveRDS(lk_decl_tab2,     file.path(OUTPUT_DIR, "lk_decl_tab2.rds"))

# =============================================================================
# 3) CFEM mensal (so adiciona a coluna "data" — nenhuma agregacao, continua 1
#    linha = 1 declaracao, igual cfem.rds)
# =============================================================================
message("[08] formatando CFEM mensal...")

cfem_mensal <- cfem |>
  dplyr::mutate(data = as.Date(sprintf("%04d-%02d-01", ANO, MES)))

saveRDS(cfem_mensal, file.path(OUTPUT_DIR, "cfem_mensal.rds"))

lk_mun_tab3      <- cfem_mensal |> dplyr::distinct(SUBSarrSIM, SUBSarr, FASE, abbrev_state, ANO, name_muni)
lk_tit_proc_tab3 <- cfem_mensal |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO)
lk_decl_tab3     <- cfem_mensal |> dplyr::distinct(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr)
saveRDS(lk_mun_tab3,      file.path(OUTPUT_DIR, "lk_mun_tab3.rds"))
saveRDS(lk_tit_proc_tab3, file.path(OUTPUT_DIR, "lk_tit_proc_tab3.rds"))
saveRDS(lk_decl_tab3,     file.path(OUTPUT_DIR, "lk_decl_tab3.rds"))

# =============================================================================
# 4) Geometrias (PMA / TI / UC / Quilombolas) — brutas + simplificadas
# =============================================================================
message("[08] processando camadas geoespaciais...")

to_wgs84 <- function(x) {
  cr <- sf::st_crs(x)
  if (is.na(cr) || isFALSE(sf::st_is_longlat(cr))) suppressWarnings(sf::st_transform(x, 4326)) else x
}

# st_make_valid() ANTES de simplificar: sem isso, geometrias invalidas/mistas
# do shapefile de origem podem virar GEOMETRYCOLLECTION apos rmapshaper::
# ms_simplify() — o leaflet::addPolygons() no app.R nao sabe desenhar isso e
# trava ("Don't know how to get polygon data from object of class
# XY,GEOMETRYCOLLECTION,sfg").
#
# CORRECAO (achado real, PMA/2026-07): aplicar st_make_valid() em TODAS as
# geometrias, mesmo nas ja validas, pode destruir geometrias corretas.
# Confirmado no shapefile do PMA: 54497 de 55415 poligonos ja eram
# "Valid Geometry" na origem (MULTIPOLYGON), mas ao rodar st_make_valid()
# em cima de TODAS mesmo assim, 6 delas (todas com area < 0.07 ha — REQ LAVRA
# GARIMPEIRA/REQ PESQUISA recem-protocolados) viraram GEOMETRYCOLLECTION
# VAZIA. Efeito colateral conhecido do GEOS por tras do st_make_valid() em
# poligonos muito pequenos/proximos da tolerancia numerica: ele renoda a
# geometria mesmo quando nao precisa, e pode colapsa-la. Teste que confirmou
# (pulando make_valid() nessas 6): permanecem MULTIPOLYGON validos, nrow
# preservado. Por isso agora so aplicamos st_make_valid() no subconjunto que
# st_is_valid() de fato aponta como invalido — o resto fica intocado.
tornar_valido <- function(x) {
  invalidas <- !sf::st_is_valid(x)
  if (any(invalidas)) {
    x[invalidas, ] <- suppressWarnings(sf::st_make_valid(x[invalidas, ]))
  }

  # Só o st_make_valid() acima (aplicado apenas ao subconjunto que era
  # inválido) pode gerar GEOMETRYCOLLECTION (polígono + fragmentos residuais
  # de linha/ponto). Isolamos só essas para extrair a parte poligonal —
  # tudo que já era (multi)polígono válido (a imensa maioria) permanece
  # intocado, sem passar de novo por st_collection_extract()/st_make_valid().
  mistas <- sf::st_geometry_type(x) == "GEOMETRYCOLLECTION"
  if (any(mistas)) {
    x_intocado <- x[!mistas, ]
    x_extraido <- suppressWarnings(sf::st_collection_extract(x[mistas, ], "POLYGON"))
    x_extraido <- sf::st_make_valid(x_extraido)
    x <- rbind(x_intocado, x_extraido)
  }
  x
}

pma <- sf::st_read(pma_shp_path, quiet = TRUE) |> to_wgs84() |> tornar_valido()
uc  <- sf::st_read(file.path(INPUT_DIR, "uc_amzl.shp"),  quiet = TRUE) |> to_wgs84() |> tornar_valido() |> dplyr::select("nome_uc", "sigla_snuc")
qui <- sf::st_read(file.path(INPUT_DIR, "qui_amzl.shp"), quiet = TRUE) |> to_wgs84() |> tornar_valido() |> dplyr::select("nm_comunid")
ti  <- sf::st_read(file.path(INPUT_DIR, "ti_amzl.shp"),  quiet = TRUE) |> to_wgs84() |> tornar_valido() |> dplyr::select("terrai_nom")

saveRDS(pma, file.path(OUTPUT_DIR, "pma.rds"))
saveRDS(ti,  file.path(OUTPUT_DIR, "ti.rds"))
saveRDS(uc,  file.path(OUTPUT_DIR, "uc.rds"))
saveRDS(qui, file.path(OUTPUT_DIR, "qui.rds"))

message("[08] simplificando geometrias (rmapshaper)...")
# CORRECAO (achado real, crash em producao 2026-07): tornar_valido() roda
# ANTES do ms_simplify() (linha ~431), mas o proprio ms_simplify() pode
# reintroduzir GEOMETRYCOLLECTION como efeito colateral da simplificacao
# (comportamento conhecido do mapshaper/rmapshaper — simplificar uma
# geometria valida pode gerar auto-intersecoes ou fragmentos). O crash
# reportado ("Don't know how to get polygon data from object of class
# XY,GEOMETRYCOLLECTION,sfg", em leaflet::addPolygons no app.R) veio do
# .rds JA simplificado, nao do pma bruto — por isso precisa re-sanear
# DEPOIS de simplificar tambem, nao so antes.
simplify_and_save <- function(sf_obj, out_path, keep_ratio) {
  if (nrow(sf_obj) > 0) {
    simplified <- rmapshaper::ms_simplify(sf_obj, keep = keep_ratio, keep_shapes = TRUE)
    simplified <- tornar_valido(simplified)
    saveRDS(simplified, out_path)
  }
}

simplify_and_save(ti,  file.path(OUTPUT_DIR, "ti_simpl.rds"),  0.3)
simplify_and_save(uc,  file.path(OUTPUT_DIR, "uc_simpl.rds"),  0.3)
simplify_and_save(qui, file.path(OUTPUT_DIR, "qui_simpl.rds"), 0.3)
simplify_and_save(pma, file.path(OUTPUT_DIR, "pma_simpl.rds"), 0.1)

message("\n=== 08_proc_shiny_geo.R — CONCLUIDO ===")




################################################################################
# checar_rds_antes_deploy.R
#
# Checagem robusta de TODOS os .rds em shiny_dashboard antes do scp pro
# droplet. Roda 100% local, so leitura — nao altera nenhum arquivo.
#
# O que detecta:
#   1) Erro de leitura (arquivo corrompido/ausente)
#   2) Warnings emitidos durante o readRDS() — pega qualquer warning, incluindo
#      o "cannot unserialize ALTVEC object... returning length zero vector"
#      (bug do arrow ALTREP que ja identificamos), sem depender do texto exato
#      da mensagem continuar igual em versoes futuras do pacote.
#   3) Checagem ESTRUTURAL independente do warning: compara o tamanho de CADA
#      coluna com nrow(df). Isso pega o bug mesmo se o R um dia parar de
#      emitir warning nesse caso — e' o teste que realmente importa, o
#      warning e so um sintoma.
#   4) Tabelas com 0 linhas (pode ser esperado ou sintoma de outro problema
#      upstream — sinalizado, nao tratado como erro automatico).
################################################################################

pasta <- "C:/GP/anm-geo/shiny_dashboard"   # <-- ajuste aqui se necessario

arquivos_rds <- list.files(pasta, pattern = "\\.rds$", full.names = TRUE)

if (length(arquivos_rds) == 0) {
  stop("Nenhum .rds encontrado em: ", pasta)
}

checar_rds <- function(caminho) {
  nome <- basename(caminho)
  warnings_capturados <- character(0)

  obj <- withCallingHandlers(
    tryCatch(readRDS(caminho), error = function(e) e),
    warning = function(w) {
      warnings_capturados <<- c(warnings_capturados, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(obj, "error")) {
    return(data.frame(arquivo = nome, status = "ERRO_LEITURA",
                       detalhe = conditionMessage(obj), stringsAsFactors = FALSE))
  }

  detalhe <- character(0)

  if (length(warnings_capturados) > 0) {
    detalhe <- c(detalhe, paste0("WARNING NA LEITURA: ", paste(unique(warnings_capturados), collapse = " || ")))
  }

  if (is.data.frame(obj)) {
    n <- nrow(obj)
    tam_colunas <- vapply(obj, length, integer(1))

    colunas_zeradas      <- names(obj)[tam_colunas == 0 & n > 0]
    colunas_incompativeis <- setdiff(names(obj)[tam_colunas != n], colunas_zeradas)

    if (length(colunas_zeradas) > 0) {
      detalhe <- c(detalhe, paste0("COLUNA(S) DE TAMANHO 0 (nrow=", n, "): ",
                                    paste(colunas_zeradas, collapse = ", ")))
    }
    if (length(colunas_incompativeis) > 0) {
      detalhe <- c(detalhe, paste0("COLUNA(S) COM TAMANHO != nrow: ",
                                    paste(colunas_incompativeis, collapse = ", ")))
    }
    if (n == 0) {
      detalhe <- c(detalhe, "AVISO: 0 linhas (confirmar se e esperado)")
    }

    # NOVO (achado real, crash em producao 2026-07): objetos sf (pma_simpl,
    # ti_simpl, uc_simpl, qui_simpl) precisam ser so POLYGON/MULTIPOLYGON —
    # qualquer outro tipo (GEOMETRYCOLLECTION em especial) faz o
    # leaflet::addPolygons() do app.R travar a sessao inteira do usuario
    # ("Don't know how to get polygon data from object..."). Checagem
    # estrutural (tipo de geometria), no mesmo espirito das checagens acima —
    # nao depende do texto do erro do leaflet continuar igual no futuro.
    if (inherits(obj, "sf")) {
      tipos <- as.character(sf::st_geometry_type(obj))
      tipos_ok <- c("POLYGON", "MULTIPOLYGON")
      tipos_ruins <- setdiff(unique(tipos), tipos_ok)
      if (length(tipos_ruins) > 0) {
        n_ruins <- sum(tipos %in% tipos_ruins)
        detalhe <- c(detalhe, paste0("GEOMETRIA INCOMPATIVEL COM leaflet::addPolygons — ",
                                      n_ruins, " linha(s) do tipo ",
                                      paste(tipos_ruins, collapse = ", ")))
      }
    }
  } else if (is.list(obj) && length(obj) == 0) {
    detalhe <- c(detalhe, "LISTA VAZIA")
  }

  status <- if (length(detalhe) == 0) "OK" else "PROBLEMA"
  data.frame(arquivo = nome, status = status,
             detalhe = paste(detalhe, collapse = " | "), stringsAsFactors = FALSE)
}

resultado <- do.call(rbind, lapply(arquivos_rds, checar_rds))

cat("\n================ RESUMO (", nrow(resultado), "arquivos ) ================\n")
print(resultado[, c("arquivo", "status")], row.names = FALSE)

problemas <- resultado[resultado$status != "OK", ]
if (nrow(problemas) > 0) {
  cat("\n================ DETALHES DOS PROBLEMAS ================\n")
  for (i in seq_len(nrow(problemas))) {
    cat("\n->", problemas$arquivo[i], "\n   ", problemas$detalhe[i], "\n")
  }
  cat("\n", nrow(problemas), "de", nrow(resultado), "arquivo(s) com problema — NAO subir pro droplet ainda.\n")
} else {
  cat("\nTodos os", nrow(resultado), "arquivos passaram limpo. Pode subir pro droplet.\n")
}