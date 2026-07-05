################################################################################
# 08_proc_shiny_geo.R
#
# Prepara os objetos "gerais" do Shiny (abas 1-3: Tabela / Anual / Mensal):
# cfem.rds, cfem_anual.rds, cfem_mensal.rds, pma_simpl.rds, ti/uc/qui_simpl.rds
# e os lookups de filtro encadeado (lk_*_tab1/2/3.rds).
#
# Atualizacao do antigo 05_proc_shiny_geo_old.R: MESMA fonte de dados (CSV/SHP
# publicos em data/result_shiny, ja processados pelo 05_integracao_final.R —
# decisao do usuario: nao ler direto do checkpoint interno, o CSV/SHP publico
# e a fonte unica de verdade), MESMA logica de agregacao (nenhuma mudanca de
# metodologia aqui). Unica mudanca real: path hardcoded "C:/GP/anm-geo"
# trocado por here::here(), para bater com o padrao do resto do pipeline.
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
# XY,GEOMETRYCOLLECTION,sfg"). Lacuna pre-existente (o 05_proc_shiny_geo_old.R
# tambem nunca aplicou isso) — corrigido aqui porque e exatamente o tipo de
# problema ja documentado como aprendizado do projeto para shapefiles do
# IBGE/pipeline.
tornar_valido <- function(x) {
  x <- suppressWarnings(sf::st_make_valid(x))
  # st_make_valid() em geometrias muito quebradas pode devolver
  # GEOMETRYCOLLECTION (polígono + fragmentos de linha/ponto residuais).
  # ms_simplify() so aceita (multi)poligono OU (multi)linha, nao mistura —
  # todas as 4 camadas aqui (PMA, TI, UC, quilombolas) sao poligonais por
  # natureza, entao extraimos so a parte poligonal e descartamos o residuo.
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  sf::st_make_valid(x)
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
simplify_and_save <- function(sf_obj, out_path, keep_ratio) {
  if (nrow(sf_obj) > 0) {
    simplified <- rmapshaper::ms_simplify(sf_obj, keep = keep_ratio, keep_shapes = TRUE)
    saveRDS(simplified, out_path)
  }
}

simplify_and_save(ti,  file.path(OUTPUT_DIR, "ti_simpl.rds"),  0.3)
simplify_and_save(uc,  file.path(OUTPUT_DIR, "uc_simpl.rds"),  0.3)
simplify_and_save(qui, file.path(OUTPUT_DIR, "qui_simpl.rds"), 0.3)
simplify_and_save(pma, file.path(OUTPUT_DIR, "pma_simpl.rds"), 0.1)

message("\n=== 08_proc_shiny_geo.R — CONCLUIDO ===")