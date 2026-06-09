################################################################################
# 03_final_proc.R  —  versão 2
#
# OBJETIVO GERAL
#   Consolidar os dados limpos (saídos do 02_pre_proc.R) em três produtos:
#     1) result_shiny/ — shapefile completo + CSVs para o dashboard Shiny
#     2) result_gee/   — idem, recortado para o BIOMA Amazônia
#     3) result_db/    — versão ENXUTA para carga no PostGIS

################################################################################

# =============================================================================
# SETUP & CONFIGURAÇÃO
# =============================================================================

rm(list = ls(all.names = TRUE))
options(scipen = 999)

# IMPORTANTE: limpa variáveis de ambiente que apontariam para a PROJ/GDAL do
# PostgreSQL (causa do erro "output crs is not valid"). Tem de vir ANTES de
# carregar o terra.
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("GDAL_DATA")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(tidyterra)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(lubridate)
  library(stringi)
  library(stringr)
  library(glue)
  library(here)
})

# --- Caminhos -----------------------------------------------------------------
ROOT         <- here::here()
RAW_DIR      <- here::here("data", "raw_data")
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
CLEAN_DIR    <- here::here("data", "clean_data")
CKPT_DIR     <- here::here("data", "_checkpoints")

RESULT_SHINY <- here::here("data", "result_shiny")
RESULT_GEE   <- here::here("data", "result_gee")
RESULT_DB    <- here::here("data", "result_db")

for (d in c(CLEAN_DIR, CKPT_DIR, RESULT_SHINY, RESULT_GEE, RESULT_DB)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

MICRO_DIR <- here::here("data", "raw_data", "anm_microdados", "microdados-scm")
MUNI_DIR  <- here::here("data", "raw_data", "BR_Municipios_2025")
AMZL_DIR  <- here::here("data", "raw_data", "Limites_Amazonia_Legal_2024")
BIOMA_DIR <- here::here("data", "raw_data", "Biomas_250mil")

# --- Funções de checkpoint ----------------------------------------------------
save_ckpt <- function(obj, nome) {
  caminho <- file.path(CKPT_DIR, paste0(nome, ".rds"))
  if (inherits(obj, "SpatVector")) {
    saveRDS(terra::wrap(obj), caminho)
  } else {
    saveRDS(obj, caminho)
  }
  invisible(caminho)
}

load_ckpt <- function(nome) {
  caminho <- file.path(CKPT_DIR, paste0(nome, ".rds"))
  if (!file.exists(caminho)) stop(glue("Checkpoint nao encontrado: {nome}.rds - rode o bloco anterior primeiro."))
  obj <- readRDS(caminho)
  if (inherits(obj, "PackedSpatVector")) terra::vect(obj) else obj
}

# --- Funções auxiliares de domínio -------------------------------------------
# Remove o ponto do dsprocesso (microdados) p/ casar com 'processo' (SIGMINE).
limpar_dsprocesso <- function(x) stringr::str_replace_all(as.character(x), "\\.", "")

# Reprojeta p/ métrico (5880), valida e remove geometrias vazias.
clean_geom_5880 <- function(x) {
  x <- terra::project(x, "EPSG:5880") # SIRGAS 2000 Polyconic para áreas e bf
  x <- terra::makeValid(x)
  x[!terra::is.empty(x), ]
}

# Lista de grupos minerais: mapeia substância declarada e cria grupo padronizado.
target_minerals_list <- list(
  ouro         = c("OURO","MINÉRIO DE OURO","OURO NATIVO","OURO PIGMENTO","ALUVIÃO AURÍFERO"),
  diamante     = c("DIAMANTE","DIAMANTE INDUSTRIAL","CASCALHO DIAMANTÍFERO"),
  litio        = c("LÍTIO","MINÉRIO DE LÍTIO","ESPODUMÊNIO","LEPIDOLITA","PETALITA","AMBLIGONITA","POLUCITA","KUNZITA"),
  niobio       = c("NIÓBIO","MINÉRIO DE NIÓBIO","COLUMBITA","PIROCLORO"),
  tantalo      = c("TÂNTALO","MINÉRIO DE TÂNTALO","TANTALITA","TANTALITA-COLUMBITA"),
  estanho      = c("ESTANHO","MINÉRIO DE ESTANHO","CASSITERITA","ALUVIÃO ESTANÍFERO"),
  tungstenio   = c("TUNGSTÊNIO","MINÉRIO DE TUNGSTÊNIO","WOLFRAMITA","SCHEELITA"),
  titanio      = c("TITÂNIO","MINÉRIO DE TITÂNIO","ILMENITA","RUTILO","TITANITA"),
  terras_raras = c("TERRAS RARAS","MONAZITA","MINÉRIO DE CÉRIO"),
  cobalto      = c("MINÉRIO DE COBALTO"),
  grafite      = c("GRAFITA"),
  niquel       = c("NÍQUEL","MINÉRIO DE NÍQUEL","SILICATOS DE NÍQUEL"),
  vanadio      = c("VANÁDIO","MINÉRIO DE VANÁDIO"),
  molibdenio   = c("MOLIBDÊNIO","MINÉRIO DE MOLIBDÊNIO","MOLIBDENITA")
)
target_minerals <- unique(unlist(target_minerals_list))

classificar_grupo <- function(subs) {
  dplyr::case_when(
    subs %in% target_minerals_list$ouro         ~ "OURO",
    subs %in% target_minerals_list$diamante     ~ "DIAMANTE",
    subs %in% target_minerals_list$litio        ~ "LÍTIO",
    subs %in% target_minerals_list$niobio       ~ "NIÓBIO",
    subs %in% target_minerals_list$tantalo      ~ "TÂNTALO",
    subs %in% target_minerals_list$estanho      ~ "ESTANHO",
    subs %in% target_minerals_list$tungstenio   ~ "TUNGSTÊNIO",
    subs %in% target_minerals_list$titanio      ~ "TITÂNIO",
    subs %in% target_minerals_list$terras_raras ~ "TERRAS RARAS",
    subs %in% target_minerals_list$cobalto      ~ "COBALTO",
    subs %in% target_minerals_list$grafite      ~ "GRAFITE",
    subs %in% target_minerals_list$niquel       ~ "NÍQUEL",
    subs %in% target_minerals_list$vanadio      ~ "VANÁDIO",
    subs %in% target_minerals_list$molibdenio   ~ "MOLIBDÊNIO",
    TRUE                                         ~ "OUTROS"
  )
}

# =============================================================================
# BLOCO 1 — CADASTRO MINEIRO (SCM)
# =============================================================================

cm <- readr::read_csv(file.path(PRE_PROC_DIR, "cadastro_mineiro.csv"),
                      show_col_types = FALSE)

dict_rename <- c(
  "^Superintendência$"                                                      = "SUPERINTEN",
  "^Processo$"                                                              = "PROCESSO",
  "^Tipo\\.de\\.requerimento$|^Tipo de requerimento$"                       = "TIPO_REQcm",
  "^Fase\\.Atual$|^Fase Atual$"                                             = "FASEcm",
  "^CPF\\.CNPJ\\.do\\.titular$|^CPF/CNPJ do titular$|^CPF CNPJ do titular$" = "CPF_CNPJcm",
  "^Titular$"                                                               = "TITULARcm",
  "^Municipio\\.s\\.$|^Municipio\\(s\\)$"                                   = "name_muni",
  "^Substância\\.s\\.$|^Substância\\(s\\)$"                                 = "SUBScm",
  "^Tipo\\.s\\.\\.de\\.Uso$|^Tipo\\(s\\) de Uso$"                           = "TIPO_USO",
  "^Situação$"                                                              = "STATUS",
  "^Localidade$"                                                            = "LOCALIDADE",
  "^QuantidadeMinerio$"                                                     = "QTD_MINERIO",
  "^DataPublicacao$"                                                        = "DT_PUBLICACAO",
  "^Data da Cessão$"                                                        = "DT_CESSAO"
)
n_orig <- names(cm); n_new <- n_orig
for (pat in names(dict_rename)) {
  n_new <- ifelse(stringr::str_detect(n_orig, stringr::regex(pat)), dict_rename[pat], n_new)
}
names(cm) <- n_new

cm_clean <- cm |>
  dplyr::select(PROCESSO, TIPO_REQcm, FASEcm, CPF_CNPJcm, TITULARcm, SUBScm, DT_CESSAO) |>
  dplyr::filter(!is.na(PROCESSO)) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper))

contagem_conflitos <- cm_clean |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    tem_conflito = (
      dplyr::n_distinct(TIPO_REQcm, na.rm = TRUE) > 1 |
      dplyr::n_distinct(FASEcm,     na.rm = TRUE) > 1 |
      dplyr::n_distinct(SUBScm,     na.rm = TRUE) > 1 |
      dplyr::n_distinct(CPF_CNPJcm, na.rm = TRUE) > 1 |
      dplyr::n_distinct(TITULARcm,  na.rm = TRUE) > 1
    ), .groups = "drop"
  )

cm_normais <- cm_clean |>
  dplyr::inner_join(dplyr::filter(contagem_conflitos, !tem_conflito), by = "PROCESSO") |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(dplyr::across(
    c(TIPO_REQcm, FASEcm, SUBScm, CPF_CNPJcm, TITULARcm),
    ~ dplyr::last(stats::na.omit(.x))), .groups = "drop")

cm_conflitos <- cm_clean |>
  dplyr::inner_join(dplyr::filter(contagem_conflitos, tem_conflito), by = "PROCESSO")

if (nrow(cm_conflitos) > 0) {
  cm_resolvidos <- cm_conflitos |>
    dplyr::mutate(DT_CESSAO_formatada = as.Date(DT_CESSAO, format = "%d/%m/%Y")) |>
    dplyr::group_by(PROCESSO) |>
    dplyr::arrange(PROCESSO, dplyr::desc(DT_CESSAO_formatada)) |>
    dplyr::summarise(dplyr::across(
      c(TIPO_REQcm, FASEcm, SUBScm, CPF_CNPJcm, TITULARcm),
      ~ dplyr::first(stats::na.omit(.x))), .groups = "drop")
} else {
  cm_resolvidos <- tibble::tibble()
}

cm_unique <- dplyr::bind_rows(cm_normais, cm_resolvidos)

save_ckpt(cm_unique, "01_cm_unique")

# =============================================================================
# BLOCO 2 — PMA
# =============================================================================

cm_unique <- load_ckpt("01_cm_unique")

pma0 <- terra::vect(file.path(PRE_PROC_DIR, "sigmine_pma.shp"))

pma1 <- pma0 |>
  dplyr::filter(AREA_HA > 0) |>
  dplyr::filter(!is.na(PROCESSO) & PROCESSO != "" & PROCESSO != "0")

invalid_geom <- !terra::is.valid(pma1)
if (any(invalid_geom)) {
  pma1 <- rbind(terra::makeValid(pma1[invalid_geom, ]), pma1[!invalid_geom, ])
}

pma2 <- pma1 |>
  dplyr::select(-any_of(c("USO", "UF", "DSProcesso", "ID", "NUMERO", "ANO"))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::rename(TITULAR = NOME) |>
  dplyr::mutate(AREA_orig = AREA_HA)

sf::sf_use_s2(FALSE)
pma3 <- pma2 |> sf::st_as_sf() |> sf::st_make_valid() |> sf::st_transform(4326)
sf::sf_use_s2(TRUE)

pma3_m <- sf::st_transform(pma3, 5880) |> sf::st_make_valid()

pma4 <- pma3_m |>
  dplyr::group_by(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS) |>
  dplyr::summarise(AREA_orig = sum(AREA_orig, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(AREA_HA = as.numeric(sf::st_area(geometry)) / 10000) |>
  sf::st_transform(4326)

ids_conflict <- sf::st_drop_geometry(pma4) |>
  dplyr::filter(duplicated(PROCESSO) | duplicated(PROCESSO, fromLast = TRUE)) |>
  dplyr::pull(PROCESSO) |> unique()

pma5 <- pma4 |> dplyr::filter(!PROCESSO %in% ids_conflict)
pma6 <- pma4 |> dplyr::filter(PROCESSO %in% ids_conflict)

if (nrow(pma6) > 0) {
  pma6_resolved <- pma6 |>
    sf::st_transform(5880) |>
    dplyr::group_by(PROCESSO) |>
    dplyr::summarise(
      ULT_EVENTO = dplyr::first(ULT_EVENTO), TITULAR = dplyr::first(TITULAR),
      SUBS = dplyr::first(SUBS), AREA_orig = dplyr::first(AREA_orig), .groups = "drop") |>
    dplyr::mutate(AREA_HA = as.numeric(sf::st_area(geometry)) / 10000) |>
    sf::st_transform(4326) |>
    dplyr::left_join(cm_unique, by = "PROCESSO") |>
    dplyr::rename(FASE = FASEcm) |>
    dplyr::select(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS, AREA_HA, AREA_orig, geometry)
}

pma7sf <- if (nrow(pma6) > 0) rbind(pma5, pma6_resolved) else pma5
pma7   <- terra::vect(pma7sf)

cm_attrs <- cm_unique |> dplyr::select(PROCESSO, TIPO_REQcm, CPF_CNPJcm)
pma_cm   <- tidyterra::left_join(pma7, cm_attrs, by = "PROCESSO")

terra::writeVector(pma_cm, file.path(CLEAN_DIR, "pma_clean_cm.shp"), overwrite = TRUE)
save_ckpt(pma_cm, "02_pma_cm")

# =============================================================================
# BLOCO 3 — RECORTE AMAZÔNIA LEGAL
# =============================================================================

pma_cm <- load_ckpt("02_pma_cm") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                              ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

amzl <- terra::vect(list.files(AMZL_DIR, pattern = "\\.shp$", full.names = TRUE)[1]) |>
  terra::project(terra::crs(pma_cm))

ti  <- terra::vect(file.path(PRE_PROC_DIR, "terras_indigenas.shp"))
uc  <- terra::vect(file.path(PRE_PROC_DIR, "unidades_conservacao.shp"))
qui <- terra::vect(file.path(PRE_PROC_DIR, "quilombolas.shp"))

intersect_ids <- terra::is.related(pma_cm, amzl, "intersects")
pma_amzl       <- pma_cm[intersect_ids, ]
processos_amzl <- unique(pma_amzl$PROCESSO)

ti_amzl  <- ti[terra::is.related(ti,   amzl, "intersects"), ]
uc_amzl  <- uc[terra::is.related(uc,   amzl, "intersects"), ]
qui_amzl <- qui[terra::is.related(qui, amzl, "intersects"), ]

terra::writeVector(pma_amzl, file.path(CLEAN_DIR, "pma_amzl.shp"), overwrite = TRUE)
save_ckpt(pma_amzl,       "03_pma_amzl")
save_ckpt(ti_amzl,        "03_ti_amzl")
save_ckpt(uc_amzl,        "03_uc_amzl")
save_ckpt(qui_amzl,       "03_qui_amzl")
save_ckpt(processos_amzl, "03_processos_amzl")

# =============================================================================
# BLOCO 4 — MUNICÍPIO
# =============================================================================

pma_amzl <- load_ckpt("03_pma_amzl")

proc_mun <- readr::read_delim(file.path(MICRO_DIR, "ProcessoMunicipio.txt"),
  delim = ";", locale = readr::locale(encoding = "Windows-1252"),
  col_types = readr::cols(.default = "c"), show_col_types = FALSE)
names(proc_mun) <- tolower(names(proc_mun))

muni_lk <- readr::read_delim(file.path(MICRO_DIR, "Municipio.txt"),
  delim = ";", locale = readr::locale(encoding = "Windows-1252"),
  col_types = readr::cols(.default = "c"), show_col_types = FALSE)
names(muni_lk) <- tolower(names(muni_lk))

col_id_mun   <- names(muni_lk)[stringr::str_detect(names(muni_lk), "idmunicip")][1]
col_nome_mun <- names(muni_lk)[stringr::str_detect(names(muni_lk), "nmmunicip|nome")][1]
col_uf_mun   <- names(muni_lk)[stringr::str_detect(names(muni_lk), "siglauf|uf")][1]

muni_lk2 <- muni_lk |>
  dplyr::transmute(
    idmunicipio = .data[[col_id_mun]],
    munic_micro = toupper(.data[[col_nome_mun]]),
    uf_micro    = if (!is.na(col_uf_mun)) toupper(.data[[col_uf_mun]]) else NA_character_
  )

proc_mun_resumo <- proc_mun |>
  dplyr::left_join(muni_lk2, by = "idmunicipio") |>
  dplyr::mutate(processo = limpar_dsprocesso(dsprocesso)) |>
  dplyr::group_by(processo) |>
  dplyr::summarise(
    n_munic     = dplyr::n_distinct(idmunicipio),
    munic_unico = dplyr::first(munic_micro),
    uf_unico    = dplyr::first(uf_micro),
    .groups = "drop"
  )

muni_ibge <- terra::vect(list.files(MUNI_DIR, pattern = "\\.shp$", full.names = TRUE)[1]) |>
  terra::project(terra::crs(pma_amzl))
centroides <- terra::centroids(pma_amzl, inside = TRUE)

nm_ibge   <- names(muni_ibge)
col_nmmun <- "NM_MUN"
col_ufmun <- "SIGLA_UF"

cent_mun <- terra::intersect(centroides, muni_ibge) |>
  as.data.frame() |>
  dplyr::transmute(
    PROCESSO,
    munic_centroide = toupper(.data[[col_nmmun]]),
    uf_centroide    = if (!is.na(col_ufmun)) toupper(.data[[col_ufmun]]) else NA_character_
  ) |>
  dplyr::distinct(PROCESSO, .keep_all = TRUE)

munic_final <- as.data.frame(pma_amzl) |>
  dplyr::select(PROCESSO) |>
  dplyr::left_join(proc_mun_resumo, by = c("PROCESSO" = "processo")) |>
  dplyr::left_join(cent_mun, by = "PROCESSO") |>
  dplyr::mutate(
    n_munic = tidyr::replace_na(n_munic, 0L),
    munic = dplyr::case_when(
      n_munic == 1 ~ munic_unico,
      n_munic >  1 ~ munic_centroide,
      n_munic == 0 & !is.na(munic_centroide) ~ munic_centroide,
      TRUE ~ NA_character_),
    uf = dplyr::case_when(
      n_munic == 1 ~ uf_unico,
      n_munic >  1 ~ uf_centroide,
      n_munic == 0 & !is.na(uf_centroide) ~ uf_centroide,
      TRUE ~ NA_character_),
    munic_fonte = dplyr::case_when(
      n_munic == 1 ~ "microdado",
      n_munic >  1 ~ "centroide",
      n_munic == 0 & !is.na(munic_centroide) ~ "centroide_sem_micro",
      TRUE ~ NA_character_)
  ) |>
  dplyr::select(PROCESSO, munic, uf, n_munic, munic_fonte)

pma_amzl <- tidyterra::left_join(pma_amzl, munic_final, by = "PROCESSO")

save_ckpt(pma_amzl, "04_pma_amzl")


# =============================================================================
# BLOCO 5 — INTERSEÇÃO
# =============================================================================

pma_amzl <- load_ckpt("04_pma_amzl")
ti_amzl  <- load_ckpt("03_ti_amzl")
uc_amzl  <- load_ckpt("03_uc_amzl")
qui_amzl <- load_ckpt("03_qui_amzl")

# Reprojeta p/ métrico + valida (clean_geom_5880 evita TopologyException).
pma_m <- clean_geom_5880(pma_amzl)
ti_m  <- clean_geom_5880(ti_amzl)
uc_m  <- clean_geom_5880(uc_amzl)
qui_m <- clean_geom_5880(qui_amzl)

# UC relevante: Proteção Integral OU RESEX.
uc_pi_resex <- uc_m[uc_m$grupo == "PROTEÇÃO INTEGRAL" | uc_m$sigla_snuc == "RESEX", ]
if (nrow(uc_pi_resex) == 0) {
  uc_pi_resex <- uc_m[toupper(uc_m$grupo) == "PROTEÇÃO INTEGRAL" | toupper(uc_m$sigla_snuc) == "RESEX", ]
}

# --- Buffers de entorno ----------------------------------
qui_bf <- terra::makeValid(terra::buffer(qui_m, width = 10000))
ti_bf  <- terra::makeValid(terra::buffer(ti_m,  width = 10000))
uc_bf  <- terra::makeValid(terra::buffer(
  uc_pi_resex,
  width = ifelse(toupper(trimws(uc_pi_resex$pl_manejo)) == "SIM", 10000, 2000)))

ti_diss  <- terra::makeValid(terra::aggregate(ti_m))
uc_diss  <- terra::makeValid(terra::aggregate(uc_pi_resex))
qui_diss <- terra::makeValid(terra::aggregate(qui_m))

qui_bf_only <- terra::makeValid(terra::erase(qui_bf, qui_diss))
ti_bf_only  <- terra::makeValid(terra::erase(ti_bf,  ti_diss))
uc_bf_only  <- terra::makeValid(terra::erase(uc_bf,  uc_diss))

# --- Função de sobreposição (>= 5% da área do processo) ----------------------
calc_overlap <- function(pma_lyr, tp_lyr, flag_name) {
  inter <- terra::intersect(pma_lyr, tp_lyr)
  if (nrow(inter) == 0) {
    return(data.frame(PROCESSO = character(0)) |> dplyr::mutate(!!flag_name := integer(0)))
  }
  inter$area_inter <- terra::expanse(inter, unit = "ha")
  inter <- inter |> tidyterra::select("PROCESSO", "AREA_HA", "area_inter") |> as.data.frame()
  inter$Propor <- as.numeric(inter$area_inter / inter$AREA_HA)
  inter |>
    dplyr::filter(Propor >= 0.05) |>
    dplyr::mutate(!!flag_name := 1L) |>
    dplyr::select(PROCESSO, !!flag_name) |>
    dplyr::distinct(PROCESSO, .keep_all = TRUE)
}

# (a) Sobreposição DIRETA -----------------------------------------------------
df_ti_pma  <- calc_overlap(pma_m, ti_m,        "TIov")
df_uc_pma  <- calc_overlap(pma_m, uc_pi_resex, "UCov")
df_qui_pma <- calc_overlap(pma_m, qui_m,       "QUIov")

pma_tp0 <- pma_m |>
  tidyterra::left_join(df_ti_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_uc_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_qui_pma, by = "PROCESSO") |>
  dplyr::mutate(TIov = tidyr::replace_na(TIov,0L),
                UCov = tidyr::replace_na(UCov,0L),
                QUIov = tidyr::replace_na(QUIov,0L))

pma_tp_uc <- as.data.frame(terra::intersect(pma_tp0, uc_pi_resex)) |>
  dplyr::select("PROCESSO","sigla_snuc","nome_uc") |>
  dplyr::rename(UCtype = sigla_snuc, UCname = nome_uc) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(UCtype = paste(unique(UCtype), collapse="|"),
                   UCname = paste(unique(UCname), collapse="|"), .groups="drop")
pma_tp_ti <- as.data.frame(terra::intersect(pma_tp0, ti_m)) |>
  dplyr::select("PROCESSO","terrai_nom") |> dplyr::rename(TIname = terrai_nom) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(TIname = toupper(paste(unique(TIname), collapse="|")), .groups="drop")
pma_tp_qui <- as.data.frame(terra::intersect(pma_tp0, qui_m)) |>
  dplyr::select("PROCESSO","nm_comunid") |> dplyr::rename(QUIname = nm_comunid) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(QUIname = paste(unique(QUIname), collapse="|"), .groups="drop")

pma_tp1 <- pma_tp0 |>
  tidyterra::left_join(pma_tp_uc,  by="PROCESSO") |>
  tidyterra::left_join(pma_tp_ti,  by="PROCESSO") |>
  tidyterra::left_join(pma_tp_qui, by="PROCESSO")

# (b) Sobreposição com o ENTORNO --------------------------------------
df_ti_donut  <- calc_overlap(pma_tp1, ti_bf_only,  "TIov10km")
df_uc_donut  <- calc_overlap(pma_tp1, uc_bf_only,  "UCov2_10km")
df_qui_donut <- calc_overlap(pma_tp1, qui_bf_only, "QUIov10km")

pma_tp2 <- pma_tp1 |>
  tidyterra::left_join(df_ti_donut,  by="PROCESSO") |>
  tidyterra::left_join(df_uc_donut,  by="PROCESSO") |>
  tidyterra::left_join(df_qui_donut, by="PROCESSO") |>
  dplyr::mutate(TIov10km = tidyr::replace_na(TIov10km,0L),
                UCov2_10km = tidyr::replace_na(UCov2_10km,0L),
                QUIov10km = tidyr::replace_na(QUIov10km,0L))

pma_tp_uc_ov <- as.data.frame(terra::intersect(pma_tp2, uc_bf_only)) |>
  dplyr::select("PROCESSO","sigla_snuc","nome_uc") |>
  dplyr::rename(UCtype_ov = sigla_snuc, UCname_ov = nome_uc) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(UCtype_ov = paste(unique(UCtype_ov), collapse="|"),
                   UCname_ov = paste(unique(UCname_ov), collapse="|"), .groups="drop")
pma_tp_ti_ov <- as.data.frame(terra::intersect(pma_tp2, ti_bf_only)) |>
  dplyr::select("PROCESSO","terrai_nom") |> dplyr::rename(TIname_ov = terrai_nom) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(TIname_ov = toupper(paste(unique(TIname_ov), collapse="|")), .groups="drop")
pma_tp_qui_ov <- as.data.frame(terra::intersect(pma_tp2, qui_bf_only)) |>
  dplyr::select("PROCESSO","nm_comunid") |> dplyr::rename(QUIname_ov = nm_comunid) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(QUIname_ov = paste(unique(QUIname_ov), collapse="|"), .groups="drop")

pma_tp <- pma_tp2 |>
  tidyterra::left_join(pma_tp_uc_ov,  by="PROCESSO") |>
  tidyterra::left_join(pma_tp_ti_ov,  by="PROCESSO") |>
  tidyterra::left_join(pma_tp_qui_ov, by="PROCESSO")

# --- Flags de embargo / infração -----------------------
EMBmtSEMA <- clean_geom_5880(terra::vect(file.path(PRE_PROC_DIR, "sema_mt_embargos.shp")))
EMBmtSIGA <- clean_geom_5880(terra::vect(file.path(PRE_PROC_DIR, "sema_mt_embargos_siga.shp")))
EMBib     <- clean_geom_5880(terra::vect(file.path(PRE_PROC_DIR, "ibama_embargos.shp")))
EMBic     <- clean_geom_5880(terra::vect(file.path(PRE_PROC_DIR, "icmbio_embargos.shp")))
INFmtSIGA <- clean_geom_5880(terra::vect(file.path(PRE_PROC_DIR, "sema_mt_infracoes_siga.shp")))
INFic     <- clean_geom_5880(terra::vect(file.path(PRE_PROC_DIR, "icmbio_infracoes.shp")))

pma_tp$inf_MT  <- as.integer(terra::is.related(pma_tp, INFmtSIGA, "intersects"))
pma_tp$inf_IC  <- as.integer(terra::is.related(pma_tp, INFic,     "intersects"))
pma_tp$emb_MTa <- as.integer(terra::is.related(pma_tp, EMBmtSEMA, "intersects"))
pma_tp$emb_MTb <- as.integer(terra::is.related(pma_tp, EMBmtSIGA, "intersects"))
pma_tp$emb_IB  <- as.integer(terra::is.related(pma_tp, EMBib,     "intersects"))
pma_tp$emb_IC  <- as.integer(terra::is.related(pma_tp, EMBic,     "intersects"))

pma_tp <- terra::project(pma_tp, "EPSG:4326")

save_ckpt(pma_tp, "05_pma_tp")

# =============================================================================
# BLOCO 6 — CFEM: limpeza, razão social, alíquotas e correção de peso
# -----------------------------------------------------------------------------
# 6.1 Lê arrecadação e autuação, reconstrói o número de processo, padroniza.
# 6.2 Traz a razão social via Pessoa.txt dos microdados.
# 6.3 Filtra para os processos da Amazônia Legal (mesma lista do bloco 3).
# 6.4 Calcula peso (kg/g), grupo mineral, alíquota e preço por grama.
# 6.5 Correção de peso do OURO (mediana hierárquica + PowerOf10).
# 6.6 Correção de peso da CASSITERITA (mesma lógica, calibrada).
# =============================================================================

processos_amzl <- load_ckpt("03_processos_amzl")
pma_tp         <- load_ckpt("05_pma_tp")

# Autuação
cfem_aut <- readr::read_csv(file.path(PRE_PROC_DIR, "CFEM_Autuacao.csv"),
                            show_col_types = FALSE) |>
  dplyr::mutate(
    AnoPublicação = as.numeric(AnoPublicação),
    MêsPublicação = as.numeric(MêsPublicação)
  ) |>
  dplyr::select(-dplyr::any_of(c("ProcessoCobrança", "Tipo_PF_PJ", "NúmeroAuto"))) |>
  dplyr::rename(
    TITULARaut = NomeTitular, PROCESSO = ProcessoMinerário, SUBSaut = Substância,
    name_muni = Município, abbrev_state = UF, ANO = AnoPublicação,
    MES = MêsPublicação, VALORaut = Valor, CPF_CNPJaut = CPF_CNPJ
  ) |>
  dplyr::filter(!is.na(PROCESSO), !is.na(ANO), !is.na(MES),
                !is.na(SUBSaut), !is.na(CPF_CNPJaut), PROCESSO != "NA/NA") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper))

# Arrecadação
cfem_arr <- readr::read_csv(file.path(PRE_PROC_DIR, "CFEM_Arrecadacao.csv"),
                            col_types = readr::cols(
                              ValorRecolhido = readr::col_double(),
                              QuantidadeComercializada = readr::col_double()
                            ))

names(cfem_arr)[names(cfem_arr) == "Processo"] <- "ProcSemNum"
cfem_arr$PROCESSO <- paste(cfem_arr$ProcSemNum, cfem_arr$AnoDoProcesso, sep = "/")

cfem_arr <- cfem_arr |>
  dplyr::select(-dplyr::any_of(c("ProcSemNum", "Tipo_PF_PJ", "AnoDoProcesso", "DataCriacao"))) |>
  dplyr::rename(
    SUBSarr = Substância, name_muni = Município, code_muni = CodigoMunicipio,
    abbrev_state = UF, ANO = Ano, MES = Mês, QTD_MINERIO = QuantidadeComercializada,
    VALORarr = ValorRecolhido, CPF_CNPJarr = CPF_CNPJ, UM = UnidadeDeMedida
  ) |>
  dplyr::filter(!is.na(PROCESSO), !is.na(ANO), !is.na(MES),
                !is.na(SUBSarr), !is.na(CPF_CNPJarr), PROCESSO != "NA/NA") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper))

# Razão social via Pessoa.txt
pessoa <- readr::read_delim(
  file.path(MICRO_DIR, "Pessoa.txt"),
  delim = ";", locale = readr::locale(encoding = "Windows-1252"),
  col_types = readr::cols(.default = "c"), show_col_types = FALSE
) |>
  dplyr::rename(CPF_CNPJarr = NRCPFCNPJ, NOME_arr = NMPessoa) |>
  dplyr::select(CPF_CNPJarr, NOME_arr) |>
  dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
  dplyr::mutate(NOME_arr = toupper(NOME_arr))

cfem_arr <- cfem_arr |>
  dplyr::left_join(pessoa, by = "CPF_CNPJarr") |>
  dplyr::mutate(NOME_arr = dplyr::if_else(is.na(NOME_arr), "NOME DESCONHECIDO", NOME_arr))

# Filtro Amazônia Legal (mesma lista de processos do bloco 3) 
cfem_arr_amzl0 <- cfem_arr |>
  dplyr::filter(PROCESSO %in% processos_amzl) |>
  dplyr::mutate(row_id = dplyr::row_number())   # chave interna da correção

cfem_aut_amzl <- cfem_aut |>
  dplyr::filter(PROCESSO %in% processos_amzl)

# Peso, grupo, alíquota, preço
fatores_kg <- c("KG" = 1, "T" = 1000, "G" = 0.001, "CT" = 0.0002)
fatores_g  <- c("KG" = 1000, "T" = 1e6, "G" = 1, "CT" = 0.2)

cfem_arr_amzl1 <- cfem_arr_amzl0 |>
  dplyr::mutate(
    PESO_KG    = round(as.double(QTD_MINERIO) * unname(fatores_kg[UM]), 10),
    PESO_G     = round(as.double(QTD_MINERIO) * unname(fatores_g[UM]), 10),
    SUBSarrSIM = classificar_grupo(SUBSarr)
  )

pma_attrs <- as.data.frame(pma_tp) |>
  dplyr::select(PROCESSO, AREA_HA, FASE, ULT_EVENTO, TITULAR, SUBS,
                TIPO_REQcm, CPF_CNPJcm) |>
  dplyr::distinct(PROCESSO, .keep_all = TRUE)

cfem_arr_amzl2 <- dplyr::left_join(cfem_arr_amzl1, pma_attrs, by = "PROCESSO") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                              ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

# Parâmetros da correção
min_peso_g            <- 0.00000000000000000001
min_grp_muni          <- 5
min_grp_state         <- 10
min_grp_month         <- 15
min_grp_ano           <- 20
min_grp_global        <- 100
p_round_min           <- -20
p_round_max           <- 20
max_mediana_plausivel <- 1000
min_mediana_plausivel <- 30
corte                 <- as.Date("2017-11-01")   # mudança de legislação CFEM

cfem_arr_amzl3 <- cfem_arr_amzl2 |>
  dplyr::mutate(
    ANO = as.integer(ANO), MES = as.integer(MES),
    VALORarr = as.numeric(VALORarr),
    PESO_KG = as.numeric(PESO_KG), PESO_G = as.numeric(PESO_G),
    data = as.Date(sprintf("%04d-%02d-01", ANO, MES))
  ) |>
  dplyr::mutate(
    ALIQUOTA_PCT = dplyr::case_when(
      data >= corte & SUBSarrSIM == "OURO"     ~ 1.5,
      data >= corte & SUBSarrSIM == "DIAMANTE" ~ 2.0,
      data >= corte & SUBSarrSIM == "NIÓBIO"   ~ 3.0,
      data >= corte                            ~ 2.0,
      data <  corte & SUBSarrSIM == "OURO"     & grepl("GARIMPEIRA", ifelse(is.na(FASE), "", FASE), ignore.case = TRUE) ~ 0.2,
      data <  corte & SUBSarrSIM == "DIAMANTE" & grepl("GARIMPEIRA", ifelse(is.na(FASE), "", FASE), ignore.case = TRUE) ~ 0.2,
      data <  corte & SUBSarrSIM == "OURO"     ~ 2.0,
      data <  corte & SUBSarrSIM == "DIAMANTE" ~ 3.0,
      data <  corte & SUBSarrSIM == "NIÓBIO"   ~ 3.0,
      data <  corte                            ~ 2.0,
      TRUE                                     ~ NA_real_
    ),
    VALORtot     = round(VALORarr * (100 / ALIQUOTA_PCT), 2),
    preco_g_orig = dplyr::if_else(!is.na(PESO_G) & PESO_G > 0, VALORtot / PESO_G, NA_real_)
  )

# Funções da correção de peso (mediana hierárquica + PowerOf10) 
compute_median_hierarchical <- function(df, preco_col = "preco_g_orig",
                                        min_muni, min_state, min_month,
                                        min_ano, min_global,
                                        max_med_plaus, min_med_plaus) {
  df2 <- df |> dplyr::mutate(.idx = dplyr::row_number())
  med_muni  <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |>
    dplyr::group_by(data, code_muni) |>
    dplyr::summarise(n_m = dplyr::n(), med_m = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_state <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |>
    dplyr::group_by(data, abbrev_state) |>
    dplyr::summarise(n_s = dplyr::n(), med_s = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_month <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |>
    dplyr::group_by(data) |>
    dplyr::summarise(n_mo = dplyr::n(), med_mo = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_ano   <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |>
    dplyr::group_by(ANO) |>
    dplyr::summarise(n_a = dplyr::n(), med_a = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_global <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |>
    dplyr::summarise(n_g = dplyr::n(), med_g = median(.data[[preco_col]], na.rm = TRUE))

  out <- df2 |>
    dplyr::left_join(med_muni,  by = c("data", "code_muni")) |>
    dplyr::left_join(med_state, by = c("data", "abbrev_state")) |>
    dplyr::left_join(med_month, by = c("data")) |>
    dplyr::left_join(med_ano,   by = c("ANO")) |>
    dplyr::mutate(
      med_preco_base = dplyr::case_when(
        !is.na(n_m)  & n_m  >= min_muni  & med_m  <= max_med_plaus & med_m  >= min_med_plaus ~ med_m,
        !is.na(n_s)  & n_s  >= min_state & med_s  <= max_med_plaus & med_s  >= min_med_plaus ~ med_s,
        !is.na(n_mo) & n_mo >= min_month & med_mo <= max_med_plaus & med_mo >= min_med_plaus ~ med_mo,
        !is.na(n_a)  & n_a  >= min_ano   & med_a  <= max_med_plaus & med_a  >= min_med_plaus ~ med_a,
        !is.na(med_global$med_g) & med_global$n_g >= min_global &
          med_global$med_g <= max_med_plaus & med_global$med_g >= min_med_plaus ~ med_global$med_g,
        TRUE ~ NA_real_
      ),
      med_level = dplyr::case_when(
        !is.na(n_m)  & n_m  >= min_muni  & med_m  <= max_med_plaus & med_m  >= min_med_plaus ~ "muni",
        !is.na(n_s)  & n_s  >= min_state & med_s  <= max_med_plaus & med_s  >= min_med_plaus ~ "state",
        !is.na(n_mo) & n_mo >= min_month & med_mo <= max_med_plaus & med_mo >= min_med_plaus ~ "month",
        !is.na(n_a)  & n_a  >= min_ano   & med_a  <= max_med_plaus & med_a  >= min_med_plaus ~ "ano",
        !is.na(med_global$med_g) & med_global$n_g >= min_global &
          med_global$med_g <= max_med_plaus & med_global$med_g >= min_med_plaus ~ "global",
        TRUE ~ NA_character_
      )
    ) |> dplyr::arrange(.idx)
  list(med = out$med_preco_base, level = out$med_level)
}

suggest_weight_row <- function(VALORtot, PESO_G, med_preco,
                               p_range = p_round_min:p_round_max) {
  if (is.na(VALORtot) | is.na(med_preco) | med_preco <= 0) {
    return(list(PESO_G_sugerido = NA_real_, preco_g_sugerido = NA_real_,
                dist_rel_sug = NA_real_, corr_motivo = "no_med",
                candidate_name = NA_character_))
  }
  cands <- list("original" = PESO_G)
  for (p in p_range) if (p != 0) cands[[paste0("pow10_p", p)]] <- PESO_G * (10^p)
  cand_df <- tibble::tibble(name = names(cands), peso_cand = unlist(cands)) |>
    dplyr::mutate(
      preco_cand = dplyr::if_else(peso_cand > min_peso_g, VALORtot / peso_cand, NA_real_),
      dist_rel   = dplyr::if_else(!is.na(preco_cand), abs(preco_cand / med_preco - 1), NA_real_)
    )
  best_i <- which.min(replace(cand_df$dist_rel, is.na(cand_df$dist_rel), Inf))
  best   <- cand_df[best_i, ]
  list(PESO_G_sugerido = as.numeric(best$peso_cand),
       preco_g_sugerido = as.numeric(best$preco_cand),
       dist_rel_sug = as.numeric(best$dist_rel),
       corr_motivo = if (best$name == "original") "original" else "pow10",
       candidate_name = as.character(best$name))
}

# Correção do OURO
cfem_arr_amzl4 <- cfem_arr_amzl3 |>
  dplyr::filter(SUBSarrSIM == "OURO" &
                  FASE %in% c("LAVRA GARIMPEIRA","REQUERIMENTO DE LAVRA GARIMPEIRA",
                              "CONCESSÃO DE LAVRA","REQUERIMENTO DE LAVRA","AUTORIZAÇÃO DE PESQUISA"))

med_info <- compute_median_hierarchical(
  cfem_arr_amzl4, preco_col = "preco_g_orig",
  min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
  min_ano = min_grp_ano, min_global = min_grp_global,
  max_med_plaus = max_mediana_plausivel, min_med_plaus = min_mediana_plausivel
)

cfem_arr_amzl5 <- cfem_arr_amzl4 |>
  dplyr::mutate(med_preco_base = med_info$med, med_level = med_info$level) |>
  dplyr::rowwise() |>
  dplyr::mutate(sug = list(suggest_weight_row(VALORtot, PESO_G, med_preco_base))) |>
  tidyr::unnest_wider(sug) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    PESO_G_final  = dplyr::if_else(!is.na(PESO_G_sugerido), PESO_G_sugerido, PESO_G),
    PESO_KG_final = dplyr::if_else(!is.na(PESO_G_final), PESO_G_final / 1000, NA_real_),
    preco_g_final = dplyr::if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g,
                                   VALORtot / PESO_G_final, NA_real_)
  )

cfem_corr_join <- cfem_arr_amzl5 |>
  dplyr::select(row_id, candidate_name, PESO_G_final, PESO_KG_final, preco_g_final) |>
  dplyr::rename(corr = candidate_name)

cfem_final <- dplyr::left_join(cfem_arr_amzl3, cfem_corr_join, by = "row_id") |>
  dplyr::mutate(
    PESO_G_final  = dplyr::if_else(is.na(PESO_G_final), PESO_G, PESO_G_final),
    PESO_KG_final = dplyr::if_else(is.na(PESO_KG_final), PESO_KG, PESO_KG_final),
    preco_g_final = dplyr::if_else(is.na(preco_g_final),
                                   dplyr::if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g,
                                                  VALORtot / PESO_G_final, NA_real_),
                                   preco_g_final),
    corr = dplyr::if_else(is.na(corr), "original", corr)
  ) |>
  dplyr::mutate(
    ULT_EV_ID  = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT = stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(
      ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  )

# Correção da CASSITERITA
cfem_final <- cfem_final |> dplyr::mutate(row_id = dplyr::row_number())

max_mediana_plausivel <- 1.0      # cassiterita vale ~0,06 R$/g
min_mediana_plausivel <- 0.001

cass_amzl4 <- cfem_final |>
  dplyr::filter(SUBSarr == "CASSITERITA" &
                  FASE %in% c("LAVRA GARIMPEIRA","REQUERIMENTO DE LAVRA GARIMPEIRA",
                              "CONCESSÃO DE LAVRA","REQUERIMENTO DE LAVRA","AUTORIZAÇÃO DE PESQUISA"))

med_info_cass <- compute_median_hierarchical(
  cass_amzl4, preco_col = "preco_g_orig",
  min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
  min_ano = min_grp_ano, min_global = min_grp_global,
  max_med_plaus = max_mediana_plausivel, min_med_plaus = min_mediana_plausivel
)

cass_amzl5 <- cass_amzl4 |>
  dplyr::mutate(med_preco_base = med_info_cass$med, med_level = med_info_cass$level) |>
  dplyr::rowwise() |>
  dplyr::mutate(sug = list(suggest_weight_row(VALORtot, PESO_G, med_preco_base,
                                              p_range = p_round_min:p_round_max))) |>
  tidyr::unnest_wider(sug) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    PESO_G_final  = dplyr::if_else(!is.na(PESO_G_sugerido), PESO_G_sugerido, PESO_G),
    PESO_KG_final = dplyr::if_else(!is.na(PESO_G_final), PESO_G_final / 1000, NA_real_),
    preco_g_final = dplyr::if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g,
                                   VALORtot / PESO_G_final, NA_real_)
  )

cass_corr_join <- cass_amzl5 |>
  dplyr::select(row_id, candidate_name, PESO_G_final, PESO_KG_final, preco_g_final) |>
  dplyr::rename(corr_new = candidate_name, PESO_G_final_new = PESO_G_final,
                PESO_KG_final_new = PESO_KG_final, preco_g_final_new = preco_g_final)

cfem_final <- cfem_final |>
  dplyr::left_join(cass_corr_join, by = "row_id") |>
  dplyr::mutate(
    PESO_G_final  = dplyr::if_else(!is.na(PESO_G_final_new),  PESO_G_final_new,  PESO_G_final),
    PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_final_new), PESO_KG_final_new, PESO_KG_final),
    preco_g_final = dplyr::if_else(!is.na(preco_g_final_new), preco_g_final_new, preco_g_final),
    corr          = dplyr::if_else(!is.na(corr_new),          corr_new,          corr)
  ) |>
  dplyr::select(-PESO_G_final_new, -PESO_KG_final_new, -preco_g_final_new, -corr_new)

cfem_final <- cfem_final |> dplyr::select(-dplyr::any_of("row_id"))

save_ckpt(cfem_final,    "06_cfem_final")
save_ckpt(cfem_aut_amzl, "06_cfem_aut_amzl")


# =============================================================================
# BLOCO 7 — AGREGAÇÕES DA CFEM POR PROCESSO
# =============================================================================

cfem_final <- load_ckpt("06_cfem_final")
pma_tp     <- load_ckpt("05_pma_tp")

arr_corr_unique <- cfem_final |>
  dplyr::arrange(PROCESSO, ANO, MES) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    arr_kg_T  = sum(PESO_KG_final, na.rm = TRUE),
    arr_kg_L  = dplyr::if_else(all(is.na(PESO_KG_final)), NA_real_, dplyr::last(na.omit(PESO_KG_final))),
    arr_g_T   = sum(PESO_G_final,  na.rm = TRUE),
    arr_g_L   = dplyr::if_else(all(is.na(PESO_G_final)),  NA_real_, dplyr::last(na.omit(PESO_G_final))),
    arr_val_T = sum(VALORarr, na.rm = TRUE),
    arr_ndcl  = dplyr::n(),
    arr_nbuy  = dplyr::n_distinct(CPF_CNPJarr, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))

# Grupo mineral do PMA + decompõe ULT_EVENTO.
pma_tp <- pma_tp |>
  dplyr::mutate(
    SUBSpmaGRP = classificar_grupo(SUBS),
    ULT_EV_ID  = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT = stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(
      ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  )

pma_full <- tidyterra::left_join(pma_tp, arr_corr_unique, by = "PROCESSO") |>
  dplyr::mutate(
    arr_kg_T = tidyr::replace_na(arr_kg_T, 0),
    arr_g_T  = tidyr::replace_na(arr_g_T,  0),
    arr_val_T = tidyr::replace_na(arr_val_T, 0),
    arr_ndcl  = tidyr::replace_na(arr_ndcl, 0L),
    arr_nbuy  = tidyr::replace_na(arr_nbuy, 0L)
  )

save_ckpt(pma_full, "07_pma_full")

# =============================================================================
# BLOCO 8 — EXPORTS (result_shiny / result_gee / result_db)
# =============================================================================

pma_full      <- load_ckpt("07_pma_full")
cfem_final    <- load_ckpt("06_cfem_final")
cfem_aut_amzl <- load_ckpt("06_cfem_aut_amzl")
ti_amzl       <- load_ckpt("03_ti_amzl")
uc_amzl       <- load_ckpt("03_uc_amzl")
qui_amzl      <- load_ckpt("03_qui_amzl")

# SHINY
terra::writeVector(pma_full, file.path(RESULT_SHINY, "pma_amzl_ALLminerals_final.shp"), overwrite = TRUE)
terra::writeVector(ti_amzl,  file.path(RESULT_SHINY, "ti_amzl.shp"),  overwrite = TRUE)
terra::writeVector(uc_amzl,  file.path(RESULT_SHINY, "uc_amzl.shp"),  overwrite = TRUE)
terra::writeVector(qui_amzl, file.path(RESULT_SHINY, "qui_amzl.shp"), overwrite = TRUE)
readr::write_csv(cfem_final, file.path(RESULT_SHINY, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"))

# ---- 8.2 GEE (recorte pelo BIOMA Amazônia) ----------------------------------
bioma_full <- terra::vect(list.files(BIOMA_DIR, pattern = "\\.shp$", full.names = TRUE)[1])
bioma <- bioma_full[bioma_full$Bioma == "Amazônia", ] |>
  terra::project(terra::crs(pma_full))
dentro_bioma    <- terra::is.related(pma_full, bioma, "intersects")
pma_bioma       <- pma_full[dentro_bioma, ]
processos_bioma <- unique(pma_bioma$PROCESSO)
cfem_bioma      <- cfem_final |> dplyr::filter(PROCESSO %in% processos_bioma)

terra::writeVector(pma_bioma, file.path(RESULT_GEE, "pma_AMAZONIA_ALLminerals_GEE.shp"), overwrite = TRUE)
readr::write_csv(cfem_bioma, file.path(RESULT_GEE, "cfem_AMAZONIA_ALLminerals_GEE.csv"))
cfem_bioma_mensal <- cfem_bioma |>
  dplyr::mutate(data = as.Date(sprintf("%04d-%02d-01", ANO, MES)),
                proc_ano = paste0(trimws(PROCESSO), "/", ANO))
readr::write_csv(cfem_bioma_mensal, file.path(RESULT_GEE, "cfem_AMAZONIA_ALLminerals_GEE_MONTHLY.csv"))

# DB
cols_drop_db <- c(
  "TIov","UCov","QUIov","TIov10km","UCov2_10km","QUIov10km",
  "UCtype","UCname","TIname","QUIname",
  "UCtype_ov","UCname_ov","TIname_ov","QUIname_ov",
  "inf_MT","inf_IC","emb_MTa","emb_MTb","emb_IB","emb_IC"
)
pma_db <- pma_full |> dplyr::select(-dplyr::any_of(cols_drop_db))

# Renomeia colunas geográficas remanescentes para nomes curtos.
rename_db <- c(name_muni = "munic_pma", abbrev_state = "uf_pma",
               name_state = "estado", name_region = "regiao", code_muni = "cod_munic")
for (old in names(rename_db)) {
  if (old %in% names(pma_db)) names(pma_db)[names(pma_db) == old] <- rename_db[[old]]
}

terra::writeVector(pma_db,      file.path(RESULT_DB, "pma_amzl_ALLminerals_final.geojson"), filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(ti_amzl,     file.path(RESULT_DB, "ti_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(uc_amzl,     file.path(RESULT_DB, "uc_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(qui_amzl,    file.path(RESULT_DB, "qui_amzl.geojson"), filetype = "GeoJSON", overwrite = TRUE)
readr::write_csv(cfem_final,    file.path(RESULT_DB, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"))
readr::write_csv(cfem_aut_amzl, file.path(RESULT_DB, "cfem_aut_all_min_amzl.csv"))

message("\n=== 03_final_proc.R v2 — CONCLUÍDO ===")