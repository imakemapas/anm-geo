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
  library(ggplot2)
  library(patchwork)
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

#   CNPJ  -> 11.111.111/1111-11
#   CPF mascarado -> ***.111.111-**
#   vazio ("-", "", NA) -> NA
padroniza_doc <- function(x) {
  x  <- trimws(as.character(x))
  d  <- gsub("\\D", "", x)            # só os dígitos
  nd <- nchar(d)
  dplyr::case_when(
    # vazios
    is.na(x) | x %in% c("", "-")                ~ NA_character_,
    grepl("\\*", x) & nd == 6 ~ sprintf("***.%s.%s-**", substr(d,1,3), substr(d,4,6)),
    nd == 14 ~ sprintf("%s.%s.%s/%s-%s",
                       substr(d,1,2),  substr(d,3,5),  substr(d,6,8),
                       substr(d,9,12), substr(d,13,14)),
    nd > 6 & nd < 14 ~ {
      d14 <- stringr::str_pad(d, 14, "left", "0")
      sprintf("%s.%s.%s/%s-%s",
              substr(d14,1,2),  substr(d14,3,5),  substr(d14,6,8),
              substr(d14,9,12), substr(d14,13,14))
    },
    nd == 11 ~ sprintf("%s.%s.%s-%s",
                       substr(d,1,3), substr(d,4,6), substr(d,7,9), substr(d,10,11)),
    TRUE ~ NA_character_
  )
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
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJcm = padroniza_doc(CPF_CNPJcm))

cm_clean |>
  dplyr::mutate(
    so_dig = gsub("\\D", "", CPF_CNPJcm),
    ndig   = nchar(so_dig),
    tem_pontuacao = grepl("\\D", CPF_CNPJcm)
  ) |>
  dplyr::count(ndig, tem_pontuacao) |>
  as.data.frame()

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
    micro_ok = n_munic == 1 & !is.na(munic_unico),
    munic = dplyr::case_when(
      micro_ok                       ~ munic_unico,
      !is.na(munic_centroide)        ~ munic_centroide,
      TRUE                           ~ NA_character_),
    uf = dplyr::case_when(
      micro_ok                       ~ uf_unico,
      !is.na(uf_centroide)           ~ uf_centroide,
      TRUE                           ~ NA_character_),
    munic_fonte = dplyr::case_when(
      micro_ok                       ~ "microdado",
      n_munic > 1 & !is.na(munic_centroide)  ~ "centroide",
      n_munic == 1 & !is.na(munic_centroide) ~ "centroide_micro_sem_nome",
      n_munic == 0 & !is.na(munic_centroide) ~ "centroide_sem_micro",
      TRUE                           ~ NA_character_)
  ) |>
  dplyr::select(PROCESSO, munic, uf, n_munic, munic_fonte)

ids_na <- munic_final |> dplyr::filter(is.na(munic)) |> dplyr::pull(PROCESSO)

if (length(ids_na) > 0) {

  cent_na  <- centroides[centroides$PROCESSO %in% ids_na, ]
  nr       <- terra::nearest(cent_na, muni_ibge) 
  idx_near <- terra::values(nr)$to_id             
  near_df <- tibble::tibble(
    PROCESSO       = cent_na$PROCESSO,
    munic_near     = toupper(terra::values(muni_ibge)[[col_nmmun]][idx_near]),
    uf_near        = toupper(terra::values(muni_ibge)[[col_ufmun]][idx_near])
  )

  munic_final <- munic_final |>
    dplyr::left_join(near_df, by = "PROCESSO") |>
    dplyr::mutate(
      preencheu_near = is.na(munic) & !is.na(munic_near),
      munic       = dplyr::if_else(preencheu_near, munic_near, munic),
      uf          = dplyr::if_else(preencheu_near, uf_near,    uf),
      munic_fonte = dplyr::if_else(preencheu_near, "centroide_nearest", munic_fonte)
    ) |>
    dplyr::select(-munic_near, -uf_near, -preencheu_near)
}

print(sum(is.na(munic_final$munic)))

pma_amzl <- tidyterra::left_join(pma_amzl, munic_final, by = "PROCESSO")
save_ckpt(pma_amzl, "04_pma_amzl")

# =============================================================================
# BLOCO 5 — INTERSEÇÃO
# =============================================================================

pma_amzl <- load_ckpt("04_pma_amzl")

ti_amzl  <- load_ckpt("03_ti_amzl")
uc_amzl  <- load_ckpt("03_uc_amzl")
qui_amzl <- load_ckpt("03_qui_amzl")

invalid_geom <- !terra::is.valid(pma_amzl)
if (any(invalid_geom)) {
  pma_amzl <- rbind(terra::buffer(pma_amzl[invalid_geom, ], 0),
                    pma_amzl[!invalid_geom, ])
}

# UC relevante: Proteção Integral OU RESEX.
uc_pi_resex <- uc_amzl[uc_amzl$grupo == "Proteção Integral" | uc_amzl$sigla_snuc == "RESEX", ]
qui_amzl    <- qui_amzl |> select(nm_comunid)

# --- Buffers de entorno ------------------------------------------------------
qui_bf <- terra::buffer(qui_amzl, width = 10000)
ti_bf  <- terra::buffer(ti_amzl,  width = 10000)
uc_bf  <- terra::buffer(uc_pi_resex,
                        width = ifelse(toupper(trimws(uc_pi_resex$pl_manejo)) == "SIM",
                                       10000, 2000))

qui_bf_only <- terra::erase(qui_bf, qui_amzl)
ti_bf_only  <- terra::erase(ti_bf,  ti_amzl)
uc_bf_only  <- terra::erase(uc_bf,  uc_pi_resex)

# --- Função de sobreposição (>= 5% da área do processo) ----------------------
calc_overlap <- function(pma_lyr, tp_lyr, flag_name) {
  inter <- terra::intersect(pma_lyr, tp_lyr)
  inter$area_inter <- terra::expanse(inter, unit = "ha")
  inter <- inter |> tidyterra::select("PROCESSO", "AREA_HA", "area_inter") |> as.data.frame()
  inter$Propor <- as.numeric(inter$area_inter / inter$AREA_HA)

  inter |>
    dplyr::filter(Propor >= 0.05) |>
    dplyr::mutate(!!flag_name := 1L) |>
    dplyr::select(PROCESSO, !!flag_name) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
    dplyr::distinct(PROCESSO, .keep_all = TRUE)
}

calc_overlap_named <- function(pma_lyr, tp_lyr, flag_name, name_col,
                               out_name, fix_encoding = FALSE,
                               extra_cols = NULL) {
  inter <- terra::intersect(pma_lyr, tp_lyr)
  inter$area_inter <- terra::expanse(inter, unit = "ha")

  keep_cols <- c("PROCESSO", "AREA_HA", "area_inter", name_col, extra_cols)
  inter <- inter |>
    tidyterra::select(dplyr::all_of(keep_cols)) |>
    as.data.frame()

  inter$Propor <- as.numeric(inter$area_inter / inter$AREA_HA)

  inter <- inter |>
    dplyr::filter(Propor >= 0.05)

  if (nrow(inter) == 0) {
    # Nenhuma sobreposição >= 5%: devolve estrutura vazia coerente
    base <- tibble::tibble(PROCESSO = character(0))
    base[[flag_name]] <- integer(0)
    base[[out_name]]  <- character(0)
    if (!is.null(extra_cols)) for (ec in extra_cols) base[[ec]] <- character(0)
    return(base)
  }

  # Renomeia a coluna de nome para o nome de saída desejado
  inter <- inter |>
    dplyr::rename(!!out_name := dplyr::all_of(name_col))

  if (fix_encoding) {
    inter[[out_name]] <- stringi::stri_encode(inter[[out_name]],
                                              from = "Windows-1252", to = "UTF-8")
  }

  inter |>
    dplyr::group_by(PROCESSO) |>
    dplyr::summarise(
      !!flag_name := 1L,
      !!out_name  := paste(unique(.data[[out_name]]), collapse = "|"),
      dplyr::across(dplyr::all_of(extra_cols), ~ paste(unique(.x), collapse = "|")),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
    dplyr::distinct(PROCESSO, .keep_all = TRUE)
}

# (a) Sobreposição DIRETA -----------------------------------------------------
# UC: flag UCov + UCname + UCtype (sigla_snuc) no mesmo passo.
df_uc_pma <- calc_overlap_named(
  pma_amzl, uc_pi_resex,
  flag_name = "UCov", name_col = "nome_uc", out_name = "UCname",
  extra_cols = "sigla_snuc"
) |> dplyr::rename(UCtype = sigla_snuc)

df_ti_pma <- calc_overlap_named(
  pma_amzl, ti_amzl,
  flag_name = "TIov", name_col = "terrai_nom", out_name = "TIname"
)

df_qui_pma <- calc_overlap_named(
  pma_amzl, qui_amzl,
  flag_name = "QUIov", name_col = "nm_comunid", out_name = "QUIname"
)

pma_tp1 <- pma_amzl |>
  tidyterra::left_join(df_ti_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_uc_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_qui_pma, by = "PROCESSO") |>
  dplyr::mutate(
    TIov  = tidyr::replace_na(TIov,  0L),
    UCov  = tidyr::replace_na(UCov,  0L),
    QUIov = tidyr::replace_na(QUIov, 0L)
  )

# (b) Sobreposição com o ENTORNO (donut) --------------------------------------
# Mesmo princípio: flag + nome saem juntos, ambos filtrados por 5%.
df_uc_donut <- calc_overlap_named(
  pma_tp1, uc_bf_only,
  flag_name = "UCov2_10km", name_col = "nome_uc", out_name = "UCname_ov",
  extra_cols = "sigla_snuc"
) |> dplyr::rename(UCtype_ov = sigla_snuc)

df_ti_donut <- calc_overlap_named(
  pma_tp1, ti_bf_only,
  flag_name = "TIov10km", name_col = "terrai_nom", out_name = "TIname_ov"
)

df_qui_donut <- calc_overlap_named(
  pma_tp1, qui_bf_only,
  flag_name = "QUIov10km", name_col = "nm_comunid", out_name = "QUIname_ov"
)

pma_tp <- pma_tp1 |>
  tidyterra::left_join(df_ti_donut,  by = "PROCESSO") |>
  tidyterra::left_join(df_uc_donut,  by = "PROCESSO") |>
  tidyterra::left_join(df_qui_donut, by = "PROCESSO") |>
  dplyr::mutate(
    TIov10km   = tidyr::replace_na(TIov10km,   0L),
    UCov2_10km = tidyr::replace_na(UCov2_10km, 0L),
    QUIov10km  = tidyr::replace_na(QUIov10km,  0L)
  )

names(as.data.frame(pma_tp))

# --- Embargos e infrações (flags de interseção) ------------------------------
EMBmtSEMA <- terra::vect(file.path(PRE_PROC_DIR, "sema_mt_embargos.shp"))
EMBmtSIGA <- terra::vect(file.path(PRE_PROC_DIR, "sema_mt_embargos_siga.shp"))
EMBib     <- terra::vect(file.path(PRE_PROC_DIR, "ibama_embargos.shp"))
EMBic     <- terra::vect(file.path(PRE_PROC_DIR, "icmbio_embargos.shp"))
INFmtSIGA <- terra::vect(file.path(PRE_PROC_DIR, "sema_mt_infracoes_siga.shp"))
INFic     <- terra::vect(file.path(PRE_PROC_DIR, "icmbio_infracoes.shp"))

pma_tp$inf_MT  <- as.integer(terra::is.related(pma_tp, INFmtSIGA, "intersects"))
pma_tp$inf_IC  <- as.integer(terra::is.related(pma_tp, INFic,     "intersects"))
pma_tp$emb_MTa <- as.integer(terra::is.related(pma_tp, EMBmtSEMA, "intersects"))
pma_tp$emb_MTb <- as.integer(terra::is.related(pma_tp, EMBmtSIGA, "intersects"))
pma_tp$emb_IB  <- as.integer(terra::is.related(pma_tp, EMBib,     "intersects"))
pma_tp$emb_IC  <- as.integer(terra::is.related(pma_tp, EMBic,     "intersects"))

names(as.data.frame(pma_tp))

save_ckpt(pma_tp, "05_pma_tp")

# =============================================================================
# BLOCO 6 — CFEM
# -----------------------------------------------------------------------------

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
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJaut = padroniza_doc(CPF_CNPJaut))

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
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr))

# Razão social via Pessoa.txt
pessoa <- readr::read_delim(
  file.path(MICRO_DIR, "Pessoa.txt"),
  delim = ";", locale = readr::locale(encoding = "Windows-1252"),
  col_types = readr::cols(.default = "c"), show_col_types = FALSE
) |>
  dplyr::rename(CPF_CNPJarr = NRCPFCNPJ, NOME_arr = NMPessoa) |>
  dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr)) |>
  dplyr::select(CPF_CNPJarr, NOME_arr) |>
  dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
  dplyr::mutate(NOME_arr = toupper(NOME_arr))

# Razão social via dado intenro GP (fallback p/ quem não está em Pessoa.txt)
RazaoSocial <- readr::read_csv(file.path(RAW_DIR, "cefem_arrecadacao(semshapes).csv"), show_col_types = FALSE) |>
  dplyr::rename(CPF_CNPJarr = cnpj_cpf, NOME_arr_alt = razao_social) |>
  dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr)) |>
  dplyr::select(CPF_CNPJarr, NOME_arr_alt) |>
  dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
  dplyr::mutate(NOME_arr_alt = toupper(NOME_arr_alt))

cfem_arr <- cfem_arr |>
  dplyr::left_join(pessoa,      by = "CPF_CNPJarr") |>
  dplyr::left_join(RazaoSocial, by = "CPF_CNPJarr") |>
  dplyr::mutate(
    NOME_arr = dplyr::coalesce(NOME_arr, NOME_arr_alt, "NOME DESCONHECIDO")
  ) |>
  dplyr::select(-NOME_arr_alt)

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

# ---- ROBUSTO ----
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

FASES_CORR <- c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA",
                "LICENCIAMENTO", "AUTORIZAÇÃO DE PESQUISA")

cfem_final <- cfem_arr_amzl3 |>
  dplyr::mutate(
    PESO_G_final  = PESO_G,
    PESO_KG_final = PESO_KG,
    preco_g_final = preco_g_orig,
    corr          = "original"
  )

# ---- SIMPLES ----
# Corrige PESO_G por fator 10^k (k de -6 a 6), menor |k| que traz VALORtot/peso à faixa (R$/g).
fatores_simples <- 10^(-6:6)
corrige_simples_g <- function(peso_g, valortot, pmin_g, pmax_g) {
  if (is.na(peso_g) || is.na(valortot) || peso_g <= 0 || valortot <= 0)
    return(c(peso = peso_g, fator = NA_real_))
  ok <- fatores_simples[ {p <- valortot / (peso_g * fatores_simples); p >= pmin_g & p <= pmax_g} ]
  if (length(ok) == 0) return(c(peso = peso_g, fator = NA_real_))
  f <- ok[which.min(abs(log10(ok)))]
  c(peso = peso_g * f, fator = f)
}

# ---- helper de check: conta fora da faixa (R$/kg), por fase ----
report_check <- function(df, mineral, etapa, pmin_kg, pmax_kg) {
  d <- df |>
    dplyr::filter(!is.na(PESO_KG_final), PESO_KG_final > 0, !is.na(VALORtot), VALORtot > 0) |>
    dplyr::mutate(rs_por_kg = VALORtot / PESO_KG_final,
                  fora = rs_por_kg < pmin_kg | rs_por_kg > pmax_kg)
  message(sprintf("[%s] %s | avaliados: %d | fora de [%g-%g] R$/kg: %d",
                  mineral, etapa, nrow(d), pmin_kg, pmax_kg, sum(d$fora, na.rm = TRUE)))
  print(d |> dplyr::count(FASE, fora) |> dplyr::arrange(dplyr::desc(fora), dplyr::desc(n)))
  invisible(d)
}

# ---- ciclo completo (3 checks) para UM mineral ----
corrige_mineral_3checks <- function(cfem_final, mineral_label,
                                     subs_keep, subs_col,
                                     pmin_kg, pmax_kg,
                                     min_med_plaus, max_med_plaus) {
  pmin_g <- pmin_kg / 1000; pmax_g <- pmax_kg / 1000

  universo <- cfem_final |>
    dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)

  # CHECK 1
  report_check(universo, mineral_label, "CHECK 1 (antes)", pmin_kg, pmax_kg)

  # ROBUSTO
  med_info <- compute_median_hierarchical(
    universo, preco_col = "preco_g_orig",
    min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
    min_ano = min_grp_ano, min_global = min_grp_global,
    max_med_plaus = max_med_plaus, min_med_plaus = min_med_plaus
  )

  rob <- universo |>
    dplyr::mutate(med_preco_base = med_info$med, med_level = med_info$level) |>
    dplyr::rowwise() |>
    dplyr::mutate(sug = list(suggest_weight_row(VALORtot, PESO_G, med_preco_base,
                                                p_range = p_round_min:p_round_max))) |>
    tidyr::unnest_wider(sug) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_sugerido), PESO_G_sugerido, PESO_G),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_G_final), PESO_G_final / 1000, NA_real_),
      preco_g_final = dplyr::if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g,
                                     VALORtot / PESO_G_final, NA_real_),
      corr = dplyr::if_else(is.na(candidate_name), "original", candidate_name)
    )

  cfem_final <- cfem_final |>
    dplyr::left_join(
      rob |> dplyr::select(row_id, PESO_G_final, PESO_KG_final, preco_g_final, corr) |>
        dplyr::rename(PESO_G_r = PESO_G_final, PESO_KG_r = PESO_KG_final,
                      preco_g_r = preco_g_final, corr_r = corr),
      by = "row_id") |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_r),  PESO_G_r,  PESO_G_final),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_r), PESO_KG_r, PESO_KG_final),
      preco_g_final = dplyr::if_else(!is.na(preco_g_r), preco_g_r, preco_g_final),
      corr          = dplyr::if_else(!is.na(corr_r),    corr_r,    corr)
    ) |>
    dplyr::select(-PESO_G_r, -PESO_KG_r, -preco_g_r, -corr_r)

  # CHECK 2
  univ2 <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  d2 <- report_check(univ2, mineral_label, "CHECK 2 (pos-robusto)", pmin_kg, pmax_kg)
  ids_fora <- d2 |> dplyr::filter(fora) |> dplyr::pull(row_id)

  # SIMPLES (só remanescentes)
  if (length(ids_fora) > 0) {
    fb <- cfem_final |>
      dplyr::filter(row_id %in% ids_fora) |>
      dplyr::rowwise() |>
      dplyr::mutate(.r = list(corrige_simples_g(PESO_G_final, VALORtot, pmin_g, pmax_g)),
                    PESO_G_fb = .r[["peso"]], fator_fb = .r[["fator"]]) |>
      dplyr::ungroup() |>
      dplyr::select(row_id, PESO_G_fb, fator_fb)

    cfem_final <- cfem_final |>
      dplyr::left_join(fb, by = "row_id") |>
      dplyr::mutate(
        aplicou = !is.na(fator_fb) & fator_fb != 1,
        PESO_G_final  = dplyr::if_else(aplicou, PESO_G_fb,        PESO_G_final),
        PESO_KG_final = dplyr::if_else(aplicou, PESO_G_fb / 1000, PESO_KG_final),
        preco_g_final = dplyr::if_else(aplicou & PESO_G_final > min_peso_g,
                                       VALORtot / PESO_G_final,   preco_g_final),
        corr = dplyr::if_else(aplicou, paste0("simples_1e", round(log10(fator_fb))), corr)
      ) |>
      dplyr::select(-PESO_G_fb, -fator_fb, -aplicou)
    message("[", mineral_label, "] SIMPLES aplicado a ", length(ids_fora), " remanescente(s).")
  } else {
    message("[", mineral_label, "] nenhum remanescente para o metodo simples.")
  }

  # CHECK 3 (FINAL)
  univ3 <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  d3 <- report_check(univ3, mineral_label, "CHECK 3 (final)", pmin_kg, pmax_kg)
  resta <- d3 |> dplyr::filter(fora)
  if (nrow(resta) > 0) {
    message("[", mineral_label, "] IRRECUPERAVEIS (provavel dado corrompido) — revisar:")
    print(resta |> dplyr::select(PROCESSO, FASE, PESO_KG, PESO_KG_final, VALORtot, rs_por_kg))
  }
  message("[", mineral_label, "] distribuicao final de 'corr':")
  print(univ3 |> dplyr::count(corr, sort = TRUE))

  cfem_final
}

# ---- CASSITERITA (faixa de mercado R$30-300/kg = 0,03-0,30 R$/g) ----
cfem_final <- corrige_mineral_3checks(
  cfem_final, "CASSITERITA",
  subs_keep = "CASSITERITA", subs_col = "SUBSarr",
  pmin_kg = 30, pmax_kg = 300,
  min_med_plaus = 0.03, max_med_plaus = 0.30
)

# ---- OURO (limites mantidos do original: 30-1000 R$/g = 30.000-1.000.000 R$/kg) ----
cfem_final <- corrige_mineral_3checks(
  cfem_final, "OURO",
  subs_keep = "OURO", subs_col = "SUBSarrSIM",
  pmin_kg = 30 * 1000, pmax_kg = 1000 * 1000,
  min_med_plaus = 30, max_med_plaus = 1000
)

cfem_final <- cfem_final |>
  dplyr::mutate(
    ULT_EV_ID  = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT = as.Date(stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"), format = "%d/%m/%Y"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(
      ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  )

cfem_final <- cfem_final |> dplyr::select(-dplyr::any_of("row_id"))

# cfem sem municipio - trazendo do processo 
pma_muni <- as.data.frame(load_ckpt("04_pma_amzl")) |>
  dplyr::select(PROCESSO, munic_pma = munic, uf_pma = uf)

cfem_final <- cfem_final |>
  dplyr::left_join(pma_muni, by = "PROCESSO") |>
  dplyr::mutate(
    muni_falta = is.na(name_muni) | name_muni == "" | code_muni == 0 | code_muni == "0",
    preencheu_pma = muni_falta & !is.na(munic_pma),
    name_muni    = dplyr::if_else(preencheu_pma, munic_pma, name_muni),
    abbrev_state = dplyr::if_else(preencheu_pma & (is.na(abbrev_state) | abbrev_state == ""),
                                  uf_pma, abbrev_state),
    muni_fonte_cfem = dplyr::if_else(preencheu_pma, "herdado_pma", "cfem_original")
  ) |>
  dplyr::select(-munic_pma, -uf_pma, -muni_falta, -preencheu_pma)

save_ckpt(cfem_final,    "06_cfem_final")
save_ckpt(cfem_aut_amzl, "06_cfem_aut_amzl")

# # ---- CHECKS ----
# # Cassiterita
# cass_check <- cfem_final |>
#   dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CORR)
# med_dbg <- compute_median_hierarchical(
#   cass_check, preco_col = "preco_g_orig",
#   min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
#   min_ano = min_grp_ano, min_global = min_grp_global,
#   max_med_plaus = 0.30, min_med_plaus = 0.03
# )
# table(med_dbg$level, useNA = "always")

# cfem_final |>
#   dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CORR,
#                 PESO_KG_final > 0, VALORtot > 0) |>
#   dplyr::mutate(rs_kg = VALORtot / PESO_KG_final) |>
#   dplyr::summarise(
#     n = dplyr::n(),
#     min = min(rs_kg), p25 = quantile(rs_kg, .25), mediana = median(rs_kg),
#     p75 = quantile(rs_kg, .75), max = max(rs_kg)
#   )

# # Ouro
# ouro_check <- cfem_final |>
#   dplyr::filter(SUBSarrSIM == "OURO", FASE %in% FASES_CORR)
# med_dbg_ouro <- compute_median_hierarchical(
#   ouro_check, preco_col = "preco_g_orig",
#   min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
#   min_ano = min_grp_ano, min_global = min_grp_global,
#   max_med_plaus = 1000, min_med_plaus = 30
# )
# table(med_dbg_ouro$level, useNA = "always")

# cfem_final |>
#   dplyr::filter(SUBSarrSIM == "OURO", FASE %in% FASES_CORR,
#                 PESO_KG_final > 0, VALORtot > 0) |>
#   dplyr::mutate(rs_kg = VALORtot / PESO_KG_final) |>
#   dplyr::summarise(
#     n = dplyr::n(),
#     min = min(rs_kg), p25 = quantile(rs_kg, .25), mediana = median(rs_kg),
#     p75 = quantile(rs_kg, .75), max = max(rs_kg)
#   )

# # VISUALIZAÇÃO 1 ==============================================================
# df_temporal_unificado <- cfem_final |>
#   dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
#   dplyr::filter(str_detect(toupper(FASE), "GARIMPEIRA")) |> 
#   dplyr::mutate(
#     substancia_plot = factor(dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"), 
#                              levels = c("CASSITERITA", "OURO")),
#     data = as.Date(sprintf("%04d-%02d-01", ANO, MES))
#   ) |>
#   dplyr::group_by(substancia_plot, data) |>
#   dplyr::summarise(
#     `1. Valor Arrecadado (R$)_Orig`   = sum(VALORarr, na.rm = TRUE),
#     `1. Valor Arrecadado (R$)_Corr`   = sum(VALORarr, na.rm = TRUE),
#     `2. Peso Declarado (Kg)_Orig`     = sum(PESO_KG, na.rm = TRUE),
#     `2. Peso Declarado (Kg)_Corr`     = sum(PESO_KG_final, na.rm = TRUE),
#     `3. Relação (R$/Kg)_Orig` = dplyr::if_else(sum(PESO_KG, na.rm = TRUE) > 0, 
#                                                sum(VALORtot, na.rm = TRUE) / sum(PESO_KG, na.rm = TRUE), 
#                                                NA_real_),
#     `3. Relação (R$/Kg)_Corr` = dplyr::if_else(sum(PESO_KG_final, na.rm = TRUE) > 0, 
#                                                sum(VALORtot, na.rm = TRUE) / sum(PESO_KG_final, na.rm = TRUE), 
#                                                NA_real_),
#     .groups = "drop"
#   ) |>
#   tidyr::pivot_longer(
#     cols = -c(substancia_plot, data),
#     names_to = c("metrica", "cenario"),
#     names_pattern = "(.*)_(Orig|Corr)",
#     values_to = "valor_metrica"
#   ) |>
#   dplyr::mutate(
#     cenario = dplyr::if_else(cenario == "Orig", "Antes (Original)", "Depois (pow10)"),
#     label_cor = dplyr::case_when(
#       str_detect(metrica, "Valor") ~ "Valor Arrecadado (Inalterado)",
#       str_detect(metrica, "Peso") & cenario == "Antes (Original)" ~ "Peso Original",
#       str_detect(metrica, "Peso") & cenario == "Depois (pow10)" ~ "Peso Depois (pow10)",
#       str_detect(metrica, "Relação") & cenario == "Antes (Original)" ~ "Relação R$/kg (original)",
#       str_detect(metrica, "Relação") & cenario == "Depois (pow10)" ~ "Relação R$/kg (depois)"
#     )
#   ) |>
#   dplyr::filter(!is.na(valor_metrica) & valor_metrica > 0)

# cores_customizadas <- c(
#   "Valor Arrecadado (Inalterado)" = "#1e3799",
#   "Peso Original"                 = "#e74c3c",
#   "Peso Depois (pow10)"           = "#27ae60",
#   "Relação R$/kg (original)"      = "#e74c3c",
#   "Relação R$/kg (depois)"        = "#8e44ad" 
# )

# p_linhas <- ggplot(df_temporal_unificado, aes(x = data, y = valor_metrica, 
#                                           color = label_cor, 
#                                           linetype = cenario, 
#                                           alpha = cenario)) +
#   geom_line(size = 0.8) +
#   geom_point(size = 1.4) +
#   scale_y_log10(labels = scales::label_comma()) +
#   facet_grid(metrica ~ substancia_plot, scales = "free_y") +
#   scale_color_manual(values = cores_customizadas) + 
#   scale_linetype_manual(values = c("Antes (Original)" = "dashed", "Depois (pow10)" = "solid")) + 
#   scale_alpha_manual(values = c("Antes (Original)" = 0.4, "Depois (pow10)" = 1.0)) +            
#   theme_bw() +
#   labs(x = "", y = "Valores em Escala Log10", color = "") +
#   guides(color = guide_legend(title = NULL, nrow = 1), linetype = "none", alpha = "none") +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold", size = 9),
#     panel.grid.minor = element_blank()
#   )
# # [EXPORTAÇÃO 1]
# ggsave(filename = file.path(CKPT_DIR, "01_serie_temporal_unificada.png"), 
#        plot = p_linhas, width = 11, height = 8.5, dpi = 300)

# # VISUALIZAÇÃO 2 ==============================================================
# df_scatterplot_clean <- cfem_final |>
#   dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
#   dplyr::filter(str_detect(toupper(FASE), "GARIMPEIRA")) |>
#   dplyr::mutate(
#     substancia_plot = factor(dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"), 
#                              levels = c("CASSITERITA", "OURO")),
#     data = as.Date(sprintf("%04d-%02d-01", ANO, MES)),
#     status_ponto = dplyr::if_else(corr == "original", "Dado Original Correto", "Corrigido pelo Algoritmo (pow10)")
#   ) |>
#   dplyr::filter(!is.na(preco_g_orig) & !is.na(preco_g_final) & preco_g_orig > 0 & preco_g_final > 0) |>
#   tidyr::pivot_longer(
#     cols = c(preco_g_orig, preco_g_final),
#     names_to = "cenario",
#     values_to = "preco_individual"
#   ) |>
#   dplyr::mutate(
#     cenario = factor(dplyr::if_else(cenario == "preco_g_orig", "Antes (Original)", "Depois (pow10)"),
#                      levels = c("Antes (Original)", "Depois (pow10)"))
#   )

# cores_scatterplot <- c(
#   "Dado Original Correto"           = "#34495e",
#   "Corrigido pelo Algoritmo (pow10)" = "#e74c3c"
# )

# p_scatter <- ggplot(df_scatterplot_clean, aes(x = data, y = preco_individual, color = status_ponto)) +
#   geom_point(alpha = 0.4, size = 1.0) +
#   scale_y_log10(labels = scales::label_comma(suffix = " R$/g")) +
#   facet_grid(substancia_plot ~ cenario, scales = "free_y") +
#   scale_color_manual(values = cores_scatterplot) +
#   theme_bw() +
#   labs(x = "", y = "R$/kg (Escala Log10)", color = "") +
#   guides(color = guide_legend(title = NULL, nrow = 1)) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold", size = 9),
#     panel.grid.minor = element_blank()
#   )
# # [EXPORTAÇÃO 2]
# ggsave(filename = file.path(CKPT_DIR, "02_scatterplot_precos.png"), 
#        plot = p_scatter, width = 11, height = 7, dpi = 300)


# # VISUALIZAÇÃO 3 ==============================================================
# prep_violino <- function(df) {
#   df |>
#     filter(PESO_KG > 0, PESO_KG_final > 0, VALORtot > 0, FASE %in% FASES_CORR) |>
#     mutate(
#       `Antes`  = VALORtot / PESO_KG,
#       `Depois` = VALORtot / PESO_KG_final
#     ) |>
#     pivot_longer(c(Antes, Depois), names_to = "cenario", values_to = "rs_kg") |>
#     mutate(cenario = factor(cenario, levels = c("Antes", "Depois")))
# }

# faixa_ref <- function(pmin, pmax) {
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = pmin, ymax = pmax,
#            alpha = 0.08, fill = "forestgreen")
# }

# p_violino_cassiterita <- prep_violino(cfem_final |> filter(SUBSarr == "CASSITERITA")) |>
#   ggplot(aes(cenario, rs_kg, fill = cenario)) +
#   faixa_ref(30, 300) +
#   geom_violin(scale = "width", alpha = 0.8, color = NA) +
#   geom_boxplot(width = 0.12, outlier.size = 0.4, alpha = 0.6) +
#   facet_wrap(~ FASE, scales = "free_x", nrow = 1) + 
#   scale_y_log10(labels = scales::comma) +
#   scale_fill_manual(values = c("Antes" = "#e74c3c", "Depois" = "#2D6A4F")) +
#   labs(#title = "Cassiterita — preço implícito (R$/kg) por fase, antes e depois da correção",
#        x = NULL, y = "R$/kg (log)", fill = NULL) +
#   theme_minimal() + 
#   theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))

# # [EXPORTAÇÃO 3]
# ggsave(filename = file.path(CKPT_DIR, "03_violino_cassiterita.png"), 
#        plot = p_violino_cassiterita, width = 12, height = 5, dpi = 300)

# # VISUALIZAÇÃO 4 ==============================================================
# p_violino_ouro <- prep_violino(cfem_final |> filter(SUBSarrSIM == "OURO")) |>
#   ggplot(aes(cenario, rs_kg, fill = cenario)) +
#   faixa_ref(30000, 1000000) +
#   geom_violin(scale = "width", alpha = 0.8, color = NA) +
#   geom_boxplot(width = 0.12, outlier.size = 0.4, alpha = 0.6) +
#   # O 'nrow = 1' força todas as fases a ficarem lado a lado
#   facet_wrap(~ FASE, scales = "free_x", nrow = 1) + 
#   scale_y_log10(labels = scales::comma) +
#   scale_fill_manual(values = c("Antes" = "#e74c3c", "Depois" = "#1e3799")) +
#   labs(#title = "Ouro — preço implícito (R$/kg) por fase, antes e depois da correção",
#        x = NULL, y = "R$/kg (log)", fill = NULL) +
#   theme_minimal() + 
#   theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))

# # [EXPORTAÇÃO 4]
# ggsave(filename = file.path(CKPT_DIR, "04_violino_ouro.png"), 
#        plot = p_violino_ouro, width = 12, height = 5, dpi = 300)

# =============================================================================
# BLOCO 7 — AGREGAÇÕES DA CFEM POR PROCESSO
# =============================================================================

cfem_final <- load_ckpt("06_cfem_final")
pma_tp     <- load_ckpt("05_pma_tp")

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

arr_corr_unique <- cfem_final |>
  dplyr::arrange(PROCESSO, ANO, MES) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    cfem_arr  = 1L,
    arr_kg_T  = sum(PESO_KG_final, na.rm = TRUE),
    arr_kg_L  = dplyr::if_else(all(is.na(PESO_KG_final)), NA_real_, dplyr::last(na.omit(PESO_KG_final))),
    arr_g_T   = sum(PESO_G_final,  na.rm = TRUE),
    arr_g_L   = dplyr::if_else(all(is.na(PESO_G_final)),  NA_real_, dplyr::last(na.omit(PESO_G_final))),
    arr_val_T = sum(VALORarr, na.rm = TRUE),
    arr_val_L = dplyr::if_else(all(is.na(VALORarr)), NA_real_, dplyr::last(na.omit(VALORarr))),
    arr_dt_F  = as.character(min(data, na.rm = TRUE)),
    arr_dt_L  = as.character(max(data, na.rm = TRUE)),
    arr_ndcl  = dplyr::n(),
    arr_nbuy  = dplyr::n_distinct(CPF_CNPJarr, na.rm = TRUE),
    arr_topb  = get_mode(NOME_arr),
    .groups = "drop"
  ) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))

# Grupo mineral do PMA + decompõe ULT_EVENTO.
pma_tp <- pma_tp |>
  dplyr::mutate(
    SUBSpmaGRP = classificar_grupo(SUBS),
    ULT_EV_ID   = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT_txt = stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"),
    ULT_EV_DES  = stringr::str_trim(stringr::str_remove_all(
      ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT_txt))),
    ULT_EV_DAT  = as.Date(ULT_EV_DAT_txt, format = "%d/%m/%Y")
  ) |>
  dplyr::select(-ULT_EV_DAT_txt)

cfem_aut_amzl <- load_ckpt("06_cfem_aut_amzl")

aut_unique <- cfem_aut_amzl |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    cfem_aut  = 1L,
    aut_val_T = round(sum(VALORaut, na.rm = TRUE), 2),
    aut_n     = dplyr::n(),
    .groups = "drop"
  )

pma_full <- pma_tp |>
  tidyterra::left_join(arr_corr_unique, by = "PROCESSO") |>
  tidyterra::left_join(aut_unique,      by = "PROCESSO") |>
  dplyr::mutate(
    cfem_arr  = tidyr::replace_na(cfem_arr, 0L),
    cfem_aut  = tidyr::replace_na(cfem_aut, 0L),
    arr_kg_T  = tidyr::replace_na(arr_kg_T, 0),
    arr_g_T   = tidyr::replace_na(arr_g_T,  0),
    arr_val_T = tidyr::replace_na(arr_val_T, 0),
    arr_ndcl  = tidyr::replace_na(arr_ndcl, 0L),
    arr_nbuy  = tidyr::replace_na(arr_nbuy, 0L),
    aut_val_T = tidyr::replace_na(aut_val_T, 0),
    aut_n     = tidyr::replace_na(aut_n, 0L)
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
  "arr_kg_T","arr_kg_L","arr_g_T","arr_g_L","arr_val_T","arr_val_L",
  "arr_dt_F","arr_dt_L","arr_ndcl","arr_nbuy","arr_topb",
  "aut_val_T","aut_n",
  "ULT_EV_ID","ULT_EV_DAT","ULT_EV_DES","ULT_EVENTO",
  "UCtype","UCname","TIname","QUIname",
  "UCtype_ov","UCname_ov","TIname_ov","QUIname_ov",
  "inf_MT","inf_IC","emb_MTa","emb_MTb","emb_IB","emb_IC"
)
pma_db <- pma_full |> dplyr::select(-dplyr::any_of(cols_drop_db))

rename_db <- c(name_muni = "munic_pma", abbrev_state = "uf_pma",
               name_state = "estado", name_region = "regiao", code_muni = "cod_munic")
for (old in names(rename_db)) {
  if (old %in% names(pma_db)) names(pma_db)[names(pma_db) == old] <- rename_db[[old]]
}
# pma_db <- terra::vect(file.path(RESULT_DB, "pma_amzl_ALLminerals_final.geojson"))
# sort(names(pma_db))

terra::writeVector(pma_db,      file.path(RESULT_DB, "pma_amzl_ALLminerals_final.geojson"), filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(ti_amzl,     file.path(RESULT_DB, "ti_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(uc_amzl,     file.path(RESULT_DB, "uc_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(qui_amzl,    file.path(RESULT_DB, "qui_amzl.geojson"), filetype = "GeoJSON", overwrite = TRUE)
readr::write_csv(cfem_final,    file.path(RESULT_DB, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"))
readr::write_csv(cfem_aut_amzl, file.path(RESULT_DB, "cfem_aut_all_min_amzl.csv"))

message("\n=== 03_final_proc.R v2 — CONCLUÍDO ===")