################################################################################
# Data Processing, Sanitation and Consolidation
################################################################################

# Setup & Configuration -----------------------------------------------------------
message("Starting Setup & Configuration...")

rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  # Spatial & Geo
  Sys.unsetenv("PROJ_LIB")
  library(terra)
  library(sf)
  library(geobr)
  library(tidyterra)

  # Data Manipulation
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(lubridate)

  # Utilities
  library(stringi)
  library(stringr)
  library(openxlsx)
  library(writexl)
  library(glue) 
  library(here)
  library(ggplot2)
  library(patchwork)
})

# Paths
ROOT         <- here::here()
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
CLEAN_DIR    <- here::here("data", "clean_data")
RESULT_DIR   <- here::here("data", "result")

dir.create(CLEAN_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(RESULT_DIR, recursive = TRUE, showWarnings = FALSE)

# CFEM helpers
get_mode <- function(v) {
  uniqv <- unique(stats::na.omit(v))
  if (length(uniqv) == 0) return(NA_character_)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Mineral target list
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

# Environment cleaner
clean_env <- function() {
  rm(list = setdiff(ls(envir = .GlobalEnv),
                    c("ROOT", "PRE_PROC_DIR", "CLEAN_DIR", "RESULT_DIR",
                      "clean_env", "get_mode", "target_minerals", "target_minerals_list",
                    "compute_median_hierarchical", "suggest_weight_row")),
     envir = .GlobalEnv)
  gc(verbose = FALSE)
}

message("Setup complete.")


# =============================================================================
# BLOCO 1: SCM (Cadastro Mineiro)
# =============================================================================
message("Integrating Cadastro Mineiro (SCM) data...")

cm <- readr::read_csv(here::here("data", "pre_proc_data", "cadastro_mineiro.csv"),
                      show_col_types = FALSE)

dict_rename <- c(
  "^Superintendência$|^Superintendência$" = "SUPERINTEN",
  "^Processo$"                            = "PROCESSO",
  "^Tipo\\.de\\.requerimento$|^Tipo de requerimento$" = "TIPO_REQcm",
  "^Fase\\.Atual$|^Fase Atual$"           = "FASEcm",
  "^CPF\\.CNPJ\\.do\\.titular$|^CPF/CNPJ do titular$|^CPF CNPJ do titular$" = "CPF_CNPJcm",
  "^Titular$"                             = "TITULARcm",
  "^Municipio\\.s\\.$|^Municipio\\(s\\)$" = "name_muni",
  "^Substância\\.s\\.$|^Substância\\(s\\)$" = "SUBScm",
  "^Tipo\\.s\\.\\.de\\.Uso$|^Tipo\\(s\\) de Uso$" = "TIPO_USO",
  "^Situação$"                            = "STATUS",
  "^Localidade$"                          = "LOCALIDADE",
  "^QuantidadeMinerio$"                   = "QTD_MINERIO",
  "^DataPublicacao$"                      = "DT_PUBLICACAO",
  "^Data da Cessão$"                      = "DT_CESSAO"
)

n_orig <- names(cm)
n_new  <- n_orig
for (pat in names(dict_rename)) {
  n_new <- ifelse(stringr::str_detect(n_orig, stringr::regex(pat)), dict_rename[pat], n_new)
}
names(cm) <- n_new

cm_clean <- cm |>
  dplyr::select(PROCESSO, TIPO_REQcm, FASEcm, CPF_CNPJcm, TITULARcm, SUBScm, DT_CESSAO) |>
  dplyr::filter(!is.na(PROCESSO)) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper))

# Detecção e resolução de conflitos por processo
contagem_conflitos <- cm_clean |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    tem_conflito = (
      dplyr::n_distinct(TIPO_REQcm, na.rm = TRUE) > 1 |
      dplyr::n_distinct(FASEcm,     na.rm = TRUE) > 1 |
      dplyr::n_distinct(SUBScm,     na.rm = TRUE) > 1 |
      dplyr::n_distinct(CPF_CNPJcm, na.rm = TRUE) > 1 |
      dplyr::n_distinct(TITULARcm,  na.rm = TRUE) > 1
    ),
    .groups = "drop")

cm_normais <- cm_clean |>
  dplyr::inner_join(dplyr::filter(contagem_conflitos, !tem_conflito), by = "PROCESSO") |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(dplyr::across(
    c(TIPO_REQcm, FASEcm, SUBScm, CPF_CNPJcm, TITULARcm),
    ~ dplyr::last(stats::na.omit(.x))
  ), .groups = "drop")

cm_conflitos <- cm_clean |>
  dplyr::inner_join(dplyr::filter(contagem_conflitos, tem_conflito), by = "PROCESSO")

if (nrow(cm_conflitos) > 0) {
  cm_resolvidos <- cm_conflitos |>
    dplyr::mutate(DT_CESSAO_formatada = as.Date(DT_CESSAO, format = "%d/%m/%Y")) |>
    dplyr::group_by(PROCESSO) |>
    dplyr::arrange(PROCESSO, dplyr::desc(DT_CESSAO_formatada)) |>
    dplyr::summarise(
      dplyr::across(
        c(TIPO_REQcm, FASEcm, SUBScm, CPF_CNPJcm, TITULARcm),
        ~ dplyr::first(stats::na.omit(.x))),
      DT_CESSAO_utilizada = dplyr::first(stats::na.omit(DT_CESSAO)),
      CNPJs_envolvidos    = stringr::str_c(unique(stats::na.omit(CPF_CNPJcm)), collapse = " VS "),
      .groups = "drop")
  cm_resolvidos <- cm_resolvidos |> dplyr::select(-CNPJs_envolvidos, -DT_CESSAO_utilizada)
} else {
  cm_resolvidos <- tibble::tibble()
}

cm_unique <- dplyr::bind_rows(cm_normais, cm_resolvidos)
message("SCM unique complete.")

# =============================================================================
# BLOCO 2: PMA — limpeza espacial e join SCM
# =============================================================================
message("Cleaning PMA raw data...")

pma0 <- terra::vect(here::here("data", "pre_proc_data", "sigmine_pma.shp"))
message(glue("CRS original: {terra::crs(pma0, describe = TRUE)$code}"))

# Filtragem básica
pma1 <- pma0 |>
  dplyr::filter(AREA_HA > 0) |>
  dplyr::filter(!is.na(PROCESSO) & PROCESSO != "" & PROCESSO != "0")

# Correção de geometrias inválidas
invalid_geom <- !terra::is.valid(pma1)
if (any(invalid_geom)) {
  message(glue("Repairing {sum(invalid_geom)} invalid geometries (terra pass)..."))
  pma1 <- rbind(
    terra::makeValid(pma1[invalid_geom, ]),
    pma1[!invalid_geom, ]
  )
}

# Padronização de atributos
pma2 <- pma1 |>
  dplyr::select(-any_of(c("USO", "UF", "DSProcesso", "ID", "NUMERO", "ANO"))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::rename(TITULAR = NOME) |>
  dplyr::mutate(AREA_orig = AREA_HA)

# Conversão para sf + validação (segunda passagem) + reprojeção
message("Validating geometries (sf pass) and reprojecting...")
sf::sf_use_s2(FALSE)  # desliga s2 para evitar erro no make_valid
pma3 <- pma2 |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  #sf::st_transform(4674)
  sf::st_transform(4326)
sf::sf_use_s2(TRUE)

message(glue("{sum(!sf::st_is_valid(pma3))} invalid geometries remaining"))

# Reprojeção métrica para o summarise
pma3_m <- sf::st_transform(pma3, 5880) |> 
  sf::st_make_valid()

message(glue("{sum(!sf::st_is_valid(pma3_m))} invalid geometries remaining after second pass"))

# Dissolve — AREA_HA recalculada geometricamente, AREA_orig = soma dos valores ANM
message("Aggregating polygons...")
pma4 <- pma3_m |>
  dplyr::group_by(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS) |>
  dplyr::summarise(
    AREA_orig = sum(AREA_orig, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  dplyr::mutate(
    AREA_HA = as.numeric(sf::st_area(geometry)) / 10000
  ) |>
  #sf::st_transform(4674)
  sf::st_transform(4326)

# Identificação de conflitos
ids_conflict <- sf::st_drop_geometry(pma4) |>
  dplyr::filter(duplicated(PROCESSO) | duplicated(PROCESSO, fromLast = TRUE)) |>
  dplyr::pull(PROCESSO) |>
  unique()

message(glue("{length(ids_conflict)} conflicting processes to resolve via SCM"))

pma5 <- pma4 |> dplyr::filter(!PROCESSO %in% ids_conflict)
pma6 <- pma4 |> dplyr::filter(PROCESSO %in% ids_conflict)

# Arbitragem SCM para conflitos
if (nrow(pma6) > 0) {
  pma6_resolved <- pma6 |>
    sf::st_transform(5880) |>
    dplyr::group_by(PROCESSO) |>
    dplyr::summarise(
      ULT_EVENTO = dplyr::first(ULT_EVENTO),
      TITULAR    = dplyr::first(TITULAR),
      SUBS       = dplyr::first(SUBS),
      AREA_orig  = dplyr::first(AREA_orig), 
      .groups    = "drop"
    ) |>
    dplyr::mutate(
      AREA_HA = as.numeric(sf::st_area(geometry)) / 10000
    ) |>
    sf::st_transform(4326) |>  
    #sf::st_transform(4674) |>
    dplyr::left_join(cm_unique, by = "PROCESSO") |>
    dplyr::rename(FASE = FASEcm) |>
    dplyr::select(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS,
                  AREA_HA, AREA_orig, geometry)
}

pma7sf <- if (nrow(pma6) > 0) rbind(pma5, pma6_resolved) else pma5
pma7   <- terra::vect(pma7sf)

# Join SCM
cm_attrs <- cm_unique |> dplyr::select(PROCESSO, TIPO_REQcm, CPF_CNPJcm)
pma_cm   <- tidyterra::left_join(pma7, cm_attrs, by = "PROCESSO")

# QA
message(glue(
  "PMA final: {nrow(pma_cm)} processos | ",
  "AREA_HA total: {round(sum(pma_cm$AREA_HA, na.rm=TRUE))} ha | ",
  "AREA_orig total: {round(sum(pma_cm$AREA_orig, na.rm=TRUE))} ha"
))

terra::writeVector(pma_cm,
                   here::here("data", "clean_data", "pma_clean_cm.shp"),
                   overwrite = TRUE)

message("SCM integration complete.")
clean_env()

# =============================================================================
# BLOCO 2b: Atribuição de Municípios
# =============================================================================
message("Assigning Municipalities to PMA centroids...")

pma_clean <- terra::vect(here::here("data", "clean_data", "pma_clean_cm.shp"))

# mun <- geobr::read_municipality(year = 2025, showProgress = FALSE) |>
#   terra::vect() |>
#   terra::project(terra::crs(pma_clean))

mun_dir <- here::here("data", "raw_data", "BR_Municipios_2023")
mun     <- terra::vect(list.files(mun_dir, pattern = "\\.shp$", full.names = TRUE)[1]) |>
  terra::project(terra::crs(pma_clean))

centroides_pma <- terra::centroids(pma_clean, inside = TRUE)

# pma_mun_pt <- terra::intersect(centroides_pma, mun) |>
#   dplyr::select(PROCESSO, code_muni, name_muni, abbrev_state, name_state, name_region) |>
#   as.data.frame()

pma_mun_pt <- terra::intersect(centroides_pma, mun) |>
  dplyr::rename(
    code_muni    = CD_MUN,
    name_muni    = NM_MUN,
    abbrev_state = SIGLA_UF,
    name_state   = NM_UF,
    name_region  = NM_REGIAO
  ) |>
  dplyr::select(PROCESSO, code_muni, name_muni, abbrev_state, name_state, name_region) |>
  as.data.frame()

pma_mun_pt <- pma_mun_pt[!duplicated(pma_mun_pt[, "PROCESSO"]), ]
pma_mun    <- tidyterra::left_join(pma_clean, pma_mun_pt)

terra::writeVector(pma_mun, here::here("data", "clean_data", "pma_clean_cm_mun.shp"),
                   overwrite = TRUE)

message("Municipality assignment complete.")
clean_env()


# =============================================================================
# BLOCO 3: CFEM — limpeza, autuação e arrecadação
# =============================================================================
message("Integrating CFEM Revenue and Penalties...")

pma_mun <- terra::vect(here::here("data", "clean_data", "pma_clean_cm_mun.shp")) |>
  dplyr::mutate(dplyr::across(
    dplyr::where(is.numeric),
    ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)
  ))

# Autuação
cfem_aut <- readr::read_csv(here::here("data", "pre_proc_data", "CFEM_Autuacao.csv"),
                            show_col_types = FALSE) |>
  dplyr::mutate(
    AnoPublicação = as.numeric(AnoPublicação),
    MêsPublicação = as.numeric(MêsPublicação)
  ) |>
  dplyr::select(-dplyr::any_of(c("ProcessoCobrança", "Tipo_PF_PJ", "NúmeroAuto"))) |>
  dplyr::rename(
    TITULARaut   = NomeTitular,
    PROCESSO     = ProcessoMinerário,
    SUBSaut      = Substância,
    name_muni    = Município,
    abbrev_state = UF,
    ANO          = AnoPublicação,
    MES          = MêsPublicação,
    VALORaut     = Valor,
    CPF_CNPJaut  = CPF_CNPJ
  ) |>
  dplyr::filter(
    !is.na(PROCESSO), !is.na(ANO), !is.na(MES),
    !is.na(SUBSaut), !is.na(CPF_CNPJaut), PROCESSO != "NA/NA"
  ) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper))

aut_unique <- cfem_aut |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    cfem_aut  = 1L,
    aut_val_T = sum(VALORaut, na.rm = TRUE),
    aut_n     = dplyr::n(),
    .groups   = "drop"
  )

# Arrecadação
cfem_arr <- readr::read_csv(here::here("data", "pre_proc_data", "CFEM_Arrecadacao.csv"),
                            col_types = readr::cols(
                              ValorRecolhido          = readr::col_double(),
                              QuantidadeComercializada = readr::col_double()
                            ))

names(cfem_arr)[names(cfem_arr) == "Processo"] <- "ProcSemNum"
cfem_arr$PROCESSO <- paste(cfem_arr$ProcSemNum, cfem_arr$AnoDoProcesso, sep = "/")

cfem_arr <- cfem_arr |>
  dplyr::select(-dplyr::any_of(c("ProcSemNum", "Tipo_PF_PJ", "AnoDoProcesso", "DataCriacao"))) |>
  dplyr::rename(
    SUBSarr      = Substância,
    name_muni    = Município,
    code_muni    = CodigoMunicipio,
    abbrev_state = UF,
    ANO          = Ano,
    MES          = Mês,
    QTD_MINERIO  = QuantidadeComercializada,
    VALORarr     = ValorRecolhido,
    CPF_CNPJarr  = CPF_CNPJ,
    UM           = UnidadeDeMedida
  ) |>
  dplyr::filter(
    !is.na(PROCESSO), !is.na(ANO), !is.na(MES),
    !is.na(SUBSarr), !is.na(CPF_CNPJarr), PROCESSO != "NA/NA"
  ) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper))

# Razão social via microdados Pessoa.txt
pessoa <- readr::read_delim(
  here::here("data", "raw_data", "anm_microdados", "microdados-scm", "Pessoa.txt"),
  delim = ";",
  locale = readr::locale(encoding = "Windows-1252"),
  col_types = readr::cols(.default = "c"),
  show_col_types = FALSE
) |>
  dplyr::rename(
    CPF_CNPJarr = NRCPFCNPJ,
    NOME_arr    = NMPessoa
  ) |>
  dplyr::select(CPF_CNPJarr, NOME_arr) |>
  dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
  dplyr::mutate(NOME_arr = toupper(NOME_arr))

cfem_arr <- cfem_arr |>
  dplyr::left_join(pessoa, by = "CPF_CNPJarr") |>
  dplyr::mutate(
    NOME_arr = dplyr::if_else(is.na(NOME_arr), "NOME DESCONHECIDO", NOME_arr)
  )

arr_unique <- cfem_arr |>
  dplyr::arrange(PROCESSO, ANO, MES) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    cfem_arr  = 1L,
    arr_val_T = sum(VALORarr, na.rm = TRUE),
    arr_val_L = dplyr::if_else(all(is.na(VALORarr)), NA_real_, dplyr::last(na.omit(VALORarr))),
    arr_dt_F  = dplyr::first(sprintf("%04d-%02d", ANO, MES)),
    arr_dt_L  = dplyr::last(sprintf("%04d-%02d", ANO, MES)),
    arr_ndcl  = dplyr::n(),
    arr_nbuy  = dplyr::n_distinct(CPF_CNPJarr, na.rm = TRUE),
    .groups   = "drop"
  )

arr_aut <- dplyr::full_join(arr_unique, aut_unique, by = "PROCESSO") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.finite(.x), round(.x, 2), NA_real_)))

# Atributos IBGE do PMA para enriquecer CFEM
pma_ibge_arr <- pma_mun |>
  as.data.frame() |>
  dplyr::select(PROCESSO, name_state, name_regio) |>
  dplyr::distinct(PROCESSO, .keep_all = TRUE)

pma_ibge_aut <- pma_mun |>
  as.data.frame() |>
  dplyr::select(PROCESSO, name_state, name_regio, code_muni) |>
  dplyr::distinct(PROCESSO, .keep_all = TRUE)

cfem_arr_ibge <- dplyr::left_join(cfem_arr, pma_ibge_arr, by = "PROCESSO")
cfem_aut_ibge <- dplyr::left_join(cfem_aut, pma_ibge_aut, by = "PROCESSO")

# Save
readr::write_csv(cfem_aut_ibge, here::here("data", "clean_data", "cfem_aut_all_min_br.csv"))
readr::write_csv(cfem_arr_ibge, here::here("data", "clean_data", "cfem_arr_all_min_br.csv"))

message("CFEM integration complete.")
clean_env()


# =============================================================================
# BLOCO 4: Filtro Amazônia Legal
# =============================================================================
message("Filtering for Legal Amazon (AMZL)...")

cfem_aut_ibge <- readr::read_csv(here::here("data", "clean_data", "cfem_aut_all_min_br.csv"),
                                 show_col_types = FALSE)
cfem_arr_ibge <- readr::read_csv(here::here("data", "clean_data", "cfem_arr_all_min_br.csv"),
                                 show_col_types = FALSE)

pma_mun <- terra::vect(here::here("data", "clean_data", "pma_clean_cm_mun.shp")) |>
  dplyr::mutate(dplyr::across(
    dplyr::where(is.numeric),
    ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)
  ))

# Camadas de terras protegidas
ti  <- terra::vect(here::here("data", "pre_proc_data", "terras_indigenas.shp"))
uc  <- terra::vect(here::here("data", "pre_proc_data", "unidades_conservacao.shp"))
qui <- terra::vect(here::here("data", "pre_proc_data", "quilombolas.shp"))

# Amazônia Legal
# amzl <- geobr::read_amazon(showProgress = FALSE) |>
#   terra::vect() |>
#   terra::project(terra::crs(pma_mun))

amzl_dir <- here::here("data", "raw_data", "Limites_Amazonia_Legal_2024")
amzl     <- terra::vect(list.files(amzl_dir, pattern = "\\.shp$", full.names = TRUE)[1]) |>
  terra::project(terra::crs(pma_mun))

# Filtra PMA
intersect_ids                <- terra::is.related(pma_mun, amzl, "intersects")
pma_amzl                     <- pma_mun[intersect_ids, ]
processos_pma_amazonia_legal <- unique(pma_amzl$PROCESSO)

# Filtra CFEM pelo mesmo conjunto de processos
cfem_aut_amzl <- cfem_aut_ibge[cfem_aut_ibge$PROCESSO %in% processos_pma_amazonia_legal, ]
cfem_arr_amzl <- cfem_arr_ibge[cfem_arr_ibge$PROCESSO %in% processos_pma_amazonia_legal, ]

# Filtra TI, UC, Quilombolas por Amazônia Legal
ti_amzl  <- ti[terra::is.related(ti,   amzl, "intersects"), ]
uc_amzl  <- uc[terra::is.related(uc,   amzl, "intersects"), ]
qui_amzl <- qui[terra::is.related(qui, amzl, "intersects"), ]

# Salva intermediários e outputs finais de terras protegidas
readr::write_csv(cfem_aut_amzl, here::here("data", "clean_data", "cfem_aut_all_min_amzl.csv"))
readr::write_csv(cfem_arr_amzl, here::here("data", "clean_data", "cfem_arr_all_min_amzl.csv"))

terra::writeVector(pma_amzl,  here::here("data", "clean_data", "pma_amzl.shp"),  overwrite = TRUE)
terra::writeVector(ti_amzl,   here::here("data", "result",     "ti_amzl.shp"),   overwrite = TRUE)
terra::writeVector(uc_amzl,   here::here("data", "result",     "uc_amzl.shp"),   overwrite = TRUE)  
terra::writeVector(qui_amzl,  here::here("data", "result",     "qui_amzl.shp"),  overwrite = TRUE)

message("Amazon Legal subset created.")
message("TI, UC e Quilombolas salvos.")
clean_env()


# =============================================================================
# BLOCO 5: Correção de peso do ouro (Hierarquia Mediana + PowerOf10)
# =============================================================================
message("Starting Gold Correction algorithm")

cfem_arr_amzl0 <- readr::read_csv(
  here::here("data", "clean_data", "cfem_arr_all_min_amzl.csv"),
  show_col_types = FALSE) |>
  dplyr::mutate(row_id = dplyr::row_number())

pma_amzl <- terra::vect(here::here("data", "clean_data", "pma_amzl.shp")) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                               ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

# Fatores de conversão de unidade
fatores_kg <- c("KG" = 1, "T" = 1000, "G" = 0.001, "CT" = 0.0002)
fatores_g  <- c("KG" = 1000, "T" = 1e6, "G" = 1, "CT" = 0.2)

# STEP 1: Pesos e agrupamentos
cfem_arr_amzl1 <- cfem_arr_amzl0 |>
  dplyr::mutate(
    PESO_KG = round(as.double(QTD_MINERIO) * unname(fatores_kg[UM]), 10),
    PESO_G  = round(as.double(QTD_MINERIO) * unname(fatores_g[UM]), 10),
    SUBSarrSIM = dplyr::case_when(
      SUBSarr %in% target_minerals_list$ouro         ~ "OURO",
      SUBSarr %in% target_minerals_list$diamante     ~ "DIAMANTE",
      SUBSarr %in% target_minerals_list$litio        ~ "LÍTIO",
      SUBSarr %in% target_minerals_list$niobio       ~ "NIÓBIO",
      SUBSarr %in% target_minerals_list$tantalo      ~ "TÂNTALO",
      SUBSarr %in% target_minerals_list$estanho      ~ "ESTANHO",
      SUBSarr %in% target_minerals_list$tungstenio   ~ "TUNGSTÊNIO",
      SUBSarr %in% target_minerals_list$titanio      ~ "TITÂNIO",
      SUBSarr %in% target_minerals_list$terras_raras ~ "TERRAS RARAS",
      SUBSarr %in% target_minerals_list$cobalto      ~ "COBALTO",
      SUBSarr %in% target_minerals_list$grafite      ~ "GRAFITE",
      SUBSarr %in% target_minerals_list$niquel       ~ "NÍQUEL",
      SUBSarr %in% target_minerals_list$vanadio      ~ "VANÁDIO",
      SUBSarr %in% target_minerals_list$molibdenio   ~ "MOLIBDÊNIO",
      TRUE                                           ~ "OUTROS"
    )
  )

# STEP 2: Join com atributos espaciais do PMA
pma_attrs <- as.data.frame(pma_amzl) |>
  dplyr::select(
    PROCESSO, AREA_HA, FASE, ULT_EVENTO, TITULAR, SUBS,
    TIPO_REQcm, CPF_CNPJcm
  )

cfem_arr_amzl2 <- dplyr::left_join(cfem_arr_amzl1, pma_attrs, by = "PROCESSO") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                               ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

# STEP 3: Alíquotas e cálculo de preço
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
corte                 <- as.Date("2017-11-01")

cfem_arr_amzl3 <- cfem_arr_amzl2 |>
  dplyr::mutate(
    ANO      = as.integer(ANO),
    MES      = as.integer(MES),
    VALORarr = as.numeric(VALORarr),
    PESO_KG  = as.numeric(PESO_KG),
    PESO_G   = as.numeric(PESO_G),
    data     = as.Date(sprintf("%04d-%02d-01", ANO, MES))
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

# STEP 4: Filtro para correção
cfem_arr_amzl4 <- cfem_arr_amzl3 |>
  dplyr::filter(SUBSarrSIM == "OURO" &
                  (FASE == "LAVRA GARIMPEIRA" | 
                  FASE == "REQUERIMENTO DE LAVRA GARIMPEIRA" | 
                  FASE == "CONCESSÃO DE LAVRA" |
                  FASE == "REQUERIMENTO DE LAVRA" |
                  FASE == "AUTORIZAÇÃO DE PESQUISA"))

# STEP 5: Mediana hierárquica
compute_median_hierarchical <- function(df, preco_col = "preco_g",
                                        min_muni, min_state, min_month,
                                        min_ano, min_global,
                                        max_med_plaus, min_med_plaus) {
  df2       <- df |> mutate(.idx = dplyr::row_number())
  med_muni  <- df2 |> filter(!is.na(.data[[preco_col]])) |>
    group_by(data, code_muni) |>
    summarise(n_m = n(), med_m = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_state <- df2 |> filter(!is.na(.data[[preco_col]])) |>
    group_by(data, abbrev_state) |>
    summarise(n_s = n(), med_s = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_month <- df2 |> filter(!is.na(.data[[preco_col]])) |>
    group_by(data) |>
    summarise(n_mo = n(), med_mo = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_ano   <- df2 |> filter(!is.na(.data[[preco_col]])) |>
    group_by(ANO) |>
    summarise(n_a = n(), med_a = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_global <- df2 |> filter(!is.na(.data[[preco_col]])) |>
    summarise(n_g = n(), med_g = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")

  out <- df2 |>
    left_join(med_muni,  by = c("data", "code_muni")) |>
    left_join(med_state, by = c("data", "abbrev_state")) |>
    left_join(med_month, by = c("data")) |>
    left_join(med_ano,   by = c("ANO")) |>
    mutate(
      med_preco_base = case_when(
        !is.na(n_m)  & n_m  >= min_muni  & med_m  <= max_med_plaus & med_m  >= min_med_plaus ~ med_m,
        !is.na(n_s)  & n_s  >= min_state & med_s  <= max_med_plaus & med_s  >= min_med_plaus ~ med_s,
        !is.na(n_mo) & n_mo >= min_month & med_mo <= max_med_plaus & med_mo >= min_med_plaus ~ med_mo,
        !is.na(n_a)  & n_a  >= min_ano   & med_a  <= max_med_plaus & med_a  >= min_med_plaus ~ med_a,
        !is.na(med_global$med_g) & med_global$n_g >= min_global &
          med_global$med_g <= max_med_plaus & med_global$med_g >= min_med_plaus ~ med_global$med_g,
        TRUE ~ NA_real_
      ),
      med_level = case_when(
        !is.na(n_m)  & n_m  >= min_muni  & med_m  <= max_med_plaus & med_m  >= min_med_plaus ~ "muni",
        !is.na(n_s)  & n_s  >= min_state & med_s  <= max_med_plaus & med_s  >= min_med_plaus ~ "state",
        !is.na(n_mo) & n_mo >= min_month & med_mo <= max_med_plaus & med_mo >= min_med_plaus ~ "month",
        !is.na(n_a)  & n_a  >= min_ano   & med_a  <= max_med_plaus & med_a  >= min_med_plaus ~ "ano",
        !is.na(med_global$med_g) & med_global$n_g >= min_global &
          med_global$med_g <= max_med_plaus & med_global$med_g >= min_med_plaus ~ "global",
        TRUE ~ NA_character_
      )
    ) |> arrange(.idx)
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
  for (p in p_range) { if (p != 0) cands[[paste0("pow10_p", p)]] <- PESO_G * (10^p) }
  cand_df <- tibble::tibble(name = names(cands), peso_cand = unlist(cands)) |>
    mutate(
      preco_cand = if_else(peso_cand > min_peso_g, VALORtot / peso_cand, NA_real_),
      dist_rel   = if_else(!is.na(preco_cand), abs(preco_cand / med_preco - 1), NA_real_)
    )
  best_i <- which.min(replace(cand_df$dist_rel, is.na(cand_df$dist_rel), Inf))
  best   <- cand_df[best_i, ]
  list(PESO_G_sugerido  = as.numeric(best$peso_cand),
       preco_g_sugerido = as.numeric(best$preco_cand),
       dist_rel_sug     = as.numeric(best$dist_rel),
       corr_motivo      = if (best$name == "original") "original" else "pow10",
       candidate_name   = as.character(best$name))
}

<<<<<<< Updated upstream
=======
# Correção do OURO
cfem_arr_amzl4 <- cfem_arr_amzl3 |>
  dplyr::filter(SUBSarrSIM == "OURO" &
                  FASE %in% c("LAVRA GARIMPEIRA","REQUERIMENTO DE LAVRA GARIMPEIRA", "AUTORIZAÇÃO DE PESQUISA"))

>>>>>>> Stashed changes
med_info <- compute_median_hierarchical(
  cfem_arr_amzl4, preco_col = "preco_g_orig",
  min_muni = min_grp_muni, min_state = min_grp_state,
  min_month = min_grp_month, min_ano = min_grp_ano,
  min_global = min_grp_global,
  max_med_plaus = max_mediana_plausivel,
  min_med_plaus = min_mediana_plausivel
)

cfem_arr_amzl5 <- cfem_arr_amzl4 |>
  mutate(med_preco_base = med_info$med, med_level = med_info$level) |>
  rowwise() |>
  mutate(sug = list(suggest_weight_row(VALORtot = VALORtot, PESO_G = PESO_G,
                                       med_preco = med_preco_base))) |>
  unnest_wider(sug) |>
  ungroup() |>
  mutate(
    PESO_G_final  = if_else(!is.na(PESO_G_sugerido), PESO_G_sugerido, PESO_G),
    PESO_KG_final = if_else(!is.na(PESO_G_final), PESO_G_final / 1000, NA_real_),
    preco_g_final = if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g,
                            VALORtot / PESO_G_final, NA_real_)
  )

cfem_corr_join <- cfem_arr_amzl5 |>
  dplyr::select(row_id, candidate_name, PESO_G_final, PESO_KG_final, preco_g_final) |>
  dplyr::rename(corr = candidate_name)

cfem_final <- dplyr::left_join(cfem_arr_amzl3, cfem_corr_join, by = "row_id") |>
  dplyr::mutate(
    PESO_G_final  = if_else(is.na(PESO_G_final), PESO_G, PESO_G_final),
    PESO_KG_final = if_else(is.na(PESO_KG_final), PESO_KG, PESO_KG_final),
    preco_g_final = if_else(is.na(preco_g_final),
                            if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g,
                                    VALORtot / PESO_G_final, NA_real_),
                            preco_g_final),
    corr = if_else(is.na(corr), "original", corr)
  ) |>
  dplyr::mutate(
    ULT_EV_ID  = stringr::str_extract(ULT_EVENTO, "^\\d+"),
<<<<<<< Updated upstream
=======
    ULT_EV_DAT = as.Date(stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"), format = "%d/%m/%Y"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(
      ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  )

# Correção da CASSITERITA
cfem_final <- cfem_final |> dplyr::mutate(row_id = dplyr::row_number())

max_mediana_plausivel_cass <- 0.15      # cassiterita vale ~0,06 R$/g
min_mediana_plausivel_cass <- 0.02

cass_amzl4 <- cfem_final |>
  dplyr::filter(SUBSarr == "CASSITERITA" &
                  FASE %in% c("LAVRA GARIMPEIRA","REQUERIMENTO DE LAVRA GARIMPEIRA","AUTORIZAÇÃO DE PESQUISA"))

med_info_cass <- compute_median_hierarchical(
  cass_amzl4, preco_col = "preco_g_orig",
  min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
  min_ano = min_grp_ano, min_global = min_grp_global,
  max_med_plaus = max_mediana_plausivel_cass, min_med_plaus = min_mediana_plausivel_cass
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

# Visualização 1

df_plot <- cfem_final |>
  dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
  dplyr::filter(
    str_detect(toupper(FASE), "GARIMPEIRA") | 
    toupper(FASE) == "REQUERIMENTO DE LAVRA"
  ) |>
  dplyr::mutate(
    substancia_plot = dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"),
    status_correcao = dplyr::case_when(
      corr == "original" ~ "Mantido (Original)",
      TRUE ~ "Corrigido (pow10)"
    ),
    FASE_plot = dplyr::if_else(
      str_detect(toupper(FASE), "REQUERIMENTO"), 
      "REQUERIMENTO", 
      "LAVRA GARIMPEIRA"
    )
  ) |>
  dplyr::filter(!is.na(preco_g_orig) & !is.na(preco_g_final) & preco_g_orig > 0 & preco_g_final > 0) |>
  tidyr::pivot_longer(
    cols = c(preco_g_orig, preco_g_final),
    names_to = "cenario",
    values_to = "preco"
  ) |>
  dplyr::mutate(
    cenario = dplyr::if_else(cenario == "preco_g_orig", "1. Original", "2. Pós-Correção"),
    cor_ponto = dplyr::case_when(
      status_correcao == "Mantido (Original)" ~ "1. Mantido (Correto)",
      cenario == "1. Original" ~ "2. Erro (Antes do ajuste)",
      cenario == "2. Pós-Correção" ~ "3. Corrigido (pow10)"
    )
  )

cores_customizadas <- c(
  "1. Mantido (Correto)"      = "#bdc3c7",
  "2. Erro (Antes do ajuste)" = "#e74c3c",
  "3. Corrigido (pow10)"      = "#27ae60"
)

plot_substancia <- function(dados, nome_substancia) {
  ggplot(dados |> dplyr::filter(substancia_plot == nome_substancia), 
         aes(x = data, y = preco, color = cor_ponto)) +
    geom_point(alpha = 0.6, size = 1.2) +
    scale_y_log10(labels = scales::label_comma(accuracy = 0.01),
                  breaks = scales::trans_breaks("log10", function(x) 10^x)) +
    facet_grid(FASE_plot ~ cenario, scales = "free_y") +
    scale_color_manual(values = cores_customizadas) +
    theme_bw() +
    labs(
      title = paste(nome_substancia, "- Original vs Pós-Correção"),
      x = NULL, 
      y = "Preço (R$/g) - Log10",
      color = "Status do Dado"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p_ouro <- plot_substancia(df_plot, "OURO")
p_cass <- plot_substancia(df_plot, "CASSITERITA")

p_ouro / p_cass + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# Visualização 2

df_linha_temporal <- cfem_final |>
  dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
  dplyr::filter(
    str_detect(toupper(FASE), "GARIMPEIRA") | 
    toupper(FASE) == "REQUERIMENTO DE LAVRA"
  ) |>
  dplyr::mutate(
    substancia_plot = dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"),
    data = as.Date(sprintf("%04d-%02d-01", ANO, MES))
  ) |>

  dplyr::group_by(substancia_plot, data) |>
  dplyr::summarise(
    VALOR_mensal_total = sum(VALORarr, na.rm = TRUE),
    PRECO_mediano_orig  = median(preco_g_orig, na.rm = TRUE),
    PRECO_mediano_final = median(preco_g_final, na.rm = TRUE),
    .groups = "drop"
  )

df_preco_linhas <- df_linha_temporal |>
  tidyr::pivot_longer(
    cols = c(PRECO_mediano_orig, PRECO_mediano_final),
    names_to = "cenario",
    values_to = "preco_valor"
  ) |>
  dplyr::mutate(
    cenario = dplyr::if_else(cenario == "PRECO_mediano_orig", "Antes (Original)", "Após Correção (pow10)")
  )

p_linha_valor <- ggplot(df_linha_temporal, aes(x = data, y = VALOR_mensal_total)) +
  geom_line(color = "#34495e", size = 1) +
  geom_point(color = "#34495e", size = 1.5) +
  scale_y_log10(labels = scales::label_comma(prefix = "R$ ", accuracy = 1),
                breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  facet_wrap(~ substancia_plot, scales = "free_y", ncol = 1) +
  theme_bw() +
  labs(
    title = "Evolução do Valor Arrecadado Total",
    subtitle = "Soma mensal das guias de garimpo",
    x = "Data de Referência",
    y = "Arrecadação Total (Escala Log10)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_linha_preco <- ggplot(df_preco_linhas, aes(x = data, y = preco_valor, color = cenario, linetype = cenario)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_y_log10(labels = scales::label_comma(suffix = " R$/g", accuracy = 0.01),
                breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  facet_wrap(~ substancia_plot, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Antes (Original)" = "#e74c3c", "Após Correção (pow10)" = "#27ae60")) +
  scale_linetype_manual(values = c("Antes (Original)" = "dashed", "Após Correção (pow10)" = "solid")) +
  theme_bw() +
  labs(
    title = "Evolução do Preço Mediano Praticado",
    subtitle = "Correção da relação Valor x Novo Peso",
    x = "Data de Referência",
    y = "Preço por Grama (Escala Log10)",
    color = "Cenário dos Dados",
    linetype = "Cenário dos Dados"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_linha_valor | p_linha_preco + plot_layout(guides = "collect") & theme(legend.position = "bottom")













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
>>>>>>> Stashed changes
    ULT_EV_DAT = stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(
      ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  )

# Export CFEM final
readr::write_csv(cfem_final,
                 here::here("data", "result", "cfem_amzl_ALLminerals_GOLDcorrected.csv"))

message("Gold correction and CFEM output finished.")
#clean_env()

# =============================================================================
# BLOCO 5b: Correção de peso da Cassiterita (Hierarquia Mediana + PowerOf10)
# Mesma lógica do ouro - calibrado para cassiterita
# min_med = 0.001 R$/g | max_med = 1.0 R$/g (mediana observada ~0.06 R$/g)
# =============================================================================
message("Starting Cassiterita Correction algorithm")

cfem_final <- readr::read_csv(
  here::here("data", "result", "cfem_amzl_ALLminerals_GOLDcorrected.csv"),
  show_col_types = FALSE) |>
  dplyr::mutate(row_id = dplyr::row_number())

# Parâmetros específicos para cassiterita
min_peso_g            <- 0.00000000000000000001
min_grp_muni          <- 5
min_grp_state         <- 10
min_grp_month         <- 15
min_grp_ano           <- 20
min_grp_global        <- 100
p_round_min           <- -20
p_round_max           <- 20
max_mediana_plausivel <- 1.0     
min_mediana_plausivel <- 0.001  

# STEP 1: Filtro — só cassiterita PLG
cass_amzl4 <- cfem_final |>
  dplyr::filter(
    SUBSarr == "CASSITERITA" &
    FASE %in% c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA", "CONCESSÃO DE LAVRA", "REQUERIMENTO DE LAVRA", "AUTORIZAÇÃO DE PESQUISA")
  )

message("Cassiterita PLG registros para correção: ", nrow(cass_amzl4))

# STEP 2: Mediana hierárquica
med_info_cass <- compute_median_hierarchical(
  cass_amzl4,
  preco_col     = "preco_g_orig",
  min_muni      = min_grp_muni,
  min_state     = min_grp_state,
  min_month     = min_grp_month,
  min_ano       = min_grp_ano,
  min_global    = min_grp_global,
  max_med_plaus = max_mediana_plausivel,
  min_med_plaus = min_mediana_plausivel
)

# STEP 3: Aplicar correção
cass_amzl5 <- cass_amzl4 |>
  mutate(med_preco_base = med_info_cass$med, med_level = med_info_cass$level) |>
  rowwise() |>
  mutate(sug = list(suggest_weight_row(
    VALORtot  = VALORtot,
    PESO_G    = PESO_G,
    med_preco = med_preco_base,
    p_range   = p_round_min:p_round_max
  ))) |>
  unnest_wider(sug) |>
  ungroup() |>
  mutate(
    PESO_G_final  = if_else(!is.na(PESO_G_sugerido), PESO_G_sugerido, PESO_G),
    PESO_KG_final = if_else(!is.na(PESO_G_final), PESO_G_final / 1000, NA_real_),
    preco_g_final = if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g,
                            VALORtot / PESO_G_final, NA_real_)
  )

# STEP 4: Aplica correções de volta no dataset completo
cass_corr_join <- cass_amzl5 |>
  dplyr::select(row_id, candidate_name, PESO_G_final, PESO_KG_final, preco_g_final) |>
  dplyr::rename(
    corr_new          = candidate_name,
    PESO_G_final_new  = PESO_G_final,
    PESO_KG_final_new = PESO_KG_final,
    preco_g_final_new = preco_g_final
  )

cfem_final <- cfem_final |>
  dplyr::left_join(cass_corr_join, by = "row_id") |>
  dplyr::mutate(
    PESO_G_final  = dplyr::if_else(!is.na(PESO_G_final_new),
                                   PESO_G_final_new,  PESO_G_final),
    PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_final_new),
                                   PESO_KG_final_new, PESO_KG_final),
    preco_g_final = dplyr::if_else(!is.na(preco_g_final_new),
                                   preco_g_final_new, preco_g_final),
    corr          = dplyr::if_else(!is.na(corr_new),
                                   corr_new,          corr)
  ) |>
  dplyr::select(-PESO_G_final_new, -PESO_KG_final_new,
                -preco_g_final_new, -corr_new)

# STEP 5: Diagnóstico
message("Cassiterita — registros corrigidos: ",
        sum(cfem_final$corr != "original" & cfem_final$SUBSarr == "CASSITERITA", na.rm = TRUE))
message("Cassiterita — registros originais mantidos: ",
        sum(cfem_final$corr == "original" & cfem_final$SUBSarr == "CASSITERITA", na.rm = TRUE))

cfem_final |>
  dplyr::filter(SUBSarr == "CASSITERITA") |>
  dplyr::summarise(
    n_corrigidos = sum(corr != "original", na.rm = TRUE),
    n_originais  = sum(corr == "original", na.rm = TRUE),
    preco_med    = median(preco_g_final, na.rm = TRUE),
    preco_min    = min(preco_g_final, na.rm = TRUE),
    preco_max    = max(preco_g_final, na.rm = TRUE)
  ) |>
  print()

# Export
readr::write_csv(
  cfem_final,
  here::here("data", "result", "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv")
)

message("Cassiterita correction complete.")
clean_env()

# =============================================================================
# BLOCO 6: Export final PMA
# =============================================================================
message("Generating final Spatial output (ALL minerals)...")

cfem_final <- readr::read_csv(
  here::here("data", "result", "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"),
  show_col_types = FALSE)

pma_amzl_final <- terra::vect(here::here("data", "clean_data", "pma_amzl.shp")) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                               ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x))) |>
  dplyr::mutate(
    SUBSpmaGRP = dplyr::case_when(
      SUBS %in% target_minerals_list$ouro         ~ "OURO",
      SUBS %in% target_minerals_list$diamante     ~ "DIAMANTE",
      SUBS %in% target_minerals_list$litio        ~ "LÍTIO",
      SUBS %in% target_minerals_list$niobio       ~ "NIÓBIO",
      SUBS %in% target_minerals_list$tantalo      ~ "TÂNTALO",
      SUBS %in% target_minerals_list$estanho      ~ "ESTANHO",
      SUBS %in% target_minerals_list$tungstenio   ~ "TUNGSTÊNIO",
      SUBS %in% target_minerals_list$titanio      ~ "TITÂNIO",
      SUBS %in% target_minerals_list$terras_raras ~ "TERRAS RARAS",
      SUBS %in% target_minerals_list$cobalto      ~ "COBALTO",
      SUBS %in% target_minerals_list$grafite      ~ "GRAFITE",
      SUBS %in% target_minerals_list$niquel       ~ "NÍQUEL",
      SUBS %in% target_minerals_list$vanadio      ~ "VANÁDIO",
      SUBS %in% target_minerals_list$molibdenio   ~ "MOLIBDÊNIO",
      TRUE                                        ~ "OUTROS"
    ),
    ULT_EV_ID  = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT = stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(
      ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  )

# Agrega CFEM corrigido por processo para join no shapefile
arr_corr_unique <- cfem_final |>
  dplyr::arrange(PROCESSO, ANO, MES) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    arr_kg_T = sum(PESO_KG_final, na.rm = TRUE),
    arr_kg_L = dplyr::if_else(all(is.na(PESO_KG_final)), NA_real_,
                               dplyr::last(na.omit(PESO_KG_final))),
    arr_g_T  = sum(PESO_G_final,  na.rm = TRUE),
    arr_g_L  = dplyr::if_else(all(is.na(PESO_G_final)), NA_real_,
                               dplyr::last(na.omit(PESO_G_final))),
    .groups  = "drop"
  ) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

pma_amzl_final <- tidyterra::left_join(pma_amzl_final, arr_corr_unique, by = "PROCESSO") |>
  dplyr::mutate(
    arr_kg_T = tidyr::replace_na(arr_kg_T, 0),
    arr_g_T  = tidyr::replace_na(arr_g_T,  0)
  )

terra::writeVector(pma_amzl_final,
                   here::here("data", "result", "pma_amzl_ALLminerals_final.shp"),
                   overwrite = TRUE)

message("Final PMA shapefile saved.")
message("PROCESS COMPLETE.")