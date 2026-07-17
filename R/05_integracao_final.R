################################################################################
# 05_integracao_final.R
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

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
  library(stringr)
  library(here)
  library(ggplot2)
})

source(here::here("R", "utils.R"))

# --- Caminhos -----------------------------------------------------------------
RAW_DIR      <- here::here("data", "raw_data")
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
MICRO_OUT_DIR <- here::here("data", "result_db", "microdados")  # parquets do 04
QA_DIR       <- here::here("data", "_qa", "05_integracao_final")

RESULT_SHINY <- here::here("data", "result_shiny")
RESULT_GEE   <- here::here("data", "result_gee")
RESULT_DB    <- here::here("data", "result_db")

MUNI_DIR  <- here::here("data", "raw_data", "BR_Municipios_2025")
BIOMA_DIR <- here::here("data", "raw_data", "Biomas_250mil")

for (d in c(RESULT_SHINY, RESULT_GEE, RESULT_DB, QA_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# =============================================================================
# BLOCO 4 — MUNICÍPIO
# =============================================================================

pma_amzl <- load_ckpt("03_pma_amzl")

proc_mun <- arrow::read_parquet(file.path(MICRO_OUT_DIR, "micro_processo_municipio.parquet"))
muni_lk  <- arrow::read_parquet(file.path(MICRO_OUT_DIR, "micro_municipio.parquet"))
muni_lk2 <- muni_lk |>
  dplyr::transmute(
    idmunicipio = idmunicipio,
    munic_micro = toupper(nmmunicipio),
    uf_micro    = toupper(sguf)
  )

proc_mun_resumo <- proc_mun |>
  dplyr::left_join(muni_lk2, by = "idmunicipio") |>
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
col_nmmun  <- "NM_MUN"
col_ufmun  <- "SIGLA_UF"

cent_mun <- terra::intersect(centroides, muni_ibge) |>
  as.data.frame() |>
  dplyr::transmute(
    PROCESSO,
    munic_centroide = toupper(.data[[col_nmmun]]),
    uf_centroide    = toupper(.data[[col_ufmun]])
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
    PROCESSO   = cent_na$PROCESSO,
    munic_near = toupper(terra::values(muni_ibge)[[col_nmmun]][idx_near]),
    uf_near    = toupper(terra::values(muni_ibge)[[col_ufmun]][idx_near])
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

# --- Check/parecer: distribuição da fonte do município ------------------------
munic_check <- munic_final |> dplyr::count(munic_fonte, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 2))
readr::write_csv(munic_check, file.path(QA_DIR, "municipio_fonte_distribuicao.csv"))
message("[05][municipio] distribuicao de fonte:")
print(munic_check)
message("[05][municipio] processos sem municipio (mesmo apos nearest): ", sum(is.na(munic_final$munic)))

pma_amzl <- tidyterra::left_join(pma_amzl, munic_final, by = "PROCESSO")
save_ckpt(pma_amzl, "05_pma_munic")

# =============================================================================
# BLOCO 5 — INTERSEÇÃO ESPACIAL (TI/UC/Quilombola + embargos)
# =============================================================================

pma_amzl <- load_ckpt("05_pma_munic")
ti_amzl  <- load_ckpt("03_ti_amzl")
uc_amzl  <- load_ckpt("03_uc_amzl")
qui_amzl <- load_ckpt("03_qui_amzl")

invalid_geom <- !terra::is.valid(pma_amzl)
if (any(invalid_geom)) {
  pma_amzl <- rbind(terra::buffer(pma_amzl[invalid_geom, ], 0), pma_amzl[!invalid_geom, ])
}

uc_pi_resex <- uc_amzl[uc_amzl$grupo == "Proteção Integral" | uc_amzl$sigla_snuc == "RESEX", ]
qui_amzl    <- qui_amzl |> dplyr::select(nm_comunid)

qui_bf <- terra::buffer(qui_amzl, width = 10000)
ti_bf  <- terra::buffer(ti_amzl,  width = 10000)
uc_bf  <- terra::buffer(uc_pi_resex,
                        width = ifelse(toupper(trimws(uc_pi_resex$pl_manejo)) == "SIM", 10000, 2000))

qui_bf_only <- terra::erase(qui_bf, qui_amzl)
ti_bf_only  <- terra::erase(ti_bf,  ti_amzl)
uc_bf_only  <- terra::erase(uc_bf,  uc_pi_resex)

calc_overlap_named <- function(pma_lyr, tp_lyr, flag_name, name_col, out_name,
                               fix_encoding = FALSE, extra_cols = NULL) {
  inter <- terra::intersect(pma_lyr, tp_lyr)
  inter$area_inter <- terra::expanse(inter, unit = "ha")

  keep_cols <- c("PROCESSO", "AREA_HA", "area_inter", name_col, extra_cols)
  inter <- inter |> tidyterra::select(dplyr::all_of(keep_cols)) |> as.data.frame()
  inter$Propor <- as.numeric(inter$area_inter / inter$AREA_HA)
  inter <- inter |> dplyr::filter(Propor >= 0.05)

  if (nrow(inter) == 0) {
    base <- tibble::tibble(PROCESSO = character(0))
    base[[flag_name]] <- integer(0)
    base[[out_name]]  <- character(0)
    if (!is.null(extra_cols)) for (ec in extra_cols) base[[ec]] <- character(0)
    return(base)
  }

  inter <- inter |> dplyr::rename(!!out_name := dplyr::all_of(name_col))
  if (fix_encoding) {
    inter[[out_name]] <- stringi::stri_encode(inter[[out_name]], from = "Windows-1252", to = "UTF-8")
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

df_uc_pma <- calc_overlap_named(pma_amzl, uc_pi_resex, "UCov", "nome_uc", "UCname",
                                extra_cols = "sigla_snuc") |> dplyr::rename(UCtype = sigla_snuc)
df_ti_pma <- calc_overlap_named(pma_amzl, ti_amzl, "TIov", "terrai_nom", "TIname")
df_qui_pma <- calc_overlap_named(pma_amzl, qui_amzl, "QUIov", "nm_comunid", "QUIname")

pma_tp1 <- pma_amzl |>
  tidyterra::left_join(df_ti_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_uc_pma,  by = "PROCESSO") |>
  tidyterra::left_join(df_qui_pma, by = "PROCESSO") |>
  dplyr::mutate(
    TIov  = tidyr::replace_na(TIov,  0L),
    UCov  = tidyr::replace_na(UCov,  0L),
    QUIov = tidyr::replace_na(QUIov, 0L)
  )

df_uc_donut <- calc_overlap_named(pma_tp1, uc_bf_only, "UCov2_10km", "nome_uc", "UCname_ov",
                                  extra_cols = "sigla_snuc") |> dplyr::rename(UCtype_ov = sigla_snuc)
df_ti_donut <- calc_overlap_named(pma_tp1, ti_bf_only, "TIov10km", "terrai_nom", "TIname_ov")
df_qui_donut <- calc_overlap_named(pma_tp1, qui_bf_only, "QUIov10km", "nm_comunid", "QUIname_ov")

pma_tp <- pma_tp1 |>
  tidyterra::left_join(df_ti_donut,  by = "PROCESSO") |>
  tidyterra::left_join(df_uc_donut,  by = "PROCESSO") |>
  tidyterra::left_join(df_qui_donut, by = "PROCESSO") |>
  dplyr::mutate(
    TIov10km   = tidyr::replace_na(TIov10km,   0L),
    UCov2_10km = tidyr::replace_na(UCov2_10km, 0L),
    QUIov10km  = tidyr::replace_na(QUIov10km,  0L)
  )

EMBmtSEMA <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "sema_mt_embargos.shp"))
EMBmtSIGA <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "sema_mt_embargos_siga.shp"))
EMBib     <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "ibama_embargos.shp"))
EMBic     <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "icmbio_embargos.shp"))
INFmtSIGA <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "sema_mt_infracoes_siga.shp"))
INFic     <- carregar_shp_opcional(file.path(PRE_PROC_DIR, "icmbio_infracoes.shp"))
 
pma_tp$inf_MT  <- relacionar_flag_opcional(pma_tp, INFmtSIGA)
pma_tp$inf_IC  <- relacionar_flag_opcional(pma_tp, INFic)
pma_tp$emb_MTa <- relacionar_flag_opcional(pma_tp, EMBmtSEMA)
pma_tp$emb_MTb <- relacionar_flag_opcional(pma_tp, EMBmtSIGA)
pma_tp$emb_IB  <- relacionar_flag_opcional(pma_tp, EMBib)
pma_tp$emb_IC  <- relacionar_flag_opcional(pma_tp, EMBic)

# --- Registro em QA: quais fontes de embargo/infracao faltaram nesta execucao
fontes_check <- tibble::tibble(
  fonte = c("sema_mt_embargos", "sema_mt_embargos_siga", "ibama_embargos", "icmbio_embargos",
           "sema_mt_infracoes_siga", "icmbio_infracoes"),
  disponivel = c(!is.null(EMBmtSEMA), !is.null(EMBmtSIGA), !is.null(EMBib), !is.null(EMBic),
                !is.null(INFmtSIGA), !is.null(INFic))
)
readr::write_csv(fontes_check, file.path(QA_DIR, "fontes_embargo_infracao_disponibilidade.csv"))
if (any(!fontes_check$disponivel)) {
  message("[05][embargos] ATENCAO — fonte(s) indisponivel(is) nesta execucao: ",
          paste(fontes_check$fonte[!fontes_check$disponivel], collapse = ", "),
          " | flag(s) correspondente(s) gravada(s) como NA (nao 0) em pma_tp.")
}

# --- Check/parecer: quantos processos carregam cada flag de sobreposição ------
flags_sobrep <- c("TIov","UCov","QUIov","TIov10km","UCov2_10km","QUIov10km",
                  "inf_MT","inf_IC","emb_MTa","emb_MTb","emb_IB","emb_IC")
sobrep_check <- as.data.frame(pma_tp) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(flags_sobrep), ~ sum(.x == 1L, na.rm = TRUE))) |>
  tidyr::pivot_longer(everything(), names_to = "flag", values_to = "n_processos") |>
  dplyr::mutate(pct = round(100 * n_processos / nrow(pma_tp), 2))
readr::write_csv(sobrep_check, file.path(QA_DIR, "sobreposicao_flags_distribuicao.csv"))
message("[05][sobreposicao] flags de sobreposicao/embargo:")
print(sobrep_check)

save_ckpt(pma_tp, "05_pma_tp")

# =============================================================================
# BLOCO 6 — CFEM (OURO e CASSITERITA)
# =============================================================================

processos_amzl <- load_ckpt("03_processos_amzl")
pma_tp         <- load_ckpt("05_pma_tp")

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

cfem_aut <- readr::read_csv(file.path(PRE_PROC_DIR, "CFEM_Autuacao.csv"), show_col_types = FALSE) |>
  dplyr::mutate(AnoPublicação = as.numeric(AnoPublicação), MêsPublicação = as.numeric(MêsPublicação)) |>
  dplyr::select(-dplyr::any_of(c("ProcessoCobrança", "Tipo_PF_PJ", "NúmeroAuto"))) |>
  dplyr::rename(TITULARaut = NomeTitular, PROCESSO = ProcessoMinerário, SUBSaut = Substância,
               name_muni = Município, abbrev_state = UF, ANO = AnoPublicação,
               MES = MêsPublicação, VALORaut = Valor, CPF_CNPJaut = CPF_CNPJ) |>
  dplyr::filter(!is.na(PROCESSO), !is.na(ANO), !is.na(MES), !is.na(SUBSaut), !is.na(CPF_CNPJaut), PROCESSO != "NA/NA") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJaut = padroniza_doc(CPF_CNPJaut))

cfem_arr <- readr::read_csv(file.path(PRE_PROC_DIR, "CFEM_Arrecadacao.csv"),
                            col_types = readr::cols(ValorRecolhido = readr::col_double(),
                                                     QuantidadeComercializada = readr::col_double()))
names(cfem_arr)[names(cfem_arr) == "Processo"] <- "ProcSemNum"
cfem_arr$PROCESSO <- paste(cfem_arr$ProcSemNum, cfem_arr$AnoDoProcesso, sep = "/")

cfem_arr <- cfem_arr |>
  dplyr::select(-dplyr::any_of(c("ProcSemNum", "Tipo_PF_PJ", "AnoDoProcesso", "DataCriacao"))) |>
  dplyr::rename(SUBSarr = Substância, name_muni = Município, code_muni = CodigoMunicipio,
               abbrev_state = UF, ANO = Ano, MES = Mês, QTD_MINERIO = QuantidadeComercializada,
               VALORarr = ValorRecolhido, CPF_CNPJarr = CPF_CNPJ, UM = UnidadeDeMedida) |>
  dplyr::filter(!is.na(PROCESSO), !is.na(ANO), !is.na(MES), !is.na(SUBSarr), !is.na(CPF_CNPJarr), PROCESSO != "NA/NA") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr))

# Razão social — CORRIGIDO: le do parquet do 04 em vez de Pessoa.txt bruto.
pessoa <- arrow::read_parquet(file.path(MICRO_OUT_DIR, "micro_pessoa.parquet")) |>
  dplyr::rename(CPF_CNPJarr = nrcpfcnpj, NOME_arr = nmpessoa) |>
  dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr)) |>
  dplyr::select(CPF_CNPJarr, NOME_arr) |>
  dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
  dplyr::mutate(NOME_arr = toupper(NOME_arr))

razao_social_path <- file.path(RAW_DIR, "cefem_arrecadacao(semshapes).csv")
if (!file.exists(razao_social_path)) {
  warning("[05][CFEM] Arquivo auxiliar de razao social nao encontrado (fonte fora do 01_download.R): ",
          razao_social_path, " — prosseguindo sem o fallback.")
  RazaoSocial <- tibble::tibble(CPF_CNPJarr = character(0), NOME_arr_alt = character(0))
} else {
  RazaoSocial <- readr::read_csv(razao_social_path, show_col_types = FALSE) |>
    dplyr::rename(CPF_CNPJarr = cnpj_cpf, NOME_arr_alt = razao_social) |>
    dplyr::mutate(CPF_CNPJarr = padroniza_doc(CPF_CNPJarr)) |>
    dplyr::select(CPF_CNPJarr, NOME_arr_alt) |>
    dplyr::distinct(CPF_CNPJarr, .keep_all = TRUE) |>
    dplyr::mutate(NOME_arr_alt = toupper(NOME_arr_alt))
}

cfem_arr <- cfem_arr |>
  dplyr::left_join(pessoa,      by = "CPF_CNPJarr") |>
  dplyr::left_join(RazaoSocial, by = "CPF_CNPJarr") |>
  dplyr::mutate(NOME_arr = dplyr::coalesce(NOME_arr, NOME_arr_alt, "NOME DESCONHECIDO")) |>
  dplyr::select(-NOME_arr_alt)

cfem_arr_amzl0 <- cfem_arr |>
  dplyr::filter(PROCESSO %in% processos_amzl) |>
  dplyr::mutate(row_id = dplyr::row_number())

cfem_aut_amzl <- cfem_aut |> dplyr::filter(PROCESSO %in% processos_amzl)

fatores_kg <- c("KG" = 1, "T" = 1000, "G" = 0.001, "CT" = 0.0002)
fatores_g  <- c("KG" = 1000, "T" = 1e6, "G" = 1, "CT" = 0.2)

cfem_arr_amzl1 <- cfem_arr_amzl0 |>
  dplyr::mutate(
    PESO_KG    = round(as.double(QTD_MINERIO) * unname(fatores_kg[UM]), 10),
    PESO_G     = round(as.double(QTD_MINERIO) * unname(fatores_g[UM]), 10),
    SUBSarrSIM = classificar_grupo(SUBSarr)
  )

pma_attrs <- as.data.frame(pma_tp) |>
  dplyr::select(PROCESSO, AREA_HA, FASE, ULT_EVENTO, TITULAR, SUBS, TIPO_REQcm, CPF_CNPJcm) |>
  dplyr::distinct(PROCESSO, .keep_all = TRUE)

cfem_arr_amzl2 <- dplyr::left_join(cfem_arr_amzl1, pma_attrs, by = "PROCESSO") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

min_peso_g            <- 0.00000000000000000001
min_grp_muni          <- 5
min_grp_state         <- 10
min_grp_month         <- 15
min_grp_ano           <- 20
min_grp_global        <- 100
p_round_min           <- -20
p_round_max           <- 20
corte                 <- as.Date("2017-11-01")

cfem_arr_amzl3 <- cfem_arr_amzl2 |>
  dplyr::mutate(
    ANO = as.integer(ANO), MES = as.integer(MES), VALORarr = as.numeric(VALORarr),
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

compute_median_hierarchical <- function(df, preco_col = "preco_g_orig",
                                        min_muni, min_state, min_month, min_ano, min_global,
                                        max_med_plaus, min_med_plaus) {
  df2 <- df |> dplyr::mutate(.idx = dplyr::row_number())
  med_muni  <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(data, code_muni) |>
    dplyr::summarise(n_m = dplyr::n(), med_m = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_state <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(data, abbrev_state) |>
    dplyr::summarise(n_s = dplyr::n(), med_s = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_month <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(data) |>
    dplyr::summarise(n_mo = dplyr::n(), med_mo = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_ano   <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(ANO) |>
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

suggest_weight_row <- function(VALORtot, PESO_G, med_preco, p_range = p_round_min:p_round_max) {
  if (is.na(VALORtot) | is.na(med_preco) | med_preco <= 0) {
    return(list(PESO_G_sugerido = NA_real_, preco_g_sugerido = NA_real_,
                dist_rel_sug = NA_real_, corr_motivo = "no_med", candidate_name = NA_character_))
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
  list(PESO_G_sugerido = as.numeric(best$peso_cand), preco_g_sugerido = as.numeric(best$preco_cand),
       dist_rel_sug = as.numeric(best$dist_rel),
       corr_motivo = if (best$name == "original") "original" else "pow10",
       candidate_name = as.character(best$name))
}

FASES_CORR <- FASES_CORR_PADRAO

cfem_final <- cfem_arr_amzl3 |>
  dplyr::mutate(PESO_G_final = PESO_G, PESO_KG_final = PESO_KG, preco_g_final = preco_g_orig, corr = "original")

fatores_simples <- 10^(-6:6)
corrige_simples_g <- function(peso_g, valortot, pmin_g, pmax_g) {
  if (is.na(peso_g) || is.na(valortot) || peso_g <= 0 || valortot <= 0) return(c(peso = peso_g, fator = NA_real_))
  ok <- fatores_simples[ {p <- valortot / (peso_g * fatores_simples); p >= pmin_g & p <= pmax_g} ]
  if (length(ok) == 0) return(c(peso = peso_g, fator = NA_real_))
  f <- ok[which.min(abs(log10(ok)))]
  c(peso = peso_g * f, fator = f)
}

report_check <- function(df, mineral, etapa, pmin_kg, pmax_kg, qa_path = NULL) {
  d <- df |>
    dplyr::filter(!is.na(PESO_KG_final), PESO_KG_final > 0, !is.na(VALORtot), VALORtot > 0) |>
    dplyr::mutate(rs_por_kg = VALORtot / PESO_KG_final, fora = rs_por_kg < pmin_kg | rs_por_kg > pmax_kg)
  resumo <- d |> dplyr::count(FASE, fora) |> dplyr::arrange(dplyr::desc(fora), dplyr::desc(n))
  message(sprintf("[%s] %s | avaliados: %d | fora de [%g-%g] R$/kg: %d",
                  mineral, etapa, nrow(d), pmin_kg, pmax_kg, sum(d$fora, na.rm = TRUE)))
  print(resumo)
  if (!is.null(qa_path)) {
    readr::write_csv(resumo |> dplyr::mutate(mineral = mineral, etapa = etapa, .before = 1),
                     qa_path, append = file.exists(qa_path))
  }
  invisible(d)
}

corrige_mineral_3checks <- function(cfem_final, mineral_label, subs_keep, subs_col,
                                    pmin_kg, pmax_kg, min_med_plaus, max_med_plaus) {
  pmin_g <- pmin_kg / 1000; pmax_g <- pmax_kg / 1000
  qa_path_checks <- file.path(QA_DIR, "cfem_correcao_checks.csv")

  universo <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  report_check(universo, mineral_label, "CHECK 1 (antes)", pmin_kg, pmax_kg, qa_path_checks)

  med_info <- compute_median_hierarchical(
    universo, preco_col = "preco_g_orig", min_muni = min_grp_muni, min_state = min_grp_state,
    min_month = min_grp_month, min_ano = min_grp_ano, min_global = min_grp_global,
    max_med_plaus = max_med_plaus, min_med_plaus = min_med_plaus
  )

  rob <- universo |>
    dplyr::mutate(med_preco_base = med_info$med, med_level = med_info$level) |>
    dplyr::rowwise() |>
    dplyr::mutate(sug = list(suggest_weight_row(VALORtot, PESO_G, med_preco_base, p_range = p_round_min:p_round_max))) |>
    tidyr::unnest_wider(sug) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_sugerido), PESO_G_sugerido, PESO_G),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_G_final), PESO_G_final / 1000, NA_real_),
      preco_g_final = dplyr::if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g, VALORtot / PESO_G_final, NA_real_),
      corr = dplyr::if_else(is.na(candidate_name), "original", candidate_name)
    )

  cfem_final <- cfem_final |>
    dplyr::left_join(
      rob |> dplyr::select(row_id, PESO_G_final, PESO_KG_final, preco_g_final, corr) |>
        dplyr::rename(PESO_G_r = PESO_G_final, PESO_KG_r = PESO_KG_final, preco_g_r = preco_g_final, corr_r = corr),
      by = "row_id") |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_r),  PESO_G_r,  PESO_G_final),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_r), PESO_KG_r, PESO_KG_final),
      preco_g_final = dplyr::if_else(!is.na(preco_g_r), preco_g_r, preco_g_final),
      corr          = dplyr::if_else(!is.na(corr_r),    corr_r,    corr)
    ) |>
    dplyr::select(-PESO_G_r, -PESO_KG_r, -preco_g_r, -corr_r)

  univ2 <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  d2 <- report_check(univ2, mineral_label, "CHECK 2 (pos-robusto)", pmin_kg, pmax_kg, qa_path_checks)
  ids_fora <- d2 |> dplyr::filter(fora) |> dplyr::pull(row_id)

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
        preco_g_final = dplyr::if_else(aplicou & PESO_G_final > min_peso_g, VALORtot / PESO_G_final, preco_g_final),
        corr = dplyr::if_else(aplicou, paste0("simples_1e", round(log10(fator_fb))), corr)
      ) |>
      dplyr::select(-PESO_G_fb, -fator_fb, -aplicou)
    message("[", mineral_label, "] SIMPLES aplicado a ", length(ids_fora), " remanescente(s).")
  } else {
    message("[", mineral_label, "] nenhum remanescente para o metodo simples.")
  }

  univ3 <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  d3 <- report_check(univ3, mineral_label, "CHECK 3 (final)", pmin_kg, pmax_kg, qa_path_checks)
  resta <- d3 |> dplyr::filter(fora)
  if (nrow(resta) > 0) {
    message("[", mineral_label, "] IRRECUPERAVEIS - revisar:")
    print(resta |> dplyr::select(PROCESSO, FASE, PESO_KG, PESO_KG_final, VALORtot, rs_por_kg))
    readr::write_csv(resta |> dplyr::mutate(mineral = mineral_label, .before = 1),
                     file.path(QA_DIR, paste0("cfem_irrecuperaveis_", tolower(mineral_label), ".csv")))
  }
  dist_corr <- univ3 |> dplyr::count(corr, sort = TRUE)
  message("[", mineral_label, "] distribuicao final de 'corr':")
  print(dist_corr)
  readr::write_csv(dist_corr |> dplyr::mutate(mineral = mineral_label, .before = 1),
                   file.path(QA_DIR, paste0("cfem_distribuicao_corr_", tolower(mineral_label), ".csv")))

  cfem_final
}

# ============================================================================
# CASSITERITA -- correcao por fator de 10 contra faixa absoluta
# ("metodo white solder", validado em investigacao_white_solder.R)
#
# SUBSTITUI corrige_mineral_3checks() APENAS PARA CASSITERITA.
# O OURO segue inalterado na funcao original (mediana hierarquica + fallback).
# FASES_CORR / FASES_CORR_PADRAO nao sao alterados -- a cassiterita passa a
# usar seu proprio vetor (FASES_CASS) abaixo.
#
# --- POR QUE MUDOU (diagnostico 2026-07-16, dados pre-correcao) -------------
#   A mediana hierarquica falhava em massa nesta substancia. No processo
#   886559/2004 (COOGER/RO), 48 de 100 declaracoes caiam no fallback
#   'simples_1e-1'. Um fallback que dispara em ~50% dos casos nao e fallback.
#
#   Causa raiz: suggest_weight_row() escolhe o fator que MINIMIZA a distancia
#   a mediana do grupo (which.min(dist_rel)) -- SEM exigir que o resultado
#   caia na faixa plausivel. Com a mediana contaminada, ele "corrige" para
#   fora da faixa e ainda marca como pow10_p*, que parece correcao de boa
#   qualidade.
#
#   Este metodo inverte a ordem: filtra PRIMEIRO os fatores que garantem
#   preco dentro de [pmin_kg, pmax_kg], e so entao escolhe o mais conservador
#   (menor |log10(fator)|). E impossivel produzir resultado fora da faixa --
#   ou corrige direito, ou marca dado_corrompido.
#
# --- ESCOPO (decisao metodologica, registrada) ------------------------------
#   Fases (FASES_CASS): PLG + REQ PLG + AUT PESQUISA.
#     Exclui CONCESSAO DE LAVRA (mineracao industrial), que dominava a serie
#     pre-2018 e tem estrutura de preco distinta do garimpo. Consequencia:
#     cassiterita em CONCESSAO DE LAVRA deixa de ser corrigida (corr fica
#     "original"). Mudanca de comportamento consciente.
#
#   Dois subsets, MESMA faixa [30,300], processados SEPARADAMENTE:
#     A) ANO >= 2018 -- faixa com respaldo empirico: 51%-94% das declaracoes
#        ja caem em [30,300] SEM correcao alguma (2018:34,5% / 2019:52,3% /
#        2020:51,1% / 2021:73,1% / 2022:84,6% / 2023:69,3% / 2024:77,7% /
#        2025:94,4% / 2026:89,1%).
#     B) ANO <= 2017 -- mesma faixa aplicada, mas aderencia observada de 0%
#        em 2006/2007/2008/2010/2012/2015 (n=145 no total, 7,5% da base).
#        Resultado esperado: alta taxa de dado_corrompido. Mantido no output
#        como EVIDENCIA de que o periodo nao reconcilia com a faixa -- NAO
#        como correcao confiavel. Analisar separadamente.
#
#   O corte em 2018 e EMPIRICO (salto de densidade e de aderencia a faixa),
#   nao economico. Nao foi encontrada serie historica publica de preco de
#   cassiterita que permitisse ancorar faixas por periodo; referencias
#   pontuais de 2024 (Sec. Fazenda RO: R$107,55/kg; IBAMA: ~R$115/kg) sao
#   compativeis com [30,300] mas nao cobrem o historico.
# ============================================================================

FASES_CASS <- c(
  "LAVRA GARIMPEIRA",
  "REQUERIMENTO DE LAVRA GARIMPEIRA",
  "AUTORIZAÇÃO DE PESQUISA"
)

ANO_CORTE_CASS <- 2018

# Nucleo do metodo: entre os fatores de 10 que jogam o preco DENTRO da faixa,
# devolve o mais conservador (menor |log10|). Se nenhum resolve, NA + motivo.
#   motivo 0 = sem_dado                 -> peso/valor ausente ou <= 0
#   motivo 1 = dado_corrompido          -> nenhum fator de 10 reconcilia
#   motivo 2 = resolvido
#   motivo 3 = sem_quantidade_declarada -> QTD_MINERIO == 0 na origem
#
# ACHADO (auditoria 2026-07-16, processo 886559/2004 / COOGER, ano 2006):
# 9 declaracoes tem QTD_MINERIO == 0 na fonte -- CFEM recolhida sem
# quantidade de minerio declarada. O peso resultante e forcado a um valor
# minimo (1 kg) em algum ponto a montante, e VALORtot/1 explode o preco
# (R$490, R$5.363, R$11.920/kg). Isso NAO e erro de unidade: nenhum fator
# de 10 conserta uma quantidade zero (0 * 10^n = 0). Marcar, nao corrigir.
ws_fator_10 <- function(peso_g, valortot, pmin_g, pmax_g, qtd_minerio = NA_real_) {
  if (!is.na(qtd_minerio) && qtd_minerio == 0) {
    return(c(fator = NA_real_, motivo = 3))
  }
  if (is.na(peso_g) || is.na(valortot) || peso_g <= 0 || valortot <= 0) {
    return(c(fator = NA_real_, motivo = 0))
  }
  ok <- fatores_simples[ {p <- valortot / (peso_g * fatores_simples); p >= pmin_g & p <= pmax_g} ]
  if (length(ok) == 0) {
    return(c(fator = NA_real_, motivo = 1))
  }
  c(fator = ok[which.min(abs(log10(ok)))], motivo = 2)
}

corrige_cassiterita_ws <- function(cfem_final, pmin_kg = 30, pmax_kg = 300,
                                   subset_label, filtro_ano) {
  pmin_g <- pmin_kg / 1000; pmax_g <- pmax_kg / 1000
  qa_path_checks <- file.path(QA_DIR, "cfem_correcao_checks.csv")
  mineral_label  <- paste0("CASSITERITA_", subset_label)

  universo <- cfem_final |>
    dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CASS, filtro_ano(ANO))

  if (nrow(universo) == 0) {
    message("[", mineral_label, "] subset vazio -- nada a fazer.")
    return(cfem_final)
  }

  message("\n### ", mineral_label, " | n = ", nrow(universo), " ###")
  report_check(universo, mineral_label, "CHECK 1 (antes)", pmin_kg, pmax_kg, qa_path_checks)

  ws <- universo |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .r     = list(ws_fator_10(PESO_G, VALORtot, pmin_g, pmax_g,
                                dplyr::if_else("QTD_MINERIO" %in% names(universo),
                                               QTD_MINERIO, NA_real_))),
      fator  = .r[["fator"]],
      motivo = .r[["motivo"]]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Peso so e alterado quando ha fator valido. Nos casos nao resolvidos
      # (motivo 0/1/3) o peso ORIGINAL e preservado -- nunca inventamos numero.
      PESO_G_ws  = dplyr::if_else(!is.na(fator), PESO_G * fator, PESO_G),
      PESO_KG_ws = PESO_G_ws / 1000,
      preco_g_ws = dplyr::if_else(!is.na(PESO_G_ws) & PESO_G_ws > 0,
                                  VALORtot / PESO_G_ws, NA_real_),
      corr_ws = dplyr::case_when(
        motivo == 3                ~ "sem_quantidade_declarada",
        motivo %in% c(0, 1)        ~ "dado_corrompido",
        !is.na(fator) & fator == 1 ~ "original",
        TRUE                       ~ paste0("pow10_p", round(log10(fator)))
      )
    ) |>
    dplyr::select(row_id, PESO_G_ws, PESO_KG_ws, preco_g_ws, corr_ws)

  cfem_final <- cfem_final |>
    dplyr::left_join(ws, by = "row_id") |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_ws),  PESO_G_ws,  PESO_G_final),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_ws), PESO_KG_ws, PESO_KG_final),
      preco_g_final = dplyr::if_else(!is.na(preco_g_ws), preco_g_ws, preco_g_final),
      corr          = dplyr::if_else(!is.na(corr_ws),    corr_ws,    corr)
    ) |>
    dplyr::select(-PESO_G_ws, -PESO_KG_ws, -preco_g_ws, -corr_ws)

  univ_pos <- cfem_final |>
    dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CASS, filtro_ano(ANO))

  report_check(univ_pos, mineral_label, "CHECK 2 (final)", pmin_kg, pmax_kg, qa_path_checks)

  dist_corr <- univ_pos |> dplyr::count(corr, sort = TRUE)
  message("[", mineral_label, "] distribuicao final de 'corr':")
  print(dist_corr)
  readr::write_csv(dist_corr |> dplyr::mutate(mineral = mineral_label, .before = 1),
                   file.path(QA_DIR, paste0("cfem_distribuicao_corr_", tolower(mineral_label), ".csv")))

  nao_corrigidos <- univ_pos |>
    dplyr::filter(corr %in% c("dado_corrompido", "sem_quantidade_declarada"))
  if (nrow(nao_corrigidos) > 0) {
    message("[", mineral_label, "] nao corrigidos: ", nrow(nao_corrigidos),
            " de ", nrow(univ_pos),
            " (", round(100 * nrow(nao_corrigidos) / nrow(univ_pos), 1), "%)")
    print(dplyr::count(nao_corrigidos, corr))
    readr::write_csv(
      nao_corrigidos |>
        dplyr::select(dplyr::any_of(c("PROCESSO", "FASE", "ANO", "MES", "NOME_arr",
                                      "TITULAR", "UM", "QTD_MINERIO", "PESO_KG",
                                      "VALORarr", "VALORtot", "corr"))) |>
        dplyr::mutate(mineral = mineral_label, .before = 1),
      file.path(QA_DIR, paste0("cfem_nao_corrigidos_", tolower(mineral_label), ".csv")))
  }

  cfem_final
}

# --- Subset A: 2018+ | faixa [30, 300] R$/kg --------------------------------
# Respaldo: 51%-94% das declaracoes ja caem nesta faixa SEM correcao alguma.
# Compativel com referencias externas de 2024 (Sec. Fazenda RO R$107,55/kg;
# IBAMA ~R$115/kg) e com a serie observada (mediana 41 em 2019 -> 150 em 2026).
cfem_final <- corrige_cassiterita_ws(
  cfem_final, pmin_kg = 30, pmax_kg = 300,
  subset_label = "2018MAIS",
  filtro_ano   = function(a) a >= ANO_CORTE_CASS
)

# --- Subset B: ate 2017 | faixa [5, 50] R$/kg -------------------------------
# ACHADO CENTRAL (auditoria 2026-07-16): a maior parte do dado pre-2018 NAO
# esta errada -- a faixa [30,300] e que nao se aplica ao periodo. Aplicar
# [30,300] aqui REPROVAVA dado correto e fabricava correcao (o teste anterior
# produziu mediana 147 R$/kg no periodo, MAIOR que a de 2018+, absurdo).
#
# Evidencia (886559/2004 / COOGER, 2006, linha a linha): 26 de 35 declaracoes
# dao R$8,69-11,0/kg -- dispersao de 25%, coesa. UM = "T" e a conversao T->kg
# esta correta (QTD_MINERIO 19,3 -> PESO_KG 19.300). Esses pesos sao reais.
#
# Faixa [5,50] derivada da serie modal observada por ano (cluster dominante,
# nao contaminado):
#     2006: ~9,0   | 2007: ~15,1 | 2008: ~17,3 | 2009: ~13,8
#     2010: ~17,0  | 2013: ~25,8 | 2014: ~32,6 | 2017: ~33,4
#   Range real observado: 8,69 a 37,2 R$/kg -> [5,50] envelopa com folga.
#
# LIMITACAO A DOCUMENTAR: a faixa sai dos proprios dados (nao ha serie
# historica publica de preco de cassiterita). O que a sustenta e (i) a coesao
# intra-ano dos precos (dispersao 4%-25%), (ii) a suavidade da curva 2006-2026,
# e (iii) a base amostral pequena (145 decl., 13 processos, 1-6 por ano) ter
# sido auditada caso a caso -- nao e estimativa cega.
cfem_final <- corrige_cassiterita_ws(
  cfem_final, pmin_kg = 5, pmax_kg = 50,
  subset_label = "ATE2017",
  filtro_ano   = function(a) a < ANO_CORTE_CASS
)

# OURO (30.000-1.000.000 R$/kg)
cfem_final <- corrige_mineral_3checks(cfem_final, "OURO", subs_keep = "OURO", subs_col = "SUBSarrSIM",
                                      pmin_kg = 30 * 1000, pmax_kg = 1000 * 1000, min_med_plaus = 30, max_med_plaus = 1000)

# cfem_correcao_extrema -- sinaliza correcao de peso incerta/agressiva.
#   OURO (metodo original): 'simples_*' = caiu no fallback sem ancora de
#     mediana confiavel. Criterio PRESERVADO, inalterado.
#   CASSITERITA (white solder): nao existe 'simples_*'. Marca-se aqui:
#     - correcao de 3+ ordens de grandeza (pow10_p-3 ou maior em modulo), e
#     - dado_corrompido (nenhum fator de 10 reconcilia com a faixa).
cfem_final <- cfem_final |>
  dplyr::mutate(
    cfem_correcao_extrema = dplyr::case_when(
      is.na(corr)                                     ~ 0L,
      stringr::str_starts(corr, "simples_")           ~ 1L,
      corr == "dado_corrompido"                       ~ 1L,
      corr == "sem_quantidade_declarada"              ~ 1L,
      stringr::str_detect(corr, "^pow10_p-?([3-9]|[1-9][0-9]+)$") ~ 1L,
      TRUE                                            ~ 0L
    )
  )

n_extrema <- sum(cfem_final$cfem_correcao_extrema, na.rm = TRUE)
message(sprintf("[CFEM] registros marcados como correcao extrema (cfem_correcao_extrema=1): %d", n_extrema))

cfem_final <- cfem_final |>
  dplyr::mutate(
    ULT_EV_ID  = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT = as.Date(stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"), format = "%d/%m/%Y"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  ) |>
  dplyr::select(-dplyr::any_of("row_id"))

pma_muni <- as.data.frame(load_ckpt("05_pma_munic")) |> dplyr::select(PROCESSO, munic_pma = munic, uf_pma = uf)

cfem_final <- cfem_final |>
  dplyr::left_join(pma_muni, by = "PROCESSO") |>
  dplyr::mutate(
    muni_falta = is.na(name_muni) | name_muni == "" | code_muni == 0 | code_muni == "0",
    preencheu_pma = muni_falta & !is.na(munic_pma),
    name_muni    = dplyr::if_else(preencheu_pma, munic_pma, name_muni),
    abbrev_state = dplyr::if_else(preencheu_pma & (is.na(abbrev_state) | abbrev_state == ""), uf_pma, abbrev_state),
    muni_fonte_cfem = dplyr::if_else(preencheu_pma, "herdado_pma", "cfem_original")
  ) |>
  dplyr::select(-munic_pma, -uf_pma, -muni_falta, -preencheu_pma)

save_ckpt(cfem_final,    "05_cfem_final")
save_ckpt(cfem_aut_amzl, "05_cfem_aut_amzl")






# ---- CHECKS ----
# Cassiterita
cass_check <- cfem_final |>
  dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CORR)
med_dbg <- compute_median_hierarchical(
  cass_check, preco_col = "preco_g_orig",
  min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
  min_ano = min_grp_ano, min_global = min_grp_global,
  max_med_plaus = 0.30, min_med_plaus = 0.03
)
table(med_dbg$level, useNA = "always")

cfem_final |>
  dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CORR,
                PESO_KG_final > 0, VALORtot > 0) |>
  dplyr::mutate(rs_kg = VALORtot / PESO_KG_final) |>
  dplyr::summarise(
    n = dplyr::n(),
    min = min(rs_kg), p25 = quantile(rs_kg, .25), mediana = median(rs_kg),
    p75 = quantile(rs_kg, .75), max = max(rs_kg)
  )

# Ouro
ouro_check <- cfem_final |>
  dplyr::filter(SUBSarrSIM == "OURO", FASE %in% FASES_CORR)
med_dbg_ouro <- compute_median_hierarchical(
  ouro_check, preco_col = "preco_g_orig",
  min_muni = min_grp_muni, min_state = min_grp_state, min_month = min_grp_month,
  min_ano = min_grp_ano, min_global = min_grp_global,
  max_med_plaus = 1000, min_med_plaus = 30
)
table(med_dbg_ouro$level, useNA = "always")

cfem_final |>
  dplyr::filter(SUBSarrSIM == "OURO", FASE %in% FASES_CORR,
                PESO_KG_final > 0, VALORtot > 0) |>
  dplyr::mutate(rs_kg = VALORtot / PESO_KG_final) |>
  dplyr::summarise(
    n = dplyr::n(),
    min = min(rs_kg), p25 = quantile(rs_kg, .25), mediana = median(rs_kg),
    p75 = quantile(rs_kg, .75), max = max(rs_kg)
  )

# VISUALIZAÇÃO 1 ==============================================================
df_temporal_unificado <- cfem_final |>
  dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
  dplyr::filter(str_detect(toupper(FASE), "GARIMPEIRA")) |> 
  dplyr::mutate(
    substancia_plot = factor(dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"), 
                             levels = c("CASSITERITA", "OURO")),
    data = as.Date(sprintf("%04d-%02d-01", ANO, MES))
  ) |>
  dplyr::group_by(substancia_plot, data) |>
  dplyr::summarise(
    `1. Valor Arrecadado (R$)_Orig`   = sum(VALORarr, na.rm = TRUE),
    `1. Valor Arrecadado (R$)_Corr`   = sum(VALORarr, na.rm = TRUE),
    `2. Peso Declarado (Kg)_Orig`     = sum(PESO_KG, na.rm = TRUE),
    `2. Peso Declarado (Kg)_Corr`     = sum(PESO_KG_final, na.rm = TRUE),
    `3. Relação (R$/Kg)_Orig` = dplyr::if_else(sum(PESO_KG, na.rm = TRUE) > 0, 
                                               sum(VALORtot, na.rm = TRUE) / sum(PESO_KG, na.rm = TRUE), 
                                               NA_real_),
    `3. Relação (R$/Kg)_Corr` = dplyr::if_else(sum(PESO_KG_final, na.rm = TRUE) > 0, 
                                               sum(VALORtot, na.rm = TRUE) / sum(PESO_KG_final, na.rm = TRUE), 
                                               NA_real_),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = -c(substancia_plot, data),
    names_to = c("metrica", "cenario"),
    names_pattern = "(.*)_(Orig|Corr)",
    values_to = "valor_metrica"
  ) |>
  dplyr::mutate(
    cenario = dplyr::if_else(cenario == "Orig", "Antes (Original)", "Depois (pow10)"),
    label_cor = dplyr::case_when(
      str_detect(metrica, "Valor") ~ "Valor Arrecadado (Inalterado)",
      str_detect(metrica, "Peso") & cenario == "Antes (Original)" ~ "Peso Original",
      str_detect(metrica, "Peso") & cenario == "Depois (pow10)" ~ "Peso Depois (pow10)",
      str_detect(metrica, "Relação") & cenario == "Antes (Original)" ~ "Relação R$/kg (original)",
      str_detect(metrica, "Relação") & cenario == "Depois (pow10)" ~ "Relação R$/kg (depois)"
    )
  ) |>
  dplyr::filter(!is.na(valor_metrica) & valor_metrica > 0)

cores_customizadas <- c(
  "Valor Arrecadado (Inalterado)" = "#1e3799",
  "Peso Original"                 = "#e74c3c",
  "Peso Depois (pow10)"           = "#27ae60",
  "Relação R$/kg (original)"      = "#e74c3c",
  "Relação R$/kg (depois)"        = "#8e44ad" 
)

p_linhas <- ggplot(df_temporal_unificado, aes(x = data, y = valor_metrica, 
                                          color = label_cor, 
                                          linetype = cenario, 
                                          alpha = cenario)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.4) +
  scale_y_log10(labels = scales::label_comma()) +
  facet_grid(metrica ~ substancia_plot, scales = "free_y") +
  scale_color_manual(values = cores_customizadas) + 
  scale_linetype_manual(values = c("Antes (Original)" = "dashed", "Depois (pow10)" = "solid")) + 
  scale_alpha_manual(values = c("Antes (Original)" = 0.4, "Depois (pow10)" = 1.0)) +            
  theme_bw() +
  labs(x = "", y = "Valores em Escala Log10", color = "") +
  guides(color = guide_legend(title = NULL, nrow = 1), linetype = "none", alpha = "none") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# [EXPORTAÇÃO 1]
ggsave(filename = file.path(QA_DIR, "01_serie_temporal_unificada.png"), 
       plot = p_linhas, width = 11, height = 8.5, dpi = 300)

# VISUALIZAÇÃO 2 ==============================================================
df_scatterplot_clean <- cfem_final |>
  dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
  dplyr::filter(str_detect(toupper(FASE), "GARIMPEIRA")) |>
  dplyr::mutate(
    substancia_plot = factor(dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"), 
                             levels = c("CASSITERITA", "OURO")),
    data = as.Date(sprintf("%04d-%02d-01", ANO, MES)),
    status_ponto = dplyr::if_else(corr == "original", "Dado Original Correto", "Corrigido pelo Algoritmo (pow10)")
  ) |>
  dplyr::filter(!is.na(preco_g_orig) & !is.na(preco_g_final) & preco_g_orig > 0 & preco_g_final > 0) |>
  tidyr::pivot_longer(
    cols = c(preco_g_orig, preco_g_final),
    names_to = "cenario",
    values_to = "preco_individual"
  ) |>
  dplyr::mutate(
    cenario = factor(dplyr::if_else(cenario == "preco_g_orig", "Antes (Original)", "Depois (pow10)"),
                     levels = c("Antes (Original)", "Depois (pow10)"))
  )

cores_scatterplot <- c(
  "Dado Original Correto"           = "#34495e",
  "Corrigido pelo Algoritmo (pow10)" = "#e74c3c"
)

p_scatter <- ggplot(df_scatterplot_clean, aes(x = data, y = preco_individual, color = status_ponto)) +
  geom_point(alpha = 0.4, size = 1.0) +
  scale_y_log10(labels = scales::label_comma(suffix = " R$/g")) +
  facet_grid(substancia_plot ~ cenario, scales = "free_y") +
  scale_color_manual(values = cores_scatterplot) +
  theme_bw() +
  labs(x = "", y = "R$/kg (Escala Log10)", color = "") +
  guides(color = guide_legend(title = NULL, nrow = 1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )
# [EXPORTAÇÃO 2]
ggsave(filename = file.path(QA_DIR, "02_scatterplot_precos.png"), 
       plot = p_scatter, width = 11, height = 7, dpi = 300)


# VISUALIZAÇÃO 3 ==============================================================
prep_violino <- function(df) {
  df |>
    filter(PESO_KG > 0, PESO_KG_final > 0, VALORtot > 0, FASE %in% FASES_CORR) |>
    mutate(
      `Antes`  = VALORtot / PESO_KG,
      `Depois` = VALORtot / PESO_KG_final
    ) |>
    pivot_longer(c(Antes, Depois), names_to = "cenario", values_to = "rs_kg") |>
    mutate(cenario = factor(cenario, levels = c("Antes", "Depois")))
}

faixa_ref <- function(pmin, pmax) {
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = pmin, ymax = pmax,
           alpha = 0.08, fill = "forestgreen")
}

p_violino_cassiterita <- prep_violino(cfem_final |> filter(SUBSarr == "CASSITERITA")) |>
  ggplot(aes(cenario, rs_kg, fill = cenario)) +
  faixa_ref(30, 300) +
  geom_violin(scale = "width", alpha = 0.8, color = NA) +
  geom_boxplot(width = 0.12, outlier.size = 0.4, alpha = 0.6) +
  facet_wrap(~ FASE, scales = "free_x", nrow = 1) + 
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("Antes" = "#e74c3c", "Depois" = "#2D6A4F")) +
  labs(#title = "Cassiterita — preço implícito (R$/kg) por fase, antes e depois da correção",
       x = NULL, y = "R$/kg (log)", fill = NULL) +
  theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))

# [EXPORTAÇÃO 3]
ggsave(filename = file.path(QA_DIR, "03_violino_cassiterita.png"), 
       plot = p_violino_cassiterita, width = 12, height = 5, dpi = 300)

# VISUALIZAÇÃO 4 ==============================================================
p_violino_ouro <- prep_violino(cfem_final |> filter(SUBSarrSIM == "OURO")) |>
  ggplot(aes(cenario, rs_kg, fill = cenario)) +
  faixa_ref(30000, 1000000) +
  geom_violin(scale = "width", alpha = 0.8, color = NA) +
  geom_boxplot(width = 0.12, outlier.size = 0.4, alpha = 0.6) +
  # O 'nrow = 1' força todas as fases a ficarem lado a lado
  facet_wrap(~ FASE, scales = "free_x", nrow = 1) + 
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("Antes" = "#e74c3c", "Depois" = "#1e3799")) +
  labs(#title = "Ouro — preço implícito (R$/kg) por fase, antes e depois da correção",
       x = NULL, y = "R$/kg (log)", fill = NULL) +
  theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))

# [EXPORTAÇÃO 4]
ggsave(filename = file.path(QA_DIR, "04_violino_ouro.png"), 
       plot = p_violino_ouro, width = 12, height = 5, dpi = 300)


# =============================================================================
# BLOCO 7 — AGREGAÇÕES DA CFEM POR PROCESSO
# =============================================================================

cfem_final <- load_ckpt("05_cfem_final")
pma_tp     <- load_ckpt("05_pma_tp")

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

pma_tp <- pma_tp |>
  dplyr::mutate(
    SUBSpmaGRP = classificar_grupo(SUBS),
    ULT_EV_ID   = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT_txt = stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"),
    ULT_EV_DES  = stringr::str_trim(stringr::str_remove_all(ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT_txt))),
    ULT_EV_DAT  = as.Date(ULT_EV_DAT_txt, format = "%d/%m/%Y")
  ) |>
  dplyr::select(-ULT_EV_DAT_txt)

cfem_aut_amzl <- load_ckpt("05_cfem_aut_amzl")

aut_unique <- cfem_aut_amzl |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(cfem_aut = 1L, aut_val_T = round(sum(VALORaut, na.rm = TRUE), 2), aut_n = dplyr::n(), .groups = "drop")

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

save_ckpt(pma_full, "05_pma_full")

# =============================================================================
# BLOCO 8 — EXPORTS (result_shiny / result_gee / result_db)
# =============================================================================

pma_full      <- load_ckpt("05_pma_full")
cfem_final    <- load_ckpt("05_cfem_final")
cfem_aut_amzl <- load_ckpt("05_cfem_aut_amzl")
ti_amzl       <- load_ckpt("03_ti_amzl")
uc_amzl       <- load_ckpt("03_uc_amzl")
qui_amzl      <- load_ckpt("03_qui_amzl")

# SHINY
terra::writeVector(pma_full, file.path(RESULT_SHINY, "pma_amzl_ALLminerals_final.shp"), overwrite = TRUE)
terra::writeVector(ti_amzl,  file.path(RESULT_SHINY, "ti_amzl.shp"),  overwrite = TRUE)
terra::writeVector(uc_amzl,  file.path(RESULT_SHINY, "uc_amzl.shp"),  overwrite = TRUE)
terra::writeVector(qui_amzl, file.path(RESULT_SHINY, "qui_amzl.shp"), overwrite = TRUE)
readr::write_csv(cfem_final, file.path(RESULT_SHINY, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"))

# GEE (recorte pelo BIOMA Amazônia)
bioma_full <- terra::vect(list.files(BIOMA_DIR, pattern = "\\.shp$", full.names = TRUE)[1])
bioma <- bioma_full[bioma_full$Bioma == "Amazônia", ] |> terra::project(terra::crs(pma_full))
dentro_bioma    <- terra::is.related(pma_full, bioma, "intersects")
pma_bioma       <- pma_full[dentro_bioma, ]
processos_bioma <- unique(pma_bioma$PROCESSO)
cfem_bioma      <- cfem_final |> dplyr::filter(PROCESSO %in% processos_bioma)

terra::writeVector(pma_bioma, file.path(RESULT_GEE, "pma_AMAZONIA_ALLminerals_GEE.shp"), overwrite = TRUE)
readr::write_csv(cfem_bioma, file.path(RESULT_GEE, "cfem_AMAZONIA_ALLminerals_GEE.csv"))
cfem_bioma_mensal <- cfem_bioma |>
  dplyr::mutate(data = as.Date(sprintf("%04d-%02d-01", ANO, MES)), proc_ano = paste0(trimws(PROCESSO), "/", ANO))
readr::write_csv(cfem_bioma_mensal, file.path(RESULT_GEE, "cfem_AMAZONIA_ALLminerals_GEE_MONTHLY.csv"))

# DB (.geojson — mantido, evita truncamento de nome de coluna)
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

terra::writeVector(pma_db,      file.path(RESULT_DB, "pma_amzl_ALLminerals_final.geojson"), filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(ti_amzl,     file.path(RESULT_DB, "ti_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(uc_amzl,     file.path(RESULT_DB, "uc_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(qui_amzl,    file.path(RESULT_DB, "qui_amzl.geojson"), filetype = "GeoJSON", overwrite = TRUE)
readr::write_csv(cfem_final,    file.path(RESULT_DB, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"))
readr::write_csv(cfem_aut_amzl, file.path(RESULT_DB, "cfem_aut_all_min_amzl.csv"))

message("\n=== 05_integracao_final.R — CONCLUÍDO ===")