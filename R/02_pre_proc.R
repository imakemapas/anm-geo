###############################################################################
# 02_pre_proc.R - Processing, cleaning, and consolidation
###############################################################################

# Setup & Configuration -----------------------------------------------------
rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(stringi)
  library(here)
})

Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("GDAL_DATA")

source(here::here("R", "utils.R"))

# Paths and parameters
ROOT         <- here::here()
RAW_DIR      <- here::here("data", "raw_data")
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
TMP_DIR      <- here::here("data", "tmp")
QA_DIR       <- here::here("data", "_qa", "02_pre_proc")

dir.create(PRE_PROC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TMP_DIR,      recursive = TRUE, showWarnings = FALSE)
dir.create(QA_DIR,       recursive = TRUE, showWarnings = FALSE)

KEYWORDS <- c(
  "GARIMP",                  # GARIMPO, GARIMPEIRO/A, GARIMPAGEM, GARIMPAR...
  "MINER",                   # MINERAL(IS), MINERARIO/A, MINERIO, MINERACAO, MINERADORA...
  "AUR[IÍ]FER[OA]",          # AURIFERO/A, AURÍFERO/A
  "CASS[IE]TERITA",          # CASSITERITA, CASSETERITA (grafia alternativa)
  "MERC[UÚ]RIO",             # MERCURIO, MERCÚRIO
  "ASSORE",                  # ASSOREAMENTO, ASSOREAR, ASSOREADO/A...
  "LEITO",
  "LAVRA",
  "BARRAGE(M|NS)",           # BARRAGEM, BARRAGENS
  "OURO",
  "DIAMANTE",
  "DIAMANT[IÍ]FER[OA]"       # DIAMANTIFERO/A, DIAMANTÍFERO/A
)
REGEX <- paste(KEYWORDS, collapse = "|")

etl_log <- list()

# ANM SCM -----------------------------------------------------
safe_step("ANM SCM consolidation", {
  scm_dir <- file.path(RAW_DIR, "anm_scm")
  scm_files <- list.files(scm_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(scm_files) == 0) stop("No SCM CSV files found in: ", scm_dir)

  cadastro_mineiro <- purrr::map_df(scm_files, \(f) {
    doc_type <- stringr::str_remove(basename(f), "\\.csv$")

    raw <- readr::read_file_raw(f)
    txt <- stringi::stri_encode(raw, from = "ISO-8859-1", to = "UTF-8")

    df <- readr::read_delim(
      txt,
      delim = ",",
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    )

    names(df) <- stringr::str_replace_all(names(df), "CPF CNPJ do titular", "CPF/CNPJ do titular")
    names(df) <- stringr::str_replace_all(names(df), "^Substancia$", "Substância(s)")

    df |> dplyr::mutate(tipo_documento = doc_type)
  })

  readr::write_excel_csv(
    cadastro_mineiro,
    file.path(PRE_PROC_DIR, "cadastro_mineiro.csv")
  )

  rm(cadastro_mineiro)
  gc()
})

safe_step("CFEM smart parsing (ANM arrecadacao)", {
  cfem_dir <- file.path(RAW_DIR, "anm_arrecadacao")
  files <- c("CFEM_Arrecadacao.csv", "CFEM_Autuacao.csv", "CFEM_Distribuicao.csv", "Tah.csv")
  paths <- file.path(cfem_dir, files)
  names(paths) <- files

  cfem_cols_valor <- list(
    "CFEM_Arrecadacao.csv"  = c("QuantidadeComercializada", "ValorRecolhido"),
    "CFEM_Autuacao.csv"     = c("Valor"),
    "CFEM_Distribuicao.csv" = c("Valor")
  )

  if (!dir.exists(cfem_dir)) stop("Missing directory: ", cfem_dir)

  purrr::iwalk(paths, \(p, nm) {
    if (!file.exists(p)) {
      message("CFEM/Tah missing: ", nm, " | Skipping.")
      return(invisible(NULL))
    }

    df <- cfem_smart_read(
      p, 
      enc = "ISO-8859-1",
      decimal_score_cols = cfem_cols_valor[[nm]],
      log_dir = QA_DIR
    )

    out <- file.path(PRE_PROC_DIR, paste0(tools::file_path_sans_ext(nm), ".csv"))
    readr::write_csv(df, out)

    message("Saved: ", basename(out))
  })
})

# SCM microdados & SIGMINE PMA ----------------------------------------------------
safe_step("ANM SCM microdados (unzip)", {
  micro_dir <- file.path(RAW_DIR, "anm_microdados")
  zip_path  <- file.path(micro_dir, "microdados-scm.zip")

  if (!file.exists(zip_path)) {
    message("Microdados zip not found. Skipping.")
    return(invisible(NULL))
  }

  ok <- safe_unzip(zip_path, micro_dir)

  txt_extraidos <- list.files(micro_dir, pattern = "\\.txt$")
  if (length(txt_extraidos) == 0) {
    message("[AVISO] microdados-scm.zip nao pode ser descompactado.")
  } else {
    message("Microdados SCM extracted: ", length(txt_extraidos), " arquivo(s) .txt.")
  }
})

safe_step("SIGMINE PMA extraction", {
  zip_path <- file.path(RAW_DIR, "anm_espacial", "BRASIL.zip")
  if (!file.exists(zip_path)) {
    message("BRASIL.zip not found. Skipping.")
    return(invisible(NULL))
  }

  exdir <- file.path(TMP_DIR, "pma_extraction")
  if (!safe_unzip(zip_path, exdir)) stop("Failed to unzip: ", zip_path)

  shp_pma <- first_match(exdir, "\\.shp$")
  if (is.na(shp_pma)) stop("No shapefile found in BRASIL.zip extraction.")

  pma <- terra::vect(shp_pma) |> clean_geometry(label = "SIGMINE PMA")

  terra::writeVector(pma, file.path(PRE_PROC_DIR, "sigmine_pma.shp"), overwrite = TRUE)
})

# FUNAI TI / MMA CNUC / INCRA) ------------------------------
safe_step("FUNAI Indigenous Lands (TI)", {
  zip_path <- file.path(RAW_DIR, "geo_territorios", "tis_poligonais.zip")
  exdir    <- file.path(TMP_DIR, "funai_ti")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "tis_poligonais.*\\.shp$")
  if (is.na(shp_path)) stop("No FUNAI TI shapefile found after unzip.")

  ti <- terra::vect(shp_path) |> clean_geometry(label = "FUNAI TI")

  if ("terrai_nom" %in% names(ti)) {
    ti$terrai_nom <- stringi::stri_encode(ti$terrai_nom, from = "Windows-1252", to = "UTF-8")
    ti$terrai_nom <- toupper(ti$terrai_nom)
  }

  cols_keep <- c("modalidade", "fase_ti", "terrai_nom", "etnia_nome")
  cols_keep <- cols_keep[cols_keep %in% names(ti)]

  ti <- ti[, cols_keep]

  terra::writeVector(ti, file.path(PRE_PROC_DIR, "terras_indigenas.shp"), overwrite = TRUE)
})

safe_step("MMA CNUC protected areas (UC)", {
  zip_path <- file.path(RAW_DIR, "geo_territorios", "shp_cnuc.zip")
  exdir    <- file.path(TMP_DIR, "mma_cnuc")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) stop("No CNUC shapefile found after unzip.")

  uc <- terra::vect(shp_path) |> clean_geometry(label = "MMA CNUC")

  # --- SNUC category mapping --------------------------------------------------
  map_snuc <- c(
    "Área de Proteção Ambiental"               = "APA",
    "Área de Relevante Interesse Ecológico"    = "ARIE",
    "Estação Ecológica"                        = "ESEC",
    "Floresta"                                 = "FLO",
    "Monumento Natural"                        = "MONA",
    "Parque"                                   = "PARQUE",
    "Refúgio de Vida Silvestre"                = "REVIS",
    "Reserva Biológica"                        = "REBIO",
    "Reserva de Desenvolvimento Sustentável"   = "RDS",
    "Reserva de Fauna"                         = "REFAU",
    "Reserva Extrativista"                     = "RESEX",
    "Reserva Particular do Patrimônio Natural" = "RPPN"
  )

  if (!("categoria" %in% names(uc))) {
    warning("Field 'categoria' not found. Skipping sigla_snuc mapping.")
    uc$sigla_snuc <- "OUTRO"
  } else {
    cat_clean <- trimws(as.character(uc$categoria))
    uc$sigla_snuc <- "OUTRO"

    idx <- match(cat_clean, names(map_snuc))
    hit <- !is.na(idx)
    uc$sigla_snuc[hit] <- unname(map_snuc[idx[hit]])
  }

  cols_keep <- c("nome_uc", "pl_manejo", "grupo", "categoria", "org_gestor", "sigla_snuc")
  cols_keep <- cols_keep[cols_keep %in% names(uc)]

  uc <- uc[, cols_keep]

  terra::writeVector(uc, file.path(PRE_PROC_DIR, "unidades_conservacao.shp"), overwrite = TRUE)
})

safe_step("INCRA Quilombolas", {
  territorios_dir <- file.path(RAW_DIR, "geo_territorios")
  exdir <- file.path(TMP_DIR, "incra_quilombolas")
  zip_candidates <- list.files(
    territorios_dir, pattern = "quilombola", ignore.case = TRUE, full.names = TRUE
  )

  shp_path <- NA_character_

  # 1) tenta o ZIP (primeiro candidato encontrado)
  if (length(zip_candidates) > 0) {
    if (safe_unzip(zip_candidates[1], exdir)) {
      shp_path <- first_match(exdir, "\\.shp$")
    }
  }

  # 2) fallback: shapefile ja extraido manualmente em geo_territorios
  if (is.na(shp_path)) {
    shp_local <- list.files(
      territorios_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
    )
    if (length(shp_local) > 0) shp_path <- shp_local[1]
  }

  # 3) se ainda nao achou, pula de verdade
  if (is.na(shp_path)) {
    message("INCRA: nenhum .shp encontrado (ZIP nem local) em ", territorios_dir, ". Skipping.")
  } else {
    message("Quilombolas source: ", shp_path)
    qui <- terra::vect(shp_path) |> clean_geometry(label = "INCRA Quilombolas")
    terra::writeVector(qui, file.path(PRE_PROC_DIR, "quilombolas.shp"), overwrite = TRUE)
    message("Quilombolas saved.")
  }
})

# IBAMA / ICMBio / SEMA-MT ----------------------
safe_step("IBAMA embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_ibama", "adm_embargos_ibama_a.zip")
  exdir    <- file.path(TMP_DIR, "ibama_embargos")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "embargo.*\\.shp$")
  if (is.na(shp_path)) stop("No embargo shapefile found after unzip.")

  EMBib <- terra::vect(shp_path) |> clean_geometry(label = "IBAMA embargos")
  EMBib <- EMBib[, c("nome_embar", "cpf_cnpj_e", "ordem_fisc", "num_proces",
                     "des_tad", "des_infrac", "dat_embarg", "dat_ult_al")]

  EMBib$des_tad    <- to_upper_utf8(EMBib$des_tad)
  EMBib$des_infrac <- to_upper_utf8(EMBib$des_infrac)

  EMBib <- aplicar_filtro_palavras_chave(
    EMBib, campos = c("des_tad", "des_infrac"), regex = REGEX,
    label = "ibama_embargos", export_dir = QA_DIR
  )

  terra::writeVector(EMBib, file.path(PRE_PROC_DIR, "ibama_embargos.shp"), overwrite = TRUE)
})

safe_step("IBAMA infractions (shp)", {

  zip_path <- file.path(RAW_DIR, "geo_ibama", "ibama_autos_de_infracao_p.zip")
  exdir    <- file.path(TMP_DIR, "ibama_infracoes")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) stop("No IBAMA infractions shapefile found after unzip.")

  INFib <- terra::vect(shp_path)

  n0 <- length(INFib)
  INFib <- terra::makeValid(INFib)
  INFib <- terra::disagg(INFib)
  INFib <- terra::project(INFib, "EPSG:4326")
  message(sprintf("[IBAMA infracoes] pontos | inicial: %d | final: %d", n0, length(INFib)))

  message("[ibama_infracoes] Campos disponiveis: ", paste(names(INFib), collapse = ", "))
  message("[ibama_infracoes] AVISO: filtro por palavra-chave e selecao de colunas ",
          "ainda NAO aplicados -- confirmar nomes de campo (equivalentes a ",
          "DES_AUTO_INFRACAO/CPF_CNPJ_INFRATOR na fonte antiga) antes de usar em producao.")

  if (length(INFib) == 0) {
    message("[IBAMA infracoes] AVISO: 0 registros apos limpeza -- shapefile nao sera gravado.")
  } else {
    terra::writeVector(INFib, file.path(PRE_PROC_DIR, "ibama_infracoes.shp"), overwrite = TRUE)
  }
})

safe_step("ICMBio embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_icmbio", "embargos_icmbio.zip")
  exdir    <- file.path(TMP_DIR, "icmbio_embargos")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "embargos_icmbio\\.shp$")
  if (is.na(shp_path)) stop("No ICMBio embargos shapefile found after unzip.")

  EMBic <- terra::vect(shp_path) |> clean_geometry(label = "ICMBio embargos")
  EMBic <- EMBic[, c("cpf_cnpj", "autuado", "desc_infra", "desc_sanc", "processo", "data", "ano")]

  EMBic$desc_infra <- to_upper_utf8(EMBic$desc_infra)
  EMBic$desc_sanc  <- to_upper_utf8(EMBic$desc_sanc)

  EMBic <- aplicar_filtro_palavras_chave(
    EMBic, campos = c("desc_infra", "desc_sanc"), regex = REGEX,
    label = "icmbio_embargos", export_dir = QA_DIR
  )

  terra::writeVector(EMBic, file.path(PRE_PROC_DIR, "icmbio_embargos.shp"), overwrite = TRUE)
})

safe_step("ICMBio infractions (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_icmbio", "autos_infracao_shp.zip")
  exdir    <- file.path(TMP_DIR, "icmbio_infracoes")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "autos_infracao_icmbio\\.shp$")
  if (is.na(shp_path)) stop("No ICMBio infractions shapefile found after unzip.")

  INFic <- terra::vect(shp_path)
  INFic <- INFic[, c("tipo", "valor_mult", "embargo", "apreensao", "autuado", "cpf_cnpj",
                     "desc_ai", "desc_sanc", "data", "ano", "tipo_infra", "municipio", "uf", "processo")]

  INFic$desc_ai    <- to_upper_utf8(INFic$desc_ai)
  INFic$tipo_infra <- to_upper_utf8(INFic$tipo_infra)

  INFic <- aplicar_filtro_palavras_chave(
    INFic, campos = c("desc_ai", "tipo_infra"), regex = REGEX,
    label = "icmbio_infracoes", export_dir = QA_DIR
  )

  terra::writeVector(INFic, file.path(PRE_PROC_DIR, "icmbio_infracoes.shp"), overwrite = TRUE)
})

safe_step("SEMA-MT embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_sema_mt", "AREAS_EMBARGADAS_SEMA.zip")
  exdir    <- file.path(TMP_DIR, "sema_mt_embargos")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "AREAS_EMBARGADAS_SEMA\\.shp$")
  if (is.na(shp_path)) stop("No SEMA-MT embargos shapefile found after unzip.")

  EMBmt <- terra::vect(shp_path) |> clean_geometry(label = "SEMA-MT embargos")
  EMBmt <- EMBmt[, c("NOME", "CPF_CNPJ", "DANO", "ANO_DESMAT", "DAT_LAVRAT", "N_PROCESSO")]

  EMBmt$DANO <- to_upper_utf8(EMBmt$DANO)
  EMBmt$NOME <- to_upper_utf8(EMBmt$NOME)

  EMBmt <- aplicar_filtro_palavras_chave(
    EMBmt, campos = "DANO", regex = REGEX,
    label = "sema_mt_embargos", export_dir = QA_DIR
  )

  terra::writeVector(EMBmt, file.path(PRE_PROC_DIR, "sema_mt_embargos.shp"), overwrite = TRUE)
})

safe_step("SEMA-MT SIGA embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_sema_mt", "AREA_EMBARGADA_SIGA_POLIGONO.zip")
  exdir    <- file.path(TMP_DIR, "sema_mt_embargos_siga")

  if (!safe_unzip(zip_path, exdir)) {
    message("SIGA embargos zip missing/unreadable. Skipping.")
    return(invisible(NULL))
  }

  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) {
    message("SIGA embargos extracted but no .shp found. Skipping.")
    return(invisible(NULL))
  }

  x <- terra::vect(shp_path) |> clean_geometry(label = "SEMA-MT SIGA embargos")

  txt_cols <- intersect(c("NOME_RAZAO", "NOME_FANTA", "TIPO", "SUBTIPO", "DISPOSITIV",
                          "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(txt_cols)) {
    vals <- terra::values(x)
    vals[txt_cols] <- lapply(vals[txt_cols], to_upper_utf8)
    terra::values(x) <- vals
    rm(vals); gc()
  }

  fcols <- intersect(c("SUBTIPO", "DISPOSITIV", "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(fcols)) {
    x <- aplicar_filtro_palavras_chave(
      x, campos = fcols, regex = REGEX,
      label = "sema_mt_embargos_siga", export_dir = QA_DIR
    )
  } else {
    message("SIGA embargos: no filterable text fields found; saving full layer.")
  }

  terra::writeVector(x, file.path(PRE_PROC_DIR, "sema_mt_embargos_siga.shp"), overwrite = TRUE)
})

safe_step("SEMA-MT SIGA infractions (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_sema_mt", "AUTOS_DE_INFRACAO_SIGA_POLIGONO.zip")
  exdir    <- file.path(TMP_DIR, "sema_mt_infracoes_siga")

  if (!safe_unzip(zip_path, exdir)) {
    message("SIGA infractions zip missing/unreadable. Skipping.")
    return(invisible(NULL))
  }

  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) {
    message("SIGA infractions extracted but no .shp found. Skipping.")
    return(invisible(NULL))
  }

  x <- terra::vect(shp_path) |> clean_geometry(label = "SEMA-MT SIGA infracoes")

  txt_cols <- intersect(c("NOME_RAZAO", "NOME_FANTA", "TIPO", "SUBTIPO", "DISPOSITIV",
                          "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(txt_cols)) {
    vals <- terra::values(x)
    vals[txt_cols] <- lapply(vals[txt_cols], to_upper_utf8)
    terra::values(x) <- vals
    rm(vals); gc()
  }

  fcols <- intersect(c("SUBTIPO", "DISPOSITIV", "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(fcols)) {
    x <- aplicar_filtro_palavras_chave(
      x, campos = fcols, regex = REGEX,
      label = "sema_mt_infracoes_siga", export_dir = QA_DIR
    )
  } else {
    message("SIGA infractions: no filterable text fields found; saving full layer.")
  }

  terra::writeVector(x, file.path(PRE_PROC_DIR, "sema_mt_infracoes_siga.shp"), overwrite = TRUE)
})

# Cleanup & Finish --------------------------------------
summary_df <- purrr::imap_dfr(etl_log, ~{
  data.frame(
    Task = .y,
    Status = if (.x$success) "SUCCESS" else "FAILED",
    Details = if (is.na(.x$error_msg)) "Completed" else .x$error_msg
  )
})

print(summary_df, row.names = FALSE)
message("\nProcess finished.")
message("Checks desta execução (logs de parsing CFEM e amostras não-capturadas pelo filtro de palavras-chave) em: ", QA_DIR)
unlink(TMP_DIR, recursive = TRUE, force = TRUE)

message("\n=== 02_pre_proc.R — CONCLUÍDO ===")