###############################################################################
# ETL - Processing, cleaning, and consolidation
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

# Paths and parameters
ROOT         <- here::here()
RAW_DIR      <- here::here("data", "raw_data")
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
TMP_DIR      <- here::here("data", "tmp")

dir.create(PRE_PROC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TMP_DIR,      recursive = TRUE, showWarnings = FALSE)

# Keyword filter
KEYWORDS <- c(
  "GARIMPO", "CASSITERITA", "MERCURIO", "MERCÚRIO", "ASSOREAMENTO",
  "LEITO", "MINERARIO", "MINERIO", "GARIMPEIRO", "MINERAL",
  "GARIMPEIRA", "LAVRA", "BARRAGEM", "AURIFERO", "AURÍFERO",
  "OURO", "CASSETERITA", "DIAMANTE"
)
REGEX <- paste(KEYWORDS, collapse = "|")

# Helpers -----------------------------------------------
etl_log <- list()

safe_step <- function(label, expr) {
  message("\n--- ", label, " ---")
  status <- tryCatch({
    force(expr)
    list(success = TRUE, error_msg = NA)
  }, error = function(e) {
    msg <- conditionMessage(e)
    warning(label, " failed: ", msg)
    list(success = FALSE, error_msg = msg)
  })
  etl_log[[label]] <<- status
  return(status$success)
}

safe_unzip <- function(zip_path, exdir) {
  if (!file.exists(zip_path)) return(FALSE)
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  tryCatch({ unzip(zip_path, exdir = exdir); TRUE }, error = function(e) FALSE)
}

to_upper_utf8 <- function(x) toupper(iconv(x, from = "", to = "UTF-8"))

clean_geometry <- function(v) {
  v <- terra::makeValid(v)
  v <- v[terra::expanse(v) > 0, ] 
  v <- terra::project(v, "EPSG:4326")
  return(v)
}

first_match <- function(path, pattern, ignore.case = TRUE) {
  x <- list.files(path, pattern = pattern, ignore.case = ignore.case, full.names = TRUE)
  if (length(x) == 0) NA_character_ else x[1]
}

# CFEM Parsing
guess_delim <- function(path, enc = "ISO-8859-1") {
  try_read <- function(delim) {
    suppressWarnings(
      try(
        readr::read_delim(
          path, delim = delim, n_max = 200, locale = readr::locale(encoding = enc),
          col_types = readr::cols(.default = readr::col_character())
        ),
        silent = TRUE
      )
    )
  }
  a <- try_read(";")
  b <- try_read(",")
  na <- if (inherits(a, "try-error")) 0 else ncol(a)
  nb <- if (inherits(b, "try-error")) 0 else ncol(b)
  if (na == 0 && nb == 0) stop("Não foi possível inferir o delimitador para: ", path)
  if (na >= nb) ";" else ","
}

looks_numeric_char <- function(x) {
  all(is.character(x)) &&
    mean(stringr::str_detect(x, "^-?[0-9\\.,]+$") | is.na(x)) > 0.5 &&
    mean(stringr::str_detect(x, "[0-9]") | is.na(x)) > 0.5
}

choose_decimal_mark <- function(df_char) {
  cand <- df_char |> dplyr::select(dplyr::where(looks_numeric_char))
  if (ncol(cand) == 0) return(".")
  
  score <- function(dec) {
    loc <- readr::locale(decimal_mark = dec)
    cand |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ readr::parse_number(.x, locale = loc))) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(is.na(.))), .groups = "drop") |>
      unlist() |> mean()
  }
  s_comma <- score(",")
  s_dot <- score(".")
  if (s_comma <= s_dot) "," else "."
}

parse_numeric_cols <- function(df_char, dec_mark = ",", keep_char = character()) {
  loc <- readr::locale(decimal_mark = dec_mark)
  num_cands <- names(df_char)[vapply(df_char, looks_numeric_char, logical(1))]
  num_cands <- setdiff(num_cands, keep_char)
  if (length(num_cands)) {
    df_char <- df_char |>
      dplyr::mutate(dplyr::across(dplyr::all_of(num_cands), ~ readr::parse_number(dplyr::na_if(.x, "-"),
  locale = loc)))
  }
  df_char
}

cfem_smart_read <- function(path, enc = "ISO-8859-1") {
  keep_char <- c(
    "CPF_CNPJ", "CPF", "CNPJ",
    "Processo", "AnoDoProcesso",
    "CodigoMunicipio",
    "UnidadeDeMedida", "UF", "Município", "Substância",
    "DataCriacao"
  )
  
  delim <- guess_delim(path, enc = enc)
  
  df_char <- readr::read_delim(
    path, delim = delim, locale = readr::locale(encoding = enc),
    col_types = readr::cols(.default = readr::col_character()),
    trim_ws = TRUE
  )

  names(df_char) <- trimws(names(df_char))
  
  dec_mark <- choose_decimal_mark(df_char)
  df <- parse_numeric_cols(df_char, dec_mark, keep_char = intersect(keep_char, names(df_char)))
  
  attr(df, "cfem_delim") <- delim
  attr(df, "cfem_decimal_mark") <- dec_mark
  df
}

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
  
  if (!dir.exists(cfem_dir)) stop("Missing directory: ", cfem_dir)
  
  purrr::iwalk(paths, \(p, nm) {
    if (!file.exists(p)) {
      message("CFEM/Tah missing: ", nm, " | Skipping.")
      return(invisible(NULL))
    }
    
    df <- cfem_smart_read(p, enc = "ISO-8859-1")
    
    out <- file.path(PRE_PROC_DIR, paste0(tools::file_path_sans_ext(nm), ".csv"))
    readr::write_csv(df, out)
    
    message(
      "Saved: ", basename(out),
      " | delim='", attr(df, "cfem_delim"),
      "' decimal='", attr(df, "cfem_decimal_mark"), "'"
    )
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
  if (ok) message("Microdados SCM extracted.")
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
  
  pma <- terra::vect(shp_pma) |> clean_geometry()
  
  terra::writeVector(pma, file.path(PRE_PROC_DIR, "sigmine_pma.shp"), overwrite = TRUE)
})

# FUNAI TI / MMA CNUC / INCRA) ------------------------------
safe_step("FUNAI Indigenous Lands (TI)", {
  zip_path <- file.path(RAW_DIR, "geo_funai", "tis_poligonais.zip")
  exdir    <- file.path(TMP_DIR, "funai_ti")
  
  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)
  
  shp_path <- first_match(exdir, "tis_poligonais.*\\.shp$")
  if (is.na(shp_path)) stop("No FUNAI TI shapefile found after unzip.")
  
  ti <- terra::vect(shp_path) |> clean_geometry()
  
  if ("terrai_nom" %in% names(ti)) {
    ti$terrai_nom <- stringi::stri_encode(ti$terrai_nom, from = "Windows-1252", to = "UTF-8")
    ti$terrai_nom <- toupper(ti$terrai_nom)
  }
  
  cols_keep <- c("modalidade",
                 "fase_ti",
                 "terrai_nom",
                 "etnia_nome")
  cols_keep <- cols_keep[cols_keep %in% names(ti)]
  
  ti <- ti[, cols_keep]
  
  terra::writeVector(ti, file.path(PRE_PROC_DIR, "terras_indigenas.shp"), overwrite = TRUE)
})

safe_step("MMA CNUC protected areas (UC)", {
  zip_path <- file.path(RAW_DIR, "geo_federal", "shp_cnuc_2025_08.zip")
  exdir    <- file.path(TMP_DIR, "mma_cnuc")
  
  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)
  
  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) stop("No CNUC shapefile found after unzip.")
  
  uc <- terra::vect(shp_path) |> clean_geometry()
  
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
  
  cols_keep <- c(
    "nome_uc",
    "pl_manejo",
    "grupo",
    "categoria",
    "org_gestor",
    "sigla_snuc"
  )
  
  cols_keep <- cols_keep[cols_keep %in% names(uc)]
  
  uc <- uc[, cols_keep]
  
  terra::writeVector(
    uc,
    file.path(PRE_PROC_DIR, "unidades_conservacao.shp"),
    overwrite = TRUE
  )
})

safe_step("INCRA Quilombolas", {
  zip_path <- file.path(RAW_DIR, "geo_incra", "areas_quilombolas.zip")
  exdir    <- file.path(TMP_DIR, "incra_quilombolas")

  shp_path <- NA_character_

  # 1) tenta o ZIP
  if (safe_unzip(zip_path, exdir)) {
    shp_path <- first_match(exdir, "\\.shp$")
  }

  # 2) fallback: shapefile local, procurando recursivamente nas subpastas
  if (is.na(shp_path)) {
    shp_local <- list.files(
      file.path(RAW_DIR, "geo_incra"),
      pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
    )
    if (length(shp_local) > 0) shp_path <- shp_local[1]
  }

  # 3) se ainda nao achou, pula de verdade
  if (is.na(shp_path)) {
    message("INCRA: nenhum .shp encontrado (ZIP nem local). Skipping.")
    return(invisible(NULL))
  }

  message("Quilombolas source: ", shp_path)
  qui <- terra::vect(shp_path) |> clean_geometry()
  terra::writeVector(qui, file.path(PRE_PROC_DIR, "quilombolas.shp"), overwrite = TRUE)
  message("Quilombolas saved.")
})

# IBAMA / ICMBio / SEMA-MT ----------------------
safe_step("IBAMA embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_ibama", "adm_embargos_ibama_a.zip")
  exdir    <- file.path(TMP_DIR, "ibama_embargos")
  
  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)
  
  shp_path <- first_match(exdir, "embargo.*\\.shp$")
  if (is.na(shp_path)) stop("No embargo shapefile found after unzip.")
  
  EMBib <- terra::vect(shp_path) |> clean_geometry()
  EMBib <- EMBib[, c("nome_embar","cpf_cnpj_e","ordem_fisc","num_proces",
                     "des_tad","des_infrac","dat_embarg","dat_ult_al")]
  
  EMBib$des_tad    <- to_upper_utf8(EMBib$des_tad)
  EMBib$des_infrac <- to_upper_utf8(EMBib$des_infrac)
  
  keep <- stringr::str_detect(EMBib$des_tad, REGEX) | stringr::str_detect(EMBib$des_infrac, REGEX)
  EMBib <- EMBib[keep, ]
  
  terra::writeVector(EMBib, file.path(PRE_PROC_DIR, "ibama_embargos.shp"), overwrite = TRUE)
})

safe_step("IBAMA infractions (csv)", {
  zip_path <- file.path(RAW_DIR, "geo_ibama", "auto_infracao_csv.zip")
  exdir    <- file.path(TMP_DIR, "ibama_infracoes")
  
  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)
  
  csv_files <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) stop("No CSV files found after unzip.")
  
  INFib <- purrr::map_df(csv_files, \(f) {
    ano <- stringr::str_extract(basename(f), "\\d{4}")
    ano <- suppressWarnings(as.integer(ano))
    
    df <- readr::read_delim(
      f,
      delim = ";",
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    )
    
    df$ANO <- ano
    df
  })
  
  INFib <- INFib |>
    dplyr::select(DES_AUTO_INFRACAO, DAT_HORA_AUTO_INFRACAO, NUM_PROCESSO, NOME_INFRATOR, CPF_CNPJ_INFRATOR, ANO) |>
    dplyr::filter(!is.na(CPF_CNPJ_INFRATOR))
  
  INFib$DES_AUTO_INFRACAO <- to_upper_utf8(INFib$DES_AUTO_INFRACAO)
  INFib <- dplyr::filter(INFib, stringr::str_detect(DES_AUTO_INFRACAO, REGEX))
  
  readr::write_excel_csv(INFib, file.path(PRE_PROC_DIR, "ibama_infracoes.csv"))
})

safe_step("ICMBio embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_federal", "embargos_icmbio.zip")
  exdir    <- file.path(TMP_DIR, "icmbio_embargos")
  
  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)
  
  shp_path <- first_match(exdir, "embargos_icmbio\\.shp$")
  if (is.na(shp_path)) stop("No ICMBio embargos shapefile found after unzip.")
  
  EMBic <- terra::vect(shp_path) |> clean_geometry()
  EMBic <- EMBic[, c("cpf_cnpj","autuado","desc_infra","desc_sanc","processo","data","ano")]
  
  EMBic$desc_infra <- to_upper_utf8(EMBic$desc_infra)
  EMBic$desc_sanc  <- to_upper_utf8(EMBic$desc_sanc)
  
  keep <- stringr::str_detect(EMBic$desc_infra, REGEX) | stringr::str_detect(EMBic$desc_sanc, REGEX)
  EMBic <- EMBic[keep, ]
  
  terra::writeVector(EMBic, file.path(PRE_PROC_DIR, "icmbio_embargos.shp"), overwrite = TRUE)
})

safe_step("ICMBio infractions (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_federal", "autos_infracao_icmbio.zip")
  exdir    <- file.path(TMP_DIR, "icmbio_infracoes")
  
  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)
  
  shp_path <- first_match(exdir, "autos_infracao_icmbio\\.shp$")
  if (is.na(shp_path)) stop("No ICMBio infractions shapefile found after unzip.")
  
  INFic <- terra::vect(shp_path)
  INFic <- INFic[, c("tipo","valor_mult","embargo","apreensao","autuado","cpf_cnpj",
                     "desc_ai","desc_sanc","data","ano","tipo_infra","municipio","uf","processo")]
  
  INFic$desc_ai    <- to_upper_utf8(INFic$desc_ai)
  INFic$tipo_infra <- to_upper_utf8(INFic$tipo_infra)
  
  keep <- stringr::str_detect(INFic$desc_ai, REGEX) | stringr::str_detect(INFic$tipo_infra, REGEX)
  INFic <- INFic[keep, ]
  
  terra::writeVector(INFic, file.path(PRE_PROC_DIR, "icmbio_infracoes.shp"), overwrite = TRUE)
})

safe_step("SEMA-MT embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_sema_mt", "AREAS_EMBARGADAS_SEMA.zip")
  exdir    <- file.path(TMP_DIR, "sema_mt_embargos")
  
  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)
  
  shp_path <- first_match(exdir, "AREAS_EMBARGADAS_SEMA\\.shp$")
  if (is.na(shp_path)) stop("No SEMA-MT embargos shapefile found after unzip.")
  
  EMBmt <- terra::vect(shp_path) |> clean_geometry()
  EMBmt <- EMBmt[, c("NOME","CPF_CNPJ","DANO","ANO_DESMAT","DAT_LAVRAT","N_PROCESSO")]
  
  EMBmt$DANO <- to_upper_utf8(EMBmt$DANO)
  EMBmt$NOME <- to_upper_utf8(EMBmt$NOME)
  
  EMBmt <- EMBmt[stringr::str_detect(EMBmt$DANO, REGEX), ]
  
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
  
  x <- terra::vect(shp_path) |> clean_geometry()
  
  # uppercase relevant text fields if present
  txt_cols <- intersect(c("NOME_RAZAO","NOME_FANTA","TIPO","SUBTIPO","DISPOSITIV","DESCRICAO_","ATIVIDADE","ATIVIDADE_"), names(x))
  if (length(txt_cols)) {
    vals <- terra::values(x)
    vals[txt_cols] <- lapply(vals[txt_cols], to_upper_utf8)
    terra::values(x) <- vals
    rm(vals); gc()
  }
  
  # filter by keywords across available fields
  fcols <- intersect(c("SUBTIPO","DISPOSITIV","DESCRICAO_","ATIVIDADE","ATIVIDADE_"), names(x))
  if (length(fcols)) {
    vals <- terra::values(x)

    keep <- Reduce(
      `|`,
      lapply(
        fcols,
        \(cc)
          stringr::str_detect(
            as.character(vals[[cc]]),
            REGEX
          )
      )
    )

    x <- x[keep, ]
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
  
  x <- terra::vect(shp_path) |> clean_geometry()
  
  txt_cols <- intersect(c("NOME_RAZAO","NOME_FANTA","TIPO","SUBTIPO","DISPOSITIV","DESCRICAO_","ATIVIDADE","ATIVIDADE_"), names(x))
  if (length(txt_cols)) {
    vals <- terra::values(x)
    vals[txt_cols] <- lapply(vals[txt_cols], to_upper_utf8)
    terra::values(x) <- vals
    rm(vals); gc()
  }
  
  fcols <- intersect(c("SUBTIPO","DISPOSITIV","DESCRICAO_","ATIVIDADE","ATIVIDADE_"), names(x))
  if (length(fcols)) {
    keep <- Reduce(`|`, lapply(fcols, \(cc) stringr::str_detect(x[[cc]], REGEX)))
    x <- x[keep, ]
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
unlink(TMP_DIR, recursive = TRUE, force = TRUE)