# prepare_data.R
message("Starting Data Preparation for Shiny Dashboard...")

# ---- Setup ----
suppressPackageStartupMessages({
  library(dplyr)       # Table manipulation
  library(readr)       # CSV reading
  library(stringr)     # String cleaning
  library(sf)          # Spatial data handling
  library(rmapshaper)  # Geometry simplification
})

options(scipen = 999)

# Paths for input and output
ROOT <- "C:/GP/anm-geo"
INPUT_DIR  <- file.path(ROOT, "data", "result_shiny")
OUTPUT_DIR <- file.path(ROOT, "data", "shiny_dashboard")
#RESULT_DIR <- file.path(ROOT, "data", "result")

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Input files
cfem_csv_path <- file.path(INPUT_DIR, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv")
pma_shp_path  <- file.path(INPUT_DIR, "pma_amzl_ALLminerals_final.shp")

# RDS output paths
cfem_rds_path       <- file.path(OUTPUT_DIR, "cfem.rds")
cfem_anual_rdsPath  <- file.path(OUTPUT_DIR, "cfem_anual.rds")
cfem_mensal_rdsPath <- file.path(OUTPUT_DIR, "cfem_mensal.rds")
pma_ocd_rds_path    <- file.path(OUTPUT_DIR, "pma.rds")
pma_simpl_path      <- file.path(OUTPUT_DIR, "pma_simpl.rds")
uc_rds_path         <- file.path(OUTPUT_DIR, "uc.rds")
ti_rds_path         <- file.path(OUTPUT_DIR, "ti.rds")
qui_rds_path        <- file.path(OUTPUT_DIR, "qui.rds")
uc_simpl_path       <- file.path(OUTPUT_DIR, "uc_simpl.rds")
ti_simpl_path       <- file.path(OUTPUT_DIR, "ti_simpl.rds")
qui_simpl_path      <- file.path(OUTPUT_DIR, "qui_simpl.rds")

# ---- 1) CFEM base ----
message("Processing base CFEM data...")
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
  ) |>
  dplyr::filter(ANO >= 2010)

saveRDS(cfem, cfem_rds_path)


# ---- 2) CFEM anual aggregation ----
message("Creating Annual Aggregation...")
cfem_anual <- cfem |>
  dplyr::group_by(
    ANO, abbrev_state, name_muni,
    TITULAR, PROCESSO, NOME_arr,
    SUBSarrSIM, SUBSarr
  ) |>
  dplyr::summarise(
    VALORarr      = sum(VALORarr,      na.rm = TRUE),
    VALORtot      = sum(VALORtot,      na.rm = TRUE),
    PESO_KG       = sum(PESO_KG,       na.rm = TRUE),
    PESO_G        = sum(PESO_G,        na.rm = TRUE),
    PESO_G_final  = sum(PESO_G_final,  na.rm = TRUE),
    PESO_KG_final = sum(PESO_KG_final, na.rm = TRUE),
    .groups       = "drop"
  )

# Get attributes from shapefile to attach to the annual table
pma_ocd_attr <- sf::st_read(pma_shp_path, quiet = TRUE) |>
  dplyr::select(
    PROCESSO, AREA_HA, FASE, ULT_EV_DAT, ULT_EV_DES,
    TIov, UCov, QUIov, TIov10km, UCov2_10km, QUIov10km,
    UCtype, UCname, TIname, QUIname
  ) |>
  sf::st_drop_geometry() |>
  # Important: Force logical flags for Shiny reporting compatibility
  dplyr::mutate(dplyr::across(c(TIov, UCov, QUIov, TIov10km, UCov2_10km, QUIov10km), ~ as.logical(.x)))

cfem_anual <- dplyr::inner_join(cfem_anual, pma_ocd_attr, by = "PROCESSO")
saveRDS(cfem_anual, cfem_anual_rdsPath)


# ---- 3) CFEM mensal ----
message("Formatting Monthly data...")
cfem_mensal <- cfem |>
  dplyr::mutate(data = as.Date(sprintf("%04d-%02d-01", ANO, MES)))

saveRDS(cfem_mensal, cfem_mensal_rdsPath)


# ---- 4) Geometry processing ----
message("Processing Geospatial layers...")

to_wgs84 <- function(x) {
  cr <- sf::st_crs(x)
  if (is.na(cr) || isFALSE(sf::st_is_longlat(cr))) {
    suppressWarnings(sf::st_transform(x, 4326))
  } else { x }
}

# Loading polygons (already filtered in the previous script)
pma <- sf::st_read(pma_shp_path, quiet = TRUE) |> to_wgs84()
uc  <- sf::st_read(file.path(INPUT_DIR, "uc_amzl.shp"),  quiet = TRUE) |> to_wgs84() |> dplyr::select("nome_uc", "sigla_snuc")
qui <- sf::st_read(file.path(INPUT_DIR, "qui_amzl.shp"), quiet = TRUE) |> to_wgs84() |> dplyr::select("nm_comunid")
ti  <- sf::st_read(file.path(INPUT_DIR, "ti_amzl.shp"),  quiet = TRUE) |> to_wgs84() |> dplyr::select("terrai_nom")

# Save raw RDS
saveRDS(pma, pma_ocd_rds_path)
saveRDS(ti,  ti_rds_path)
saveRDS(uc,  uc_rds_path)
saveRDS(qui, qui_rds_path)

message("Simplifying geometries for performance...")
simplify_and_save <- function(sf_obj, out_path, keep_ratio) {
  if (nrow(sf_obj) > 0) {
    simplified <- rmapshaper::ms_simplify(sf_obj, keep = keep_ratio, keep_shapes = TRUE)
    saveRDS(simplified, out_path)
  }
}

simplify_and_save(ti,  ti_simpl_path,  0.3)
simplify_and_save(uc,  uc_simpl_path,  0.3)
simplify_and_save(qui, qui_simpl_path, 0.3)
simplify_and_save(pma, pma_simpl_path, 0.1)

message("✅ FINAL PREPARATION COMPLETE. Files saved in 'shiny_dashboard/result'.")

#GEE 
#(cfem_mensal, file.path(RESULT_DIR, "cfem_mensal"))
