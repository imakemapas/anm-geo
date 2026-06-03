################################################################################
# Open Data Downloader (ANM + Geo)
################################################################################

# Setup and Configuration -------------------------------------------------
rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(purrr)
  library(curl)
  library(here)
})

ROOT    <- here::here()
RAW_DIR <- here::here("data", "raw_data")
TIMEOUT <- 300

download_file <- function(url, dest_dir, filename = basename(url), timeout = TIMEOUT) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dst <- file.path(dest_dir, filename)
  
  message("Downloading: ", filename)
  
  ok <- tryCatch({
    h <- curl::new_handle(
      timeout = timeout,
      connecttimeout = 30,
      low_speed_time = 30,
      low_speed_limit = 1
    )
    
    curl::curl_download(
      url = url,
      destfile = dst,
      mode = "wb",
      handle = h
    )
    
    if (!file.exists(dst) || is.na(file.info(dst)$size) || file.info(dst)$size == 0) {
      stop("Downloaded file is missing or empty.")
    }
    
    message("OK: ", filename, " | size=", file.info(dst)$size)
    TRUE
  }, error = function(e) {
    warning("Download failed: ", filename, " | ", conditionMessage(e))
    FALSE
  })
  
  invisible(ok)
}

download_named_urls <- function(named_urls, dest_dir, timeout = TIMEOUT) {
  purrr::imap(named_urls, ~{
    ok <- download_file(
      url = .x,
      dest_dir = dest_dir,
      filename = .y,
      timeout = timeout
    )
    
    Sys.sleep(runif(1, 1, 3))
    
    list(url = .x, dest = dest_dir, filename = .y, success = ok)
  })
}

# Config: ANM ------------------------------------------------------------------
config_anm <- list(
  scm = list(
    dest = "anm_scm",
    base_url = "https://dadosabertos.anm.gov.br/SCM/",
    files = c(
      "Alvara_de_Pesquisa.csv",
      "Cessoes_de_Direitos.csv",
      "Licenciamento.csv",
      "PLG.csv",
      "Portaria_de_Lavra.csv",
      "Registro_de_Extracao_Publicado.csv",
      "Relatorio_de_Pesquisa_Aprovado.csv",
      "Requerimento_de_Lavra.csv",
      "Requerimento_de_Licenciamento.csv",
      "Requerimento_de_Pesquisa.csv",
      "Requerimento_de_PLG.csv",
      "Requerimento_de_Registro_de_Extracao_Protocolizado.csv",
      "metadados-scm.ods"
    )
  ),
  protocolo_digital = list(
    dest = "anm_protocolo_digital",
    base_url = "https://dadosabertos.anm.gov.br/PD/",
    files = c(
      "Assunto.txt",
      "AssuntoDocumento.txt",
      "AssuntoParametro.txt",
      "Indisponibilidade.txt",
      "mer_pd.png",
      "metadados-pd.ods",
      "Protocolo.txt",
      "ProtocoloAnexoSei.txt"
    )
  ),
  microdados = list(
    dest = "anm_microdados",
    base_url = "https://dadosabertos.anm.gov.br/SCM/microdados/",
    files = c(
      "microdados-scm.zip",
      "mer-microdados-scm.pdf",
      "mer-dbanm_gdb.pdf",
      "metadados-microdados-scm.ods"
    )
  ),
  sigmine = list(
    dest = "anm_espacial",
    base_url = "https://dadosabertos.anm.gov.br/SIGMINE/",
    files = c("BRASIL.zip")
  ),
  arrecadacao = list(
    dest = "anm_arrecadacao",
    base_url = "https://dadosabertos.anm.gov.br/CFEM/",
    files = c(
      "CFEM_Arrecadacao.csv",
      "CFEM_Autuacao.csv",
      "CFEM_Distribuicao.csv",
      "metadados-cfem.ods"
    )
  )
  
  # ,
  # Tah = list(
  #   dest = "anm_Tah",
  #   base_url = "https://dadosabertos.anm.gov.br/TAH/",
  #   files = c(      
  #     "Tah.csv",
  #     "metadados-tah.ods")
  #)
)

anm_targets <- purrr::imap(config_anm, \(cfg, name) {
  urls <- setNames(paste0(cfg$base_url, cfg$files), cfg$files)
  list(name = paste0("anm_", name), dest = file.path(RAW_DIR, cfg$dest), urls = urls)
})

# Config: GEO (protected lands + enforcement) ----------------------------------
config_geo <- list(
  funai = list(
    dest = "geo_funai",
    urls = c(
      "tis_poligonais.zip" =
        "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_poligonais&maxFeatures=10000&outputFormat=SHAPE-ZIP"
    )
  ),
  ambiental_federal = list(
    dest = "geo_federal",
    urls = c(
      "shp_cnuc_2025_08.zip" =
        "https://dados.mma.gov.br/dataset/44b6dc8a-dc82-4a84-8d95-1b0da7c85dac/resource/7a142cc0-dae9-4a0b-8180-3016994d2932/download/shp_cnuc_2025_08.zip",
      "embargos_icmbio.zip" =
        "https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais/embargos_icmbio_shp.zip",
      "autos_infracao_icmbio.zip" =
        "https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais/autos_infracao_icmbio_shp.zip"
    )
  ),
  ibama = list(
    dest = "geo_ibama",
    urls = c(
      "adm_embargos_ibama_a.zip" =
        "https://ftp-pamgia.ibama.gov.br/dados/adm_embargos_ibama_a.zip",
      "auto_infracao_csv.zip" =
        "https://dadosabertos.ibama.gov.br/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao_csv.zip"
    )
  ),
  sema_mt = list(
    dest = "geo_sema_mt",
    base_url =
      "https://geo.sema.mt.gov.br/geoserver/wfs?authkey=541085de-9a2e-454e-bdba-eb3d57a2f492&request=getfeature&service=wfs&version=1.0.0&outputformat=SHAPE-ZIP&typename=Geoportal:",
    layers = c(
      "AREAS_EMBARGADAS_SEMA",
      "AREA_EMBARGADA_SIGA_POLIGONO",
      "AUTOS_DE_INFRACAO_SIGA_POLIGONO"
    )
  ),
  incra = list(
    dest = "geo_incra",
    urls = c(
      "areas_quilombolas.zip" =
        "https://certificacao.incra.gov.br/csv_shp/zip/%C3%81reas%20de%20Quilombolas.zip"
    )
  )
)

sema_urls <- setNames(
  paste0(config_geo$sema_mt$base_url, config_geo$sema_mt$layers),
  paste0(config_geo$sema_mt$layers, ".zip")
)

geo_targets <- list(
  list(name = "geo_funai",            dest = file.path(RAW_DIR, config_geo$funai$dest),            urls = config_geo$funai$urls),
  list(name = "geo_federal",          dest = file.path(RAW_DIR, config_geo$ambiental_federal$dest), urls = config_geo$ambiental_federal$urls),
  list(name = "geo_ibama",            dest = file.path(RAW_DIR, config_geo$ibama$dest),            urls = config_geo$ibama$urls),
  list(name = "geo_sema_mt",          dest = file.path(RAW_DIR, config_geo$sema_mt$dest),          urls = sema_urls),
  list(name = "geo_incra",            dest = file.path(RAW_DIR, config_geo$incra$dest),            urls = config_geo$incra$urls)
)

# Run all targets --------------------------------------------------------------
targets <- c(anm_targets, geo_targets)

results_list <- purrr::map(targets, \(t) {
  message("\n--- Target: ", t$name, " ---")
  download_named_urls(t$urls, t$dest, timeout = TIMEOUT)
})

# Finish ---------------------------------------------------------------------
todos_arquivos <- purrr::list_flatten(results_list)
erros          <- purrr::keep(todos_arquivos, ~ .x$success == FALSE)

if (length(erros) > 0) {
  message("\n ATTENTION: ", length(erros), " download(s) failed.")
  purrr::walk(erros, ~ message(.x$filename))
  
  tentar_denovo <- readline(prompt = "Would you like to try downloading these errors now?? (y/n): ")
  
  if (tolower(tentar_denovo) == "y") {
    message("\n--- downloading ... ---")
    purrr::walk(erros, ~ {
      download_file(url = .x$url, dest_dir = .x$dest, filename = .x$filename)
    })
  }
} else {
  message("\nProcess completed successfully.")
}