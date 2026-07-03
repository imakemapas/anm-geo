################################################################################
# 01_download.R - Open Data Downloader (ANM + Geo)
################################################################################

# Setup and Configuration -------------------------------------------------
rm(list = ls(all.names = TRUE))
options(scipen = 999)
options(timeout = 1800)

suppressPackageStartupMessages({
  library(purrr)
  library(here)
  library(readr)
  library(dplyr)
  library(tibble)
})

ROOT      <- here::here()
RAW_DIR   <- here::here("data", "raw_data")

source(here::here("R", "utils.R"))  # download_file, download_named_urls, sha256_file, hash_anterior

dir.create(MANIFEST_DIR, recursive = TRUE, showWarnings = FALSE)

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
      # https://metadados.inde.gov.br/geonetwork/srv/por/catalog.search#/metadata/63019b03-0937-4461-a6ed-abef1457c1a1
      "tis_poligonais.zip" =
        "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_poligonais&maxFeatures=10000&outputFormat=SHAPE-ZIP"
    )
  ),
  ambiental_federal = list(
    dest = "geo_federal",
    urls = c(
      # https://dados.mma.gov.br/dataset/unidadesdeconservacao
      # https://www.gov.br/icmbio/pt-br/dados-icmbio/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais
      "shp_cnuc_2025_08.zip" =
        "https://dados.mma.gov.br/dataset/44b6dc8a-dc82-4a84-8d95-1b0da7c85dac/resource/6ba9a557-87e8-4882-acb7-b3e0f0ea192d/download/shp_cnuc_2025_08.zip",
      "embargos_icmbio.zip" =
        "https://www.gov.br/icmbio/pt-br/dados-icmbio/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais/embargos_icmbio_shp.zip",
      "autos_infracao_icmbio.zip" =
        "https://www.gov.br/icmbio/pt-br/dados-icmbio/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais/autos_infracao_icmbio_shp.zip"
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
  list(name = "geo_funai",   dest = file.path(RAW_DIR, config_geo$funai$dest),            urls = config_geo$funai$urls),
  list(name = "geo_federal", dest = file.path(RAW_DIR, config_geo$ambiental_federal$dest), urls = config_geo$ambiental_federal$urls),
  list(name = "geo_ibama",   dest = file.path(RAW_DIR, config_geo$ibama$dest),             urls = config_geo$ibama$urls),
  list(name = "geo_sema_mt", dest = file.path(RAW_DIR, config_geo$sema_mt$dest),           urls = sema_urls),
  list(name = "geo_incra",   dest = file.path(RAW_DIR, config_geo$incra$dest),             urls = config_geo$incra$urls)
)

# Run all targets --------------------------------------------------------------
targets <- c(anm_targets, geo_targets)

# Timestamp
TS_EXECUCAO <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

manifests_anteriores <- listar_manifests_anteriores()

results_list <- purrr::map(targets, \(t) {
  download_named_urls(t$urls, t$dest, target_name = t$name)
})

# Finish ---------------------------------------------------------------------
todos_arquivos <- purrr::list_flatten(results_list)
erros          <- purrr::keep(todos_arquivos, ~ .x$success == FALSE)

if (length(erros) > 0) {
  message("\n ATTENTION: ", length(erros), " download(s) failed.")
  purrr::walk(erros, ~ message(.x$filename))

  tentar_denovo <- readline(prompt = "Would you like to try downloading these errors now?? (y/n): ")

  if (tolower(tentar_denovo) == "y") {
    # Reprocessa só os que falharam
    retries <- purrr::map(erros, ~ {
      r <- download_file(url = .x$url, dest_dir = .x$dest_dir, filename = .x$filename)
      list(
        target = .x$target, filename = .x$filename, url = .x$url, dest_dir = .x$dest_dir,
        success = r$success, sha256 = r$sha256, size_bytes = r$size_bytes,
        attempts_used = r$attempts_used, note = paste0("retry_manual: ", r$note)
      )
    })

    for (r in retries) {
      idx <- purrr::detect_index(
        todos_arquivos,
        ~ .x$dest_dir == r$dest_dir && .x$filename == r$filename
      )
      if (idx > 0) todos_arquivos[[idx]] <- r
    }
  }
} else {
  message("\nProcess completed successfully.")
}

# --- Manifest desta execução --------------------------------------------------
manifest_df <- purrr::map_dfr(todos_arquivos, \(r) {
  hash_ant <- hash_anterior(r$dest_dir, r$filename, manifests = manifests_anteriores)
  tibble::tibble(
    timestamp_execucao = TS_EXECUCAO,
    target              = r$target,
    filename            = r$filename,
    url                 = r$url,
    dest_dir            = r$dest_dir,
    success             = r$success,
    sha256              = r$sha256,
    sha256_anterior     = hash_ant,
    mudou               = dplyr::case_when(
      is.na(hash_ant) | is.na(r$sha256) ~ NA,
      hash_ant == r$sha256               ~ FALSE,
      TRUE                                ~ TRUE
    ),
    size_bytes          = r$size_bytes,
    attempts_used       = r$attempts_used,
    note                = r$note
  )
})

manifest_path <- file.path(MANIFEST_DIR, paste0("download_log_", TS_EXECUCAO, ".csv"))
readr::write_csv(manifest_df, manifest_path)

n_mudaram <- sum(manifest_df$mudou, na.rm = TRUE)
n_novos   <- sum(is.na(manifest_df$mudou))
message("Arquivos com conteúdo alterado desde a última execução: ", n_mudaram)
message("Arquivos novos: ", n_novos)