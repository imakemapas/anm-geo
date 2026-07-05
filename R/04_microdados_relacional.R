################################################################################
# 04_microdados_relacional.R
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(arrow)
  library(purrr)
  library(here)
})

source(here::here("R", "utils.R"))

# --- Caminhos -----------------------------------------------------------------
RAW_DIR      <- here::here("data", "raw_data")
MICRO_DIR    <- here::here("data", "raw_data", "anm_microdados", "microdados-scm")
METADADOS_ODS <- here::here("data", "raw_data", "anm_microdados", "metadados-microdados-scm.ods")
OUT_DIR      <- here::here("data", "result_db", "microdados")
QA_DIR       <- here::here("data", "_qa", "04_microdados_relacional")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(QA_DIR,  recursive = TRUE, showWarnings = FALSE)

# --- Dependência do 03 ---------------------------------------------------------
processos_amzl <- load_ckpt("03_processos_amzl")

# --- Schema oficial -------------------------------------------------------------
if (!file.exists(METADADOS_ODS)) {
  stop("Metadados oficiais nao encontrados: ", METADADOS_ODS,
       " — confirme que 01_download.R baixou metadados-microdados-scm.ods")
}
schema_micro <- ler_schema_microdados(METADADOS_ODS)

message(sprintf("[04] schema oficial carregado | %d tabelas mapeadas", length(schema_micro)))

# Locale numérica: os txt da ANM seguem convenção BR (vírgula decimal, ponto
# de milhar) — mesma convenção já vista nos arquivos CFEM no 02_pre_proc.R.
LOCALE_MICRO <- readr::locale(encoding = "Windows-1252", decimal_mark = ",", grouping_mark = ".")

# --- Leitura por schema, com log de problemas de parsing e de datas -----------
problemas_log <- list()
datas_log     <- list()

ler_micro_schema <- function(arquivo) {
  spec <- schema_micro[[arquivo]]
  if (is.null(spec)) {
    # fallback: caso o Título no .ods venha sem a extensão .txt
    spec <- schema_micro[[stringr::str_remove(arquivo, "\\.txt$")]]
  }
  if (is.null(spec)) {
    warning("[04] schema nao encontrado para ", arquivo, " — lendo como texto (fallback).")
    spec <- readr::cols(.default = readr::col_character())
  }

  df <- readr::read_delim(
    file.path(MICRO_DIR, arquivo),
    delim = ";",
    locale = LOCALE_MICRO,
    col_types = spec,
    show_col_types = FALSE
  )

  probs <- readr::problems(df)
  if (nrow(probs) > 0) {
    problemas_log[[arquivo]] <<- probs |> dplyr::mutate(arquivo = arquivo, .before = 1)
  }

  datas_aqui <- checar_formato_datas(df, arquivo)
  if (nrow(datas_aqui) > 0) datas_log[[arquivo]] <<- datas_aqui

  names(df) <- tolower(names(df))
  df
}

# --- Arquivos COM dsprocesso (filtrados pela Amazônia) -------------------------
arquivos_fato <- c(
  "Processo.txt"                = "micro_processo",
  "ProcessoEvento.txt"          = "micro_processo_evento",
  "ProcessoPessoa.txt"          = "micro_processo_pessoa",
  "ProcessoSubstancia.txt"      = "micro_processo_substancia",
  "ProcessoMunicipio.txt"       = "micro_processo_municipio",
  "ProcessoTitulo.txt"          = "micro_processo_titulo",
  "ProcessoDocumentacao.txt"    = "micro_processo_documentacao",
  "ProcessoAssociacao.txt"      = "micro_processo_associacao",
  "ProcessoPropriedadeSolo.txt" = "micro_processo_propriedade_solo"
)

# --- Arquivos SEM dsprocesso (mantidos inteiros) --------------------------------
arquivos_inteiros <- c(
  "Pessoa.txt"                        = "micro_pessoa",
  "Municipio.txt"                     = "micro_municipio",
  "Evento.txt"                        = "micro_evento",
  "FaseProcesso.txt"                  = "micro_fase_processo",
  "Substancia.txt"                    = "micro_substancia",
  "TipoRequerimento.txt"              = "micro_tipo_requerimento",
  "TipoAssociacao.txt"                = "micro_tipo_associacao",
  "TipoDocumento.txt"                 = "micro_tipo_documento",
  "TipoDocumentoLegal.txt"            = "micro_tipo_documento_legal",
  "TipoRelacao.txt"                   = "micro_tipo_relacao",
  "TipoRepresentacaoLegal.txt"        = "micro_tipo_representacao_legal",
  "TipoResponsabilidadeTecnica.txt"   = "micro_tipo_responsabilidade_tecnica",
  "TipoUsoSubstancia.txt"             = "micro_tipo_uso_substancia",
  "CondicaoPropriedadeSolo.txt"       = "micro_condicao_propriedade_solo",
  "MotivoEncerramentoSubstancia.txt"  = "micro_motivo_encerramento_substancia",
  "SituacaoDocumentoLegal.txt"        = "micro_situacao_documento_legal",
  "DocumentoLegal.txt"                = "micro_documento_legal",
  "UnidadeAdministrativaRegional.txt" = "micro_unidade_administrativa",
  "UnidadeProtocolizadora.txt"        = "micro_unidade_protocolizadora"
)

# --- Processa os arquivos FATO (filtrados pela Amazônia) -----------------------
processo_txt_processo_key <- NULL  # guarda a chave 'processo' de Processo.txt p/ o check de descompasso

for (arq in names(arquivos_fato)) {
  tabela  <- arquivos_fato[[arq]]
  caminho <- file.path(MICRO_DIR, arq)
  if (!file.exists(caminho)) { message("[04] ausente, pulando: ", arq); next }

  df <- ler_micro_schema(arq)

  df <- df |>
    dplyr::mutate(processo = limpar_dsprocesso(dsprocesso)) |>
    dplyr::filter(processo %in% processos_amzl)

  if (arq == "Processo.txt") processo_txt_processo_key <- unique(df$processo)

  arrow::write_parquet(df, file.path(OUT_DIR, paste0(tabela, ".parquet")))
  message(sprintf("[04] %-32s -> %-38s | %d linhas", arq, tabela, nrow(df)))
}

# --- Processa os arquivos INTEIROS ---------------------------------------------
for (arq in names(arquivos_inteiros)) {
  tabela  <- arquivos_inteiros[[arq]]
  caminho <- file.path(MICRO_DIR, arq)
  if (!file.exists(caminho)) { message("[04] ausente, pulando: ", arq); next }

  df <- ler_micro_schema(arq)

  arrow::write_parquet(df, file.path(OUT_DIR, paste0(tabela, ".parquet")))
  message(sprintf("[04] %-32s -> %-38s | %d linhas", arq, tabela, nrow(df)))
}

# --- Exporta checks --------------------------------------------------------------
if (length(problemas_log) > 0) {
  readr::write_csv(dplyr::bind_rows(problemas_log), file.path(QA_DIR, "problemas_parsing.csv"))
  message(sprintf("[04] problemas de parsing (schema x dado real) em %d arquivo(s) — ver %s",
                  length(problemas_log), file.path(QA_DIR, "problemas_parsing.csv")))
} else {
  message("[04] nenhum problema de parsing contra o schema oficial.")
}

if (length(datas_log) > 0) {
  readr::write_csv(dplyr::bind_rows(datas_log), file.path(QA_DIR, "formato_datas.csv"))
  message("[04] amostra de formato de datas em: ", file.path(QA_DIR, "formato_datas.csv"))
}

# --- Check: descompasso entre extração espacial (03) e microdado relacional ----
if (!is.null(processo_txt_processo_key)) {
  sem_microdado <- setdiff(processos_amzl, processo_txt_processo_key)
  readr::write_csv(
    tibble::tibble(processo = sem_microdado),
    file.path(QA_DIR, "processos_sem_microdado.csv")
  )
  message(sprintf(
    "[04] processos_amzl sem correspondencia em Processo.txt: %d de %d (%.1f%%) | ver %s",
    length(sem_microdado), length(processos_amzl),
    100 * length(sem_microdado) / length(processos_amzl),
    file.path(QA_DIR, "processos_sem_microdado.csv")
  ))
}

message("\n=== 04_microdados_relacional.R — CONCLUÍDO ===")