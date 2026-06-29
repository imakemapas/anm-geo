################################################################################
# 04_microdados.R  —  filtragem dos microdados SCM para a Amazônia Legal e padronizar a chave de junção.
################################################################################

# SETUP

rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(arrow) 
  library(glue)
  library(here)
})

MICRO_DIR <- here::here("data", "raw_data", "anm_microdados", "microdados-scm")
CKPT_DIR  <- here::here("data", "_checkpoints")
OUT_DIR   <- here::here("data", "result_db", "microdados")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

limpar_dsprocesso <- function(x) stringr::str_replace_all(as.character(x), "\\.", "")

# Lê um TXT de microdado (";", Windows-1252, tudo como texto p/ não corromper
# zeros à esquerda nem IDs).
ler_micro <- function(arquivo) {
  readr::read_delim(
    file.path(MICRO_DIR, arquivo),
    delim = ";",
    locale = readr::locale(encoding = "Windows-1252"),
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
}

# Converte texto com vírgula decimal ("30000,00") para número (30000.00).
converter_virgula_decimal <- function(df) {
  for (col in names(df)) {
    v <- df[[col]]
    if (!is.character(v)) next
    nao_nulos <- v[!is.na(v) & v != ""]
    if (length(nao_nulos) == 0) next
    if (all(stringr::str_detect(nao_nulos, "^-?[0-9]+,[0-9]+$"))) {
      df[[col]] <- as.numeric(stringr::str_replace(v, ",", "."))
    }
  }
  df
}

# Converte colunas de data (formato "AAAA-MM-DD HH:MM:SS") para Date.
# Aplica-se a toda coluna cujo nome começa com "dt".
converter_datas <- function(df) {
  cols_data <- names(df)[stringr::str_starts(names(df), "dt")]
  for (col in cols_data) {
    df[[col]] <- as.Date(df[[col]])   # ignora a parte de hora "00:00:00"
  }
  df
}

# Lista de processos da Amazônia
caminho_ckpt <- file.path(CKPT_DIR, "03_processos_amzl.rds")
if (!file.exists(caminho_ckpt)) {
  stop("Checkpoint 03_processos_amzl.rds nao encontrado. Rode o bloco 3 do 03_final_proc.R primeiro.")
}
processos_amzl <- readRDS(caminho_ckpt)

# Arquivos COM dsprocesso
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

# Arquivos SEM dsprocesso
arquivos_inteiros <- c(
  "Pessoa.txt"                       = "micro_pessoa",
  "Municipio.txt"                    = "micro_municipio",
  "Evento.txt"                       = "micro_evento",
  "FaseProcesso.txt"                 = "micro_fase_processo",
  "Substancia.txt"                   = "micro_substancia",
  "TipoRequerimento.txt"             = "micro_tipo_requerimento",
  "TipoAssociacao.txt"               = "micro_tipo_associacao",
  "TipoDocumento.txt"                = "micro_tipo_documento",
  "TipoDocumentoLegal.txt"           = "micro_tipo_documento_legal",
  "TipoRelacao.txt"                  = "micro_tipo_relacao",
  "TipoRepresentacaoLegal.txt"       = "micro_tipo_representacao_legal",
  "TipoResponsabilidadeTecnica.txt"  = "micro_tipo_responsabilidade_tecnica",
  "TipoUsoSubstancia.txt"            = "micro_tipo_uso_substancia",
  "CondicaoPropriedadeSolo.txt"      = "micro_condicao_propriedade_solo",
  "MotivoEncerramentoSubstancia.txt" = "micro_motivo_encerramento_substancia",
  "SituacaoDocumentoLegal.txt"       = "micro_situacao_documento_legal",
  "DocumentoLegal.txt"               = "micro_documento_legal",
  "UnidadeAdministrativaRegional.txt"= "micro_unidade_administrativa",
  "UnidadeProtocolizadora.txt"       = "micro_unidade_protocolizadora"
)

# filtra pela Amazônia

total_fato <- length(arquivos_fato)
i <- 0
for (arq in names(arquivos_fato)) {
  i <- i + 1
  tabela <- arquivos_fato[[arq]]

  caminho <- file.path(MICRO_DIR, arq)
  if (!file.exists(caminho)) {
    next
  }

  df <- ler_micro(arq)
  names(df) <- tolower(names(df))
  df <- converter_virgula_decimal(df)
  df <- converter_datas(df)
  n_orig <- nrow(df)

  # cria a chave limpa e filtra pela Amazônia
  df <- df |>
    dplyr::mutate(processo = limpar_dsprocesso(dsprocesso)) |>
    dplyr::filter(processo %in% processos_amzl)

  out <- file.path(OUT_DIR, paste0(tabela, ".parquet"))
  arrow::write_parquet(df, out)
}

# Processa os arquivos INTEIROS

total_int <- length(arquivos_inteiros)
i <- 0
for (arq in names(arquivos_inteiros)) {
  i <- i + 1
  tabela <- arquivos_inteiros[[arq]]

  caminho <- file.path(MICRO_DIR, arq)
  if (!file.exists(caminho)) {
    next
  }

  df <- ler_micro(arq)
  names(df) <- tolower(names(df))
  df <- converter_virgula_decimal(df)
  df <- converter_datas(df)

  out <- file.path(OUT_DIR, paste0(tabela, ".parquet"))
  arrow::write_parquet(df, out)
}

message("\n=== 04_microdados.R — CONCLUÍDO ===")