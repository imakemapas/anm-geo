################################################################################
# 07_proc_shiny_dossie.R
# Prepara os objetos da aba 4 do Shiny ("Consulta de Processos")
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)
options(arrow.use_altrep = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(readr)
  library(here)
  library(stringr)
})

source(here::here("R", "utils.R"))

# --- Caminhos -----------------------------------------------------------------
MICRO_DIR    <- here::here("data", "result_db", "microdados")
ST_DIR       <- here::here("data", "result_db", "serie_temporal")
CFEM_PATH    <- here::here("data", "result_shiny", "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv")
OUTPUT_DIR   <- here::here("shiny_dashboard")
QA_DIR       <- here::here("data", "_qa", "07_proc_shiny_dossie")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(QA_DIR,     recursive = TRUE, showWarnings = FALSE)

ler_parquet <- function(dir, nome) {
  caminho <- file.path(dir, paste0(nome, ".parquet"))
  if (!file.exists(caminho)) {
    message("[07] ausente, pulando: ", caminho)
    return(NULL)
  }
  df <- as.data.frame(arrow::read_parquet(caminho))
  cols_dt <- names(df)[stringr::str_starts(names(df), "dt")]
  for (cc in cols_dt) {
    if (is.character(df[[cc]])) {
      df[[cc]] <- suppressWarnings(as.Date(df[[cc]]))
    }
  }
  df
}

# =============================================================================
# PARTE 1 — CATALOGO RELACIONAL (micro_*)
# =============================================================================
message("[07][parte1] lendo microdados relacionais...")

catalogo <- function(df, id_col, novo_nome) {
  if (is.null(df)) return(NULL)
  nms <- names(df)
  if (!id_col %in% nms) {
    cand_id <- nms[stringr::str_starts(nms, "id")]
    if (length(cand_id)) id_col <- cand_id[1] else return(NULL)
  }
  desc_col <- nms[stringr::str_starts(nms, "ds") | stringr::str_starts(nms, "nm") | stringr::str_starts(nms, "no")]
  desc_col <- setdiff(desc_col, id_col)
  desc_col <- if (length(desc_col)) desc_col[1] else setdiff(nms, id_col)[1]
  out <- df[, c(id_col, desc_col)]
  names(out) <- c(id_col, novo_nome)
  out[[id_col]] <- as.character(out[[id_col]])
  dplyr::distinct(out)
}
jl <- function(df, cat, id_col) {
  if (is.null(df) || is.null(cat)) return(df)
  if (!id_col %in% names(df)) return(df)
  df[[id_col]] <- as.character(df[[id_col]])
  dplyr::left_join(df, cat, by = id_col)
}
to_chr_proc <- function(df) {
  if (!is.null(df) && "processo" %in% names(df)) df$processo <- as.character(df$processo)
  df
}

p_processo     <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo"))
p_pessoa       <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_pessoa"))
p_substancia   <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_substancia"))
p_municipio    <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_municipio"))
p_titulo       <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_titulo"))
p_documentacao <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_documentacao"))
p_associacao   <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_associacao"))
p_propsolo     <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_propriedade_solo"))

d_pessoa       <- ler_parquet(MICRO_DIR, "micro_pessoa")
d_municipio    <- ler_parquet(MICRO_DIR, "micro_municipio")
d_fase         <- ler_parquet(MICRO_DIR, "micro_fase_processo")
d_substancia   <- ler_parquet(MICRO_DIR, "micro_substancia")
d_tiporeq      <- ler_parquet(MICRO_DIR, "micro_tipo_requerimento")
d_tipoassoc    <- ler_parquet(MICRO_DIR, "micro_tipo_associacao")
d_tipodoc      <- ler_parquet(MICRO_DIR, "micro_tipo_documento")
d_tipodoclegal <- ler_parquet(MICRO_DIR, "micro_tipo_documento_legal")
d_tiporel      <- ler_parquet(MICRO_DIR, "micro_tipo_relacao")
d_tiporepleg   <- ler_parquet(MICRO_DIR, "micro_tipo_representacao_legal")
d_tiporesptec  <- ler_parquet(MICRO_DIR, "micro_tipo_responsabilidade_tecnica")
d_tipouso      <- ler_parquet(MICRO_DIR, "micro_tipo_uso_substancia")
d_condsolo     <- ler_parquet(MICRO_DIR, "micro_condicao_propriedade_solo")
d_motivoenc    <- ler_parquet(MICRO_DIR, "micro_motivo_encerramento_substancia")
d_sitdoclegal  <- ler_parquet(MICRO_DIR, "micro_situacao_documento_legal")

# --- micro_processos: 1 linha por processo (catalogo) ------------------------
processos <- p_processo |>
  jl(catalogo(d_fase, "idfaseprocesso", "fase"), "idfaseprocesso") |>
  jl(catalogo(d_tiporeq, "idtiporequerimento", "tipo_requerimento"), "idtiporequerimento") |>
  dplyr::transmute(
    processo, nup = nrnup, ativo = ifelse(btativo == "S", "Sim", "Nao"),
    tipo_requerimento, fase, area_ha = suppressWarnings(as.numeric(qtareaha)),
    dt_protocolo = dtprotocolo, dt_prioridade = dtprioridade
  )

if (!is.null(p_municipio) && !is.null(d_municipio)) {
  mc <- d_municipio; mc$idmunicipio <- as.character(mc$idmunicipio)
  processos <- processos |> dplyr::left_join(
    p_municipio |> dplyr::mutate(idmunicipio = as.character(idmunicipio)) |>
      dplyr::left_join(mc, by = "idmunicipio") |> dplyr::group_by(processo) |>
      dplyr::summarise(uf = paste(sort(unique(na.omit(sguf))), collapse = ", "),
                        municipios = paste(sort(unique(na.omit(nmmunicipio))), collapse = ", "), .groups = "drop"),
    by = "processo"
  )
}
if (!is.null(p_substancia) && !is.null(d_substancia)) {
  sc <- catalogo(d_substancia, "idsubstancia", "substancia")
  processos <- processos |> dplyr::left_join(
    p_substancia |> dplyr::mutate(idsubstancia = as.character(idsubstancia)) |>
      dplyr::left_join(sc, by = "idsubstancia") |> dplyr::group_by(processo) |>
      dplyr::summarise(substancias = paste(sort(unique(na.omit(substancia))), collapse = "; "), .groups = "drop"),
    by = "processo"
  )
}
if (!is.null(p_pessoa) && !is.null(d_pessoa) && !is.null(d_tiporel)) {
  rel <- catalogo(d_tiporel, "idtiporelacao", "relacao")
  pc  <- d_pessoa; pc$idpessoa <- as.character(pc$idpessoa)
  processos <- processos |> dplyr::left_join(
    p_pessoa |> dplyr::mutate(idpessoa = as.character(idpessoa), idtiporelacao = as.character(idtiporelacao)) |>
      dplyr::left_join(rel, by = "idtiporelacao") |> dplyr::left_join(pc, by = "idpessoa") |>
      dplyr::filter(grepl("titular", relacao, ignore.case = TRUE)) |> dplyr::group_by(processo) |>
      dplyr::summarise(titular = paste(sort(unique(na.omit(nmpessoa))), collapse = "; "), .groups = "drop"),
    by = "processo"
  )
}
processos$ano_protocolo <- suppressWarnings(as.integer(format(processos$dt_protocolo, "%Y")))
saveRDS(processos, file.path(OUTPUT_DIR, "micro_processos.rds"))
message(sprintf("[07][parte1] micro_processos.rds: %d processos", nrow(processos)))

# --- micro_pessoas / micro_pessoa_resumo -------------------------------------
pessoas <- p_pessoa |> dplyr::mutate(idpessoa = as.character(idpessoa)) |>
  jl(catalogo(d_tiporel, "idtiporelacao", "relacao"), "idtiporelacao") |>
  jl(catalogo(d_tiporesptec, "idtiporesponsabilidadetecnica", "resp_tecnica"), "idtiporesponsabilidadetecnica") |>
  jl(catalogo(d_tiporepleg, "idtiporepresentacaolegal", "repr_legal"), "idtiporepresentacaolegal")

if (!is.null(d_pessoa)) { pc <- d_pessoa; pc$idpessoa <- as.character(pc$idpessoa); pessoas <- dplyr::left_join(pessoas, pc, by = "idpessoa") }
pessoas <- pessoas |> dplyr::transmute(
  processo, idpessoa,
  nome        = if ("nmpessoa" %in% names(pessoas)) nmpessoa else NA_character_,
  cpf_cnpj    = if ("nrcpfcnpj" %in% names(pessoas)) nrcpfcnpj else NA_character_,
  tipo_pessoa = if ("tppessoa" %in% names(pessoas)) tppessoa else NA_character_,
  relacao, resp_tecnica, repr_legal,
  dt_inicio = dtiniciovigencia, dt_fim = dtfimvigencia
)
saveRDS(pessoas, file.path(OUTPUT_DIR, "micro_pessoas.rds"))

pessoa_resumo <- pessoas |>
  dplyr::group_by(idpessoa, nome, cpf_cnpj) |>
  dplyr::summarise(n_processos = dplyr::n_distinct(processo), n_vinculos = dplyr::n(),
                    papeis = paste(sort(unique(na.omit(relacao))), collapse = ", "), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(n_processos))
saveRDS(pessoa_resumo, file.path(OUTPUT_DIR, "micro_pessoa_resumo.rds"))
message(sprintf("[07][parte1] micro_pessoas.rds: %d vinculos | micro_pessoa_resumo.rds: %d pessoas", nrow(pessoas), nrow(pessoa_resumo)))

# --- micro_substancias / micro_titulos / micro_municipios / documentacao / associacoes / propsolo ---
substancias <- p_substancia |>
  jl(catalogo(d_substancia, "idsubstancia", "substancia"), "idsubstancia") |>
  jl(catalogo(d_tipouso, "idtipousosubstancia", "tipo_uso"), "idtipousosubstancia") |>
  jl(catalogo(d_motivoenc, "idmotivoencerramentosubstancia", "motivo_encerramento"), "idmotivoencerramentosubstancia") |>
  dplyr::transmute(processo, substancia, tipo_uso, motivo_encerramento, dt_inicio = dtiniciovigencia, dt_fim = dtfimvigencia)
saveRDS(substancias, file.path(OUTPUT_DIR, "micro_substancias.rds"))

titulos <- p_titulo |>
  jl(catalogo(d_tipodoclegal, "idtipodocumentolegal", "tipo_documento"), "idtipodocumentolegal") |>
  jl(catalogo(d_sitdoclegal, "idsituacaodocumentolegal", "situacao"), "idsituacaodocumentolegal") |>
  dplyr::transmute(processo, nr_titulo = nrtitulo, tipo_documento, situacao,
                    dt_publicacao = dtpublicacao, dt_vencimento = dtvencimento)
saveRDS(titulos, file.path(OUTPUT_DIR, "micro_titulos.rds"))

if (!is.null(p_municipio) && !is.null(d_municipio)) {
  mc <- d_municipio; mc$idmunicipio <- as.character(mc$idmunicipio)
  municipios_proc <- p_municipio |> dplyr::mutate(idmunicipio = as.character(idmunicipio)) |>
    dplyr::left_join(mc, by = "idmunicipio") |> dplyr::transmute(processo, municipio = nmmunicipio, uf = sguf)
  saveRDS(municipios_proc, file.path(OUTPUT_DIR, "micro_municipios.rds"))
}
documentacao <- p_documentacao |>
  jl(catalogo(d_tipodoc, "idtipodocumento", "tipo_documento"), "idtipodocumento") |>
  dplyr::transmute(processo, tipo_documento, dt_protocolo = dtprotocolo)
saveRDS(documentacao, file.path(OUTPUT_DIR, "micro_documentacao.rds"))

associacoes <- p_associacao |>
  jl(catalogo(d_tipoassoc, "idtipoassociacao", "tipo_associacao"), "idtipoassociacao") |>
  dplyr::transmute(processo, processo_associado = dsprocessoassociado, tipo_associacao,
                    dt_associacao = dtassociacao, dt_desassociacao = dtdesassociacao, obs = obassociacao)
saveRDS(associacoes, file.path(OUTPUT_DIR, "micro_associacoes.rds"))

propsolo <- p_propsolo |>
  jl(catalogo(d_condsolo, "idcondicaopropriedadesolo", "condicao_solo"), "idcondicaopropriedadesolo") |>
  dplyr::transmute(processo, condicao_solo)
saveRDS(propsolo, file.path(OUTPUT_DIR, "micro_propsolo.rds"))

message("[07][parte1] concluida.")

# =============================================================================
# PARTE 2 — DOSSIE DE ALERTAS (le o 06 atual, nao reclassifica nada)
# =============================================================================
message("[07][parte2] lendo produtos ja corrigidos do 06_serie_temporal.R...")

situacao_atual               <- ler_parquet(ST_DIR, "situacao_atual")
situacao_documental          <- ler_parquet(ST_DIR, "situacao_documental")
protocolos_licenca_ambiental <- ler_parquet(ST_DIR, "protocolos_licenca_ambiental")
eventos_classificados        <- ler_parquet(ST_DIR, "eventos_classificados")
serie_fase_status            <- ler_parquet(ST_DIR, "serie_fase_status")
acessorios_ambiental         <- ler_parquet(ST_DIR, "acessorios_ambiental")
dic_classificado             <- ler_parquet(ST_DIR, "dicionario_eventos_classificado")
eventos_renovacao_plg        <- ler_parquet(ST_DIR, "eventos_renovacao_plg_211_213")
# NOVO (Bloco J do 06 — GU para AUT PESQ, achado 2026-07-21): mesmo
# tratamento opcional dos demais acima — necessario para AUT PESQ com GU
# valida contar como apto, tanto na reconstrucao historica quanto na faixa
# do grafico.
intervalos_gu_aut_pesq       <- ler_parquet(ST_DIR, "intervalos_gu_aut_pesq")

eventos_penalidades_ocorrencias <- ler_parquet(ST_DIR, "eventos_penalidades_ocorrencias")

if (is.null(situacao_atual) || is.null(situacao_documental) ||
    is.null(eventos_classificados) || is.null(dic_classificado) ||
    is.null(serie_fase_status) || is.null(acessorios_ambiental)) {
  stop("[07] um ou mais produtos do 06_serie_temporal.R nao foram encontrados em ",
       ST_DIR, " — rode o 06 (com as duas exportacoes novas) antes deste script.")
}
if (is.null(eventos_renovacao_plg)) {
  warning("[07] eventos_renovacao_plg_211_213.parquet nao encontrado em ", ST_DIR,
          " — rode o 06 atualizado (Bloco G) para a protecao Art. 211/213 funcionar. ",
          "Seguindo sem ela (comportamento equivalente ao anterior a esta mudanca).")
}
if (is.null(intervalos_gu_aut_pesq)) {
  warning("[07] intervalos_gu_aut_pesq.parquet nao encontrado em ", ST_DIR,
          " — rode o 06 atualizado (Bloco J) para AUT PESQ com GU valida contar como apto. ",
          "Seguindo sem ela (comportamento equivalente ao anterior a esta mudanca).")
}
if (is.null(eventos_penalidades_ocorrencias)) {
  warning("[07] eventos_penalidades_ocorrencias.parquet nao encontrado em ", ST_DIR,
          " — rode o 06 atualizado (Bloco I) para a tabela de penalidades/ocorrencias funcionar. ",
          "Seguindo sem ela.")
}

# --- serie_fase_status: copiado para o shiny_dashboard (faltava) -----------
saveRDS(serie_fase_status, file.path(OUTPUT_DIR, "serie_fase_status.rds"))
message(sprintf("[07][parte2] serie_fase_status.rds: %d linhas em %d processos",
                nrow(serie_fase_status), dplyr::n_distinct(serie_fase_status$processo)))

# --- micro_eventos: log BRUTO de eventos (todos, classificados ou nao) ------
p_evento <- to_chr_proc(ler_parquet(MICRO_DIR, "micro_processo_evento"))
d_evento <- ler_parquet(MICRO_DIR, "micro_evento")

eventos <- p_evento |>
  jl(catalogo(d_evento, "idevento", "evento"), "idevento") |>
  dplyr::mutate(idevento = as.character(idevento)) |>
  dplyr::left_join(dic_classificado |> dplyr::select(idevento, papel), by = "idevento") |>
  dplyr::transmute(processo, idevento, data = dtevento, evento,
                    observacao = if ("obevento" %in% names(p_evento)) obevento else NA_character_,
                    publicacao = if ("dspublicacaodou" %in% names(p_evento)) dspublicacaodou else NA_character_,
                    papel = dplyr::coalesce(papel, "NAO_CLASSIFICADO")) |>
  dplyr::arrange(processo, data)
saveRDS(eventos, file.path(OUTPUT_DIR, "micro_eventos.rds"))
message(sprintf("[07][parte2] micro_eventos.rds: %d eventos (log bruto, todos os papeis)", nrow(eventos)))

# --- Tabela de referencia de eventos (aba 4) ---
cfem_eventos_ref <- dic_classificado |>
  dplyr::filter(papel %in% c("MUDA_FASE", "FECHA", "SUSPENDE", "RETOMA")) |>
  dplyr::transmute(
    tipo_proc = dplyr::coalesce(tipo_proc, "Outros (sem fase associada)"),
    idevento, dsevento, papel,
    descricao = sub(", $", "", paste0(
      ifelse(categ_licenca,    "Licença ambiental, ", ""),
      ifelse(categ_gu,         "Guia de utilização, ", ""),
      ifelse(categ_barragem,   "Barragem, ", ""),
      ifelse(categ_acidente,   "Acidente ambiental, ", ""),
      ifelse(categ_espacial,   "Área indígena/UC/bloqueio/fronteira, ", ""),
      ifelse(categ_judicial,   "Decisão judicial, ", ""),
      ifelse(categ_oneracao,   "Ônus/oneração, ", ""),
      ifelse(categ_penalidade, "Multa/infração/advertência, ", "")
    ))
  ) |>
  dplyr::arrange(tipo_proc, factor(papel, levels = c("MUDA_FASE", "FECHA", "SUSPENDE", "RETOMA")), idevento)
saveRDS(cfem_eventos_ref, file.path(OUTPUT_DIR, "cfem_eventos_ref.rds"))
message(sprintf("[07][parte2] cfem_eventos_ref.rds: %d idevento", nrow(cfem_eventos_ref)))

# --- Tabela de referencia de motivo_nao_apto (aba 4, legenda de alertas) -----
cfem_motivo_ref <- tibble::tribble(
  ~motivo,                                 ~apto_operar_relacionado, ~rotulo,                              ~descricao,
  "fase_de_tramitacao_ou_pesquisa",        "em_analise",             "Em tramitacao/pesquisa",             "Processo ainda nao chegou a uma fase que autoriza operacao (CONC LAV/LICEN/PLG/REG EXT)",
  "suspensa_ou_encerrada",                 "FALSE",                  "Suspensa ou encerrada",               "Fase atual do processo esta suspensa ou encerrada",
  "sem_licenca_ambiental_previa",          "FALSE",                  "Sem licenca ambiental previa",        "Nunca houve protocolo de licenca ambiental registrado para este processo",
  "titulo_vencido",                        "FALSE",                  "Titulo vencido",                      "Vencimento do titulo (situacao_documental) ja passou e nao ha renovacao/prorrogacao vigente",
  "titulo_vencido_renovacao_protocolada",  "FALSE",                  "Vencido, renovacao PLG protocolada",  "Titulo vencido, mas com pedido de renovacao da PLG (idevento 521) protocolado ate a data de vencimento e sem indeferimento (522) registrado — Art. 211/213, Portaria DNPM 155/2016, mantem o titulo em vigor ate manifestacao da ANM.",
  "vencimento_sem_data_a_revisar",         "FALSE",                  "Vencimento sem data a revisar",       "Titulo sem data de vencimento registrada — bloqueia apto_operar por cautela, requer revisao manual"
)
saveRDS(cfem_motivo_ref, file.path(OUTPUT_DIR, "cfem_motivo_ref.rds"))
message(sprintf("[07][parte2] cfem_motivo_ref.rds: %d motivos", nrow(cfem_motivo_ref)))

# --- CFEM, declaracao a declaracao (fonte publica, mesma da Peca A) ----------
cfem <- readr::read_csv(CFEM_PATH, show_col_types = FALSE) |>
  dplyr::mutate(
    PROCESSO  = as.character(PROCESSO),
    ANO       = as.integer(ANO),
    MES       = as.integer(MES),
    data_cfem = as.Date(sprintf("%04d-%02d-01", ANO, MES))
  ) |>
  dplyr::mutate(cfem_id = dplyr::row_number())

# --- Gaps de vigencia de titulo, para TODOS os processos com titulo cadastrado ---
gaps_completo <- gaps_vigencia_titulo(situacao_documental)
processos_com_titulo <- unique(situacao_documental$processo)

if (nrow(gaps_completo) > 0) {
  match_gap <- cfem |>
    dplyr::rename(processo = PROCESSO) |>
    dplyr::filter(processo %in% gaps_completo$processo) |>
    dplyr::select(cfem_id, processo, data_cfem) |>
    dplyr::inner_join(gaps_completo, by = "processo", relationship = "many-to-many") |>
    dplyr::mutate(dentro_gap = data_cfem >= xmin & data_cfem < xmax) |>
    dplyr::group_by(cfem_id) |>
    dplyr::summarise(fora_vigencia = any(dentro_gap, na.rm = TRUE), .groups = "drop")
} else {
  match_gap <- tibble::tibble(cfem_id = integer(), fora_vigencia = logical())
}

cfem_declaracoes_dossie <- cfem |>
  dplyr::left_join(match_gap, by = "cfem_id") |>
  dplyr::mutate(
    tem_titulo_cadastrado = PROCESSO %in% processos_com_titulo,
    fora_vigencia = dplyr::case_when(
      !tem_titulo_cadastrado ~ NA,
      is.na(fora_vigencia)   ~ FALSE,   # tem titulo, nao caiu em nenhum gap
      TRUE                   ~ fora_vigencia
    )
  )

# =============================================================================
# RECONSTRUCAO HISTORICA DE APTIDAO — declaracao a declaracao
# =============================================================================
message("[07][parte2] reconstruindo aptidao historica (fonte unica: segmentos_aptidao_processo, tambem usada pela faixa vermelha do grafico)...")

processos_com_cfem <- unique(cfem$PROCESSO)

segmentos_todos <- dplyr::bind_rows(lapply(processos_com_cfem, function(p) {
  segmentos_aptidao_processo(p, serie_fase_status, situacao_documental, protocolos_licenca_ambiental,
                              eventos_renovacao_plg, intervalos_gu_aut_pesq)
}))

base_decl <- cfem |> dplyr::rename(processo = PROCESSO) |> dplyr::select(cfem_id, processo, data_cfem)

FLAG_COLS <- c("flag_fase_nao_operacional", "flag_status_nao_ativo", "flag_sem_licenca_ambiental_previa",
               "flag_vencimento_sem_data_a_revisar", "flag_titulo_vencido", "flag_titulo_vencido_renovacao_protocolada")

match_seg <- if (!is.null(segmentos_todos) && nrow(segmentos_todos) > 0) {
  base_decl |>
    dplyr::inner_join(segmentos_todos, by = "processo", relationship = "many-to-many") |>
    dplyr::filter(data_cfem >= xmin, data_cfem <= xmax) |>
    dplyr::group_by(cfem_id) |>
    dplyr::slice_max(xmin, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(cfem_id, apto_na_data, motivo_nao_apto_na_data, dplyr::all_of(FLAG_COLS))
} else {
  tibble::as_tibble(setNames(
    c(list(integer()), list(character()), list(character()), rep(list(logical()), length(FLAG_COLS))),
    c("cfem_id", "apto_na_data", "motivo_nao_apto_na_data", FLAG_COLS)
  ))
}

apto_na_data_df <- base_decl |>
  dplyr::select(cfem_id) |>
  dplyr::left_join(match_seg, by = "cfem_id") |>
  dplyr::mutate(
    sem_segmento = is.na(apto_na_data),
    apto_na_data = dplyr::if_else(sem_segmento, "em_analise", apto_na_data),
    motivo_nao_apto_na_data = dplyr::if_else(sem_segmento, "fase_de_tramitacao_ou_pesquisa", motivo_nao_apto_na_data),
    flag_fase_nao_operacional                 = dplyr::if_else(sem_segmento, TRUE,  flag_fase_nao_operacional),
    flag_status_nao_ativo                     = dplyr::if_else(sem_segmento, FALSE, flag_status_nao_ativo),
    flag_sem_licenca_ambiental_previa         = dplyr::if_else(sem_segmento, FALSE, flag_sem_licenca_ambiental_previa),
    flag_vencimento_sem_data_a_revisar        = dplyr::if_else(sem_segmento, FALSE, flag_vencimento_sem_data_a_revisar),
    flag_titulo_vencido                       = dplyr::if_else(sem_segmento, FALSE, flag_titulo_vencido),
    flag_titulo_vencido_renovacao_protocolada = dplyr::if_else(sem_segmento, FALSE, flag_titulo_vencido_renovacao_protocolada)
  ) |>
  dplyr::select(cfem_id, apto_na_data, motivo_nao_apto_na_data, dplyr::all_of(FLAG_COLS))

cfem_declaracoes_dossie <- cfem_declaracoes_dossie |>
  dplyr::left_join(apto_na_data_df, by = "cfem_id")

saveRDS(cfem_declaracoes_dossie, file.path(OUTPUT_DIR, "cfem_declaracoes_dossie.rds"))
message(sprintf(
  "[07][parte2] cfem_declaracoes_dossie.rds: %d declaracoes | %d fora de vigencia (so titulo) | %d em periodo NAO apto (criterio completo, fonte unica) | motivo titulo_vencido: %d",
  nrow(cfem_declaracoes_dossie),
  sum(cfem_declaracoes_dossie$fora_vigencia, na.rm = TRUE),
  sum(cfem_declaracoes_dossie$apto_na_data != "TRUE", na.rm = TRUE),
  sum(cfem_declaracoes_dossie$motivo_nao_apto_na_data == "titulo_vencido", na.rm = TRUE)
))

cfem_com_foco <- cfem_declaracoes_dossie |>
  dplyr::mutate(foco = categorizar_foco(SUBSarr, SUBSarrSIM))

n_foco_por_processo <- cfem_com_foco |>
  dplyr::distinct(PROCESSO, foco) |>
  dplyr::count(PROCESSO, name = "n_foco_distintos")

cfem_com_foco <- cfem_com_foco |>
  dplyr::left_join(n_foco_por_processo, by = "PROCESSO")

decl_single <- cfem_com_foco |> dplyr::filter(n_foco_distintos == 1)
decl_multi  <- cfem_com_foco |> dplyr::filter(n_foco_distintos > 1)

message(sprintf(
  "[07][foco] processos com 1 substancia (CFEM): %d | com 2-3 substancias: %d",
  dplyr::n_distinct(decl_single$PROCESSO), dplyr::n_distinct(decl_multi$PROCESSO)
))

agrega_peso_valor_foco <- function(df) {
  df |>
    dplyr::group_by(PROCESSO, foco) |>
    dplyr::summarise(
      valor_total      = round(sum(VALORarr, na.rm = TRUE), 2),
      peso_total_kg    = round(sum(PESO_KG_final_limpo, na.rm = TRUE), 2),
      valor_suspeito   = round(sum(VALORarr[apto_na_data != "TRUE"], na.rm = TRUE), 2),
      peso_suspeito_kg = round(sum(PESO_KG_final_limpo[apto_na_data != "TRUE"], na.rm = TRUE), 2),
      .groups = "drop"
    )
}

peso_valor_por_foco <- dplyr::bind_rows(
  agrega_peso_valor_foco(decl_single),
  agrega_peso_valor_foco(decl_multi)
)

# --- Resumo por processo, SO para a lista/picker da barra lateral -----------
resumo_por_processo <- cfem_declaracoes_dossie |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    n_declaracoes                   = dplyr::n(),
    n_declaracoes_fora_vig          = sum(fora_vigencia, na.rm = TRUE),
    n_declaracoes_periodo_nao_apto  = sum(apto_na_data != "TRUE", na.rm = TRUE),
    tem_declaracao_periodo_nao_apto = any(apto_na_data != "TRUE", na.rm = TRUE),
    motivos_periodo_nao_apto        = paste(sort(unique(stats::na.omit(motivo_nao_apto_na_data))), collapse = "; "),
    # AJUSTE (2026-07, flags binarias): tem_<flag> = essa flag foi TRUE em
    # QUALQUER periodo da vida do processo, calculada de forma independente
    # das demais (nao so a que "venceu" na cascata categorica acima). Fonte
    # do filtro "Tipo de alerta" da aba 4 a partir de agora.
    tem_flag_fase_nao_operacional                 = any(flag_fase_nao_operacional, na.rm = TRUE),
    tem_flag_status_nao_ativo                     = any(flag_status_nao_ativo, na.rm = TRUE),
    tem_flag_sem_licenca_ambiental_previa         = any(flag_sem_licenca_ambiental_previa, na.rm = TRUE),
    tem_flag_vencimento_sem_data_a_revisar        = any(flag_vencimento_sem_data_a_revisar, na.rm = TRUE),
    tem_flag_titulo_vencido                       = any(flag_titulo_vencido, na.rm = TRUE),
    tem_flag_titulo_vencido_renovacao_protocolada = any(flag_titulo_vencido_renovacao_protocolada, na.rm = TRUE),
    tem_titulo_cadastrado           = dplyr::first(tem_titulo_cadastrado),
    dt_primeira_declaracao          = min(data_cfem, na.rm = TRUE),
    dt_ultima_declaracao            = max(data_cfem, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(peso_valor_por_foco, by = "PROCESSO") |>
  dplyr::rename(processo = PROCESSO) |>
  dplyr::left_join(
    situacao_atual |> dplyr::select(processo, apto_operar, motivo_nao_apto,
                                     dplyr::any_of(c("fase_evento", "status_evento",
                                                      "fase_pma", "fase_diverge_pma"))),
    by = "processo"
  ) |>
  dplyr::left_join(
    eventos_classificados |> dplyr::filter(papel == "SUSPENDE") |>
      dplyr::group_by(processo) |> dplyr::summarise(tem_evento_suspensao = TRUE, .groups = "drop"),
    by = "processo"
  ) |>
  dplyr::left_join(
    eventos_classificados |> dplyr::filter(papel == "FECHA") |>
      dplyr::group_by(processo) |> dplyr::summarise(tem_evento_anulacao = TRUE, .groups = "drop"),
    by = "processo"
  ) |>
  dplyr::mutate(
    tem_evento_suspensao = tidyr::replace_na(tem_evento_suspensao, FALSE),
    tem_evento_anulacao  = tidyr::replace_na(tem_evento_anulacao,  FALSE)
  ) |>
  dplyr::left_join(
    processos |> dplyr::select(processo, dplyr::any_of(c("titular", "uf", "municipios"))),
    by = "processo"
  )

saveRDS(resumo_por_processo, file.path(OUTPUT_DIR, "dossie_resumo_processo.rds"))
message(sprintf(
  "[07][parte2] dossie_resumo_processo.rds: %d linhas (processo x foco) | %d processos distintos | %d com pelo menos 1 declaracao em periodo NAO apto (novo criterio de entrada na lista de suspeitos)",
  nrow(resumo_por_processo), dplyr::n_distinct(resumo_por_processo$processo),
  sum(resumo_por_processo$tem_declaracao_periodo_nao_apto, na.rm = TRUE)
))

# --- Copia das fontes do grafico para DENTRO de shiny_dashboard ------------
saveRDS(situacao_documental,          file.path(OUTPUT_DIR, "situacao_documental.rds"))
saveRDS(eventos_classificados,        file.path(OUTPUT_DIR, "eventos_classificados.rds"))
if (!is.null(protocolos_licenca_ambiental))
  saveRDS(protocolos_licenca_ambiental, file.path(OUTPUT_DIR, "protocolos_licenca_ambiental.rds"))
if (!is.null(situacao_atual))
  saveRDS(situacao_atual, file.path(OUTPUT_DIR, "situacao_atual.rds"))
if (!is.null(eventos_renovacao_plg))
  saveRDS(eventos_renovacao_plg, file.path(OUTPUT_DIR, "eventos_renovacao_plg_211_213.rds"))
if (!is.null(intervalos_gu_aut_pesq))
  saveRDS(intervalos_gu_aut_pesq, file.path(OUTPUT_DIR, "intervalos_gu_aut_pesq.rds"))
if (!is.null(eventos_penalidades_ocorrencias))
  saveRDS(eventos_penalidades_ocorrencias, file.path(OUTPUT_DIR, "eventos_penalidades_ocorrencias.rds"))

message(sprintf(
  "[07][parte2] copiados para %s: situacao_documental.rds (%d), eventos_classificados.rds (%d), protocolos_licenca_ambiental.rds (%s), situacao_atual.rds (%s), eventos_penalidades_ocorrencias.rds (%s)",
  OUTPUT_DIR, nrow(situacao_documental), nrow(eventos_classificados),
  if (!is.null(protocolos_licenca_ambiental)) nrow(protocolos_licenca_ambiental) else "ausente",
  if (!is.null(situacao_atual)) nrow(situacao_atual) else "ausente",
  if (!is.null(eventos_penalidades_ocorrencias)) nrow(eventos_penalidades_ocorrencias) else "ausente"

))

message("\n=== 07_proc_shiny_dossie.R — CONCLUIDO ===")