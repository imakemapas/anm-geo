################################################################################
# 07_proc_shiny_dossie.R
#
# Prepara os objetos da aba 4 do Shiny ("Consulta de Processos" / dossie por
# processo), em duas partes:
#
#   PARTE 1 — CATALOGO RELACIONAL (micro_*): joins fato x dimensao dos
#             microdados SCM (processos, pessoas, substancias, titulos,
#             municipios, documentacao, associacoes, propriedade do solo).
#             Mesma logica do antigo 06_proc_shiny_microdados_inaptos_old.R —
#             essa parte NAO depende de classificacao de evento, so de joins
#             de catalogo, e continua valida contra o 04_microdados_relacional.R
#             atual (mesmos nomes de tabela/parquet, confirmado).
#
#   PARTE 2 — DOSSIE DE ALERTAS: NAO reclassifica nada. Le direto os produtos
#             ja corrigidos do 06_serie_temporal.R (situacao_atual,
#             situacao_documental, eventos_classificados, serie_fase_status) e
#             cruza com CFEM (declaracao a declaracao, SEM AGREGACAO) so para
#             marcar se cada declaracao caiu ou nao dentro de um periodo com
#             titulo valido. O motor de classificacao (aptidao, fase, status)
#             e o do 06 — este script so consome e cruza.
#
# Principio (decisao do usuario, nao renegociavel neste script): nenhuma
# agregacao por mes/processo na granularidade analitica. O unico lugar onde
# aparece uma contagem/soma e o resumo da barra lateral (picker de processo),
# que e uma conveniencia de interface, nao uma reclassificacao dos dados.
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

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

# Leitor generico de parquet por diretorio (MICRO_DIR ou ST_DIR).
# Colunas "dt*" que ainda vierem como texto (schema do 04 classificou como
# character, ou parquet nao preservou o tipo Date) sao convertidas aqui — sem
# isso, format(x, "%Y") cai em format.default() (2o argumento = trim) em vez
# de format.Date(), e quebra com "invalid 'trim' argument".
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

# micro_titulos: catalogo BRUTO (documento a documento, direto do microdado).
# ATENCAO: para o grafico/dossie da aba 4, a fonte usada e situacao_documental
# (06_serie_temporal.R), que ja tem status_vencimento calculado — este objeto
# aqui e so referencia/consulta bruta, nao alimenta o grafico.
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

if (is.null(situacao_atual) || is.null(situacao_documental) ||
    is.null(eventos_classificados) || is.null(dic_classificado) ||
    is.null(serie_fase_status) || is.null(acessorios_ambiental)) {
  stop("[07] um ou mais produtos do 06_serie_temporal.R nao foram encontrados em ",
       ST_DIR, " — rode o 06 (com as duas exportacoes novas) antes deste script.")
}

# --- serie_fase_status: copiado para o shiny_dashboard (faltava) -----------
# Fonte da tabela "Historico de autorizacoes (fases do processo)" na aba 4.
# O enriquecimento (texto de evento inicio/fim + injecao da fase "VENCIDA")
# e feito NO app.R, por processo, sob demanda (mesmo padrao do app antigo) —
# aqui so exportamos o produto bruto do 06, sem reprocessar nada.
saveRDS(serie_fase_status, file.path(OUTPUT_DIR, "serie_fase_status.rds"))
message(sprintf("[07][parte2] serie_fase_status.rds: %d linhas em %d processos",
                nrow(serie_fase_status), dplyr::n_distinct(serie_fase_status$processo)))

# --- micro_eventos: log BRUTO de eventos (todos, classificados ou nao) ------
# Diferente de eventos_classificados.parquet (so os 4 papeis decisivos da
# maquina de estado), aqui e o historico COMPLETO do processo, incluindo
# NAO_CLASSIFICADO — para navegacao/investigacao livre no dossie (aba 4,
# tabela "Historico de eventos"). E so um join de catalogo, nao depende de
# nenhuma regra de aptidao.
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

# --- Tabela de referencia de eventos (aba 4), a partir do dicionario ja corrigido ---
# NOTA METODOLOGICA: o dicionario antigo tinha uma coluna "descricao" escrita a
# mao por idevento (glosa manual). O dicionario NOVO (dic_classificado) so tem
# o texto OFICIAL do evento (dsevento) + a classificacao (papel) + a categoria
# ambiental/juridica (categoria_amb_jur) — nao existe mais uma glosa manual, de
# proposito, pra nao reintroduzir texto nao auditavel. Uso categoria_amb_jur
# como "descricao" (quando existe) em vez de inventar um texto novo.
cfem_eventos_ref <- dic_classificado |>
  dplyr::filter(papel %in% c("MUDA_FASE", "FECHA", "SUSPENDE", "RETOMA")) |>
  dplyr::transmute(
    # NA em tipo_proc acontece por design no classificador do 06 (eventos
    # "TORNA S/EFEITO..." ou sem "/" na descricao nao tem fase associada).
    # Rotulado explicitamente aqui — deixar NA quebra qualquer subsetting
    # base R (df[df$tipo_proc == NA, ]) rio abaixo, incluindo o cp_eventos_ref
    # do app.R (foi exatamente isso que causou o "subscript out of bounds").
    tipo_proc = dplyr::coalesce(tipo_proc, "Outros (sem fase associada)"),
    idevento, dsevento, papel,
    descricao = dplyr::coalesce(categoria_amb_jur, "")
  ) |>
  dplyr::arrange(tipo_proc, factor(papel, levels = c("MUDA_FASE", "FECHA", "SUSPENDE", "RETOMA")), idevento)
saveRDS(cfem_eventos_ref, file.path(OUTPUT_DIR, "cfem_eventos_ref.rds"))
message(sprintf("[07][parte2] cfem_eventos_ref.rds: %d idevento", nrow(cfem_eventos_ref)))

# --- Tabela de referencia de motivo_nao_apto (aba 4, legenda de alertas) -----
# Vocabulario FIXO, direto do case_when de situacao_atual no 06_serie_temporal.R
# (nao inventado aqui — os 5 valores abaixo sao exatamente os unicos que o
# case_when de motivo_nao_apto pode gerar). Se o 06 mudar essa arvore de
# decisao no futuro, este objeto precisa ser revisado junto.
cfem_motivo_ref <- tibble::tribble(
  ~motivo,                          ~apto_operar_relacionado, ~rotulo,                              ~descricao,
  "fase_de_tramitacao_ou_pesquisa", "em_analise",             "Em tramitacao/pesquisa",             "Processo ainda nao chegou a uma fase que autoriza operacao (CONC LAV/LICEN/PLG/REG EXT)",
  "suspensa_ou_encerrada",          "FALSE",                  "Suspensa ou encerrada",               "Fase atual do processo esta suspensa ou encerrada",
  "sem_licenca_ambiental_previa",   "FALSE",                  "Sem licenca ambiental previa",        "Protocolo de licenca ambiental e posterior (ou inexistente) ao inicio da fase de operacao",
  "titulo_vencido",                 "FALSE",                  "Titulo vencido",                      "Vencimento do titulo (situacao_documental) ja passou e nao ha renovacao/prorrogacao vigente",
  "vencimento_sem_data_a_revisar",  "FALSE",                  "Vencimento sem data a revisar",       "Titulo sem data de vencimento registrada — bloqueia apto_operar por cautela, requer revisao manual"
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
# Reaproveita gaps_vigencia_titulo() de utils.R (Peca C) — mesma funcao usada
# no grafico, sem duplicar a logica de uniao de intervalos.
gaps_completo <- gaps_vigencia_titulo(situacao_documental)
processos_com_titulo <- unique(situacao_documental$processo)

# --- Cruza CADA declaracao de CFEM com os gaps do seu processo (sem agregar) ---
# fora_vigencia: TRUE  = a declaracao caiu num periodo sem titulo valido
#                FALSE = processo tem titulo cadastrado e a declaracao caiu
#                        dentro de um periodo com titulo valido
#                NA    = processo NAO tem nenhum titulo cadastrado em
#                        situacao_documental — ausencia de dado, nao "esta ok"
#                        (principio ja registrado: NA sobre 0/FALSE para fonte
#                        ausente)
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
# Ate a versao anterior, esta reconstrucao tinha logica PROPRIA (separada da
# faixa vermelha do grafico), e podia divergir dela — mesmo processo podia
# aparecer "vencido" num lugar e nao no outro. Agora usa a MESMA funcao
# (segmentos_aptidao_processo(), de graficos_historico.R) que a faixa
# vermelha usa — fonte unica, sem excecao.
#
# Tambem corrige o undercounting de "titulo_vencido": a versao anterior so
# comparava a data da declaracao contra o vencimento NOMINAL do titulo
# (fora_vigencia), sem saber se o titulo ja tinha morrido antes por renuncia/
# cassacao/nulidade (FECHA) — ou o contrario, se a fase ja estava fechada por
# outro motivo antes do titulo vencer de fato (achado real: 850292/2016).
message("[07][parte2] reconstruindo aptidao historica (fonte unica: segmentos_aptidao_processo, tambem usada pela faixa vermelha do grafico)...")

processos_com_cfem <- unique(cfem$PROCESSO)

segmentos_todos <- dplyr::bind_rows(lapply(processos_com_cfem, function(p) {
  segmentos_aptidao_processo(p, serie_fase_status, situacao_documental, protocolos_licenca_ambiental)
}))

base_decl <- cfem |> dplyr::rename(processo = PROCESSO) |> dplyr::select(cfem_id, processo, data_cfem)

match_seg <- if (!is.null(segmentos_todos) && nrow(segmentos_todos) > 0) {
  base_decl |>
    dplyr::inner_join(segmentos_todos, by = "processo", relationship = "many-to-many") |>
    dplyr::filter(data_cfem >= xmin, data_cfem <= xmax) |>
    dplyr::group_by(cfem_id) |>
    dplyr::slice_max(xmin, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(cfem_id, apto_na_data, motivo_nao_apto_na_data)
} else {
  tibble::tibble(cfem_id = integer(), apto_na_data = character(), motivo_nao_apto_na_data = character())
}

# Declaracao sem segmento cobrindo a data (processo sem nenhuma fase
# registrada, ou data fora do range coberto) — cauteloso, mesmo padrao do
# resto do projeto: tratado como "em analise", nao como apto.
apto_na_data_df <- base_decl |>
  dplyr::select(cfem_id) |>
  dplyr::left_join(match_seg, by = "cfem_id") |>
  dplyr::mutate(
    sem_segmento = is.na(apto_na_data),
    apto_na_data = dplyr::if_else(sem_segmento, "em_analise", apto_na_data),
    motivo_nao_apto_na_data = dplyr::if_else(sem_segmento, "fase_de_tramitacao_ou_pesquisa", motivo_nao_apto_na_data)
  ) |>
  dplyr::select(cfem_id, apto_na_data, motivo_nao_apto_na_data)

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

# --- Resumo por processo, SO para a lista/picker da barra lateral -----------
# Isto e uma conveniencia de UI (selecionar um processo numa tabela), nao uma
# reclassificacao: apto_operar/motivo_nao_apto (situacao_atual) continuam
# aqui so como CONTEXTO (foto de hoje) — quem decide se o processo entra na
# lista de "suspeitos" agora e tem_declaracao_periodo_nao_apto (reconstrucao
# historica acima), NAO apto_operar. Essa e a correcao do erro relatado:
# processo com CFEM so em periodo em que ERA apto nao deve aparecer, mesmo
# que hoje esteja nao-apto por outro motivo.
resumo_por_processo <- cfem_declaracoes_dossie |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    n_declaracoes                   = dplyr::n(),
    n_declaracoes_fora_vig          = sum(fora_vigencia, na.rm = TRUE),
    n_declaracoes_periodo_nao_apto  = sum(apto_na_data != "TRUE", na.rm = TRUE),
    tem_declaracao_periodo_nao_apto = any(apto_na_data != "TRUE", na.rm = TRUE),
    motivos_periodo_nao_apto        = paste(sort(unique(stats::na.omit(motivo_nao_apto_na_data))), collapse = "; "),
    tem_titulo_cadastrado           = dplyr::first(tem_titulo_cadastrado),
    valor_total                     = round(sum(VALORarr, na.rm = TRUE), 2),
    peso_total_kg                   = round(sum(PESO_KG_final, na.rm = TRUE), 2),
    valor_suspeito                  = round(sum(VALORarr[apto_na_data != "TRUE"], na.rm = TRUE), 2),
    peso_suspeito_kg                = round(sum(PESO_KG_final[apto_na_data != "TRUE"], na.rm = TRUE), 2),
    dt_primeira_declaracao          = min(data_cfem, na.rm = TRUE),
    dt_ultima_declaracao            = max(data_cfem, na.rm = TRUE),
    .groups = "drop"
  ) |>
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
  "[07][parte2] dossie_resumo_processo.rds: %d processos | %d com pelo menos 1 declaracao em periodo NAO apto (novo criterio de entrada na lista de suspeitos)",
  nrow(resumo_por_processo), sum(resumo_por_processo$tem_declaracao_periodo_nao_apto, na.rm = TRUE)
))

# --- Copia das fontes do grafico para DENTRO de shiny_dashboard ------------
# CORRECAO DE ROTA: a decisao original era deixar o app.R ler estes parquets
# direto de ST_DIR via arrow (menos I/O redundante). Mas o deploy sobe SO a
# pasta shiny_dashboard para o droplet — data/result_db/serie_temporal nao
# existe no servidor. Por isso, copiamos aqui (como .rds, para nao precisar
# adicionar o pacote arrow como dependencia de runtime do app em producao).
saveRDS(situacao_documental,          file.path(OUTPUT_DIR, "situacao_documental.rds"))
saveRDS(eventos_classificados,        file.path(OUTPUT_DIR, "eventos_classificados.rds"))
if (!is.null(protocolos_licenca_ambiental))
  saveRDS(protocolos_licenca_ambiental, file.path(OUTPUT_DIR, "protocolos_licenca_ambiental.rds"))
if (!is.null(situacao_atual))
  saveRDS(situacao_atual, file.path(OUTPUT_DIR, "situacao_atual.rds"))

message(sprintf(
  "[07][parte2] copiados para %s: situacao_documental.rds (%d), eventos_classificados.rds (%d), protocolos_licenca_ambiental.rds (%s), situacao_atual.rds (%s)",
  OUTPUT_DIR, nrow(situacao_documental), nrow(eventos_classificados),
  if (!is.null(protocolos_licenca_ambiental)) nrow(protocolos_licenca_ambiental) else "ausente",
  if (!is.null(situacao_atual)) nrow(situacao_atual) else "ausente"

))

message("\n=== 07_proc_shiny_dossie.R — CONCLUIDO ===")

# NOTA: graficos_historico.R NAO e copiado para shiny_dashboard. O deploy no
# droplet sobe so "app.R" e "*.rds" (scp), entao qualquer .R solto aqui nunca
# chegaria ao servidor mesmo. As funcoes da Peca C estao embutidas direto no
# app.R (copia manual, documentada la) — R/graficos_historico.R continua
# sendo a fonte da verdade para o PIPELINE (usado pelo utils.R via source()),
# mas nao faz parte do que precisa ir para o shiny_dashboard/droplet.