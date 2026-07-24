################################################################################
# 06_serie_temporal.R
#
# Reconstrói a dimensão TEMPORAL do processo minerário, complementando o
# estado atual já calculado no 05 (que vem do PMA). Cinco produtos, um
# arquivo só, lendo direto dos parquets já filtrados/tipados pelo 04 — sem
# reler nenhum .txt bruto:
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(readr)
  library(arrow)
  library(here)
})

source(here::here("R", "utils.R"))

MICRO_OUT_DIR <- here::here("data", "result_db", "microdados")
OUT_DIR       <- here::here("data", "result_db", "serie_temporal")
QA_DIR        <- here::here("data", "_qa", "06_serie_temporal")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(QA_DIR,  recursive = TRUE, showWarnings = FALSE)

ler <- function(nome) arrow::read_parquet(file.path(MICRO_OUT_DIR, paste0(nome, ".parquet")))

HOJE <- Sys.Date()

# =============================================================================
# CLASSIFICADOR DE EVENTOS ADMINISTRATIVOS (dicionário completo, não amostra)
# =============================================================================
classificar_dicionario_eventos <- function(dic_evento) {

  dic <- dic_evento |>
    dplyr::mutate(
      idevento = as.character(idevento),
      abre_torna_sefeito = stringr::str_starts(dsevento, stringr::fixed("TORNA S/EFEITO")),
      tipo_proc = dplyr::if_else(
        abre_torna_sefeito | !stringr::str_detect(dsevento, "/"),
        NA_character_,
        stringr::str_trim(stringr::str_extract(dsevento, "^[^/]+"))
      ),
      resto = dplyr::if_else(
        abre_torna_sefeito | !stringr::str_detect(dsevento, "/"),
        dsevento,
        stringr::str_trim(stringr::str_remove(dsevento, "^[^/]+/"))
      ),
      sufixo = dplyr::case_when(
        stringr::str_ends(stringr::str_trim(dsevento), "PROTOC") ~ "PROTOC",
        stringr::str_ends(stringr::str_trim(dsevento), "PUBL")   ~ "PUBL",
        TRUE ~ "OUTRO"
      ),
      S = stringi::stri_trans_general(toupper(resto), "Latin-ASCII")
    )

  dic <- dic |>
    dplyr::mutate(
      papel = dplyr::case_when(
        # --- casos especiais confirmados (ver cabecalho) ---
        idevento == "1974" ~ "NEUTRO_processual",
        idevento == "1975" ~ "NEUTRO_processual",
        idevento == "318"  ~ "FECHA",
        idevento == "2138" ~ "RETOMA",

        # NOVO (2026-07-21): eventos de BARRAGEM (autorizacao de estrutura de
        # rejeito/dique) sao regidos por norma propria, independente da fase
        # do titulo minerario -- nao devem mudar fase. Achado real:
        # 886043/2017, "BARRAGENS REQUERIMENTO DEFERIDO PUBL" caia na regra
        # generica de REQUERIMENTO (abaixo) e zerava fase_atual pra NA,
        # fazendo o processo parecer "em tramitacao/pesquisa" quando na
        # verdade ja operava havia anos.
        stringr::str_detect(S, "BARRAGE") ~ "NEUTRO_barragem",

        # --- INSTAURACAO de processo administrativo (abertura de
        stringr::str_detect(S, "INSTAURA (PROC|PROCESSO)") ~ "NEUTRO_processual",

        # --- excecao: requerimento inicial ABRE a fase, mesmo sendo PROTOC ---
        sufixo == "PROTOC" & stringr::str_detect(S, "REQUERIMENTO") &
          !stringr::str_detect(S, "RECONSIDERA|CUMPRIMENTO|PROVENIENTE") ~ "MUDA_FASE",

        sufixo == "PROTOC" ~ "PROTOC",

        # --- NEUTRO: processual/financeiro/relatorio/transferencia/informativo ---
        stringr::str_detect(S, "\\bEXIGENCIA\\b") & !stringr::str_detect(S, "TORNA S/EFEITO") ~ "NEUTRO_processual",
        stringr::str_detect(S, "\\bTAH\\b") &
          !stringr::str_detect(S, "TORNA S/EFEITO|CADUCID|NULID|CANCEL") ~ "NEUTRO_financeiro",
        stringr::str_detect(S, "\\bRAL\\b") ~ "NEUTRO_relatorio",
        stringr::str_detect(S, "MULTA|AUTO (DE )?INFRACAO|DEBITO|PAGAMENTO|PARCELAMENTO|NOTIFICACAO ADM|VISTORIA") &
          !stringr::str_detect(S, "SUSPENS|CADUCID|CANCEL|NAO AUTORIZAD") ~ "NEUTRO_financeiro",
        stringr::str_detect(S, "COVID|RESOLUCAO 76/2021") ~ "NEUTRO_covid_prorrogacao",
        stringr::str_detect(S, "CESSAO (TOTAL|PARCIAL).*EFETIVADA|TRANSF DIREITOS") ~ "NEUTRO_transferencia_direitos",
        stringr::str_detect(S, "DECLARACAO DE APTIDAO EMITIDA|ASSENTIMENTO CDN AUTORIZADO") ~ "NEUTRO_marco_informativo",

        stringr::str_detect(S, "\\bNEGAD[OA]\\b") &
          stringr::str_detect(S, "SUSPENS|INTERDI|EMBARGO|DESEMBARGO|DESINTERDI|RECONSIDERA|ALVARA") ~ "NEUTRO_negado",

        # --- RETOMA: reversao efetivada de impedimento ---
        stringr::str_detect(S, "TORNA S/EFEITO") &
        stringr::str_detect(S, "SUSPENS|INTERDI|EMBARGO|CADUCID|NULID|CANCEL|REVOGA|INDEFER|ARQUIVA|CASSA|BAIXA") ~ "RETOMA",
        stringr::str_detect(S, "DESINTERDI") & !stringr::str_detect(S, "SOLICITA") ~ "RETOMA",
        stringr::str_detect(S, "DESEMBARGO") & !stringr::str_detect(S, "SOLICITA") ~ "RETOMA",
        stringr::str_detect(S, "DESBLOQUEAD") ~ "RETOMA",
        stringr::str_detect(S, "REVOGACAO.*SUSPENS") ~ "RETOMA",
        stringr::str_detect(S, "RETOMADA") ~ "RETOMA",

        # --- SUSPENDE: impedimento efetivado ---
        stringr::str_detect(S, "SUSPENSAO JUDICIAL") ~ "SUSPENDE",
        stringr::str_detect(S, "SUSPENSAO.*(AUTORIZAD|APLICAD)") | stringr::str_ends(stringr::str_trim(S), "SUSPENSA") ~ "SUSPENDE",
        stringr::str_detect(S, "INTERDICAO") & !stringr::str_detect(S, "DESINTERDI") ~ "SUSPENDE",
        stringr::str_detect(S, "\\bEMBARGO\\b") & !stringr::str_detect(S, "ARQUIVAMENTO") ~ "SUSPENDE",
        stringr::str_detect(S, "BLOQUEAD[AO] JUDICIALMENTE") ~ "SUSPENDE",

        # --- neutro: arquivamento de punicao libera, nao fecha o direito ---
        stringr::str_detect(S, "ARQUIVAMENTO (PROC ADM|AUTO DE (EMBARGO|INFRA))") ~ "NEUTRO_arquiv_punitivo",

        # --- FECHA: encerramento efetivo do direito ---
        stringr::str_detect(S, "CADUCID|CADUCADO|CADUCO") ~ "FECHA",
        stringr::str_detect(S, "NULIDADE|DECLARAD[OA] NUL[OA]") ~ "FECHA",
        stringr::str_detect(S, "CANCELAD[OA]|CANCELAMENTO") & !stringr::str_detect(S, "TORNA S/EFEITO") ~ "FECHA",
        stringr::str_detect(S, "\\bARQUIVAMENTO\\b") & !stringr::str_detect(S, "PROC ADM|AUTO DE") ~ "FECHA",
        stringr::str_detect(S, "REVOGACAO") & !stringr::str_detect(S, "SUSPENS|TORNA S/EFEITO") ~ "FECHA",
        stringr::str_detect(S, "INDEFERIMENTO") & !stringr::str_detect(S, "TORNA S/EFEITO") ~ "FECHA",
        stringr::str_detect(S, "RENUNCIA.*HOMOLOGAD|DESISTENCIA.*HOMOLOGAD") ~ "FECHA",
        stringr::str_detect(S, "CASSAD[OA]|CASSACAO") & !stringr::str_detect(S, "TORNA S/EFEITO") ~ "FECHA",
        stringr::str_detect(S, "\\bVENCID[OA]\\b") ~ "FECHA",
        stringr::str_detect(S, "EXTINCAO|EXTINT[OA]") ~ "FECHA",
        stringr::str_detect(S, "BAIXA TRANSCRI") & !stringr::str_detect(S, "TORNA S/EFEITO") ~ "FECHA",
        stringr::str_detect(S, "RENOVACAO.*PRAZO PLG") & stringr::str_detect(S, "INDEFERID") ~ "FECHA",

        # --- MUDA_FASE: cria ou renova a fase (agora DEPOIS do FECHA) ---
        stringr::str_detect(S, "ALVARA DE PESQUISA") & sufixo == "PUBL" &
          !stringr::str_detect(S, "INDEFERID|CANCELAD|VENCID|NUL[OA]") ~ "MUDA_FASE",
        stringr::str_detect(S, "PRORROGACAO.*PRAZO") & sufixo == "PUBL" &
          !stringr::str_detect(S, "INDEFERID|NEGAD") ~ "MUDA_FASE",
        stringr::str_detect(S, "ALVARA.*RENOVACAO|RENOVACAO.*ALVARA") & sufixo == "PUBL" &
          !stringr::str_detect(S, "INDEFERID|NEGAD|CANCELAD|NUL[OA]") ~ "MUDA_FASE",
        stringr::str_detect(S, "PERMISSAO LAVRA GARIMPEIRA|RENOVACAO.*PRAZO PLG|\\bPLG PUBLICADA\\b") &
          sufixo == "PUBL" & !stringr::str_detect(S, "CANCEL|NUL[OA]") ~ "MUDA_FASE",
        stringr::str_detect(S, "RELATORIO PESQUISA APROVADO") & sufixo == "PUBL" ~ "MUDA_FASE",
        stringr::str_detect(S, "REGISTRO DE EXTRACAO.*ANO") & sufixo == "PUBL" ~ "MUDA_FASE",

        # --- MUDA_FASE generico (publicacao/portaria/registro/GU) ---
        sufixo == "PUBL" & stringr::str_detect(S, "PUBLICAD[AO]|PORTARIA|REGISTRO.*AUTORIZAD|GU.*APROVAD|APROVAD[OA] PUBL$|LAVRA DECLARADA|AUTORIZADA PUBL$") ~ "MUDA_FASE",
        stringr::str_detect(S, "REQUERIMENTO") & sufixo != "PROTOC" ~ "MUDA_FASE",
        stringr::str_detect(S, "APTA PARA DISPONIBILIDADE|DISPONIBILIDADE PARA PESQUISA|EDITAL OFERTA PUBLICA|EDITAL.*DISPON|LEILAO ELETRONICO|AREA DISPONIVEL ART 26|AREA DESCARTADA") ~ "MUDA_FASE",

        TRUE ~ "NAO_CLASSIFICADO"
      ),
      categ_licenca    = stringr::str_detect(S, "LICENCA AMBIENTAL|PROTOCOLO ORGAO AMBIENTAL|ORGAO AMBIENTAL"),
      categ_gu         = stringr::str_detect(S, "GUIA (DE )?UTILIZACAO|\\bGU\\b"),
      categ_barragem   = stringr::str_detect(S, "BARRAGE"),
      categ_acidente   = stringr::str_detect(S, "ACIDENTE AMBIENTAL"),
      categ_espacial   = stringr::str_detect(S, "AREA INDIGENA|UNIDADE DE CONSERVA|AREA BLOQUEADA|FAIXA DE FRONTEIRA|FUNAI"),
      categ_judicial   = stringr::str_detect(S, "DECISAO JUDICIAL|SUSPENSAO JUDICIAL|BLOQUEAD[OA] JUDICIALMENTE|DESBLOQUEAD[OA] JUDICIALMENTE|PROTESTO JUDICIAL"),
      categ_oneracao   = stringr::str_detect(S, "ONERACAO DIREITOS|PENHOR|ARRESTO|CAUCAO"),
      categ_penalidade = stringr::str_detect(S, "MULTA|AUTO (DE )?INFRACAO|ADVERTENCIA")
    )

  # NAO_CLASSIFICADO tratado como neutro por padrao (decisao registrada) —
  dic |>
    dplyr::select(idevento, dsevento, tipo_proc, sufixo, papel, dplyr::starts_with("categ_"))
}

dic_evento_raw <- ler("micro_evento")
dic_classificado <- classificar_dicionario_eventos(dic_evento_raw)

readr::write_csv(dic_classificado, file.path(QA_DIR, "dicionario_eventos_classificado.csv"))
message(sprintf("[06] dicionario de eventos classificado: %d idevento distintos", nrow(dic_classificado)))
arrow::write_parquet(dic_classificado, file.path(OUT_DIR, "dicionario_eventos_classificado.parquet"))

# =============================================================================
# A) FASE/STATUS — maquina de estados a partir de ProcessoEvento
# =============================================================================

proc_evento <- ler("micro_processo_evento") |>
  dplyr::mutate(
    processo = as.character(processo),
    idevento = as.character(idevento),
    dtevento = as.Date(dtevento)
  ) |>
  dplyr::filter(!is.na(dtevento)) |>
  dplyr::left_join(dic_classificado, by = "idevento")

eventos_estado <- proc_evento |>
  dplyr::filter(papel %in% c("MUDA_FASE", "FECHA", "SUSPENDE", "RETOMA")) |>
  # AJUSTE (2026-07-21, achado real: 886238/2022): eventos com a MESMA data
  # (dtevento so tem granularidade de dia, sem hora) desempatavam por
  # idevento (comparacao de string) -- ordem arbitraria, sem relacao com a
  # ordem real dos atos. Padrao comum "revoga e reemite no mesmo dia" (ex:
  # Portaria X revoga a concessao, Portaria X+1 concede de novo, minutos
  # depois) ficava com o resultado invertido quando o numero do evento de
  # FECHA era MAIOR que o de MUDA_FASE. Novo criterio: processa FECHA/
  # SUSPENDE primeiro, MUDA_FASE/RETOMA por ultimo -- "abrir/retomar" sempre
  # vence como estado final do dia, batendo com o padrao real observado.
  # idevento continua como desempate final dentro do mesmo grupo de papel.
  dplyr::mutate(.prioridade_tie = dplyr::case_when(
    papel %in% c("FECHA", "SUSPENDE")   ~ 1L,
    papel %in% c("MUDA_FASE", "RETOMA") ~ 2L,
    TRUE                                 ~ 3L
  )) |>
  dplyr::arrange(processo, dtevento, .prioridade_tie, idevento) |>
  dplyr::select(-.prioridade_tie)
eventos_classificados <- eventos_estado |>
  dplyr::select(processo, dtevento, idevento, dsevento, tipo_proc, papel, dplyr::starts_with("categ_"))

arrow::write_parquet(eventos_classificados, file.path(OUT_DIR, "eventos_classificados.parquet"))
message(sprintf(
  "[06][A] eventos classificados (individuais, nao agregados): %d linhas em %d processos",
  nrow(eventos_classificados), dplyr::n_distinct(eventos_classificados$processo)
))

n_ignorados_fora_seq <- 0L

construir_linha_do_tempo <- function(eventos_proc) {
  proc_id <- eventos_proc$processo[1]
  linhas <- list()

  dt_ini <- as.Date(NA); fase_atual <- NA_character_; status_atual <- NA_character_
  n_ignorados_local <- 0L

  for (i in seq_len(nrow(eventos_proc))) {
    ev <- eventos_proc[i, ]
    dt <- ev$dtevento; papel <- ev$papel; tp_proc <- ev$tipo_proc

    if (papel == "MUDA_FASE") {
      if (!is.na(dt_ini) && dt > dt_ini) {
        linhas[[length(linhas) + 1]] <- tibble::tibble(
          processo = proc_id, dt_inicio = dt_ini, dt_fim = dt - 1, fase = fase_atual, status = status_atual)
      }
      dt_ini <- dt; fase_atual <- tp_proc; status_atual <- "ATIVA"

    } else if (papel == "FECHA" && !is.na(dt_ini)) {
      if (dt > dt_ini) {
        linhas[[length(linhas) + 1]] <- tibble::tibble(
          processo = proc_id, dt_inicio = dt_ini, dt_fim = dt - 1, fase = fase_atual, status = status_atual)
      }
      linhas[[length(linhas) + 1]] <- tibble::tibble(
        processo = proc_id, dt_inicio = dt, dt_fim = dt, fase = fase_atual, status = "ENCERRADA")
      dt_ini <- as.Date(NA); fase_atual <- NA_character_; status_atual <- NA_character_

    } else if (papel == "SUSPENDE" && !is.na(dt_ini) && status_atual == "ATIVA") {
      if (dt > dt_ini) {
        linhas[[length(linhas) + 1]] <- tibble::tibble(
          processo = proc_id, dt_inicio = dt_ini, dt_fim = dt - 1, fase = fase_atual, status = status_atual)
      }
      dt_ini <- dt; status_atual <- "SUSPENSA"

    } else if (papel == "RETOMA" && !is.na(dt_ini) && status_atual == "SUSPENSA") {
      if (dt > dt_ini) {
        linhas[[length(linhas) + 1]] <- tibble::tibble(
          processo = proc_id, dt_inicio = dt_ini, dt_fim = dt - 1, fase = fase_atual, status = status_atual)
      }
      dt_ini <- dt; status_atual <- "ATIVA"

    } else if (papel %in% c("SUSPENDE", "RETOMA")) {
      n_ignorados_local <<- n_ignorados_local + 1L
    }
  }

  if (!is.na(dt_ini)) {
    linhas[[length(linhas) + 1]] <- tibble::tibble(
      processo = proc_id, dt_inicio = dt_ini, dt_fim = as.Date(NA), fase = fase_atual, status = status_atual)
  }

  n_ignorados_fora_seq <<- n_ignorados_fora_seq + n_ignorados_local
  if (length(linhas) == 0) return(NULL)
  df_linhas <- dplyr::bind_rows(linhas)
  linhas_finais <- list()
  for (k in seq_len(nrow(df_linhas))) {
    l_atual <- df_linhas[k, ]

    if (k == 1 && !is.na(l_atual$dt_inicio)) {
      linhas_finais[[length(linhas_finais) + 1]] <- tibble::tibble(
        processo = proc_id, dt_inicio = as.Date(NA), dt_fim = l_atual$dt_inicio - 1,
        fase = "SEM REGISTRO", status = "PRE_AUTORIZACAO")
    }

    if (k > 1) {
      l_ant <- df_linhas[k - 1, ]
      if (!is.na(l_ant$dt_fim) && !is.na(l_atual$dt_inicio) && l_atual$dt_inicio > l_ant$dt_fim + 1) {
        status_gap <- if (l_ant$status == "ENCERRADA") "ENCERRADA" else "GAP"
        linhas_finais[[length(linhas_finais) + 1]] <- tibble::tibble(
          processo = proc_id, dt_inicio = l_ant$dt_fim + 1, dt_fim = l_atual$dt_inicio - 1,
          fase = l_ant$fase, status = status_gap)
      }
    }
    linhas_finais[[length(linhas_finais) + 1]] <- l_atual
  }
  dplyr::bind_rows(linhas_finais)
}

serie_fase_status <- eventos_estado |>
  dplyr::group_by(processo) |>
  dplyr::group_split() |>
  lapply(construir_linha_do_tempo) |>
  dplyr::bind_rows() |>
  dplyr::arrange(processo, dplyr::coalesce(dt_inicio, as.Date("1900-01-01")))

message(sprintf(
  "[06][A] serie fase/status: %d processos | %d linhas | eventos SUSPENDE/RETOMA fora de sequencia (ignorados): %d",
  dplyr::n_distinct(serie_fase_status$processo), nrow(serie_fase_status), n_ignorados_fora_seq
))

arrow::write_parquet(serie_fase_status, file.path(OUT_DIR, "serie_fase_status.parquet"))

# =============================================================================
# B) VIGÊNCIA SEQUENCIAL GENÉRICA — titular, substância, associação
# =============================================================================
reconstruir_vigencia_sequencial <- function(df, id_cols, dt_inicio_col, dt_fim_col) {
  df |>
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)), .data[[dt_inicio_col]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::mutate(
      dt_fim_seq = dplyr::coalesce(
        .data[[dt_fim_col]],
        dplyr::lead(.data[[dt_inicio_col]]) - 1
      ),
      vigente_hoje = dt_fim_seq |> is.na() | dt_fim_seq >= HOJE
    ) |>
    dplyr::ungroup()
}

# --- B1) Titular (papel de titularidade resolvido dinamicamente, sem hardcode) ---
tipo_relacao <- ler("micro_tipo_relacao")
ids_titular <- tipo_relacao |>
  dplyr::filter(stringr::str_detect(toupper(dstiporelacao), "TITULAR")) |>
  dplyr::pull(idtiporelacao)

proc_pessoa <- ler("micro_processo_pessoa")
pessoa      <- ler("micro_pessoa")

vig_titular <- proc_pessoa |>
  dplyr::filter(idtiporelacao %in% ids_titular) |>
  dplyr::mutate(
    dtiniciovigencia = as.Date(dtiniciovigencia),
    dtfimvigencia     = as.Date(dtfimvigencia)
  ) |>
  dplyr::filter(!is.na(dtiniciovigencia)) |>
  reconstruir_vigencia_sequencial(
    id_cols = c("processo", "idpessoa", "idtiporelacao"),
    dt_inicio_col = "dtiniciovigencia", dt_fim_col = "dtfimvigencia"
  ) |>
  dplyr::left_join(pessoa |> dplyr::select(idpessoa, nrcpfcnpj, nmpessoa, tppessoa), by = "idpessoa") |>
  dplyr::select(processo, idpessoa, nrcpfcnpj, nmpessoa, tppessoa,
               dt_inicio = dtiniciovigencia, dt_fim = dt_fim_seq, vigente_hoje)

message(sprintf("[06][B1] vigencia titular: %d processos | %d linhas | vigentes hoje: %d",
                dplyr::n_distinct(vig_titular$processo), nrow(vig_titular), sum(vig_titular$vigente_hoje)))
arrow::write_parquet(vig_titular, file.path(OUT_DIR, "serie_titular.parquet"))

# --- B2) Substância ---
substancia <- ler("micro_substancia")

vig_substancia <- ler("micro_processo_substancia") |>
  dplyr::mutate(
    dtiniciovigencia = as.Date(dtiniciovigencia),
    dtfimvigencia     = as.Date(dtfimvigencia)
  ) |>
  dplyr::filter(!is.na(dtiniciovigencia)) |>
  reconstruir_vigencia_sequencial(
    id_cols = c("processo", "idsubstancia"),
    dt_inicio_col = "dtiniciovigencia", dt_fim_col = "dtfimvigencia"
  ) |>
  dplyr::left_join(substancia |> dplyr::select(idsubstancia, nmsubstancia), by = "idsubstancia") |>
  dplyr::select(processo, idsubstancia, nmsubstancia,
               dt_inicio = dtiniciovigencia, dt_fim = dt_fim_seq, vigente_hoje)

message(sprintf("[06][B2] vigencia substancia: %d processos | %d linhas | vigentes hoje: %d",
                dplyr::n_distinct(vig_substancia$processo), nrow(vig_substancia), sum(vig_substancia$vigente_hoje)))
arrow::write_parquet(vig_substancia, file.path(OUT_DIR, "serie_substancia.parquet"))

# --- B3) Associação (fusão/reaproveitamento de processo) ---
tipo_associacao <- ler("micro_tipo_associacao")

vig_associacao <- ler("micro_processo_associacao") |>
  dplyr::mutate(
    processo_associado = limpar_dsprocesso(dsprocessoassociado),
    dtassociacao   = as.Date(dtassociacao),
    dtdesassociacao = as.Date(dtdesassociacao)
  ) |>
  dplyr::filter(!is.na(dtassociacao)) |>
  reconstruir_vigencia_sequencial(
    id_cols = c("processo"), dt_inicio_col = "dtassociacao", dt_fim_col = "dtdesassociacao"
  ) |>
  dplyr::left_join(tipo_associacao |> dplyr::select(idtipoassociacao, dstipoassociacao), by = "idtipoassociacao") |>
  dplyr::select(processo, processo_associado, dstipoassociacao, obassociacao,
               dt_inicio = dtassociacao, dt_fim = dt_fim_seq, vigente_hoje)

message(sprintf("[06][B3] vigencia associacao: %d processos | %d linhas | vigentes hoje: %d",
                dplyr::n_distinct(vig_associacao$processo), nrow(vig_associacao), sum(vig_associacao$vigente_hoje)))
arrow::write_parquet(vig_associacao, file.path(OUT_DIR, "serie_associacao.parquet"))

# =============================================================================
# C) ACESSÓRIOS COM DATA — licença ambiental, GU, barragem
# =============================================================================
acessorios_ambiental <- proc_evento |>
  dplyr::filter(categ_licenca | categ_gu | categ_barragem) |>
  tidyr::pivot_longer(
    cols = c(categ_licenca, categ_gu, categ_barragem),
    names_to = "categoria", values_to = "bate"
  ) |>
  dplyr::filter(bate) |>
  dplyr::mutate(categoria = dplyr::recode(categoria,
    categ_licenca = "ambiental_licenca",
    categ_gu = "ambiental_gu",
    categ_barragem = "ambiental_barragem"
  )) |>
  dplyr::group_by(processo, categoria) |>
  dplyr::summarise(
    dt_primeiro_protocolo = min(dtevento, na.rm = TRUE),
    dt_ultimo_protocolo   = max(dtevento, na.rm = TRUE),
    n_protocolos          = dplyr::n(),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    id_cols = processo,
    names_from = categoria,
    values_from = c(dt_primeiro_protocolo, dt_ultimo_protocolo, n_protocolos),
    names_glue = "{categoria}_{.value}"
  )

message(sprintf("[06][C] acessorios ambiental/juridico: %d processos com algum protocolo", nrow(acessorios_ambiental)))
arrow::write_parquet(acessorios_ambiental, file.path(OUT_DIR, "acessorios_ambiental.parquet"))

protocolos_licenca_ambiental <- proc_evento |>
  dplyr::filter(categ_licenca, papel == "PROTOC") |>
  dplyr::select(processo, dt_protocolo = dtevento, idevento, dsevento) |>
  dplyr::arrange(processo, dt_protocolo)

message(sprintf(
  "[06][C] protocolos de licenca ambiental (granular, nao agregado): %d eventos | %d processos distintos",
  nrow(protocolos_licenca_ambiental), dplyr::n_distinct(protocolos_licenca_ambiental$processo)
))
arrow::write_parquet(protocolos_licenca_ambiental, file.path(OUT_DIR, "protocolos_licenca_ambiental.parquet"))

# =============================================================================
# D) SITUAÇÃO DOCUMENTAL — ProcessoTitulo, direto
# =============================================================================
tipo_doc_legal   <- ler("micro_tipo_documento_legal")
situacao_doc_leg <- ler("micro_situacao_documento_legal")

situacao_documental <- ler("micro_processo_titulo") |>
  dplyr::mutate(
    dtpublicacao = as.Date(dtpublicacao),
    dtvencimento = as.Date(dtvencimento)
  ) |>
  dplyr::left_join(tipo_doc_legal,   by = "idtipodocumentolegal") |>
  dplyr::left_join(situacao_doc_leg, by = "idsituacaodocumentolegal") |>
  dplyr::mutate(
    # AJUSTE (2026-07-21): renomeado de "sem_data_vencimento" para
    # "sem_data_a_revisar" -- a cascata de motivo_nao_apto (Bloco F) checa
    # literalmente essa string; com o nome antigo, a comparacao nunca batia
    # e QUALQUER titulo sem dtvencimento (nao so Concessao de Lavra) caia
    # direto em "titulo_vencido" por engano.
    status_vencimento = dplyr::case_when(
      is.na(dtvencimento) ~ "sem_data_a_revisar",
      dtvencimento < HOJE  ~ "vencido",
      TRUE                 ~ "vigente"
    )
  ) |>
  dplyr::select(processo, nrtitulo, dstipodocumentolegal, dssituacaodocumentolegal,
               dt_publicacao = dtpublicacao, dt_vencimento = dtvencimento, status_vencimento)

message(sprintf("[06][D] situacao documental: %d processos | vencidos: %d | vigentes: %d | sem data: %d",
                dplyr::n_distinct(situacao_documental$processo),
                sum(situacao_documental$status_vencimento == "vencido"),
                sum(situacao_documental$status_vencimento == "vigente"),
                sum(situacao_documental$status_vencimento == "sem_data_a_revisar")))
arrow::write_parquet(situacao_documental, file.path(OUT_DIR, "situacao_documental.parquet"))

# =============================================================================
# E) PROPRIEDADE DO SOLO — junção direta, sem reconstrução (pode ser 1:N)
# =============================================================================
condicao_solo <- ler("micro_condicao_propriedade_solo")

propriedade_solo <- ler("micro_processo_propriedade_solo") |>
  dplyr::left_join(condicao_solo, by = "idcondicaopropriedadesolo") |>
  dplyr::select(processo, idcondicaopropriedadesolo, dscondicaopropriedadesolo)

n_multi_condicao <- propriedade_solo |> dplyr::count(processo) |> dplyr::filter(n > 1) |> nrow()
message(sprintf("[06][E] propriedade do solo: %d processos | com mais de 1 condicao: %d",
                dplyr::n_distinct(propriedade_solo$processo), n_multi_condicao))
arrow::write_parquet(propriedade_solo, file.path(OUT_DIR, "propriedade_solo.parquet"))

# =============================================================================
# G) PROTEÇÃO ART. 211/213 (Portaria DNPM 155/2016) — RENOVAÇÃO DE PLG
# =============================================================================
eventos_renovacao_plg <- proc_evento |>
  dplyr::filter(idevento %in% c("521", "522")) |>
  dplyr::transmute(
    processo, idevento, dtevento,
    tipo_evento_211_213 = dplyr::if_else(idevento == "521", "protocolo_renovacao", "indeferimento_renovacao")
  ) |>
  dplyr::arrange(processo, dtevento)

message(sprintf(
  "[06][G] eventos de renovacao PLG (Art. 211/213): %d protocolos (521) | %d indeferimentos (522) | %d processos distintos",
  sum(eventos_renovacao_plg$idevento == "521"),
  sum(eventos_renovacao_plg$idevento == "522"),
  dplyr::n_distinct(eventos_renovacao_plg$processo)
))
arrow::write_parquet(eventos_renovacao_plg, file.path(OUT_DIR, "eventos_renovacao_plg_211_213.parquet"))

protecao_211_213_por_processo <- eventos_renovacao_plg |>
  dplyr::group_by(processo) |>
  dplyr::summarise(
    dt_protocolo_renovacao_plg = suppressWarnings(max(dtevento[idevento == "521"], na.rm = TRUE)),
    tem_indeferimento_renovacao_plg = any(
      idevento == "522" &
        dtevento > suppressWarnings(min(dtevento[idevento == "521"], na.rm = TRUE))
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(dt_protocolo_renovacao_plg = dplyr::if_else(
    is.infinite(dt_protocolo_renovacao_plg), as.Date(NA), dt_protocolo_renovacao_plg
  ))

# =============================================================================
# H) CLASSIFICACAO JUDICIAL DE "DESPACHO DIVERSO PUBL" (achado 2026-07)
# =============================================================================
ideventos_despacho_diverso <- dic_classificado |>
  dplyr::filter(stringr::str_detect(
    stringi::stri_trans_general(toupper(dsevento), "Latin-ASCII"),
    "DESPACHO DIVERSO PUBL$"
  )) |>
  dplyr::pull(idevento)

eventos_judicial_despacho_diverso <- proc_evento |>
  dplyr::filter(idevento %in% ideventos_despacho_diverso) |>
  dplyr::mutate(
    publ_norm = if ("dspublicacaodou" %in% names(proc_evento))
      stringi::stri_trans_general(toupper(dspublicacaodou), "Latin-ASCII")
      else NA_character_,

    categ_judicial_despacho = dplyr::case_when(
      is.na(publ_norm) ~ NA_character_,
      stringr::str_detect(publ_norm, "SUSPEN\\w* (DO |DA |DE )?(TITULO|EFEITOS|LAVRA)") ~ "suspensao_titulo_lavra_judicial",
      stringr::str_detect(publ_norm, "SUSPEN\\w* D[AE] ANALISE|SOBRESTA") ~ "suspensao_analise_ou_sobrestamento",
      stringr::str_detect(publ_norm, "TERMINO D[AE] SUSPEN") ~ "termino_suspensao",
      stringr::str_detect(publ_norm, "ANULAR?.*CADUCID|TUTELA ANTECIPADA.*CADUCID") ~ "anulacao_caducidade_judicial",
      stringr::str_detect(publ_norm, "(SUSPEN\\w*|BLOQUEIO).*DISPONIBILIDADE|TRAMITE.*DISPONIBILIDADE.*JUDICIAL") ~ "bloqueio_disponibilidade_leilao",
      stringr::str_detect(publ_norm, "NEGA DEFESA.*(AUTO DE )?INFRA|MANT[EE]M.*AUTO DE INFRA") ~ "defesa_infracao_mantida",
      stringr::str_detect(publ_norm, "DECISAO JUDICIAL|ORDEM JUDICIAL|TUTELA ANTECIPADA") ~ "decisao_judicial_outra",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(categ_judicial_despacho)) |>
  dplyr::select(processo, dtevento, idevento, dsevento, categ_judicial_despacho, dspublicacaodou) |>
  dplyr::arrange(processo, dtevento)

message(sprintf(
  "[06][H] classificacao judicial de DESPACHO DIVERSO PUBL: %d eventos identificados em %d processos",
  nrow(eventos_judicial_despacho_diverso), dplyr::n_distinct(eventos_judicial_despacho_diverso$processo)
))
print(eventos_judicial_despacho_diverso |> dplyr::count(categ_judicial_despacho, sort = TRUE))

arrow::write_parquet(eventos_judicial_despacho_diverso, file.path(OUT_DIR, "eventos_judicial_despacho_diverso.parquet"))

# =============================================================================
# J) GU (GUIA DE UTILIZAÇÃO) PARA AUT PESQ (achado 2026-07-21)
# =============================================================================
# Autorizacao de Pesquisa pode legalmente declarar CFEM quando tem Guia de
# Utilizacao valida -- sem isso, toda declaracao em AUT PESQ caia em
# "fase_de_tramitacao_ou_pesquisa" (nao apto), mesmo quando a comercializacao
# era autorizada. construir_intervalos_gu() (utils.R) reconstroi as janelas
# de validade a partir de AUTORIZADA/APROVADO (regex "Validade:DD/MM/AAAA" na
# publicacao, com fallback de duracao 1/2/3 anos), truncando por CANCELADA/
# SUSPENSA -- mesmo raciocinio do art. 211/213 acima, aplicado a GU.
eventos_gu_aut_pesq <- proc_evento |>
  dplyr::filter(categ_gu, tipo_proc == "AUT PESQ") |>
  dplyr::transmute(
    processo, dtevento, idevento, dsevento,
    publicacao = if ("dspublicacaodou" %in% names(proc_evento)) dspublicacaodou else NA_character_
  )

intervalos_gu_aut_pesq <- construir_intervalos_gu(eventos_gu_aut_pesq)

message(sprintf(
  "[06][J] GU/AUT PESQ: %d eventos de GU | %d processos com pelo menos 1 janela de validade reconstruida",
  nrow(eventos_gu_aut_pesq), dplyr::n_distinct(intervalos_gu_aut_pesq$processo)
))
arrow::write_parquet(intervalos_gu_aut_pesq, file.path(OUT_DIR, "intervalos_gu_aut_pesq.parquet"))

gu_valida_hoje_por_processo <- intervalos_gu_aut_pesq |>
  dplyr::filter(xmin <= HOJE, HOJE <= xmax) |>
  dplyr::distinct(processo) |>
  dplyr::mutate(gu_valida_hoje = TRUE)

# =============================================================================
# F) SITUAÇÃO ATUAL
fase_atual <- serie_fase_status |>
  dplyr::group_by(processo) |>
  dplyr::slice_max(dt_inicio, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(processo, fase_evento = fase, status_evento = status, dt_ultimo_evento_fase = dt_inicio)

# --- fase segundo o PMA (05) — fonte independente, coluna à parte, nunca
pma_full <- load_ckpt("05_pma_full") |> as.data.frame()
fase_pma_df <- pma_full |>
  dplyr::transmute(processo = as.character(PROCESSO), fase_pma = FASE) |>
  dplyr::distinct(processo, .keep_all = TRUE)

mapa_fase_evento_pma <- c(
  "CONC LAV"    = "CONCESSAO DE LAVRA",
  "PLG"         = "LAVRA GARIMPEIRA",
  "REQ PLG"     = "REQUERIMENTO DE LAVRA GARIMPEIRA",
  "AUT PESQ"    = "AUTORIZACAO DE PESQUISA",
  "REQ PESQ"    = "REQUERIMENTO DE PESQUISA",
  "REQ LAV"     = "REQUERIMENTO DE LAVRA",
  "LICEN"       = "LICENCIAMENTO",
  "REQ LICEN"   = "REQUERIMENTO DE LICENCIAMENTO",
  "REG EXT"     = "REGISTRO DE EXTRACAO",
  "REQ EXT"     = "REQUERIMENTO DE REGISTRO DE EXTRACAO",
  "DISPONIB"    = "DISPONIBILIDADE",
  "DIR REQ LAV" = "DIREITO DE REQUERER A LAVRA",
  "APTO DISP"   = "APTO PARA DISPONIBILIDADE"
)
normaliza_fase <- function(x) stringi::stri_trans_general(toupper(trimws(x)), "Latin-ASCII")

crosstab_fase_bruto <- fase_atual |>
  dplyr::left_join(fase_pma_df, by = "processo") |>
  dplyr::count(fase_evento, fase_pma, sort = TRUE)
readr::write_csv(crosstab_fase_bruto, file.path(QA_DIR, "crosstab_fase_evento_x_fase_pma.csv"))

# --- título mais recente por processo (Bloco D) ---
doc_atual <- situacao_documental |>
  dplyr::filter(!is.na(dt_publicacao)) |>
  dplyr::group_by(processo) |>
  dplyr::slice_max(dt_publicacao, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(processo, status_vencimento, dt_vencimento)

doc_sem_data <- situacao_documental |>
  dplyr::filter(is.na(dt_publicacao)) |>
  dplyr::distinct(processo) |>
  dplyr::anti_join(doc_atual, by = "processo") |>
  dplyr::mutate(status_vencimento = "sem_data_a_revisar", dt_vencimento = as.Date(NA))

doc_atual_completo <- dplyr::bind_rows(doc_atual, doc_sem_data)

# --- licença ambiental (Bloco C) ---

lic_amb <- acessorios_ambiental |>
  { \(df) if (!"ambiental_licenca_dt_primeiro_protocolo" %in% names(df))
      dplyr::mutate(df,
                    ambiental_licenca_dt_primeiro_protocolo = as.Date(NA),
                    ambiental_licenca_dt_ultimo_protocolo   = as.Date(NA),
                    ambiental_licenca_n_protocolos          = NA_integer_)
    else df }() |>
  dplyr::select(processo,
               dt_protocolo_licenca_ambiental       = ambiental_licenca_dt_primeiro_protocolo,
               dt_ultimo_protocolo_licenca_ambiental = ambiental_licenca_dt_ultimo_protocolo,
               n_protocolos_licenca_ambiental        = ambiental_licenca_n_protocolos)

# --- titular atual (Bloco B1) — sem co-titularidade ---
titular_atual_df <- vig_titular |>
  dplyr::filter(vigente_hoje) |>
  dplyr::group_by(processo) |>
  dplyr::arrange(dplyr::desc(dt_inicio)) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(processo, titular_atual = nmpessoa, nrcpfcnpj_titular_atual = nrcpfcnpj)

FASES_QUE_OPERAM <- c("CONC LAV", "LICEN", "PLG", "REG EXT")

situacao_atual <- fase_atual |>
  dplyr::left_join(fase_pma_df,        by = "processo") |>
  dplyr::left_join(doc_atual_completo, by = "processo") |>
  dplyr::left_join(lic_amb,            by = "processo") |>
  dplyr::left_join(titular_atual_df,   by = "processo") |>
  dplyr::left_join(protecao_211_213_por_processo, by = "processo") |>
  dplyr::left_join(gu_valida_hoje_por_processo,    by = "processo") |>
  dplyr::mutate(
    fase_evento_traduzida = dplyr::coalesce(mapa_fase_evento_pma[fase_evento], fase_evento),
    fase_diverge_pma = normaliza_fase(fase_evento_traduzida) != normaliza_fase(fase_pma),
    nunca_protocolou_lic_amb = is.na(dt_protocolo_licenca_ambiental),
    gu_valida_hoje = tidyr::replace_na(gu_valida_hoje, FALSE),
    # AJUSTE (2026-07-21): Concessao de Lavra e por prazo INDETERMINADO --
    # nao existe vencimento nesse regime (diferente de PLG/Aut. Pesquisa,
    # que tem prazo fixo e exigem renovacao). Por isso dtvencimento fica
    # sempre NA pra CONC LAV, e sem essa isencao caia em "titulo_vencido"
    # por engano (achado real: 886043/2017 e outros em CONC LAV).
    titulo_ok_vencimento = fase_evento %in% "CONC LAV" | status_vencimento == "vigente",
    protegido_211_213 = !titulo_ok_vencimento &
      !is.na(dt_protocolo_renovacao_plg) &
      !is.na(dt_vencimento) &
      dt_protocolo_renovacao_plg <= dt_vencimento &
      !tem_indeferimento_renovacao_plg,

    # AJUSTE (2026-07-21): Autorizacao de Pesquisa com GU (Guia de
    # Utilizacao) valida pode legalmente declarar CFEM -- ver Bloco J acima.
    # Checado ANTES da regra generica de FASES_QUE_OPERAM, pra nao passar
    # por "fase_de_tramitacao_ou_pesquisa" mesmo com GU valida.
    aut_pesq_com_gu = fase_evento %in% "AUT PESQ" & gu_valida_hoje,

    apto_operar = dplyr::case_when(
      aut_pesq_com_gu                       ~ "TRUE",
      !(fase_evento %in% FASES_QUE_OPERAM) ~ "em_analise",
      status_evento != "ATIVA"              ~ "FALSE",
      nunca_protocolou_lic_amb              ~ "FALSE",
      !titulo_ok_vencimento                 ~ "FALSE",
      TRUE                                   ~ "TRUE"
    ),
    motivo_nao_apto = dplyr::case_when(
      apto_operar == "TRUE"                 ~ NA_character_,
      !(fase_evento %in% FASES_QUE_OPERAM)  ~ "fase_de_tramitacao_ou_pesquisa",
      status_evento != "ATIVA"              ~ "suspensa_ou_encerrada",
      nunca_protocolou_lic_amb              ~ "sem_licenca_ambiental_previa",
      !titulo_ok_vencimento & status_vencimento == "sem_data_a_revisar" ~ "vencimento_sem_data_a_revisar",
      !titulo_ok_vencimento & protegido_211_213                        ~ "titulo_vencido_renovacao_protocolada",
      !titulo_ok_vencimento                                            ~ "titulo_vencido",
      TRUE                                   ~ NA_character_
    ),

    # --- FLAGS BINARIAS INDEPENDENTES
    flag_fase_nao_operacional                  = !(fase_evento %in% FASES_QUE_OPERAM) & !aut_pesq_com_gu,
    flag_status_nao_ativo                      = status_evento != "ATIVA",
    flag_sem_licenca_ambiental_previa          = (fase_evento %in% FASES_QUE_OPERAM) & nunca_protocolou_lic_amb,
    flag_vencimento_sem_data_a_revisar         = !titulo_ok_vencimento & status_vencimento == "sem_data_a_revisar",
    flag_titulo_vencido                        = !titulo_ok_vencimento & status_vencimento != "sem_data_a_revisar" & !protegido_211_213,
    flag_titulo_vencido_renovacao_protocolada  = !titulo_ok_vencimento & status_vencimento != "sem_data_a_revisar" & protegido_211_213
  ) |>
  dplyr::select(processo, fase_evento, fase_pma, fase_diverge_pma, status_evento, dt_ultimo_evento_fase,
               status_vencimento, dt_vencimento, dt_protocolo_licenca_ambiental,
               dt_ultimo_protocolo_licenca_ambiental, n_protocolos_licenca_ambiental,
               dt_protocolo_renovacao_plg, tem_indeferimento_renovacao_plg, protegido_211_213,
               gu_valida_hoje, aut_pesq_com_gu,
               titular_atual, nrcpfcnpj_titular_atual, apto_operar, motivo_nao_apto,
               flag_fase_nao_operacional, flag_status_nao_ativo, flag_sem_licenca_ambiental_previa,
               flag_titulo_vencido, flag_titulo_vencido_renovacao_protocolada, flag_vencimento_sem_data_a_revisar)

resumo_apto <- situacao_atual |> dplyr::count(apto_operar, motivo_nao_apto, sort = TRUE)
readr::write_csv(resumo_apto, file.path(QA_DIR, "resumo_apto_operar.csv"))

# --- CHECK DE CONSISTENCIA
divergencias_motivo_flag <- situacao_atual |>
  dplyr::filter(!is.na(motivo_nao_apto)) |>
  dplyr::mutate(
    flag_correspondente = dplyr::case_when(
      motivo_nao_apto == "fase_de_tramitacao_ou_pesquisa"       ~ flag_fase_nao_operacional,
      motivo_nao_apto == "suspensa_ou_encerrada"                ~ flag_status_nao_ativo,
      motivo_nao_apto == "sem_licenca_ambiental_previa"         ~ flag_sem_licenca_ambiental_previa,
      motivo_nao_apto == "vencimento_sem_data_a_revisar"        ~ flag_vencimento_sem_data_a_revisar,
      motivo_nao_apto == "titulo_vencido_renovacao_protocolada" ~ flag_titulo_vencido_renovacao_protocolada,
      motivo_nao_apto == "titulo_vencido"                       ~ flag_titulo_vencido,
      TRUE ~ NA
    )
  ) |>
  dplyr::filter(!flag_correspondente)

if (nrow(divergencias_motivo_flag) > 0) {
  readr::write_csv(divergencias_motivo_flag, file.path(QA_DIR, "ALERTA_divergencia_motivo_x_flag.csv"))
  warning(sprintf(
    "[06] ALERTA: %d processos com motivo_nao_apto sem a flag binaria correspondente marcada TRUE! Ver %s.",
    nrow(divergencias_motivo_flag), file.path(QA_DIR, "ALERTA_divergencia_motivo_x_flag.csv")
  ))
} else {
  message("[06] validacao OK: motivo_nao_apto categorico bate com a flag binaria de prioridade mais alta em todos os processos.")
}

message(sprintf(
  "[06][G] protecao Art. 211/213: %d processos vencidos protegidos por renovacao protocolada pendente",
  sum(situacao_atual$flag_titulo_vencido_renovacao_protocolada, na.rm = TRUE)
))

message("[06][F] situacao atual (foto de hoje):")
print(situacao_atual |> dplyr::count(apto_operar, sort = TRUE))
message(sprintf("[06][F] processos com vencimento sem_data_a_revisar: %d",
                sum(situacao_atual$status_vencimento == "sem_data_a_revisar", na.rm = TRUE)))
message(sprintf(
  "[06][F] fase_evento diverge de fase_pma (apos traducao/normalizacao): %d de %d | crosstab bruto em: %s",
  sum(situacao_atual$fase_diverge_pma, na.rm = TRUE), nrow(situacao_atual),
  file.path(QA_DIR, "crosstab_fase_evento_x_fase_pma.csv")
))

arrow::write_parquet(situacao_atual, file.path(OUT_DIR, "situacao_atual.parquet"))

# =============================================================================
# I) "HISTORICO DE PENALIDADES E OCORRENCIAS" (Fase 3) — consolida 3 fontes
# =============================================================================
tem_dspub <- "dspublicacaodou" %in% names(proc_evento)

penalidades_1 <- proc_evento |>
  dplyr::filter(categ_penalidade) |>
  dplyr::mutate(
    S_local = stringi::stri_trans_general(toupper(dsevento), "Latin-ASCII"),
    tipo = dplyr::case_when(
      stringr::str_detect(S_local, "MULTA") ~ "Multa",
      stringr::str_detect(S_local, "AUTO (DE )?INFRACAO") ~ "Infração",
      TRUE ~ "Advertência"
    ),
    fonte = "categ_penalidade",
    dspublicacaodou = if (tem_dspub) dspublicacaodou else NA_character_
  ) |>
  dplyr::select(processo, dtevento, idevento, dsevento, tipo, fonte, dspublicacaodou)

penalidades_2 <- proc_evento |>
  dplyr::filter(papel %in% c("SUSPENDE", "RETOMA")) |>
  dplyr::mutate(
    tipo = dplyr::if_else(papel == "SUSPENDE", "Suspensão/Embargo", "Retomada/Desembargo"),
    fonte = "papel_administrativo",
    dspublicacaodou = if (tem_dspub) dspublicacaodou else NA_character_
  ) |>
  dplyr::select(processo, dtevento, idevento, dsevento, tipo, fonte, dspublicacaodou)

penalidades_3 <- eventos_judicial_despacho_diverso |>
  dplyr::mutate(
    tipo = dplyr::case_when(
      categ_judicial_despacho == "suspensao_titulo_lavra_judicial"    ~ "Suspensão judicial (título/lavra)",
      categ_judicial_despacho == "suspensao_analise_ou_sobrestamento" ~ "Suspensão/sobrestamento judicial (análise)",
      categ_judicial_despacho == "termino_suspensao"                  ~ "Término de suspensão judicial",
      categ_judicial_despacho == "anulacao_caducidade_judicial"       ~ "Anulação judicial de caducidade",
      categ_judicial_despacho == "bloqueio_disponibilidade_leilao"    ~ "Bloqueio judicial de leilão/disponibilidade",
      categ_judicial_despacho == "defesa_infracao_mantida"            ~ "Infração mantida (defesa negada)",
      TRUE ~ "Decisão judicial (outra)"
    ),
    fonte = "judicial_despacho_diverso"
  ) |>
  dplyr::select(processo, dtevento, idevento, dsevento, tipo, fonte, dspublicacaodou)

eventos_penalidades_ocorrencias <- dplyr::bind_rows(penalidades_1, penalidades_2, penalidades_3) |>
  dplyr::arrange(processo, dtevento)

message(sprintf(
  "[06][I] historico de penalidades/ocorrencias: %d eventos em %d processos",
  nrow(eventos_penalidades_ocorrencias), dplyr::n_distinct(eventos_penalidades_ocorrencias$processo)
))
print(eventos_penalidades_ocorrencias |> dplyr::count(fonte, tipo, sort = TRUE))

arrow::write_parquet(eventos_penalidades_ocorrencias, file.path(OUT_DIR, "eventos_penalidades_ocorrencias.parquet"))

# =============================================================================
# CHECKS FINAIS
# =============================================================================
resumo_papel_dic <- dic_classificado |> dplyr::count(papel, sort = TRUE)
readr::write_csv(resumo_papel_dic, file.path(QA_DIR, "resumo_papel_dicionario.csv"))

message("\nProdutos salvos em: ", OUT_DIR)
message("Checks/QA em: ", QA_DIR)
message("\n=== 06_serie_temporal.R — CONCLUÍDO ===")