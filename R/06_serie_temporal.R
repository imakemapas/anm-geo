################################################################################
# 06_serie_temporal.R
#
# Reconstrói a dimensão TEMPORAL do processo minerário, complementando o
# estado atual já calculado no 05 (que vem do PMA). Cinco produtos, um
# arquivo só, lendo direto dos parquets já filtrados/tipados pelo 04 — sem
# reler nenhum .txt bruto:
#
#   A) FASE/STATUS   — máquina de estados a partir de ProcessoEvento
#                       (ABRE/MUDA_FASE, FECHA, SUSPENDE, RETOMA)
#   B) VIGÊNCIA       — reconstrução sequencial genérica (mesma função,
#                       aplicada 3x): titular, substância, associação
#   C) ACESSÓRIOS     — data do primeiro protocolo: licença ambiental, GU,
#                       barragem (marcação ambiental/jurídica)
#   D) DOCUMENTAL     — ProcessoTitulo, direto (situação + vencimento)
#   E) PROPRIEDADE DO SOLO — junção direta, sem reconstrução (pode ser 1:N)
#
# BASE DO CLASSIFICADOR DE EVENTOS (Bloco A): construído e validado em
# conjunto, rodando sobre o dicionário OFICIAL COMPLETO (Evento.txt, via
# 04) cruzado com frequência real de uso — não uma amostra, não um CSV
# residual. Decisões registradas:
#   - idevento 318 NÃO abre REQ PESQUISA (erro herdado do 06b antigo) — é
#     reprovação de relatório de pesquisa (fase AUT PESQ) => FECHA.
#   - idevento 100 (e demais "REQUERIMENTO ... PROTOC" iniciais) SÃO quem
#     abre a fase de requerimento, apesar de PROTOC — exceção documentada.
#   - idevento 1974 ("suspensão de análise") testado empiricamente (o que
#     vem depois, temporalmente, para os processos afetados): não há
#     concentração em resolução formal (só 0,5% via 1975/cancelada), a
#     atividade processual comum continua depois => NEUTRO, não SUSPENDE.
#   - Alvará de Pesquisa e PLG publicados/renovados => MUDA_FASE (criam ou
#     renovam a fase), não eventos neutros.
#   - Resíduo não classificado (~4% do volume total) tratado como NEUTRO
#     por padrão — não gera transição de estado sem confiança suficiente.
#
# CORREÇÃO CRÍTICA (validação externa, 2026-07): usuário identificou, com
# fonte externa (Diário Oficial), dois processos PLG da COOGAM suspensos
# judicialmente em 12/05/2026 que nosso pipeline não mostrava como
# suspensos. Investigação encontrou: (1) idevento 2969 ("PLG/SUSPENSÃO
# JUDICIAL DA PLG PUBL") estava caindo em NAO_CLASSIFICADO — a regra de
# SUSPENDE exigia "AUTORIZADA"/"APLICADA" junto de "SUSPENSÃO", mas esse
# texto não tem nenhuma das duas. (2) Verificação sistemática contra os 69
# idevento já validados no 06b antigo achou 30 regressões introduzidas ao
# reconstruir o classificador do zero sobre o dicionário completo — a
# maior causa: a regra de "Alvará/PLG publicado = MUDA_FASE" (adicionada
# numa revisão anterior) vinha ANTES da regra de FECHA no case_when, então
# "ALVARÁ...DECLARADO NULO PUBL" batia na regra de abertura por engano.
# Corrigido: FECHA agora vem antes de MUDA_FASE; adicionadas regras para
# "SUSPENSÃO JUDICIAL" isolada, "INTERDIÇÃO" sem exigir qualificador,
# "REVOGAÇÃO...SUSPENSÃO" tolerando palavra no meio (ex. "DE"), cobertura
# de Registro de Extração (REG EXT) que não tinha regra nenhuma. Restam 7
# das 69 divergências sem explicação por bug — são casos onde o PRÓPRIO ID
# hardcoded no 06b antigo não corresponde ao conceito pretendido (ex.
# idevento 1111 deveria abrir "Disponibilidade" mas no dicionário oficial
# é "Vistoria de Lavra Não Autorizada") ou onde o rótulo antigo parece
# semanticamente errado (271/272 "Alvará Renovação" rotulado como FECHA,
# quando o texto sugere MUDA_FASE) — não corrigidos aqui, ver mensagem de
# acompanhamento para decisão.
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
# Aplica-se sobre micro_evento (dicionário oficial inteiro). Retorna, por
# idevento: tipo_proc (fase, extraída do prefixo antes da "/"), papel
# (MUDA_FASE/FECHA/SUSPENDE/RETOMA/NEUTRO_*/PROTOC/NAO_CLASSIFICADO), e
# categoria_amb_jur (marcação transversal, não substitui papel).
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
      # normaliza acento (mesma correcao aplicada no diagnostico em Python)
      S = stringi::stri_trans_general(toupper(resto), "Latin-ASCII")
    )

  dic <- dic |>
    dplyr::mutate(
      papel = dplyr::case_when(
        # --- casos especiais confirmados (ver cabecalho) ---
        idevento == "1974" ~ "NEUTRO_processual",
        idevento == "1975" ~ "NEUTRO_processual",
        idevento == "318"  ~ "FECHA",
        # excecao: "RETOMADA DOS TRABALHOS...COMUNICADO PROTOC" comunica um
        # FATO ja ocorrido (retomada), nao um pedido — foge da regra geral
        # de que PROTOC nunca efetiva.
        idevento == "2138" ~ "RETOMA",

        # --- INSTAURACAO de processo administrativo (abertura de
        # investigacao, NAO o resultado): "instaura processo administrativo
        # de nulidade/caducidade/cancelamento/cassacao/etc." so abre um
        # prazo de defesa — o titulo continua no estado em que estava ate
        # uma decisao final (que teria seu PROPRIO evento, esse sim FECHA).
        # Sem esta regra, "INSTAURA PROC ADM NULIDADE" cai na regra de FECHA
        # so por conter a palavra "NULIDADE" (achado real, 2026-07: processo
        # 850578/1993 aparecia com PLG ENCERRADA em 2024, na ABERTURA do
        # processo de nulidade, nao numa decisao final que nem aconteceu
        # ainda). Mesmo raciocinio vale para caducidade/cancelamento/
        # cassacao/suspensao — instaurar != decidir. Precisa vir ANTES de
        # qualquer regra de FECHA/SUSPENDE baseada em palavra-chave.
        stringr::str_detect(S, "INSTAURA (PROC|PROCESSO)") ~ "NEUTRO_processual",

        # --- excecao: requerimento inicial ABRE a fase, mesmo sendo PROTOC ---
        sufixo == "PROTOC" & stringr::str_detect(S, "REQUERIMENTO") &
          !stringr::str_detect(S, "RECONSIDERA|CUMPRIMENTO|PROVENIENTE") ~ "MUDA_FASE",

        sufixo == "PROTOC" ~ "PROTOC",

        # --- NEUTRO: processual/financeiro/relatorio/transferencia/informativo ---
        stringr::str_detect(S, "\\bEXIGENCIA\\b") & !stringr::str_detect(S, "TORNA S/EFEITO") ~ "NEUTRO_processual",
        stringr::str_detect(S, "\\bTAH\\b") ~ "NEUTRO_financeiro",
        stringr::str_detect(S, "\\bRAL\\b") ~ "NEUTRO_relatorio",
        stringr::str_detect(S, "MULTA|AUTO (DE )?INFRACAO|DEBITO|PAGAMENTO|PARCELAMENTO|NOTIFICACAO ADM|VISTORIA") &
          !stringr::str_detect(S, "SUSPENS|CADUCID|CANCEL") ~ "NEUTRO_financeiro",
        stringr::str_detect(S, "COVID|RESOLUCAO 76/2021") ~ "NEUTRO_covid_prorrogacao",
        stringr::str_detect(S, "CESSAO (TOTAL|PARCIAL).*EFETIVADA|TRANSF DIREITOS") ~ "NEUTRO_transferencia_direitos",
        stringr::str_detect(S, "DECLARACAO DE APTIDAO EMITIDA|ASSENTIMENTO CDN AUTORIZADO|BAIXA TRANSCRICAO") ~ "NEUTRO_marco_informativo",

        stringr::str_detect(S, "\\bNEGAD[OA]\\b") &
          stringr::str_detect(S, "SUSPENS|INTERDI|EMBARGO|DESEMBARGO|DESINTERDI|RECONSIDERA|ALVARA") ~ "NEUTRO_negado",

        # --- RETOMA: reversao efetivada de impedimento ---
        # FIX: "CASSA" incluido (cobre "TORNA S/EFEITO CASSACAO...") e
        # "REVOGACAO.*SUSPENS" agora tolera palavra no meio (ex. "DE")
        stringr::str_detect(S, "TORNA S/EFEITO") &
          stringr::str_detect(S, "SUSPENS|INTERDI|EMBARGO|CADUCID|NULID|CANCEL|REVOGA|INDEFER|ARQUIVA|CASSA") ~ "RETOMA",
        stringr::str_detect(S, "DESINTERDI") & !stringr::str_detect(S, "SOLICITA") ~ "RETOMA",
        stringr::str_detect(S, "DESEMBARGO") & !stringr::str_detect(S, "SOLICITA") ~ "RETOMA",
        stringr::str_detect(S, "DESBLOQUEAD") ~ "RETOMA",
        stringr::str_detect(S, "REVOGACAO.*SUSPENS") ~ "RETOMA",
        stringr::str_detect(S, "RETOMADA") ~ "RETOMA",

        # --- SUSPENDE: impedimento efetivado ---
        # FIX: "SUSPENSAO JUDICIAL" isolado (antes so pegava com AUTORIZAD/
        # APLICAD, deixando passar "PLG/SUSPENSÃO JUDICIAL DA PLG PUBL" —
        # achado real, ver cabecalho); "INTERDICAO" sem exigir "APLICAD"
        # (cobre "BARRAGENS INTERDIÇÃO PUBL")
        stringr::str_detect(S, "SUSPENSAO JUDICIAL") ~ "SUSPENDE",
        stringr::str_detect(S, "SUSPENSAO.*(AUTORIZAD|APLICAD)") | stringr::str_ends(stringr::str_trim(S), "SUSPENSA") ~ "SUSPENDE",
        stringr::str_detect(S, "INTERDICAO") & !stringr::str_detect(S, "DESINTERDI") ~ "SUSPENDE",
        stringr::str_detect(S, "\\bEMBARGO\\b") & !stringr::str_detect(S, "ARQUIVAMENTO") ~ "SUSPENDE",
        stringr::str_detect(S, "BLOQUEAD[AO] JUDICIALMENTE") ~ "SUSPENDE",

        # --- neutro: arquivamento de punicao libera, nao fecha o direito ---
        stringr::str_detect(S, "ARQUIVAMENTO (PROC ADM|AUTO DE (EMBARGO|INFRA))") ~ "NEUTRO_arquiv_punitivo",

        # --- FECHA: encerramento efetivo do direito ---
        # MOVIDO PARA ANTES do bloco MUDA_FASE de Alvara/PLG abaixo — corrige
        # bug real: "ALVARÁ...DECLARADO NULO PUBL" estava caindo na regra de
        # abertura por engano, porque a regra generica vinha primeiro.
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

        # --- MUDA_FASE: cria ou renova a fase (agora DEPOIS do FECHA) ---
        stringr::str_detect(S, "ALVARA DE PESQUISA") & sufixo == "PUBL" &
          !stringr::str_detect(S, "INDEFERID|CANCELAD|VENCID|NUL[OA]") ~ "MUDA_FASE",
        # FIX: generalizado de "PRORROGACAO PRAZO ALVARA" para qualquer
        # "PRORROGACAO...PRAZO" publicada (cobre tambem REG EXT)
        stringr::str_detect(S, "PRORROGACAO.*PRAZO") & sufixo == "PUBL" &
          !stringr::str_detect(S, "INDEFERID|NEGAD") ~ "MUDA_FASE",
        stringr::str_detect(S, "PERMISSAO LAVRA GARIMPEIRA|RENOVACAO.*PRAZO PLG|\\bPLG PUBLICADA\\b") &
          sufixo == "PUBL" & !stringr::str_detect(S, "CANCEL|NUL[OA]") ~ "MUDA_FASE",
        stringr::str_detect(S, "RELATORIO PESQUISA APROVADO") & sufixo == "PUBL" ~ "MUDA_FASE",
        # FIX NOVO: "REG EXT/REGISTRO DE EXTRAÇÃO 0X ANO(S) PUBL" nao tinha regra
        stringr::str_detect(S, "REGISTRO DE EXTRACAO.*ANO") & sufixo == "PUBL" ~ "MUDA_FASE",

        # --- MUDA_FASE generico (publicacao/portaria/registro/GU) ---
        sufixo == "PUBL" & stringr::str_detect(S, "PUBLICAD[AO]|PORTARIA|REGISTRO.*AUTORIZAD|GU.*APROVAD|APROVAD[OA] PUBL$|LAVRA DECLARADA|AUTORIZADA PUBL$") ~ "MUDA_FASE",
        stringr::str_detect(S, "REQUERIMENTO") & sufixo != "PROTOC" ~ "MUDA_FASE",
        stringr::str_detect(S, "APTA PARA DISPONIBILIDADE|DISPONIBILIDADE PARA PESQUISA|EDITAL OFERTA PUBLICA|LEILAO ELETRONICO|AREA DISPONIVEL ART 26|AREA DESCARTADA") ~ "MUDA_FASE",

        TRUE ~ "NAO_CLASSIFICADO"
      ),
      categoria_amb_jur = dplyr::case_when(
        stringr::str_detect(S, "LICENCA AMBIENTAL|PROTOCOLO ORGAO AMBIENTAL|ORGAO AMBIENTAL") ~ "ambiental_licenca",
        stringr::str_detect(S, "GUIA (DE )?UTILIZACAO|\\bGU\\b") ~ "ambiental_gu",
        stringr::str_detect(S, "BARRAGE") ~ "ambiental_barragem",
        stringr::str_detect(S, "ACIDENTE AMBIENTAL") ~ "ambiental_acidente",
        stringr::str_detect(S, "AREA INDIGENA|UNIDADE DE CONSERVA|AREA BLOQUEADA|FAIXA DE FRONTEIRA") ~ "juridico_espacial",
        stringr::str_detect(S, "DECISAO JUDICIAL|SUSPENSAO JUDICIAL|BLOQUEAD[OA] JUDICIALMENTE|DESBLOQUEAD[OA] JUDICIALMENTE|PROTESTO JUDICIAL") ~ "juridico_judicial",
        stringr::str_detect(S, "ONERACAO DIREITOS|PENHOR|ARRESTO|CAUCAO") ~ "juridico_oneracao",
        stringr::str_detect(S, "MULTA|AUTO (DE )?INFRACAO|ADVERTENCIA") ~ "ambiental_ou_adm_penalidade",
        TRUE ~ NA_character_
      )
    )

  # NAO_CLASSIFICADO tratado como neutro por padrao (decisao registrada) —
  # nao gera transicao de estado, mas fica marcado distinto de um NEUTRO_*
  # "de proposito", para auditoria futura.
  dic |>
    dplyr::select(idevento, dsevento, tipo_proc, sufixo, papel, categoria_amb_jur)
}

dic_evento_raw <- ler("micro_evento")
dic_classificado <- classificar_dicionario_eventos(dic_evento_raw)

readr::write_csv(dic_classificado, file.path(QA_DIR, "dicionario_eventos_classificado.csv"))
message(sprintf("[06] dicionario de eventos classificado: %d idevento distintos", nrow(dic_classificado)))

# [NOVO] Export oficial em result_db (nao so QA) — fonte unica de verdade para
# qualquer script consumidor (aba 4 do Shiny, graficos de historico por
# processo) usar a MESMA classificacao ja corrigida aqui, sem duplicar a
# funcao classificar_dicionario_eventos() em outro lugar e sem depender de um
# artefato de QA como se fosse dado oficial.
arrow::write_parquet(dic_classificado, file.path(OUT_DIR, "dicionario_eventos_classificado.parquet"))

# --- Validação de regressão contra os 69 idevento já validados no 06b antigo ---
# Roda sempre, todo o pipeline — garante que uma mudança futura no
# classificador não volte a quebrar silenciosamente algo que já sabíamos
# estar certo (foi exatamente isso que causou o bug encontrado em 2026-07).
# 318 e 919 ficam de fora de propósito: 318 já tem tratamento especial
# acima (era erro no 06b, confirmado FECHA); 919 não existe no dicionário
# oficial (idevento inválido, herdado do 06b antigo).
validacao_69_original <- tibble::tribble(
  ~idevento, ~papel_original,
  "1111","MUDA_FASE","2811","MUDA_FASE","2812","MUDA_FASE","382","MUDA_FASE","493","MUDA_FASE",
  "388","MUDA_FASE","201","MUDA_FASE","321","MUDA_FASE","322","MUDA_FASE","323","MUDA_FASE","964","MUDA_FASE",
  "271","FECHA","272","FECHA","273","FECHA","274","FECHA",
  "2128","SUSPENDE","2129","RETOMA","1018","RETOMA",
  "400","MUDA_FASE","2611","MUDA_FASE","2132","MUDA_FASE",
  "499","FECHA","2135","FECHA","496","FECHA","2134","FECHA","498","FECHA","410","FECHA",
  "441","SUSPENDE","443","SUSPENDE","445","SUSPENDE","446","SUSPENDE","447","SUSPENDE",
  "2130","SUSPENDE","2363","SUSPENDE","2515","SUSPENDE",
  "444","RETOMA","2137","RETOMA","2138","RETOMA","2131","RETOMA","2365","RETOMA","2530","RETOMA",
  "513","MUDA_FASE","523","MUDA_FASE",
  "532","FECHA","713","FECHA","714","FECHA","961","FECHA",
  "1254","SUSPENDE","2373","SUSPENDE","2517","SUSPENDE","2969","SUSPENDE",
  "1241","RETOMA","2375","RETOMA","2532","RETOMA","2970","RETOMA",
  "920","MUDA_FASE","921","MUDA_FASE","922","MUDA_FASE","923","MUDA_FASE","924","MUDA_FASE",
  "940","MUDA_FASE","941","MUDA_FASE","927","MUDA_FASE",
  "943","FECHA","1330","FECHA","951","FECHA","944","RETOMA"
)

divergencias_regressao <- validacao_69_original |>
  dplyr::left_join(dic_classificado, by = "idevento") |>
  dplyr::filter(papel_original != papel)

if (nrow(divergencias_regressao) > 0) {
  readr::write_csv(divergencias_regressao, file.path(QA_DIR, "ALERTA_divergencia_classificacao_original.csv"))
  warning(sprintf(
    "[06] ALERTA: %d de 67 eventos já validados no 06b antigo divergem da classificação atual! Ver %s antes de confiar no resultado.",
    nrow(divergencias_regressao), file.path(QA_DIR, "ALERTA_divergencia_classificacao_original.csv")
  ))
} else {
  message("[06] validacao OK: todos os 67 idevento previamente validados batem com a classificacao atual.")
}

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
  # desempate deterministico para eventos no mesmo dia (mesmo processo):
  # nao ha como saber a ordem real intra-dia so com a data — usar idevento
  # como criterio estavel e auditavel, nao a ordem de leitura do parquet.
  dplyr::arrange(processo, dtevento, idevento)

# [NOVO] Export dos eventos administrativos INDIVIDUAIS (evento a evento, sem
# agregar em blocos de fase) — esta e a granularidade pedida para a aba 4 e
# para o grafico de historico por processo: cada abertura, renovacao,
# anulacao/encerramento, suspensao e retomada e um ponto proprio no tempo, com
# sua propria data e descricao. serie_fase_status (abaixo) e o produto
# AGREGADO (blocos de fase) e continua existindo para quem precisar do estado
# consolidado; este e o produto DESAGREGADO, para timelines evento a evento.
eventos_classificados <- eventos_estado |>
  dplyr::select(processo, dtevento, idevento, dsevento, tipo_proc, papel, categoria_amb_jur)

arrow::write_parquet(eventos_classificados, file.path(OUT_DIR, "eventos_classificados.parquet"))
message(sprintf(
  "[06][A] eventos classificados (individuais, nao agregados): %d linhas em %d processos",
  nrow(eventos_classificados), dplyr::n_distinct(eventos_classificados$processo)
))

n_ignorados_fora_seq <- 0L  # contador de SUSPENDE/RETOMA fora de sequencia (log abaixo)

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
      # evento fora de sequencia (ex.: SUSPENDE quando ja SUSPENSA, ou sem
      # fase aberta) — ignorado, mas CONTADO (nao mais silencioso).
      n_ignorados_local <<- n_ignorados_local + 1L
    }
  }

  if (!is.na(dt_ini)) {
    linhas[[length(linhas) + 1]] <- tibble::tibble(
      processo = proc_id, dt_inicio = dt_ini, dt_fim = as.Date(NA), fase = fase_atual, status = status_atual)
  }

  n_ignorados_fora_seq <<- n_ignorados_fora_seq + n_ignorados_local
  if (length(linhas) == 0) return(NULL)

  # [NOVO] Segunda passada — PRE_AUTORIZACAO + GAP entre segmentos. Sem isso,
  # qualquer descontinuidade (fase encerrada e so reaberta bem depois, ou
  # qualquer buraco temporal entre blocos) fica sem NENHUMA linha cobrindo o
  # periodo — um "buraco silencioso" na serie. Esta e a mesma logica de
  # segunda passada que existia no construir_historico_fases() do script
  # antigo (06_proc_shiny_microdados_inaptos_old.R) e que faltava aqui —
  # achado real, comparando a tabela nova com a antiga lado a lado.
  df_linhas <- dplyr::bind_rows(linhas)
  linhas_finais <- list()
  for (k in seq_len(nrow(df_linhas))) {
    l_atual <- df_linhas[k, ]

    if (k == 1 && !is.na(l_atual$dt_inicio)) {
      # periodo antes do processo ter qualquer fase registrada
      linhas_finais[[length(linhas_finais) + 1]] <- tibble::tibble(
        processo = proc_id, dt_inicio = as.Date(NA), dt_fim = l_atual$dt_inicio - 1,
        fase = "SEM REGISTRO", status = "PRE_AUTORIZACAO")
    }

    if (k > 1) {
      l_ant <- df_linhas[k - 1, ]
      if (!is.na(l_ant$dt_fim) && !is.na(l_atual$dt_inicio) && l_atual$dt_inicio > l_ant$dt_fim + 1) {
        # buraco temporal entre o fim do bloco anterior e o inicio do atual —
        # mantem ENCERRADA se ja estava encerrada, senao marca como GAP
        # (fase ainda "existe" nominalmente, mas sem evento nenhum cobrindo)
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
  # arrange() por padrao coloca NA (a linha PRE_AUTORIZACAO, dt_inicio = NA)
  # por ULTIMO — errado, ela representa o INICIO da historia do processo.
  # Sentinela garante NA primeiro sem alterar a data de verdade.
  dplyr::arrange(processo, dplyr::coalesce(dt_inicio, as.Date("1900-01-01")))

message(sprintf(
  "[06][A] serie fase/status: %d processos | %d linhas | eventos SUSPENDE/RETOMA fora de sequencia (ignorados): %d",
  dplyr::n_distinct(serie_fase_status$processo), nrow(serie_fase_status), n_ignorados_fora_seq
))

arrow::write_parquet(serie_fase_status, file.path(OUT_DIR, "serie_fase_status.parquet"))

# =============================================================================
# B) VIGÊNCIA SEQUENCIAL GENÉRICA — titular, substância, associação
# =============================================================================
# Mesma logica nas tres aplicacoes: ordena por data de inicio dentro de cada
# grupo, e preenche o fim (quando vazio) com o inicio do proximo registro do
# MESMO grupo menos 1 dia. O ultimo registro de cada grupo, sem fim
# preenchido, fica em aberto (vigente ate hoje) — resolve exatamente o caso
# que vocês descreveram (fim vazio != atual, se já existe um próximo início).
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
# Nao sao maquina de estados — so a data do primeiro protocolo por processo,
# usando a categoria_amb_jur ja resolvida no dicionario classificado.
acessorios_ambiental <- proc_evento |>
  dplyr::filter(categoria_amb_jur %in% c("ambiental_licenca", "ambiental_gu", "ambiental_barragem")) |>
  dplyr::group_by(processo, categoria_amb_jur) |>
  dplyr::summarise(
    dt_primeiro_protocolo = min(dtevento, na.rm = TRUE),
    dt_ultimo_protocolo   = max(dtevento, na.rm = TRUE),
    n_protocolos          = dplyr::n(),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    id_cols = processo,
    names_from = categoria_amb_jur,
    values_from = c(dt_primeiro_protocolo, dt_ultimo_protocolo, n_protocolos),
    names_glue = "{categoria_amb_jur}_{.value}"
  )

message(sprintf("[06][C] acessorios ambiental/juridico: %d processos com algum protocolo", nrow(acessorios_ambiental)))
arrow::write_parquet(acessorios_ambiental, file.path(OUT_DIR, "acessorios_ambiental.parquet"))

# --- registro granular (nao agregado) dos protocolos de licenca ambiental ---
# Diferente de acessorios_ambiental acima (so min/max/contagem), aqui fica
# 1 linha por evento de protocolo - necessario para plotar cada protocolo
# individualmente (estudo de caso). So papel == "PROTOC" (protocolizacao de
# fato) - eventos NEUTRO_processual (ex. "EXIGENCIA...PUBL", que e aviso de
# cobranca, nao protocolo - ver checks/ do caso COOGAM) ficam de fora.
# Escopo: dataset inteiro, mesmo escopo do resto do Bloco C - filtragem por
# processo especifico (ex. COOGAM/Tapajos) e feita depois, em checks/.
protocolos_licenca_ambiental <- proc_evento |>
  dplyr::filter(categoria_amb_jur == "ambiental_licenca", papel == "PROTOC") |>
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
    status_vencimento = dplyr::case_when(
      is.na(dtvencimento) ~ "sem_data_vencimento",
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
                sum(situacao_documental$status_vencimento == "sem_data_vencimento")))
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
# F) SITUAÇÃO ATUAL — "foto de hoje". Cruza os blocos acima SEM misturar
#    fontes: cada origem fica em coluna própria (fase_evento e fase_pma nunca
#    se sobrescrevem — decisão explícita: "não confundir a fase LICEN com o
#    evento de protocolo de licença ambiental, são coisas diferentes").
#
# Regra de apto_operar (validada com fonte oficial ANM — ANMlegis, regimes de
# exploração mineral):
#   1) fase_evento precisa ser uma das 4 que autorizam extração de fato:
#      Concessão de Lavra, Licenciamento, PLG, Registro de Extração.
#      (Autorização de Pesquisa autoriza só pesquisar — fica em "em_analise".
#      GU fica de fora aqui — só entra quando cruzarmos com CFEM.)
#   2) status_evento (Bloco A) precisa ser "ATIVA" — não suspensa sem
#      retomada, não encerrada.
#   3) Precisa existir protocolo de licença ambiental (Bloco C) ANTERIOR à
#      data em que a fase abriu (evento MUDA_FASE) — respeitando a
#      temporalidade, não só "existe em algum momento".
#
# Vencimento (dt_vencimento) em branco: registrado como categoria própria
# ("sem_data_a_revisar"), NÃO tratado como erro nem como ok — decisão
# adiada de propósito para uma etapa futura de análise da série completa.
# =============================================================================

# --- último segmento de fase/status por processo (Bloco A) ---
fase_atual <- serie_fase_status |>
  dplyr::group_by(processo) |>
  dplyr::slice_max(dt_inicio, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(processo, fase_evento = fase, status_evento = status, dt_ultimo_evento_fase = dt_inicio)

# --- fase segundo o PMA (05) — fonte independente, coluna à parte, nunca
# sobrescreve fase_evento ---
pma_full <- load_ckpt("05_pma_full") |> as.data.frame()
fase_pma_df <- pma_full |>
  dplyr::transmute(processo = as.character(PROCESSO), fase_pma = FASE) |>
  dplyr::distinct(processo, .keep_all = TRUE)

# CORRIGIDO: fase_evento usa os codigos abreviados do dicionario de eventos
# (ex. "CONC LAV"), fase_pma usa nome por extenso (ex. "CONCESSAO DE LAVRA")
# — sao a MESMA fase, so grafia diferente. Comparar direto (!=) dava
# divergencia falsa em ~99% dos casos (bug, nao achado). Mapeamento abaixo
# e o meu melhor entendimento a partir do que ja vimos nos dados deste
# projeto — NAO tenho certeza absoluta de "REG EXT"/"REQ EXT"/"DIR REQ LAV"/
# "APTO DISP", por isso o crosstab bruto (antes do mapeamento) tambem sai
# exportado para auditoria — corrijam o mapa se algo estiver errado.
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

# crosstab BRUTO (pre-mapeamento) para auditoria — confirmar/corrigir o mapa acima
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

# processos cujo(s) titulo(s) nao tem NENHUMA dt_publicacao valida tambem
# entram como sem_data_a_revisar (nao tem base pra saber qual e o "atual")
doc_sem_data <- situacao_documental |>
  dplyr::filter(is.na(dt_publicacao)) |>
  dplyr::distinct(processo) |>
  dplyr::anti_join(doc_atual, by = "processo") |>
  dplyr::mutate(status_vencimento = "sem_data_a_revisar", dt_vencimento = as.Date(NA))

doc_atual_completo <- dplyr::bind_rows(doc_atual, doc_sem_data)

# --- licença ambiental (Bloco C) ---
# dt_protocolo_licenca_ambiental (primeira data) NAO muda — continua sendo o
# que alimenta a regra de apto_operar (licenca_antes_da_fase) mais abaixo.
# dt_ultimo_protocolo_licenca_ambiental / n_protocolos_licenca_ambiental sao
# NOVAS, so para consulta/relatorio — nao entram em nenhuma regra de
# elegibilidade. Nao ha evento de aprovacao/vencimento especifico de licenca
# ambiental no dicionario (confirmado em dicionario_eventos_classificado.csv
# — so PROTOC e EXIGENCIA PUBL) — por isso o teto do que se pode afirmar e
# "protocolizacao" (existencia de pedido registrado), nao aprovacao real.
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

# --- titular atual (Bloco B1) — sem co-titularidade (premissa já validada) ---
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
  dplyr::mutate(
    fase_evento_traduzida = dplyr::coalesce(mapa_fase_evento_pma[fase_evento], fase_evento),
    fase_diverge_pma = normaliza_fase(fase_evento_traduzida) != normaliza_fase(fase_pma),

    licenca_antes_da_fase = !is.na(dt_protocolo_licenca_ambiental) &
      dt_protocolo_licenca_ambiental < dt_ultimo_evento_fase,

    # NOVO: vencimento entra na arvore (fluxograma v2). sem_data_a_revisar
    # tambem BLOQUEIA apto_operar (opcao mais cautelosa, coerente com o
    # resto do projeto) — na pratica, hoje, essa categoria tem 0 casos.
    titulo_ok_vencimento = status_vencimento == "vigente",

    apto_operar = dplyr::case_when(
      !(fase_evento %in% FASES_QUE_OPERAM) ~ "em_analise",
      status_evento != "ATIVA"              ~ "FALSE",
      !licenca_antes_da_fase                ~ "FALSE",
      !titulo_ok_vencimento                 ~ "FALSE",
      TRUE                                   ~ "TRUE"
    ),
    motivo_nao_apto = dplyr::case_when(
      apto_operar == "TRUE"                 ~ NA_character_,
      !(fase_evento %in% FASES_QUE_OPERAM)  ~ "fase_de_tramitacao_ou_pesquisa",
      status_evento != "ATIVA"              ~ "suspensa_ou_encerrada",
      !licenca_antes_da_fase                ~ "sem_licenca_ambiental_previa",
      !titulo_ok_vencimento                 ~ dplyr::if_else(
                                                 status_vencimento == "sem_data_a_revisar",
                                                 "vencimento_sem_data_a_revisar", "titulo_vencido"),
      TRUE                                   ~ NA_character_
    )
  ) |>
  dplyr::select(processo, fase_evento, fase_pma, fase_diverge_pma, status_evento, dt_ultimo_evento_fase,
               status_vencimento, dt_vencimento, dt_protocolo_licenca_ambiental,
               dt_ultimo_protocolo_licenca_ambiental, n_protocolos_licenca_ambiental,
               titular_atual, nrcpfcnpj_titular_atual, apto_operar, motivo_nao_apto)

resumo_apto <- situacao_atual |> dplyr::count(apto_operar, motivo_nao_apto, sort = TRUE)
readr::write_csv(resumo_apto, file.path(QA_DIR, "resumo_apto_operar.csv"))

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
# CHECKS FINAIS
# =============================================================================
resumo_papel_dic <- dic_classificado |> dplyr::count(papel, sort = TRUE)
readr::write_csv(resumo_papel_dic, file.path(QA_DIR, "resumo_papel_dicionario.csv"))

message("\nProdutos salvos em: ", OUT_DIR)
message("Checks/QA em: ", QA_DIR)
message("\n=== 06_serie_temporal.R — CONCLUÍDO ===")