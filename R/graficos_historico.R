# ################################################################################
# # graficos_historico.R
# #
# # Copia autocontida da secao G do utils.R do pipeline (calcular_gaps_titulo,
# # camada_marcacao, eventos_marcacao, grafico_historico_processo e helpers).
# # Existe como arquivo separado, e nao via source() do utils.R inteiro, por um
# # motivo pontual de deploy: utils.R tem uma linha de topo
# # (CKPT_DIR_PADRAO <- here::here(...)) que roda assim que o arquivo e
# # carregado e exige o pacote 'here' instalado — o app.R nao usa checkpoints e
# # nao deveria depender disso so para desenhar um grafico. Este arquivo so usa
# # dplyr/ggplot2/tibble, que o app.R ja carrega.
# #
# # FONTE DA VERDADE: utils.R (secao G), no repositorio do pipeline. Este
# # arquivo e uma copia gerada por 07_proc_shiny_dossie.R a cada rodada — nao
# # edite aqui direto, edite o utils.R e rode o 07 de novo.
# ################################################################################

formata_num_br <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

tema_historico_processo <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())

# Cores das faixas "nao apto" no grafico historico, por motivo (achado
# 2026-07: antes toda faixa "nao apto" usava a mesma cor vermelha uniforme,
# escondendo a diferenca entre "vencido de vez" e "vencido mas com renovacao
# de PLG protocolada, aguardando a ANM" — mesma paleta usada na tabela de
# Historico de autorizacoes do app.R (cores_status), pra ficar consistente
# entre tabela e grafico. Motivo ausente do mapa cai no default (vermelho).
CORES_MOTIVO_NAO_APTO_HEX <- c(
  "fase_de_tramitacao_ou_pesquisa"       = "#6C757D",
  "titulo_vencido_renovacao_protocolada" = "#F39C12"
)
CORES_MOTIVO_NAO_APTO_HEX_DEFAULT <- "#C0392B"
COR_APTO_HEX <- "#27AE60"

CORES_MOTIVO_NAO_APTO_RGBA <- c(
  "fase_de_tramitacao_ou_pesquisa"       = "rgba(108,117,125,0.15)",
  "titulo_vencido_renovacao_protocolada" = "rgba(243,156,18,0.18)"
)
CORES_MOTIVO_NAO_APTO_RGBA_DEFAULT <- "rgba(192,57,43,0.12)"
COR_APTO_RGBA <- "rgba(39,174,96,0.12)"

calcular_gaps_titulo <- function(inicio, fim, data_referencia = Sys.Date()) {
  if (length(inicio) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character())))
  }

  ord <- order(inicio)
  inicio <- inicio[ord]; fim <- fim[ord]

  intervalos_ini <- c(); intervalos_fim <- c()
  cur_ini <- inicio[1]; cur_fim <- fim[1]
  if (length(inicio) > 1) {
    for (i in 2:length(inicio)) {
      if (inicio[i] <= cur_fim) {
        cur_fim <- max(cur_fim, fim[i])
      } else {
        intervalos_ini <- c(intervalos_ini, cur_ini)
        intervalos_fim <- c(intervalos_fim, cur_fim)
        cur_ini <- inicio[i]; cur_fim <- fim[i]
      }
    }
  }
  intervalos_ini <- c(intervalos_ini, cur_ini)
  intervalos_fim <- c(intervalos_fim, cur_fim)

  gaps_ini <- c(); gaps_fim <- c()
  if (length(intervalos_ini) > 1) {
    for (i in 1:(length(intervalos_ini) - 1)) {
      gaps_ini <- c(gaps_ini, intervalos_fim[i])
      gaps_fim <- c(gaps_fim, intervalos_ini[i + 1])
    }
  }

  ultimo_fim <- intervalos_fim[length(intervalos_fim)]
  if (ultimo_fim < data_referencia) {
    gaps_ini <- c(gaps_ini, ultimo_fim)
    gaps_fim <- c(gaps_fim, data_referencia)
  }

  tibble::tibble(xmin = as.Date(gaps_ini, origin = "1970-01-01"),
                 xmax = as.Date(gaps_fim, origin = "1970-01-01"))
}

gaps_vigencia_titulo <- function(situacao_documental, processos = NULL) {
  d <- situacao_documental |>
    dplyr::filter(!is.na(dt_publicacao), !is.na(dt_vencimento))
  if (!is.null(processos)) d <- d |> dplyr::filter(processo %in% processos)
  if (nrow(d) == 0) {
    return(tibble::tibble(processo = character(), xmin = as.Date(character()), xmax = as.Date(character())))
  }
  d |>
    dplyr::group_by(processo) |>
    dplyr::group_modify(~ calcular_gaps_titulo(.x$dt_publicacao, .x$dt_vencimento)) |>
    dplyr::ungroup()
}

# -----------------------------------------------------------------------------
# RECONSTRUCAO COMPLETA DE APTIDAO, COMO INTERVALOS (nao so vigencia de
# titulo). Usada na faixa vermelha do grafico — antes ela so olhava
# publicacao/vencimento NOMINAL do titulo (situacao_documental), e por isso
# nao "via" quando um titulo morria ANTES do vencimento nominal por renuncia,
# cassacao, nulidade, revogacao etc. (achado real: processo 850292/2016,
# PLG encerrada por renuncia em 2018, mas a faixa vermelha so comecava em
# 2021 — a data de vencimento nominal, ja irrelevante apos a renuncia).
#
# Mesmos 3 criterios ja aprovados (fase/status operacional + licenca antes da
# fase + titulo vigente), so que agora como UNIAO/INTERSECAO de intervalos,
# nao um teste ponto a ponto por declaracao (isso ja existe em outro lugar,
# para a lista de suspeitos — aqui e o mesmo raciocinio, para desenhar shape).
# -----------------------------------------------------------------------------

# Uniao de intervalos genérica (mesma logica de calcular_gaps_titulo, so que
# devolve os intervalos UNIDOS em vez dos gaps entre eles).
unir_intervalos <- function(inicio, fim) {
  if (length(inicio) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character())))
  }
  ord <- order(inicio)
  inicio <- inicio[ord]; fim <- fim[ord]
  ini_out <- c(); fim_out <- c()
  cur_ini <- inicio[1]; cur_fim <- fim[1]
  if (length(inicio) > 1) {
    for (i in 2:length(inicio)) {
      if (inicio[i] <= cur_fim) {
        cur_fim <- max(cur_fim, fim[i])
      } else {
        ini_out <- c(ini_out, cur_ini); fim_out <- c(fim_out, cur_fim)
        cur_ini <- inicio[i]; cur_fim <- fim[i]
      }
    }
  }
  ini_out <- c(ini_out, cur_ini); fim_out <- c(fim_out, cur_fim)
  tibble::tibble(xmin = as.Date(ini_out, origin = "1970-01-01"),
                 xmax = as.Date(fim_out, origin = "1970-01-01"))
}

# Complementa um conjunto de intervalos (ja unidos) dentro de uma janela —
# devolve os "buracos" (nao cobertos) entre inicio_janela e fim_janela.
complementar_intervalos <- function(intervalos, inicio_janela, fim_janela) {
  if (is.null(intervalos) || nrow(intervalos) == 0) {
    return(tibble::tibble(xmin = inicio_janela, xmax = fim_janela))
  }
  intervalos <- intervalos[order(intervalos$xmin), , drop = FALSE]
  gaps_ini <- c(); gaps_fim <- c()
  cursor <- inicio_janela
  for (i in seq_len(nrow(intervalos))) {
    if (intervalos$xmin[i] > cursor) {
      gaps_ini <- c(gaps_ini, cursor); gaps_fim <- c(gaps_fim, intervalos$xmin[i] - 1)
    }
    cursor <- max(cursor, intervalos$xmax[i] + 1)
  }
  if (cursor <= fim_janela) {
    gaps_ini <- c(gaps_ini, cursor); gaps_fim <- c(gaps_fim, fim_janela)
  }
  if (length(gaps_ini) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character())))
  }
  tibble::tibble(xmin = as.Date(gaps_ini, origin = "1970-01-01"),
                 xmax = as.Date(gaps_fim, origin = "1970-01-01"))
}

# Periodos NAO APTO de um processo, combinando fase/status + licenca + titulo
# — o que a faixa vermelha do grafico deve mostrar. Retorna tibble(xmin,xmax)
# ou NULL se nao houver base (processo sem nenhuma fase registrada).
# Timeline COMPLETA de aptidao de um processo, particionada em segmentos com
# motivo explicito — fonte UNICA usada tanto pela faixa vermelha do grafico
# quanto pela reconstrucao historica da lista de suspeitos (07). Antes essas
# duas coisas tinham logicas SEPARADAS e podiam divergir entre si (achado
# real: processo podia aparecer "vencido" na lista mas nao no grafico, ou
# vice-versa) — agora as duas derivam do mesmo lugar, sem exceção.
#
# Retorna tibble(processo, xmin, xmax, apto_na_data, motivo_nao_apto_na_data)
# cobrindo do inicio da serie de fase (primeiro registro) até data_referencia,
# ou NULL se o processo nao tiver nenhuma fase registrada.
segmentos_aptidao_processo <- function(processo_alvo, serie_fase_status, situacao_documental,
                                        protocolos_licenca_ambiental, eventos_renovacao_plg = NULL,
                                        intervalos_gu_aut_pesq = NULL,
                                        data_referencia = Sys.Date()) {
  processo_alvo <- as.character(processo_alvo)[1]
  FASES_QUE_OPERAM <- c("CONC LAV", "LICEN", "PLG", "REG EXT")  # mesmo vocabulario do 06/07

  fs <- if (!is.null(serie_fase_status)) serie_fase_status[serie_fase_status$processo == processo_alvo, , drop = FALSE] else NULL
  if (is.null(fs) || nrow(fs) == 0) return(NULL)

  doc <- if (!is.null(situacao_documental)) situacao_documental[situacao_documental$processo == processo_alvo, , drop = FALSE] else NULL
  lic <- if (!is.null(protocolos_licenca_ambiental)) protocolos_licenca_ambiental[protocolos_licenca_ambiental$processo == processo_alvo, , drop = FALSE] else NULL
  dt_primeiro_protocolo_lic <- if (!is.null(lic) && nrow(lic) > 0) min(lic$dt_protocolo, na.rm = TRUE) else as.Date(NA)

  # PROTECAO ART. 211/213 (Portaria DNPM 155/2016, achado 2026-07): protocolos
  # (521) e indeferimentos (522) de renovacao de PLG para ESTE processo — ver
  # mesma logica em situacao_atual (06_serie_temporal.R), fonte unica exportada
  # em eventos_renovacao_plg_211_213.parquet. eventos_renovacao_plg == NULL
  # (chamador antigo, sem o argumento novo) equivale a "sem protecao nenhuma",
  # comportamento identico ao de antes desta mudanca.
  ev_renov <- if (!is.null(eventos_renovacao_plg)) eventos_renovacao_plg[eventos_renovacao_plg$processo == processo_alvo, , drop = FALSE] else NULL
  protocolos_renov <- if (!is.null(ev_renov)) sort(ev_renov$dtevento[ev_renov$idevento == "521"]) else as.Date(character())
  indeferimentos_renov <- if (!is.null(ev_renov)) sort(ev_renov$dtevento[ev_renov$idevento == "522"]) else as.Date(character())

  # GU (Guia de Utilizacao) para AUT PESQ (achado 2026-07-21) -- mesma logica
  # de situacao_atual (06_serie_temporal.R), fonte unica exportada em
  # intervalos_gu_aut_pesq.parquet. intervalos_gu_aut_pesq == NULL (chamador
  # antigo, sem o argumento novo) equivale a "sem GU nenhuma", comportamento
  # identico ao de antes desta mudanca.
  gu_proc <- if (!is.null(intervalos_gu_aut_pesq)) intervalos_gu_aut_pesq[intervalos_gu_aut_pesq$processo == processo_alvo, , drop = FALSE] else NULL
  intervalos_gu <- if (!is.null(gu_proc) && nrow(gu_proc) > 0) gu_proc[, c("xmin", "xmax")] else tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()))

  # Para uma data de vencimento (xmin de um "buraco"), acha o protocolo mais
  # recente feito ATE aquela data (art. 211 — depois do vencimento nao conta)
  # e verifica se ja existe indeferimento POSTERIOR a esse protocolo (art.
  # 213 — indeferimento nega e valida o vencimento, retroativo, sem protecao
  # parcial). Retorna TRUE = protegido, FALSE = titulo_vencido puro.
  buraco_protegido_211_213 <- function(dt_vencimento_buraco) {
    candidatos <- protocolos_renov[protocolos_renov <= dt_vencimento_buraco]
    if (length(candidatos) == 0) return(FALSE)
    dt_protocolo <- max(candidatos)
    !any(indeferimentos_renov > dt_protocolo)
  }

  intervalos_titulo <- if (!is.null(doc)) {
    doc_v <- doc[!is.na(doc$dt_publicacao) & !is.na(doc$dt_vencimento), , drop = FALSE]
    unir_intervalos(doc_v$dt_publicacao, doc_v$dt_vencimento)
  } else {
    tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()))
  }

  fs2 <- fs
  fs2$dt_fim_efetivo <- dplyr::coalesce(fs2$dt_fim, data_referencia)

  segmentos <- list()
  for (i in seq_len(nrow(fs2))) {
    ini <- fs2$dt_inicio[i]; fim <- fs2$dt_fim_efetivo[i]
    if (is.na(ini) || is.na(fim) || fim < ini) next  # PRE_AUTORIZACAO (dt_inicio=NA) — sem como avaliar, pula
    fase_i <- fs2$fase[i]; status_i <- fs2$status[i]

    fase_ok    <- !is.na(fase_i) && (fase_i %in% FASES_QUE_OPERAM)
    status_ok  <- !is.na(status_i) && identical(status_i, "ATIVA")
    # SIMPLIFICADO (2026-07): antes comparava dt_primeiro_protocolo_lic com o
    # inicio do segmento (>= ini), replicando timing. Agora e so existencia
    # de protocolo em qualquer momento — mesma regra do 06_serie_temporal.R
    # (flag_sem_licenca_ambiental_previa) e do app.R.
    licenca_ok <- !is.na(dt_primeiro_protocolo_lic)

    # GU (Guia de Utilizacao) para AUT PESQ (achado 2026-07-21): particiona
    # [ini, fim] em sub-intervalos cobertos/nao cobertos por GU valida --
    # coberto vira fase_ok = TRUE (bypass, mesma logica de aut_pesq_com_gu no
    # 06_serie_temporal.R); nao coberto segue com fase_ok normal (FALSE, ja
    # que AUT PESQ nao esta em FASES_QUE_OPERAM). Para qualquer outra fase,
    # cobertura_gu fica sempre vazia e o comportamento e IDENTICO ao de antes
    # desta mudanca (1 unico sub-intervalo = [ini, fim] inteiro).
    if (!is.na(fase_i) && identical(fase_i, "AUT PESQ") && nrow(intervalos_gu) > 0) {
      cobertura_gu <- unir_intervalos(pmax(intervalos_gu$xmin, ini), pmin(intervalos_gu$xmax, fim))
      cobertura_gu <- cobertura_gu[cobertura_gu$xmin <= cobertura_gu$xmax, , drop = FALSE]
    } else {
      cobertura_gu <- tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()))
    }
    buracos_gu <- complementar_intervalos(cobertura_gu, ini, fim)
    subintervalos_fase <- dplyr::bind_rows(
      if (nrow(cobertura_gu) > 0) tibble::tibble(xmin = cobertura_gu$xmin, xmax = cobertura_gu$xmax, fase_ok_j = TRUE) else NULL,
      if (nrow(buracos_gu) > 0)   tibble::tibble(xmin = buracos_gu$xmin,   xmax = buracos_gu$xmax,   fase_ok_j = fase_ok) else NULL
    )
    if (nrow(subintervalos_fase) == 0) subintervalos_fase <- tibble::tibble(xmin = ini, xmax = fim, fase_ok_j = fase_ok)
    subintervalos_fase <- subintervalos_fase[order(subintervalos_fase$xmin), , drop = FALSE]

    for (j in seq_len(nrow(subintervalos_fase))) {
      ini_j <- subintervalos_fase$xmin[j]; fim_j <- subintervalos_fase$xmax[j]
      fase_ok_efetivo <- subintervalos_fase$fase_ok_j[j]

      # AJUSTE (2026-07, flags binarias independentes — pedido: filtro "Tipo de
      # alerta" da aba 4 refletir colunas binarias, nao so a flag encadeada,
      # que mascara uma condicao "atras" de outra na cascata quando as duas
      # ocorrem juntas). A vigencia de titulo agora e SEMPRE particionada
      # dentro de [ini_j, fim_j], independente de fase/status/licenca ja
      # terem falhado — assim as 6 condicoes sao avaliadas cada uma por conta
      # propria. A cascata apto_na_data/motivo_nao_apto_na_data (usada pela
      # faixa vermelha do grafico e por apto_operar) fica com o MESMO
      # resultado de sempre — so passa a ser derivada das condicoes
      # independentes em vez de if/next; segmentos contiguos com o mesmo
      # motivo continuam sendo remontados por unir_intervalos() nos wrappers
      # (periodos_nao_apto_processo/periodos_aptidao_processo), entao a
      # granularidade extra aqui nao muda o grafico.
      # AJUSTE (2026-07-21): Concessao de Lavra e por prazo INDETERMINADO --
      # nunca tem dt_vencimento (nao e falta de dado, e o regime nao ter
      # prazo). Sem essa isencao, CONC LAV caia sempre em "buraco" (nunca
      # coberto por intervalos_titulo, que exige dt_vencimento nao-nula) e
      # aparecia como "vencimento_sem_data_a_revisar" pra sempre -- mesma
      # causa raiz do achado em 06_serie_temporal.R/titulo_ok_vencimento.
      if (fase_i %in% "CONC LAV") {
        titulo_part <- tibble::tibble(xmin = ini_j, xmax = fim_j, titulo_valido = TRUE, motivo_titulo = NA_character_)
      } else if (nrow(intervalos_titulo) == 0) {
        titulo_part <- tibble::tibble(
          xmin = ini_j, xmax = fim_j, titulo_valido = FALSE, motivo_titulo = "vencimento_sem_data_a_revisar")
      } else {
        cobertura <- unir_intervalos(pmax(intervalos_titulo$xmin, ini_j), pmin(intervalos_titulo$xmax, fim_j))
        cobertura <- cobertura[cobertura$xmin <= cobertura$xmax, , drop = FALSE]
        parte_cobertura <- if (nrow(cobertura) > 0) {
          tibble::tibble(xmin = cobertura$xmin, xmax = cobertura$xmax, titulo_valido = TRUE, motivo_titulo = NA_character_)
        } else {
          tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()), titulo_valido = logical(), motivo_titulo = character())
        }
        buracos <- complementar_intervalos(cobertura, ini_j, fim_j)
        parte_buracos <- if (nrow(buracos) > 0) {
          # buracos$xmin e o dia seguinte ao fim do intervalo de titulo
          # anterior — o vencimento nominal que abriu este buraco. Usa essa
          # data pra checar protocolo de renovacao (art. 211/213).
          motivo_buraco <- vapply(buracos$xmin, function(x0) {
            if (buraco_protegido_211_213(x0 - 1)) "titulo_vencido_renovacao_protocolada" else "titulo_vencido"
          }, character(1))
          tibble::tibble(xmin = buracos$xmin, xmax = buracos$xmax, titulo_valido = FALSE, motivo_titulo = motivo_buraco)
        } else {
          tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()), titulo_valido = logical(), motivo_titulo = character())
        }
        titulo_part <- dplyr::bind_rows(parte_cobertura, parte_buracos)
        titulo_part <- titulo_part[order(titulo_part$xmin), , drop = FALSE]
      }
      if (nrow(titulo_part) == 0) next  # defensivo — nao deveria ocorrer (cobertura+buracos sempre cobrem [ini_j,fim_j])

      for (k in seq_len(nrow(titulo_part))) {
        tk <- titulo_part[k, ]

        flag_fase_nao_operacional                 <- !fase_ok_efetivo
        flag_status_nao_ativo                     <- !status_ok
        flag_sem_licenca_ambiental_previa         <- fase_ok_efetivo && !licenca_ok
        flag_vencimento_sem_data_a_revisar        <- identical(tk$motivo_titulo, "vencimento_sem_data_a_revisar")
        flag_titulo_vencido                       <- identical(tk$motivo_titulo, "titulo_vencido")
        flag_titulo_vencido_renovacao_protocolada <- identical(tk$motivo_titulo, "titulo_vencido_renovacao_protocolada")

        # Cascata — fase > status > licenca > titulo. AJUSTE (2026-07-20/21):
        # renovacao protocolada em dia (art. 211/213), sem indeferimento,
        # conta como apto -- mora administrativa da ANM nao e irregularidade
        # do titular. Achado real: 880391/1987, titulo aparecia "vencido" no
        # historico mesmo com renovacao em dia.
        if (flag_fase_nao_operacional) {
          apto_k <- "em_analise"; motivo_k <- "fase_de_tramitacao_ou_pesquisa"
        } else if (flag_status_nao_ativo) {
          apto_k <- "FALSE"; motivo_k <- "suspensa_ou_encerrada"
        } else if (flag_sem_licenca_ambiental_previa) {
          apto_k <- "FALSE"; motivo_k <- "sem_licenca_ambiental_previa"
        } else if (!tk$titulo_valido && identical(tk$motivo_titulo, "titulo_vencido_renovacao_protocolada")) {
          apto_k <- "TRUE"; motivo_k <- NA_character_
        } else if (!tk$titulo_valido) {
          apto_k <- "FALSE"; motivo_k <- tk$motivo_titulo
        } else {
          apto_k <- "TRUE"; motivo_k <- NA_character_
        }

        segmentos[[length(segmentos) + 1]] <- tibble::tibble(
          xmin = tk$xmin, xmax = tk$xmax, apto_na_data = apto_k, motivo_nao_apto_na_data = motivo_k,
          flag_fase_nao_operacional                 = flag_fase_nao_operacional,
          flag_status_nao_ativo                     = flag_status_nao_ativo,
          flag_sem_licenca_ambiental_previa         = flag_sem_licenca_ambiental_previa,
          flag_vencimento_sem_data_a_revisar        = flag_vencimento_sem_data_a_revisar,
          flag_titulo_vencido                       = flag_titulo_vencido,
          flag_titulo_vencido_renovacao_protocolada = flag_titulo_vencido_renovacao_protocolada)
      }
    }
  }

  if (length(segmentos) == 0) return(NULL)
  out <- dplyr::bind_rows(segmentos)
  out$processo <- processo_alvo
  out[order(out$xmin), c("processo", "xmin", "xmax", "apto_na_data", "motivo_nao_apto_na_data",
                          "flag_fase_nao_operacional", "flag_status_nao_ativo",
                          "flag_sem_licenca_ambiental_previa", "flag_vencimento_sem_data_a_revisar",
                          "flag_titulo_vencido", "flag_titulo_vencido_renovacao_protocolada")]
}

# Wrapper sobre segmentos_aptidao_processo() — so os intervalos NAO apto,
# unidos (usado pela faixa vermelha do grafico). Delegar aqui garante que
# grafico e lista de suspeitos nunca mais divirjam entre si.
periodos_nao_apto_processo <- function(processo_alvo, serie_fase_status, situacao_documental,
                                        protocolos_licenca_ambiental, eventos_renovacao_plg = NULL,
                                        intervalos_gu_aut_pesq = NULL,
                                        data_referencia = Sys.Date()) {
  seg <- segmentos_aptidao_processo(processo_alvo, serie_fase_status, situacao_documental,
                                     protocolos_licenca_ambiental, eventos_renovacao_plg,
                                     intervalos_gu_aut_pesq, data_referencia)
  if (is.null(seg)) return(NULL)
  nao_apto <- seg[seg$apto_na_data != "TRUE", , drop = FALSE]
  if (nrow(nao_apto) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()),
                           motivo_nao_apto_na_data = character()))
  }
  # Agrupa por motivo ANTES de unir intervalos — preserva o motivo (necessario
  # pra colorir o grafico por motivo) e evita fundir num unico retangulo dois
  # periodos adjacentes com motivos diferentes (ex.: titulo_vencido seguido
  # de titulo_vencido_renovacao_protocolada).
  dplyr::bind_rows(lapply(split(nao_apto, nao_apto$motivo_nao_apto_na_data), function(df) {
    u <- unir_intervalos(df$xmin, df$xmax)
    u$motivo_nao_apto_na_data <- df$motivo_nao_apto_na_data[1]
    u
  })) |> dplyr::arrange(xmin)
}

# NOVO (achado 2026-07, pedido direto): versao COMPLETA de
# periodos_nao_apto_processo() — inclui tambem os periodos APTOS (TRUE), pra
# sombrear de verde no grafico o "periodo outorgado real, sem problemas", nao
# so os periodos problematicos. periodos_nao_apto_processo() continua existindo
# como estava (so nao-apto) para quem depender desse contrato; esta e usada
# especificamente para desenhar TODAS as faixas de fundo do grafico.
periodos_aptidao_processo <- function(processo_alvo, serie_fase_status, situacao_documental,
                                       protocolos_licenca_ambiental, eventos_renovacao_plg = NULL,
                                       intervalos_gu_aut_pesq = NULL,
                                       data_referencia = Sys.Date()) {
  seg <- segmentos_aptidao_processo(processo_alvo, serie_fase_status, situacao_documental,
                                     protocolos_licenca_ambiental, eventos_renovacao_plg,
                                     intervalos_gu_aut_pesq, data_referencia)
  if (is.null(seg) || nrow(seg) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()),
                           apto_na_data = character(), motivo_nao_apto_na_data = character()))
  }
  # "__APTO__" agrupa os TRUE (motivo e sempre NA ali, nao serve de chave de
  # grupo); os demais agrupam por motivo, igual periodos_nao_apto_processo().
  grupo <- ifelse(seg$apto_na_data == "TRUE", "__APTO__", seg$motivo_nao_apto_na_data)
  dplyr::bind_rows(lapply(split(seg, grupo), function(df) {
    u <- unir_intervalos(df$xmin, df$xmax)
    u$apto_na_data <- df$apto_na_data[1]
    u$motivo_nao_apto_na_data <- df$motivo_nao_apto_na_data[1]
    u
  })) |> dplyr::arrange(xmin)
}

camada_marcacao <- function(dados, col_data, cor, col_label = NULL,
                             label_italico = FALSE, mostrar_texto = TRUE) {
  if (is.null(dados) || nrow(dados) == 0) return(list())

  dados <- dados |>
    dplyr::mutate(.label = if (!is.null(col_label))
      paste0(.data[[col_label]], " ", format(.data[[col_data]], "%d/%m/%Y"))
      else format(.data[[col_data]], "%d/%m/%Y"))

  camadas <- list(
    ggplot2::geom_vline(
      data = dados, ggplot2::aes(xintercept = .data[[col_data]]),
      color = cor, linetype = "dashed", linewidth = 0.4
    )
  )
  if (mostrar_texto) {
    camadas <- c(camadas, list(
      ggplot2::geom_text(
        data = dados, ggplot2::aes(x = .data[[col_data]], y = Inf, label = .label),
        inherit.aes = FALSE, color = cor, size = 2,
        fontface = if (label_italico) "italic" else "plain",
        angle = 90, hjust = 1.1, vjust = -0.4
      )
    ))
  }
  camadas
}

eventos_marcacao <- function(eventos_classificados, papeis, processos = NULL) {
  if (is.null(eventos_classificados)) return(NULL)
  d <- eventos_classificados |> dplyr::filter(papel %in% papeis)
  if (!is.null(processos)) d <- d |> dplyr::filter(processo %in% processos)
  d
}

grafico_historico_processo <- function(processo_alvo,
                                        dados_cfem = NULL,
                                        situacao_documental = NULL,
                                        protocolos_licenca_ambiental = NULL,
                                        eventos_classificados = NULL,
                                        serie_fase_status = NULL,
                                        eventos_renovacao_plg = NULL,
                                        variavel = c("valor", "peso"),
                                        cores_evento = list(),
                                        mostrar_texto = TRUE,
                                        ncol_facet = 1) {

  variavel <- match.arg(variavel)
  processo_alvo <- as.character(processo_alvo)

  cores <- utils::modifyList(list(
    publicacao = "darkgreen",
    vencimento = "red",
    protocolo  = "black",
    suspensao  = "#B9770E",
    retomada   = "#1F618D",
    anulacao   = "#7B241C",
    protocolo_renovacao_plg = "#16A085",
    baixa      = "#8E44AD"
  ), cores_evento)

  cfem_p <- NULL
  if (!is.null(dados_cfem)) {
    cfem_p <- dados_cfem |>
      dplyr::mutate(PROCESSO = as.character(PROCESSO)) |>
      dplyr::filter(PROCESSO %in% processo_alvo) |>
      dplyr::rename(processo = PROCESSO) |>
      dplyr::mutate(
        data_cfem = if ("data_cfem" %in% names(dados_cfem)) data_cfem
                    else as.Date(sprintf("%04d-%02d-01", ANO, MES))
      )
  }
  tem_cfem <- !is.null(cfem_p) && nrow(cfem_p) > 0

  doc_p <- if (!is.null(situacao_documental))
    situacao_documental |> dplyr::filter(processo %in% processo_alvo) else NULL
  lic_p <- if (!is.null(protocolos_licenca_ambiental))
    protocolos_licenca_ambiental |> dplyr::filter(processo %in% processo_alvo) else NULL
  ev_p <- if (!is.null(eventos_classificados))
    eventos_classificados |> dplyr::filter(processo %in% processo_alvo) else NULL

  publicacao_p <- if (!is.null(doc_p))
    doc_p |> dplyr::filter(!is.na(dt_publicacao)) |>
      dplyr::distinct(processo, dt_publicacao, dssituacaodocumentolegal) else NULL
  vencimento_p <- if (!is.null(doc_p))
    doc_p |> dplyr::filter(!is.na(dt_vencimento)) |>
      dplyr::distinct(processo, dt_vencimento, dssituacaodocumentolegal) else NULL

  # CORRECAO (achado real, processo 850292/2016): mesma logica da versao
  # plotly — a faixa vermelha cruza fase/status + licenca + titulo, nao so
  # vencimento nominal (que pode ja estar irrelevante por renuncia/cassacao/
  # nulidade antes da data nominal).
  gaps_p <- if (!is.null(serie_fase_status)) {
    dplyr::bind_rows(lapply(processo_alvo, function(p) {
      g <- periodos_aptidao_processo(p, serie_fase_status, situacao_documental, protocolos_licenca_ambiental, eventos_renovacao_plg)
      if (is.null(g) || nrow(g) == 0) return(NULL)
      g$processo <- p
      g
    }))
  } else NULL

  protocolo_p <- if (!is.null(lic_p))
    lic_p |> dplyr::mutate(rotulo_fixo = "Lic Amb Protoc") else NULL

  suspensao_p <- eventos_marcacao(ev_p, papeis = "SUSPENDE")
  retomada_p  <- eventos_marcacao(ev_p, papeis = "RETOMA")
  anulacao_p_bruto <- eventos_marcacao(ev_p, papeis = "FECHA")

  # NOVO (achado 2026-07): separa "BAIXA TRANSCRICAO" (Art. 216 e
  # equivalentes) do restante dos fechamentos (cassacao/caducidade/nulidade/
  # indeferimento) — marcador proprio, ja que e o evento mais definitivo do
  # nosso achado de hoje e merece se destacar do FECHA generico no grafico.
  baixa_p    <- NULL
  anulacao_p <- anulacao_p_bruto
  if (!is.null(anulacao_p_bruto) && nrow(anulacao_p_bruto) > 0) {
    eh_baixa <- stringr::str_detect(anulacao_p_bruto$dsevento, "BAIXA TRANSCRI")
    if (any(eh_baixa))  baixa_p    <- anulacao_p_bruto[eh_baixa, , drop = FALSE]
    if (any(!eh_baixa)) anulacao_p <- anulacao_p_bruto[!eh_baixa, , drop = FALSE] else anulacao_p <- NULL
  }

  # NOVO: protocolo de renovacao da PLG (idevento 521, Art. 211) — vem de
  # eventos_renovacao_plg, nao de eventos_classificados, porque papel PROTOC
  # fica de fora dali por design (ver Bloco G do 06_serie_temporal.R).
  renovacao_plg_p <- if (!is.null(eventos_renovacao_plg)) {
    d <- eventos_renovacao_plg |> dplyr::filter(processo %in% processo_alvo, idevento == "521") |>
      dplyr::mutate(rotulo_fixo = "Protocolo renovacao PLG")
    if (nrow(d) > 0) d else NULL
  } else NULL

  eixo_y_label <- if (variavel == "valor") "Valor arrecadado (R$)" else "Peso comercializado (kg)"
  col_y <- if (variavel == "valor") "VALORarr" else "PESO_KG_final"

  if (tem_cfem) {
    p <- ggplot2::ggplot(cfem_p, ggplot2::aes(x = data_cfem, y = .data[[col_y]])) +
      ggplot2::geom_line(ggplot2::aes(group = processo), color = "black", linewidth = 0.3) +
      ggplot2::geom_point(color = "black", size = 0.8)
  } else {
    datas_disponiveis <- c(
      if (!is.null(publicacao_p)) publicacao_p$dt_publicacao,
      if (!is.null(vencimento_p)) vencimento_p$dt_vencimento,
      if (!is.null(protocolo_p))  protocolo_p$dt_protocolo,
      if (!is.null(ev_p))         ev_p$dtevento,
      if (!is.null(renovacao_plg_p)) renovacao_plg_p$dtevento
    )
    datas_disponiveis <- as.Date(datas_disponiveis, origin = "1970-01-01")
    if (length(datas_disponiveis) == 0) datas_disponiveis <- c(Sys.Date() - 3650, Sys.Date())

    esqueleto <- tibble::tibble(
      processo = rep(processo_alvo, each = 2),
      x = rep(c(min(datas_disponiveis), max(datas_disponiveis)), length(processo_alvo)),
      y = 0
    )
    p <- ggplot2::ggplot(esqueleto, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_blank() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  if (!is.null(gaps_p) && nrow(gaps_p) > 0) {
    gaps_p$cor_motivo <- dplyr::case_when(
      gaps_p$apto_na_data == "TRUE" ~ COR_APTO_HEX,
      TRUE ~ dplyr::coalesce(CORES_MOTIVO_NAO_APTO_HEX[gaps_p$motivo_nao_apto_na_data], CORES_MOTIVO_NAO_APTO_HEX_DEFAULT)
    )
    p <- p + ggplot2::geom_rect(
      data = gaps_p, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = cor_motivo),
      inherit.aes = FALSE, alpha = 0.15
    ) + ggplot2::scale_fill_identity()
  }

  p <- p +
    camada_marcacao(publicacao_p, "dt_publicacao", cores$publicacao,
                     col_label = "dssituacaodocumentolegal", mostrar_texto = mostrar_texto) +
    camada_marcacao(vencimento_p, "dt_vencimento", cores$vencimento,
                     col_label = "dssituacaodocumentolegal", label_italico = TRUE, mostrar_texto = mostrar_texto) +
    camada_marcacao(protocolo_p, "dt_protocolo", cores$protocolo,
                     col_label = "rotulo_fixo", mostrar_texto = mostrar_texto) +
    camada_marcacao(suspensao_p, "dtevento", cores$suspensao,
                     col_label = "dsevento", mostrar_texto = mostrar_texto) +
    camada_marcacao(retomada_p, "dtevento", cores$retomada,
                     col_label = "dsevento", mostrar_texto = mostrar_texto) +
    camada_marcacao(anulacao_p, "dtevento", cores$anulacao,
                     col_label = "dsevento", label_italico = TRUE, mostrar_texto = mostrar_texto) +
    camada_marcacao(renovacao_plg_p, "dtevento", cores$protocolo_renovacao_plg,
                     col_label = "rotulo_fixo", mostrar_texto = mostrar_texto) +
    camada_marcacao(baixa_p, "dtevento", cores$baixa,
                     col_label = "dsevento", label_italico = TRUE, mostrar_texto = mostrar_texto)

  if (tem_cfem) {
    p <- p + ggplot2::scale_y_continuous(
      labels = if (variavel == "valor") \(x) paste0("R$ ", formata_num_br(x))
               else \(x) paste0(formata_num_br(x), " kg")
    )
  }

  if (length(processo_alvo) > 1) {
    p <- p + ggplot2::facet_wrap(~ processo, scales = "fixed", ncol = ncol_facet)
  }

  p + ggplot2::labs(x = NULL, y = eixo_y_label) + tema_historico_processo
}

# -----------------------------------------------------------------------------
# VERSAO PLOTLY NATIVA (nao ggplotly) — para uso interativo no Shiny (aba 4).
# Mesmo padrao visual do app.R original (plot_ly() direto, hovertemplate,
# shapes de layout para a faixa vermelha) — e por isso funciona bem: nunca
# passa pela conversao ggplot2->plotly, que foi o que quebrava a faixa
# vermelha e os labels antes. As marcacoes de evento (vencimento/suspensao/
# retomada/anulacao/publicacao/protocolo) entram como tracinhos coloridos na
# base do grafico (symbol "line-ns-open"), com o texto no HOVER em vez de
# rotacionado sempre visivel — menos poluido, mesma informacao.
#
# Diferenca de uso: so aceita 1 processo por vez (uso interativo no dossie,
# nao para relatorio/facet — para isso, use grafico_historico_processo()).
grafico_historico_processo_plotly <- function(processo_alvo,
                                               dados_cfem = NULL,
                                               situacao_documental = NULL,
                                               protocolos_licenca_ambiental = NULL,
                                               eventos_classificados = NULL,
                                               serie_fase_status = NULL,
                                               eventos_renovacao_plg = NULL,
                                               variavel = c("valor", "peso"),
                                               cores_evento = list()) {

  variavel <- match.arg(variavel)
  processo_alvo <- as.character(processo_alvo)[1]

  cores <- utils::modifyList(list(
    linha = "#1B4332", ponto_ok = "#2D6A4F", ponto_alerta = "#C0392B",
    publicacao = "#1B7A3D", vencimento = "#C0392B", protocolo = "#2C3E50",
    suspensao  = "#B9770E", retomada = "#1F618D", anulacao = "#7B241C",
    protocolo_renovacao_plg = "#16A085", baixa = "#8E44AD"
  ), cores_evento)

  col_y <- if (variavel == "valor") "VALORarr" else "PESO_KG_final"
  eixo_y_titulo <- if (variavel == "valor") "R$" else "kg"

  cfem_p <- NULL
  if (!is.null(dados_cfem)) {
    cfem_p <- dados_cfem |>
      dplyr::mutate(PROCESSO = as.character(PROCESSO)) |>
      dplyr::filter(PROCESSO == processo_alvo) |>
      dplyr::mutate(data_cfem = if ("data_cfem" %in% names(dados_cfem)) data_cfem
                                 else as.Date(sprintf("%04d-%02d-01", ANO, MES))) |>
      dplyr::arrange(data_cfem)
  }
  tem_cfem <- !is.null(cfem_p) && nrow(cfem_p) > 0

  doc_p <- if (!is.null(situacao_documental)) situacao_documental |> dplyr::filter(processo == processo_alvo) else NULL
  lic_p <- if (!is.null(protocolos_licenca_ambiental)) protocolos_licenca_ambiental |> dplyr::filter(processo == processo_alvo) else NULL
  ev_p  <- if (!is.null(eventos_classificados)) eventos_classificados |> dplyr::filter(processo == processo_alvo) else NULL

  publicacao_p <- if (!is.null(doc_p)) doc_p |> dplyr::filter(!is.na(dt_publicacao)) |> dplyr::distinct(dt_publicacao, dssituacaodocumentolegal) else NULL
  vencimento_p <- if (!is.null(doc_p)) doc_p |> dplyr::filter(!is.na(dt_vencimento)) |> dplyr::distinct(dt_vencimento, dssituacaodocumentolegal) else NULL

  # CORRECAO (achado real, processo 850292/2016): a faixa vermelha nao pode
  # olhar so vencimento NOMINAL do titulo — um titulo pode morrer ANTES do
  # vencimento nominal por renuncia/cassacao/nulidade/revogacao (evento
  # FECHA). periodos_nao_apto_processo() cruza fase/status + licenca +
  # titulo, os mesmos 3 criterios ja aprovados, como intervalos.
  gaps_p <- periodos_aptidao_processo(processo_alvo, serie_fase_status, situacao_documental, protocolos_licenca_ambiental, eventos_renovacao_plg)

  suspensao_p <- eventos_marcacao(ev_p, papeis = "SUSPENDE")
  retomada_p  <- eventos_marcacao(ev_p, papeis = "RETOMA")
  anulacao_p_bruto <- eventos_marcacao(ev_p, papeis = "FECHA")

  # NOVO (achado 2026-07): mesma separacao da versao ggplot — baixa de
  # transcricao ganha marcador proprio, separado do FECHA generico.
  baixa_p    <- NULL
  anulacao_p <- anulacao_p_bruto
  if (!is.null(anulacao_p_bruto) && nrow(anulacao_p_bruto) > 0) {
    eh_baixa <- stringr::str_detect(anulacao_p_bruto$dsevento, "BAIXA TRANSCRI")
    if (any(eh_baixa))  baixa_p    <- anulacao_p_bruto[eh_baixa, , drop = FALSE]
    if (any(!eh_baixa)) anulacao_p <- anulacao_p_bruto[!eh_baixa, , drop = FALSE] else anulacao_p <- NULL
  }

  # NOVO: protocolo de renovacao da PLG (idevento 521, Art. 211) — vem de
  # eventos_renovacao_plg (papel PROTOC, fora de eventos_classificados).
  renovacao_plg_p <- if (!is.null(eventos_renovacao_plg)) {
    d <- eventos_renovacao_plg |> dplyr::filter(processo == processo_alvo, idevento == "521")
    if (nrow(d) > 0) d else NULL
  } else NULL
  # --- shapes: faixas de fundo, verde quando apto, coloridas por motivo
  # quando nao apto (achado 2026-07) ---
  shapes <- list()
  if (!is.null(gaps_p) && nrow(gaps_p) > 0) {
    for (i in seq_len(nrow(gaps_p))) {
      if (!is.na(gaps_p$apto_na_data[i]) && gaps_p$apto_na_data[i] == "TRUE") {
        cor_i <- COR_APTO_RGBA
      } else {
        cor_i <- CORES_MOTIVO_NAO_APTO_RGBA[gaps_p$motivo_nao_apto_na_data[i]]
        if (is.na(cor_i)) cor_i <- CORES_MOTIVO_NAO_APTO_RGBA_DEFAULT
      }
      shapes[[length(shapes) + 1]] <- list(
        type = "rect", xref = "x", yref = "paper",
        x0 = as.character(gaps_p$xmin[i]), x1 = as.character(gaps_p$xmax[i]),
        y0 = 0, y1 = 1, fillcolor = unname(cor_i), line = list(width = 0), layer = "below"
      )
    }
  }

  # --- marcacoes de evento: acumula p/ trace de hover (tracinhos na base) ---
  marcas <- list()
  empilhar_marca <- function(datas, rotulos, cor) {
    if (is.null(datas) || length(datas) == 0) return(invisible(NULL))
    marcas[[length(marcas) + 1]] <<- data.frame(x = as.Date(datas), texto = rotulos, cor = cor, stringsAsFactors = FALSE)
  }
  if (!is.null(publicacao_p) && nrow(publicacao_p) > 0)
    empilhar_marca(publicacao_p$dt_publicacao, paste0(publicacao_p$dssituacaodocumentolegal, " — ", format(publicacao_p$dt_publicacao, "%d/%m/%Y")), cores$publicacao)
  if (!is.null(vencimento_p) && nrow(vencimento_p) > 0)
    empilhar_marca(vencimento_p$dt_vencimento, paste0(vencimento_p$dssituacaodocumentolegal, " — ", format(vencimento_p$dt_vencimento, "%d/%m/%Y")), cores$vencimento)
  if (!is.null(lic_p) && nrow(lic_p) > 0)
    empilhar_marca(lic_p$dt_protocolo, paste0("Lic Amb Protoc — ", format(lic_p$dt_protocolo, "%d/%m/%Y")), cores$protocolo)
  if (!is.null(suspensao_p) && nrow(suspensao_p) > 0)
    empilhar_marca(suspensao_p$dtevento, paste0(suspensao_p$dsevento, " — ", format(suspensao_p$dtevento, "%d/%m/%Y")), cores$suspensao)
  if (!is.null(retomada_p) && nrow(retomada_p) > 0)
    empilhar_marca(retomada_p$dtevento, paste0(retomada_p$dsevento, " — ", format(retomada_p$dtevento, "%d/%m/%Y")), cores$retomada)
  if (!is.null(anulacao_p) && nrow(anulacao_p) > 0)
    empilhar_marca(anulacao_p$dtevento, paste0(anulacao_p$dsevento, " — ", format(anulacao_p$dtevento, "%d/%m/%Y")), cores$anulacao)
  if (!is.null(baixa_p) && nrow(baixa_p) > 0)
    empilhar_marca(baixa_p$dtevento, paste0(baixa_p$dsevento, " — ", format(baixa_p$dtevento, "%d/%m/%Y")), cores$baixa)
  if (!is.null(renovacao_plg_p) && nrow(renovacao_plg_p) > 0)
    empilhar_marca(renovacao_plg_p$dtevento, paste0("Protocolo renovacao PLG — ", format(renovacao_plg_p$dtevento, "%d/%m/%Y")), cores$protocolo_renovacao_plg)
  marcas_df <- if (length(marcas) > 0) dplyr::bind_rows(marcas) else NULL

  # --- trace principal: CFEM (linha+pontos) ou esqueleto vazio (sem CFEM) ---
  if (tem_cfem) {
    cor_ponto <- ifelse(!is.na(cfem_p$apto_na_data) & cfem_p$apto_na_data != "TRUE",
                         cores$ponto_alerta, cores$ponto_ok)
    p <- plotly::plot_ly(
      cfem_p, x = ~data_cfem, y = stats::as.formula(paste0("~", col_y)),
      type = "scatter", mode = "lines+markers",
      line = list(color = cores$linha, width = 2),
      marker = list(color = cor_ponto, size = 7),
      hovertemplate = paste0("%{x|%d/%m/%Y}<br>",
                             if (variavel == "valor") "R$ %{y:,.2f}" else "%{y:,.2f} kg",
                             "<extra></extra>"),
      name = ""
    )
  } else {
    datas_disponiveis <- c(
      if (!is.null(publicacao_p)) publicacao_p$dt_publicacao,
      if (!is.null(vencimento_p)) vencimento_p$dt_vencimento,
      if (!is.null(lic_p))        lic_p$dt_protocolo,
      if (!is.null(ev_p))         ev_p$dtevento
    )
    datas_disponiveis <- as.Date(datas_disponiveis, origin = "1970-01-01")
    if (length(datas_disponiveis) == 0) datas_disponiveis <- c(Sys.Date() - 3650, Sys.Date())
    p <- plotly::plot_ly(
      x = c(min(datas_disponiveis), max(datas_disponiveis)), y = c(0, 0),
      type = "scatter", mode = "markers", opacity = 0, hoverinfo = "none", showlegend = FALSE
    )
  }

  if (!is.null(marcas_df) && nrow(marcas_df) > 0) {
    p <- p |> plotly::add_markers(
      data = marcas_df, x = ~x, y = 0, inherit = FALSE, showlegend = FALSE,
      marker = list(color = marcas_df$cor, size = 10, symbol = "line-ns-open", line = list(width = 2)),
      text = ~texto, hovertemplate = "%{text}<extra></extra>"
    )
  }

  p |> plotly::layout(
    shapes = shapes,
    xaxis = list(title = ""), yaxis = list(title = eixo_y_titulo),
    margin = list(l = 60, r = 20, t = 10, b = 30),
    showlegend = FALSE
  )
}