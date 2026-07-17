################################################################################
# 05b_log_correcao_pontual_cfem.R
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
})

source(here::here("R", "utils.R"))

QA_DIR <- here::here("data", "_qa", "correcao_pontual_cfem")
dir.create(QA_DIR, recursive = TRUE, showWarnings = FALSE)

cfem_final <- load_ckpt("05_cfem_final")

# NOTA (2026-07-16): apos a troca do metodo da CASSITERITA para o white
# solder, cfem_correcao_extrema == 1 passou a incluir tambem 'dado_corrompido'
# -- casos em que NENHUM fator de 10 reconcilia o peso com a faixa plausivel.
# Nesses, PESO_KG_final == PESO_KG (peso intocado), logo fator_correcao == 1 e
# log10() == 0, o que os faria aparecer como direcao "nenhum", misturados com
# correcoes reais. A coluna 'tipo_caso' separa os dois regimes.
extremos <- cfem_final |>
  dplyr::filter(cfem_correcao_extrema == 1) |>
  dplyr::mutate(
    tipo_caso = dplyr::case_when(
      corr == "sem_quantidade_declarada" ~ "sem_quantidade",
      corr == "dado_corrompido"          ~ "nao_reconciliavel",
      TRUE                               ~ "correcao_aplicada"
    ),
    fator_correcao = dplyr::if_else(
      !is.na(PESO_KG) & PESO_KG > 0 &
        !corr %in% c("dado_corrompido", "sem_quantidade_declarada"),
      PESO_KG_final / PESO_KG, NA_real_),
    expoente_10     = round(log10(fator_correcao)),
    direcao         = dplyr::case_when(
      corr == "sem_quantidade_declarada" ~ "sem_quantidade",
      corr == "dado_corrompido"          ~ "nao_reconciliavel",
      is.na(expoente_10)                 ~ NA_character_,
      expoente_10 < 0                    ~ "dividir",
      expoente_10 > 0                    ~ "multiplicar",
      TRUE                               ~ "nenhum"
    ),
    casas_decimais  = abs(expoente_10)
  )

# --- Registro oficial: 1 linha por caso de correção pontual --------------------

log_correcao_pontual <- extremos |>
  dplyr::select(PROCESSO, SUBSarr, tipo_caso, CPF_CNPJarr, NOME_arr, ANO, MES, UM,
                QTD_MINERIO, PESO_KG, PESO_KG_final, fator_correcao,
                expoente_10, direcao, casas_decimais,
                preco_g_orig, preco_g_final, corr) |>
  dplyr::arrange(SUBSarr, tipo_caso, dplyr::desc(casas_decimais))

readr::write_csv(log_correcao_pontual, file.path(QA_DIR, "log_correcao_pontual_cfem.csv"))

# --- Resumo por mineral x expoente x direção (auditoria agregada) --------------
resumo_expoente <- extremos |> dplyr::count(SUBSarr, tipo_caso, direcao, expoente_10, sort = TRUE)
readr::write_csv(resumo_expoente, file.path(QA_DIR, "resumo_expoente.csv"))

# --- Resumo por declarante ------------------------------------------------------
# NOTA (2026-07-16): expoente_predominante so faz sentido para casos com
# correcao aplicada. Declarantes que tenham APENAS 'nao_reconciliavel' tem
# expoente_10 inteiramente NA; table() ignora NA e devolve tabela vazia, logo
# names(...)[1] devolve NULL -- e summarise() nao consegue combinar NULL com
# character entre grupos ("Can't combine NULL and non NULL results").
# moda_expoente() blinda esse caso devolvendo NA_character_ explicito.
moda_expoente <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tb <- sort(table(x), decreasing = TRUE)
  names(tb)[1]
}

resumo_declarante <- extremos |>
  dplyr::group_by(SUBSarr, CPF_CNPJarr, NOME_arr) |>
  dplyr::summarise(
    n_registros           = dplyr::n(),
    n_corr_aplicada       = sum(tipo_caso == "correcao_aplicada", na.rm = TRUE),
    n_nao_reconciliavel   = sum(tipo_caso == "nao_reconciliavel", na.rm = TRUE),
    n_sem_quantidade      = sum(tipo_caso == "sem_quantidade", na.rm = TRUE),
    n_expoentes_distintos = dplyr::n_distinct(expoente_10, na.rm = TRUE),
    expoente_predominante = moda_expoente(expoente_10),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n_registros))
readr::write_csv(resumo_declarante, file.path(QA_DIR, "resumo_declarante.csv"))

message(sprintf("[correcao_pontual_cfem] registros no log de auditoria: %d", nrow(log_correcao_pontual)))
message("[correcao_pontual_cfem] log oficial salvo em: ", file.path(QA_DIR, "log_correcao_pontual_cfem.csv"))
message("\n=== 05b_log_correcao_pontual_cfem.R — CONCLUÍDO ===")