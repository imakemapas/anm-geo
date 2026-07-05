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

extremos <- cfem_final |>
  dplyr::filter(cfem_correcao_extrema == 1) |>
  dplyr::mutate(
    fator_correcao = dplyr::if_else(!is.na(PESO_KG) & PESO_KG > 0,
                                    PESO_KG_final / PESO_KG, NA_real_),
    expoente_10     = round(log10(fator_correcao)),
    direcao         = dplyr::case_when(
      is.na(expoente_10)  ~ NA_character_,
      expoente_10 < 0      ~ "dividir",
      expoente_10 > 0      ~ "multiplicar",
      TRUE                 ~ "nenhum"
    ),
    casas_decimais  = abs(expoente_10)
  )

# --- Registro oficial: 1 linha por caso de correção pontual --------------------

log_correcao_pontual <- extremos |>
  dplyr::select(PROCESSO, SUBSarr, CPF_CNPJarr, NOME_arr, ANO, MES, UM,
                QTD_MINERIO, PESO_KG, PESO_KG_final, fator_correcao,
                expoente_10, direcao, casas_decimais,
                preco_g_orig, preco_g_final, corr) |>
  dplyr::arrange(SUBSarr, dplyr::desc(casas_decimais))

readr::write_csv(log_correcao_pontual, file.path(QA_DIR, "log_correcao_pontual_cfem.csv"))

# --- Resumo por mineral x expoente x direção (auditoria agregada) --------------
resumo_expoente <- extremos |> dplyr::count(SUBSarr, direcao, expoente_10, sort = TRUE)
readr::write_csv(resumo_expoente, file.path(QA_DIR, "resumo_expoente.csv"))

# --- Resumo por declarante ------------------------------------------------------
resumo_declarante <- extremos |>
  dplyr::group_by(SUBSarr, CPF_CNPJarr, NOME_arr) |>
  dplyr::summarise(
    n_registros           = dplyr::n(),
    n_expoentes_distintos = dplyr::n_distinct(expoente_10),
    expoente_predominante = names(sort(table(expoente_10), decreasing = TRUE))[1],
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n_registros))
readr::write_csv(resumo_declarante, file.path(QA_DIR, "resumo_declarante.csv"))

message(sprintf("[correcao_pontual_cfem] registros no log de auditoria: %d", nrow(log_correcao_pontual)))
message("[correcao_pontual_cfem] log oficial salvo em: ", file.path(QA_DIR, "log_correcao_pontual_cfem.csv"))
message("\n=== 05b_log_correcao_pontual_cfem.R — CONCLUÍDO ===")