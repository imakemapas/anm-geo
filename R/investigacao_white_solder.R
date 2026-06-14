# CASSITERITA PLG — correção de unidade, base enxuta e resumo por processo
# Investigação CFEM/ANM.

options(scipen = 999)
library(dplyr)
library(tidyr)
library(writexl)
library(readr)

cfem    <- readRDS(here::here("data", "_checkpoints", "06_cfem_final.rds"))
dir_out <- here::here("data", "result_investigacao")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

PRECO_MIN <- 30
PRECO_MAX <- 300
FATORES   <- 10^(-6:6)

corrige_unidade <- function(peso, valortot) {
  if (is.na(peso) || is.na(valortot) || peso <= 0 || valortot <= 0)
    return(tibble(peso_corr = peso, fator = NA_real_, flag = "sem_dado"))
  ok <- FATORES[ {p <- valortot / (peso * FATORES); p >= PRECO_MIN & p <= PRECO_MAX} ]
  if (length(ok) == 0)
    return(tibble(peso_corr = peso, fator = NA_real_, flag = "dado_corrompido"))
  f <- ok[which.min(abs(log10(ok)))]
  tibble(peso_corr = peso * f, fator = f,
         flag = if (f == 1) "ok" else paste0("corr_1e", round(log10(f))))
}

base_raw <- cfem |>
  filter(ANO >= 2020,
         SUBSarr == "CASSITERITA",
         FASE %in% c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA",
                     "LICENCIAMENTO", "AUTORIZAÇÃO DE PESQUISA"),
         code_muni != 0)

corr_tbl <- base_raw |>
  mutate(.id = row_number()) |>
  rowwise() |>
  reframe(.id, corrige_unidade(PESO_KG, VALORtot)) |>
  ungroup()

base <- base_raw |>
  mutate(.id = row_number()) |>
  left_join(corr_tbl, by = ".id") |>
  mutate(rs_por_kg = VALORtot / peso_corr)

cat("=== Correção — flags ===\n")
print(count(base, flag))

cat("\n=== Caso(s) dado_corrompido (não corrigível por fator de 10) ===\n")
base |>
  filter(flag == "dado_corrompido") |>
  select(ANO, MES, NOME_arr, TITULAR, PROCESSO, PESO_KG, VALORtot) |>
  print(n = Inf)

raw <- base |>
  select(ANO, MES, abbrev_state, name_muni, code_muni,
         NOME_arr, CPF_CNPJarr, TITULAR, CPF_CNPJcm,
         PROCESSO, FASE,
         PESO_KG, peso_corr, fator, flag,
         VALORarr, VALORtot, rs_por_kg)

write_xlsx(raw, file.path(dir_out, "cassiterita_plg_corrigido.xlsx"))

processos <- sort(unique(base$PROCESSO))
print(processos)

top_buyer <- base |>
  count(PROCESSO, NOME_arr, name = "n_decl") |>
  group_by(PROCESSO) |>
  slice_max(n_decl, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(PROCESSO, arr_topb = NOME_arr)

resumo_cfem <- base |>
  filter(flag != "dado_corrompido") |>        
  group_by(PROCESSO) |>
  summarise(
    TITULAR      = first(TITULAR),
    CPF_CNPJcm   = first(CPF_CNPJcm),
    FASE         = first(FASE),
    name_muni    = first(name_muni),
    abbrev_state = first(abbrev_state),
    code_muni    = first(code_muni),
    arr_kg_T  = sum(peso_corr, na.rm = TRUE),
    arr_g_T   = sum(peso_corr, na.rm = TRUE) * 1000,
    arr_val_T = sum(VALORarr, na.rm = TRUE),     # CFEM recolhida (R$)
    arr_fat_T = sum(VALORtot, na.rm = TRUE),     # faturamento (R$)
    arr_ndcl  = n(),                             # nº de declarações
    arr_nbuy  = n_distinct(CPF_CNPJarr),         # nº de compradores distintos
    arr_dt_F  = min(as.Date(sprintf("%04d-%02d-01", ANO, MES))),
    arr_dt_L  = max(as.Date(sprintf("%04d-%02d-01", ANO, MES))),
    anos      = paste(sort(unique(ANO)), collapse = ","),
    .groups = "drop"
  ) |>
  left_join(top_buyer, by = "PROCESSO") |>
  mutate(rs_por_kgT = arr_val_T / arr_kg_T,      # sanidade (CFEM/kg)
         rs_por_kgF = arr_fat_T / arr_kg_T) |>   # sanidade (faturamento/kg ~ preço de venda)
  arrange(desc(arr_kg_T))

print(
  resumo_cfem |>
    select(PROCESSO, TITULAR, abbrev_state, arr_kg_T, arr_val_T, arr_fat_T,
           arr_ndcl, arr_nbuy, arr_topb, rs_por_kgF) |>
    slice_head(n = 20),
  n = 20
)

write_xlsx(resumo_cfem, file.path(dir_out, "cassiterita_plg_resumo_processo.xlsx"))
write_csv(resumo_cfem,  file.path(dir_out, "cassiterita_plg_resumo_processo.csv"))