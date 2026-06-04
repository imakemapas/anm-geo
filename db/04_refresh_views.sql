-- =============================================================================
-- REFRESH DAS MATERIALIZED VIEWS
-- Rodar após cada atualização de dados:
--   1. Scripts R (01_download → 02_pre_proc → 03_final_proc)
--   2. Python etl_load/01_load_postgis.py
--   3. Este script
-- =============================================================================

REFRESH MATERIALIZED VIEW mv_cfem_anual;
REFRESH MATERIALIZED VIEW mv_cfem_mensal;
REFRESH MATERIALIZED VIEW mv_sobreposicoes;
REFRESH MATERIALIZED VIEW mv_historico_titular;