-- Índices espaciais nas tabelas que vieram do Python (não têm índice ainda)
CREATE INDEX IF NOT EXISTS idx_processos_geom
    ON processos USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_ti_geom
    ON terras_indigenas USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_uc_geom
    ON unidades_conservacao USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_qui_geom
    ON quilombolas USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_emb_ib_geom
    ON embargos_ibama USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_emb_ic_geom
    ON embargos_icmbio USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_emb_mt_geom
    ON embargos_sema_mt USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_emb_siga_geom
    ON embargos_sema_mt_siga USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_inf_siga_geom
    ON infracoes_sema_mt_siga USING GIST (geometry);

-- Índices tabulares no CFEM
CREATE INDEX IF NOT EXISTS idx_cfem_processo
    ON cfem_eventos (processo);

CREATE INDEX IF NOT EXISTS idx_cfem_ano_mes
    ON cfem_eventos (ano, mes);

CREATE INDEX IF NOT EXISTS idx_cfem_subs
    ON cfem_eventos (subs_arr_sim);