-- =============================================================================
-- ÍNDICES ESPACIAIS E TABULARES
-- Rodar após: 01_schema.sql e carga dos dados
-- =============================================================================

-- -----------------------------------------------------------------------------
-- Processos
-- -----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_processos_geom
    ON processos USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_processos_fase
    ON processos (fase);

CREATE INDEX IF NOT EXISTS idx_processos_subs_grp
    ON processos (subs_grp);

-- -----------------------------------------------------------------------------
-- Terras Indígenas
-- -----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_ti_geom
    ON terras_indigenas USING GIST (geometry);

-- -----------------------------------------------------------------------------
-- Unidades de Conservação
-- -----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_uc_geom
    ON unidades_conservacao USING GIST (geometry);

-- -----------------------------------------------------------------------------
-- Quilombolas
-- -----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_qui_geom
    ON quilombolas USING GIST (geometry);

-- -----------------------------------------------------------------------------
-- Embargos e Infrações
-- -----------------------------------------------------------------------------
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

-- -----------------------------------------------------------------------------
-- CFEM eventos — índices tabulares
-- -----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_cfem_processo
    ON cfem_eventos (processo);

CREATE INDEX IF NOT EXISTS idx_cfem_ano_mes
    ON cfem_eventos (ano, mes);

CREATE INDEX IF NOT EXISTS idx_cfem_subs
    ON cfem_eventos (subs_arr_sim);

CREATE INDEX IF NOT EXISTS idx_cfem_cpf_cnpj
    ON cfem_eventos (cpf_cnpj_arr);

-- -----------------------------------------------------------------------------
-- Autuações
-- -----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_aut_processo
    ON cfem_autuacoes (processo);

-- -----------------------------------------------------------------------------
-- Microdados SCM
-- -----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_micro_proc_ds
    ON micro_processo (dsprocesso);

CREATE INDEX IF NOT EXISTS idx_micro_evento_proc
    ON micro_processo_evento (dsprocesso);

CREATE INDEX IF NOT EXISTS idx_micro_evento_tipo
    ON micro_processo_evento (idevento);

CREATE INDEX IF NOT EXISTS idx_micro_evento_dt
    ON micro_processo_evento (dtevento);

CREATE INDEX IF NOT EXISTS idx_micro_pp_proc
    ON micro_processo_pessoa (dsprocesso);

CREATE INDEX IF NOT EXISTS idx_micro_pp_pessoa
    ON micro_processo_pessoa (idpessoa);

CREATE INDEX IF NOT EXISTS idx_micro_subs_proc
    ON micro_processo_substancia (dsprocesso);

CREATE INDEX IF NOT EXISTS idx_micro_mun_proc
    ON micro_processo_municipio (dsprocesso);

CREATE INDEX IF NOT EXISTS idx_micro_tit_proc
    ON micro_processo_titulo (dsprocesso);

CREATE INDEX IF NOT EXISTS idx_micro_pessoa_cpf
    ON micro_pessoa (nrcpfcnpj);

CREATE INDEX IF NOT EXISTS idx_micro_pessoa_id
    ON micro_pessoa (idpessoa);
