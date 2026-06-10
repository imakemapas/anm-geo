<<<<<<< Updated upstream
-- =============================================================================
-- SCHEMA ANM_GEO
-- =============================================================================
=======
-- ============================================================================
-- 01_schema.sql  —  cria tabelas VAZIAS, com tipos fortes e PKs
-- ----------------------------------------------------------------------------
-- Abordagem robusta: o SQL define a estrutura; o Python insere (append).
-- Rodar conectado ao anm_geo:   \c anm_geo   depois   \i 01_schema.sql
-- ============================================================================
>>>>>>> Stashed changes

-- -----------------------------------------------------------------------------
-- Processos minerários (geometria do SIGMINE)
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS processos (
    processo        TEXT PRIMARY KEY,
    fase            TEXT,
    ult_evento      TEXT,
    ult_ev_id       TEXT,
    ult_ev_dat      TEXT,
    ult_ev_des      TEXT,
    titular         TEXT,
    subs            TEXT,
    subs_grp        TEXT,
    area_ha         NUMERIC,
    area_orig       NUMERIC,
    tipo_req        TEXT,
    cpf_cnpj_cm     TEXT,
    code_muni       TEXT,
    name_muni       TEXT,
    abbrev_state    TEXT,
    name_state      TEXT,
    name_region     TEXT,
    geometry        GEOMETRY(GEOMETRY, 4326)
);

CREATE INDEX IF NOT EXISTS idx_processos_geom
    ON processos USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_processos_fase
    ON processos (fase);

CREATE INDEX IF NOT EXISTS idx_processos_subs_grp
    ON processos (subs_grp);

-- -----------------------------------------------------------------------------
-- CFEM arrecadação — granularidade mínima (uma linha por declaração)
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS cfem_eventos (
    row_id          BIGINT PRIMARY KEY,
    processo        TEXT,
    ano             INTEGER,
    mes             INTEGER,
    subs_arr        TEXT,
    subs_arr_sim    TEXT,
    cpf_cnpj_arr    TEXT,
    nome_arr        TEXT,
    code_muni       TEXT,
    name_muni       TEXT,
    abbrev_state    TEXT,
    name_state      TEXT,
    name_region     TEXT,
    qtd_minerio     NUMERIC,
    um              TEXT,
    peso_kg         NUMERIC,
    peso_g          NUMERIC,
    valor_arr       NUMERIC,
    valor_tot       NUMERIC,
    aliquota_pct    NUMERIC,
    peso_kg_final   NUMERIC,
    peso_g_final    NUMERIC,
    preco_g_orig    NUMERIC,
    preco_g_final   NUMERIC,
    corr            TEXT,
    data            DATE,
    -- atributos do processo associado
    area_ha         NUMERIC,
    fase            TEXT,
    ult_evento      TEXT,
    ult_ev_id       TEXT,
    ult_ev_dat      TEXT,
    ult_ev_des      TEXT,
    titular         TEXT,
    subs            TEXT,
    tipo_req        TEXT,
    cpf_cnpj_cm     TEXT
);

CREATE INDEX IF NOT EXISTS idx_cfem_processo
    ON cfem_eventos (processo);

CREATE INDEX IF NOT EXISTS idx_cfem_ano_mes
    ON cfem_eventos (ano, mes);

CREATE INDEX IF NOT EXISTS idx_cfem_subs_arr_sim
    ON cfem_eventos (subs_arr_sim);

CREATE INDEX IF NOT EXISTS idx_cfem_cpf_cnpj
    ON cfem_eventos (cpf_cnpj_arr);

-- -----------------------------------------------------------------------------
-- Terras Indígenas
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS terras_indigenas (
    id              SERIAL PRIMARY KEY,
    terrai_nom      TEXT,
    modalidade      TEXT,
    fase_ti         TEXT,
    etnia_nome      TEXT,
    geometry        GEOMETRY(GEOMETRY, 4326)
);

CREATE INDEX IF NOT EXISTS idx_ti_geom
    ON terras_indigenas USING GIST (geometry);

-- -----------------------------------------------------------------------------
-- Unidades de Conservação
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS unidades_conservacao (
    id              SERIAL PRIMARY KEY,
    nome_uc         TEXT,
    sigla_snuc      TEXT,
    grupo           TEXT,
    categoria       TEXT,
    org_gestor      TEXT,
    pl_manejo       TEXT,
    geometry        GEOMETRY(GEOMETRY, 4326)
);

CREATE INDEX IF NOT EXISTS idx_uc_geom
    ON unidades_conservacao USING GIST (geometry);

CREATE INDEX IF NOT EXISTS idx_uc_sigla
    ON unidades_conservacao (sigla_snuc);

-- -----------------------------------------------------------------------------
-- Quilombolas
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS quilombolas (
    id              SERIAL PRIMARY KEY,
    nm_comunid      TEXT,
    geometry        GEOMETRY(GEOMETRY, 4326)
);

CREATE INDEX IF NOT EXISTS idx_qui_geom
    ON quilombolas USING GIST (geometry);

-- -----------------------------------------------------------------------------
-- CFEM autuações
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS cfem_autuacoes (
    id              SERIAL PRIMARY KEY,
    processo        TEXT,
    ano             INTEGER,
    mes             INTEGER,
    subs_aut        TEXT,
    cpf_cnpj_aut    TEXT,
    titular_aut     TEXT,
    name_muni       TEXT,
    abbrev_state    TEXT,
    valor_aut       NUMERIC,
    code_muni       TEXT,
    name_state      TEXT,
    name_region     TEXT
);

CREATE INDEX IF NOT EXISTS idx_aut_processo
    ON cfem_autuacoes (processo);

CREATE TABLE IF NOT EXISTS embargos_ibama (
    id SERIAL PRIMARY KEY,
    geometry GEOMETRY(GEOMETRY, 4326)
);

CREATE TABLE IF NOT EXISTS infracoes_ibama (
    id SERIAL PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS embargos_icmbio (
    id SERIAL PRIMARY KEY,
    geometry GEOMETRY(GEOMETRY, 4326)
);

CREATE TABLE IF NOT EXISTS infracoes_icmbio (
    id SERIAL PRIMARY KEY,
    geometry GEOMETRY(GEOMETRY, 4326)
);

CREATE TABLE IF NOT EXISTS embargos_sema_mt (
    id SERIAL PRIMARY KEY,
    geometry GEOMETRY(GEOMETRY, 4326)
);

CREATE TABLE IF NOT EXISTS embargos_sema_mt_siga (
    id SERIAL PRIMARY KEY,
    geometry GEOMETRY(GEOMETRY, 4326)
);

CREATE TABLE IF NOT EXISTS infracoes_sema_mt_siga (
    id SERIAL PRIMARY KEY,
    geometry GEOMETRY(GEOMETRY, 4326)
);