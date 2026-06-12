-- ============================================================================
-- 01_schema.sql  —  cria tabelas VAZIAS, com tipos fortes e PKs
-- ----------------------------------------------------------------------------
-- o SQL define a estrutura; o Python só insere (append).
-- Rode conectado ao anm_geo:   \c anm_geo   depois   \i 01_schema.sql
-- ============================================================================

CREATE EXTENSION IF NOT EXISTS postgis;

-- ============================================================================
-- A. DADOS ESPACIAIS
-- ============================================================================

-- A.1 processos (do pma_amzl_ALLminerals_final.geojson) -----------------------
DROP TABLE IF EXISTS processos CASCADE;
CREATE TABLE processos (
    processo      TEXT PRIMARY KEY,          -- chave natural, sem ponto
    titular       TEXT, 
	cpf_cnpjcm    TEXT,
    subs          TEXT,
	subspmagrp    TEXT,
	fase          TEXT,
	tipo_reqcm    TEXT,
    area_orig     NUMERIC,                   -- área oficial ANM (ha)
    area_ha       NUMERIC,                   -- área geométrica recalculada (ha)
    munic         TEXT,
    uf            TEXT,
    n_munic       INTEGER,
    munic_fonte   TEXT,
	tiov          BOOLEAN,
    ucov          BOOLEAN,
    quiov         BOOLEAN,
    tiov10km      BOOLEAN,
    ucov2_10km    BOOLEAN,
    quiov10km     BOOLEAN,
	cfem_arr      BOOLEAN,
    cfem_aut      BOOLEAN,
    geom          GEOMETRY(MultiPolygon, 4326)
);

-- A.2 terras_indigenas (ti_amzl.geojson) --------------------------------------
DROP TABLE IF EXISTS terras_indigenas CASCADE;
CREATE TABLE terras_indigenas (
    id          SERIAL PRIMARY KEY,          -- chave artificial (não há natural)
    modalidade  TEXT,
    fase_ti     TEXT,
    terrai_nom  TEXT,
    etnia_nome  TEXT,
    geom        GEOMETRY(MultiPolygon, 4326)
);

-- A.3 unidades_conservacao (uc_amzl.geojson) ----------------------------------
DROP TABLE IF EXISTS unidades_conservacao CASCADE;
CREATE TABLE unidades_conservacao (
    id          SERIAL PRIMARY KEY,
    nome_uc     TEXT,
    pl_manejo   TEXT,
    grupo       TEXT,
    categoria   TEXT,
    org_gestor  TEXT,
    sigla_snuc  TEXT,
    geom        GEOMETRY(MultiPolygon, 4326)
);

-- A.4 quilombolas (qui_amzl.geojson) ------------------------------------------
DROP TABLE IF EXISTS quilombolas CASCADE;
CREATE TABLE quilombolas (
    id          SERIAL PRIMARY KEY,
    cd_quilomb  NUMERIC,
    cd_sr       TEXT,
    nr_process  TEXT,
    nm_comunid  TEXT,
    nm_municip  TEXT,
    cd_uf       TEXT,
    dt_publica  TEXT,
    dt_public1  TEXT,
    nr_familia  INTEGER,
    dt_titulac  TEXT,
    nr_area_ha  NUMERIC,
    cd_sipra    TEXT,
    nr_perimet  NUMERIC,
    ob_descric  TEXT,
    st_titulad  TEXT,
    dt_decreto  TEXT,
    tp_levanta  TEXT,
    nr_escalao  TEXT,
    area_calc_  NUMERIC,
    perimetro_  NUMERIC,
    esfera      TEXT,
    fase        TEXT,
    responsave  TEXT,
    geom        GEOMETRY(MultiPolygon, 4326)
);

-- ============================================================================
-- B. CFEM (royalties)
-- ============================================================================

-- B.1 cfem_eventos (cfem_amzl_ALLminerals_GOLD_CASScorrected.csv) -------------
-- Sem PK natural (uma declaração não tem id único próprio). PK artificial.
DROP TABLE IF EXISTS cfem_eventos CASCADE;
CREATE TABLE cfem_eventos (
    id            BIGSERIAL PRIMARY KEY,
    ano           INTEGER,
    mes           INTEGER,
    cpf_cnpjarr   TEXT,
    subsarr       TEXT,
    abbrev_state  TEXT,
    code_muni     NUMERIC,
    name_muni     TEXT,
    qtd_minerio   NUMERIC,
    um            TEXT,
    valorarr      NUMERIC,
    processo      TEXT,                       -- FK p/ processos (no 02_constraints)
    nome_arr      TEXT,
    peso_kg       NUMERIC,
    peso_g        NUMERIC,
    subsarrsim    TEXT,
    area_ha       NUMERIC,
    fase          TEXT,
    ult_evento    TEXT,
    titular       TEXT,
    subs          TEXT,
    tipo_reqcm    TEXT,
    cpf_cnpjcm    TEXT,
    data          DATE,
    aliquota_pct  NUMERIC,
    valortot      NUMERIC,
    preco_g_orig  NUMERIC,
    corr          TEXT,
    peso_g_final  NUMERIC,
    peso_kg_final NUMERIC,
    preco_g_final NUMERIC,
    ult_ev_id     NUMERIC,
    ult_ev_dat    DATE,
    ult_ev_des    TEXT
);

-- B.2 cfem_autuacoes (cfem_aut_all_min_amzl.csv) ------------------------------
DROP TABLE IF EXISTS cfem_autuacoes CASCADE;
CREATE TABLE cfem_autuacoes (
    id            BIGSERIAL PRIMARY KEY,
    ano           INTEGER,
    mes           INTEGER,
    cpf_cnpjaut   TEXT,
    titularaut    TEXT,
    processo      TEXT,                       -- FK p/ processos
    subsaut       TEXT,
    name_muni     TEXT,
    abbrev_state  TEXT,
    valoraut      NUMERIC
);

-- ============================================================================
-- C — MICRODADOS
-- ============================================================================

-- C.1 micro_processo (mestre dos microdados) ----------------------------------
DROP TABLE IF EXISTS micro_processo CASCADE;
CREATE TABLE micro_processo (
    dsprocesso                      TEXT PRIMARY KEY,   -- chave natural (com ponto)
    nrprocesso                      TEXT,
    nranoprocesso                   TEXT,
    btativo                         TEXT,
    nrnup                           TEXT,
    idtiporequerimento              TEXT,
    idfaseprocesso                  TEXT,
    idunidadeadministrativaregional TEXT,
    idunidadeprotocolizadora        TEXT,
    dtprotocolo                     DATE,
    dtprioridade                    DATE,
    qtareaha                        NUMERIC,
    processo                        TEXT                -- chave limpa (sem ponto) p/ ponte
);

-- C.2 Tabelas de FATO (ligadas a micro_processo via dsprocesso) ---------------
DROP TABLE IF EXISTS micro_processo_evento CASCADE;
CREATE TABLE micro_processo_evento (
    id              BIGSERIAL PRIMARY KEY,
    dsprocesso      TEXT,
    idevento        TEXT,
    dtevento        DATE,
    obevento        TEXT,
    dspublicacaodou TEXT,
    processo        TEXT
);

DROP TABLE IF EXISTS micro_processo_pessoa CASCADE;
CREATE TABLE micro_processo_pessoa (
    id                            BIGSERIAL PRIMARY KEY,
    dsprocesso                    TEXT,
    idpessoa                      TEXT,
    idtiporelacao                 TEXT,
    idtiporesponsabilidadetecnica TEXT,
    idtiporepresentacaolegal      TEXT,
    dtprazoarrendamento           DATE,
    dtiniciovigencia              DATE,
    dtfimvigencia                 DATE,
    processo                      TEXT
);

DROP TABLE IF EXISTS micro_processo_substancia CASCADE;
CREATE TABLE micro_processo_substancia (
    id                             BIGSERIAL PRIMARY KEY,
    dsprocesso                     TEXT,
    idsubstancia                   TEXT,
    idtipousosubstancia            TEXT,
    idmotivoencerramentosubstancia TEXT,
    dtiniciovigencia               DATE,
    dtfimvigencia                  DATE,
    processo                       TEXT
);

-- Tabela de ligação: PK COMPOSTA (dsprocesso + idmunicipio)
DROP TABLE IF EXISTS micro_processo_municipio CASCADE;
CREATE TABLE micro_processo_municipio (
    dsprocesso  TEXT,
    idmunicipio TEXT,
    processo    TEXT,
    PRIMARY KEY (dsprocesso, idmunicipio)
);

DROP TABLE IF EXISTS micro_processo_titulo CASCADE;
CREATE TABLE micro_processo_titulo (
    id                       BIGSERIAL PRIMARY KEY,
    dsprocesso               TEXT,
    nrtitulo                 TEXT,
    iddocumentolegal         TEXT,
    idtipodocumentolegal     TEXT,
    idsituacaodocumentolegal TEXT,
    dtpublicacao             DATE,
    dtvencimento             DATE,
    processo                 TEXT
);

DROP TABLE IF EXISTS micro_processo_documentacao CASCADE;
CREATE TABLE micro_processo_documentacao (
    id              BIGSERIAL PRIMARY KEY,
    dsprocesso      TEXT,
    idtipodocumento TEXT,
    dtprotocolo     DATE,
    processo        TEXT
);

DROP TABLE IF EXISTS micro_processo_associacao CASCADE;
CREATE TABLE micro_processo_associacao (
    id                  BIGSERIAL PRIMARY KEY,
    dsprocesso          TEXT,
    dsprocessoassociado TEXT,
    idtipoassociacao    TEXT,
    dtassociacao        DATE,
    dtdesassociacao     DATE,
    obassociacao        TEXT,
    processo            TEXT
);

DROP TABLE IF EXISTS micro_processo_propriedade_solo CASCADE;
CREATE TABLE micro_processo_propriedade_solo (
    id                        BIGSERIAL PRIMARY KEY,
    dsprocesso                TEXT,
    idcondicaopropriedadesolo TEXT,
    processo                  TEXT
);

-- ============================================================================
-- BLOCO D — MICRODADOS: lookups (id -> descrição). PK no id.
-- ============================================================================
DROP TABLE IF EXISTS micro_municipio CASCADE;
CREATE TABLE micro_municipio (
    idmunicipio TEXT PRIMARY KEY,
    nmmunicipio TEXT,
    sguf        TEXT
);

DROP TABLE IF EXISTS micro_pessoa CASCADE;
CREATE TABLE micro_pessoa (
    idpessoa  TEXT PRIMARY KEY,
    nrcpfcnpj TEXT,
    tppessoa  TEXT,
    nmpessoa  TEXT
);

DROP TABLE IF EXISTS micro_evento CASCADE;
CREATE TABLE micro_evento (idevento TEXT PRIMARY KEY, dsevento TEXT);

DROP TABLE IF EXISTS micro_fase_processo CASCADE;
CREATE TABLE micro_fase_processo (idfaseprocesso TEXT PRIMARY KEY, dsfaseprocesso TEXT);

DROP TABLE IF EXISTS micro_substancia CASCADE;
CREATE TABLE micro_substancia (idsubstancia TEXT PRIMARY KEY, nmsubstancia TEXT);

DROP TABLE IF EXISTS micro_tipo_requerimento CASCADE;
CREATE TABLE micro_tipo_requerimento (idtiporequerimento TEXT PRIMARY KEY, dstiporequerimento TEXT);

DROP TABLE IF EXISTS micro_tipo_associacao CASCADE;
CREATE TABLE micro_tipo_associacao (idtipoassociacao TEXT PRIMARY KEY, dstipoassociacao TEXT);

DROP TABLE IF EXISTS micro_tipo_documento CASCADE;
CREATE TABLE micro_tipo_documento (idtipodocumento TEXT PRIMARY KEY, dstipodocumento TEXT);

DROP TABLE IF EXISTS micro_tipo_documento_legal CASCADE;
CREATE TABLE micro_tipo_documento_legal (idtipodocumentolegal TEXT PRIMARY KEY, dstipodocumentolegal TEXT);

DROP TABLE IF EXISTS micro_tipo_relacao CASCADE;
CREATE TABLE micro_tipo_relacao (idtiporelacao TEXT PRIMARY KEY, dstiporelacao TEXT);

DROP TABLE IF EXISTS micro_tipo_representacao_legal CASCADE;
CREATE TABLE micro_tipo_representacao_legal (idtiporepresentacaolegal TEXT PRIMARY KEY, dstiporepresentacaolegal TEXT);

DROP TABLE IF EXISTS micro_tipo_responsabilidade_tecnica CASCADE;
CREATE TABLE micro_tipo_responsabilidade_tecnica (idtiporesponsabilidadetecnica TEXT PRIMARY KEY, dstiporesponsabilidadetecnica TEXT);

DROP TABLE IF EXISTS micro_tipo_uso_substancia CASCADE;
CREATE TABLE micro_tipo_uso_substancia (idtipousosubstancia TEXT PRIMARY KEY, dstipousosubstancia TEXT);

DROP TABLE IF EXISTS micro_condicao_propriedade_solo CASCADE;
CREATE TABLE micro_condicao_propriedade_solo (idcondicaopropriedadesolo TEXT PRIMARY KEY, dscondicaopropriedadesolo TEXT);

DROP TABLE IF EXISTS micro_motivo_encerramento_substancia CASCADE;
CREATE TABLE micro_motivo_encerramento_substancia (idmotivoencerramentosubstancia TEXT PRIMARY KEY, dsmotivoencerramentosubstancia TEXT);

DROP TABLE IF EXISTS micro_situacao_documento_legal CASCADE;
CREATE TABLE micro_situacao_documento_legal (idsituacaodocumentolegal TEXT PRIMARY KEY, dssituacaodocumentolegal TEXT);

DROP TABLE IF EXISTS micro_documento_legal CASCADE;
CREATE TABLE micro_documento_legal (iddocumentolegal TEXT PRIMARY KEY, dsdocumentolegal TEXT);

DROP TABLE IF EXISTS micro_unidade_administrativa CASCADE;
CREATE TABLE micro_unidade_administrativa (idunidadeadministrativaregional TEXT PRIMARY KEY, dsunidadeadministrativaregional TEXT);

DROP TABLE IF EXISTS micro_unidade_protocolizadora CASCADE;
CREATE TABLE micro_unidade_protocolizadora (idunidadeprotocolizadora TEXT PRIMARY KEY, dsunidadeprotocolizadora TEXT);

-- Fim do schema. Próximo: carga (Python), depois 02_constraints.sql.
SELECT COUNT(*) FROM information_schema.tables WHERE table_schema='public';