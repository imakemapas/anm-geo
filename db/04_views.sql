-- ============================================================================
-- 04_views.sql
--   1. mv_cfem_anual         — CFEM agregada por processo/ano
--   2. mv_cfem_mensal        — CFEM agregada por processo/mês
--   3. mv_cfem_total         — CFEM agregada por processo (totais, sem ano/mês)
--   4. mv_sobreposicoes      — TI/UC/QUI: flags de sobreposição/influência (do R)
--   5. mv_historico_titular  — histórico de titulares por processo
--   6. mv_historico_evento   — linha do tempo administrativa do processo
-- ----------------------------------------------------------------------------
-- DEPOIS de 03_indexes.sql. Conectar ao anm_geo.

-- ----------------------------------------------------------------------------
-- VIEW 1: CFEM anual
-- ----------------------------------------------------------------------------
DROP MATERIALIZED VIEW IF EXISTS mv_cfem_anual CASCADE;
CREATE MATERIALIZED VIEW mv_cfem_anual AS
SELECT
    c.processo, c.ano,
    c.subsarrsim AS subs_grp, c.subsarr,
    c.cpf_cnpjarr, c.nome_arr, c.abbrev_state, c.name_muni,
    c.fase, c.titular,
    SUM(c.valorarr)      AS valor_arr_total,
    SUM(c.valortot)      AS valor_tot_total,
    SUM(c.peso_kg_final) AS peso_kg_total,
    SUM(c.peso_g_final)  AS peso_g_total,
    COUNT(*)             AS n_declaracoes
FROM cfem_eventos c
GROUP BY c.processo, c.ano, c.subsarrsim, c.subsarr,
         c.cpf_cnpjarr, c.nome_arr, c.abbrev_state, c.name_muni, c.fase, c.titular;

CREATE INDEX ix_mv_cfem_anual_processo ON mv_cfem_anual (processo);
CREATE INDEX ix_mv_cfem_anual_ano      ON mv_cfem_anual (ano);
CREATE INDEX ix_mv_cfem_anual_subs     ON mv_cfem_anual (subs_grp);

-- ----------------------------------------------------------------------------
-- VIEW 2: CFEM mensal
-- ----------------------------------------------------------------------------
DROP MATERIALIZED VIEW IF EXISTS mv_cfem_mensal CASCADE;
CREATE MATERIALIZED VIEW mv_cfem_mensal AS
SELECT
    c.processo, c.ano, c.mes,
    make_date(c.ano, c.mes, 1) AS data,
    c.subsarrsim AS subs_grp, c.subsarr,
    c.abbrev_state, c.name_muni,
    SUM(c.valorarr)      AS valor_arr_total,
    SUM(c.valortot)      AS valor_tot_total,
    SUM(c.peso_kg_final) AS peso_kg_total,
    SUM(c.peso_g_final)  AS peso_g_total,
    COUNT(*)             AS n_declaracoes
FROM cfem_eventos c
WHERE c.ano IS NOT NULL AND c.mes IS NOT NULL
GROUP BY c.processo, c.ano, c.mes, c.subsarrsim, c.subsarr, c.abbrev_state, c.name_muni;

CREATE INDEX ix_mv_cfem_mensal_processo ON mv_cfem_mensal (processo);
CREATE INDEX ix_mv_cfem_mensal_data     ON mv_cfem_mensal (data);

-- ----------------------------------------------------------------------------
-- VIEW 3: CFEM total por processo
-- ----------------------------------------------------------------------------
DROP MATERIALIZED VIEW IF EXISTS mv_cfem_total CASCADE;
CREATE MATERIALIZED VIEW mv_cfem_total AS
SELECT
    processo,
    SUM(peso_kg_final)          AS arr_kg_t,
    SUM(peso_g_final)           AS arr_g_t,
    SUM(valorarr)                AS arr_val_t,
    COUNT(*)                     AS arr_ndcl,
    COUNT(DISTINCT cpf_cnpjarr)  AS arr_nbuy
FROM cfem_eventos
GROUP BY processo;

CREATE INDEX ix_mv_cfem_total_processo ON mv_cfem_total (processo);

-- ----------------------------------------------------------------------------
-- VIEW 4: SOBREPOSIÇÕES
-- ----------------------------------------------------------------------------
DROP MATERIALIZED VIEW IF EXISTS mv_sobreposicoes CASCADE;
CREATE MATERIALIZED VIEW mv_sobreposicoes AS
SELECT
    processo, subspmagrp, fase, area_ha,
	COALESCE(tiov,false)       AS ti_ov,
    COALESCE(ucov,false)       AS uc_ov,
    COALESCE(quiov,false)      AS qui_ov,
    COALESCE(tiov10km,false)   AS ti_influencia,
    COALESCE(ucov2_10km,false) AS uc_influencia,
    COALESCE(quiov10km,false)  AS qui_influencia,
    geom
FROM processos;

CREATE INDEX ix_mv_sobre_processo ON mv_sobreposicoes (processo);
CREATE INDEX ix_mv_sobre_ti_ov    ON mv_sobreposicoes (ti_ov);
CREATE INDEX ix_mv_sobre_uc_ov    ON mv_sobreposicoes (uc_ov);
CREATE INDEX ix_mv_sobre_qui_ov   ON mv_sobreposicoes (qui_ov);
CREATE INDEX ix_mv_sobre_ti_inf   ON mv_sobreposicoes (ti_influencia);
CREATE INDEX ix_mv_sobre_uc_inf   ON mv_sobreposicoes (uc_influencia);
CREATE INDEX gix_mv_sobre_geom    ON mv_sobreposicoes USING GIST (geom);

-- ----------------------------------------------------------------------------
-- VIEW 5: HISTÓRICO DE TITULARES
-- ----------------------------------------------------------------------------
DROP MATERIALIZED VIEW IF EXISTS mv_historico_titular CASCADE;
CREATE MATERIALIZED VIEW mv_historico_titular AS
SELECT
    pp.dsprocesso          AS dsprocesso,
    pp.processo            AS processo,
    pe.nrcpfcnpj           AS cpf_cnpj,
    pe.nmpessoa            AS nome,
    pe.tppessoa            AS tipo_pessoa,
    pp.dtiniciovigencia    AS dt_inicio,
    pp.dtfimvigencia       AS dt_fim,
    pp.idtiporelacao       AS tipo_relacao,
    (pp.dtfimvigencia IS NULL) AS ativo,
    p.subspmagrp,
    p.fase,
    p.area_ha,
    p.uf,
    p.munic
FROM micro_processo_pessoa pp
JOIN micro_pessoa pe ON pp.idpessoa = pe.idpessoa
LEFT JOIN processos p ON pp.processo = p.processo;

CREATE INDEX ix_mv_hist_processo ON mv_historico_titular (processo);
CREATE INDEX ix_mv_hist_cpf      ON mv_historico_titular (cpf_cnpj);
CREATE INDEX ix_mv_hist_nome     ON mv_historico_titular (nome);
CREATE INDEX ix_mv_hist_ativo    ON mv_historico_titular (ativo);

-- ----------------------------------------------------------------------------
-- VIEW 6: HISTÓRICO DE EVENTOS (linha do tempo administrativa do processo)
-- ----------------------------------------------------------------------------
DROP MATERIALIZED VIEW IF EXISTS mv_historico_evento CASCADE;
CREATE MATERIALIZED VIEW mv_historico_evento AS
SELECT
    pe.processo,
    pe.dsprocesso,
    pe.dtevento         AS data_evento,
    ev.dsevento         AS evento,
    pe.obevento         AS observacao,
    pe.dspublicacaodou  AS publicacao_dou,
    p.subspmagrp,
    p.fase              AS fase_atual,
    p.uf,
    p.munic
FROM micro_processo_evento pe
LEFT JOIN micro_evento ev ON pe.idevento = ev.idevento
LEFT JOIN processos p     ON pe.processo = p.processo
ORDER BY pe.processo, pe.dtevento;

CREATE INDEX ix_mv_hevento_processo ON mv_historico_evento (processo);
CREATE INDEX ix_mv_hevento_data     ON mv_historico_evento (data_evento);
CREATE INDEX ix_mv_hevento_proc_dt  ON mv_historico_evento (processo, data_evento);

ANALYZE;

-- Fim do banco v2.