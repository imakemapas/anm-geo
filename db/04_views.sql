-- ============================================================================
-- 04_views.sql  —  materialized views do anm_geo v2
--   1. mv_cfem_anual         — CFEM agregada por processo/ano
--   2. mv_cfem_mensal        — CFEM agregada por processo/mês
--   3. mv_sobreposicoes      — TI/UC/QUI: sobreposição direta (>=5%) + influência
--   4. mv_historico_titular  — histórico de titulares por processo
-- ----------------------------------------------------------------------------
-- Roda DEPOIS de 03_indexes.sql. Conectar ao anm_geo.
--
-- REGRA DE SOBREPOSIÇÃO (alinhada ao objetivo investigativo):
--   * Direta (TI, UC, QUI): conta se a área de interseção >= 5% da área do
--     processo (descarta sobreposições desprezíveis).
--   * Influência (TI, UC apenas): processo a até X km da feição (ST_DWithin).
--       TI: 10 km.  UC: 10 km se pl_manejo='SIM', senão 2 km.
--   * QUI: só sobreposição direta (sem influência).
--   * UC considerada: só grupo='PROTEÇÃO INTEGRAL' OU sigla_snuc='RESEX'.
--   * Cálculos métricos em EPSG:5880.
-- ============================================================================
 
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
-- VIEW 3: SOBREPOSIÇÕES
-- ----------------------------------------------------------------------------
DROP MATERIALIZED VIEW IF EXISTS mv_sobreposicoes CASCADE;
CREATE MATERIALIZED VIEW mv_sobreposicoes AS
WITH
p AS (
    SELECT processo, subspmagrp, fase, area_ha,
           ST_Transform(geom, 5880) AS g
    FROM processos
),
ti AS (
    SELECT terrai_nom, etnia_nome, fase_ti, ST_Transform(geom, 5880) AS g
    FROM terras_indigenas
),
uc AS (
    SELECT nome_uc, sigla_snuc, grupo, categoria, pl_manejo,
           ST_Transform(geom, 5880) AS g
    FROM unidades_conservacao
    WHERE upper(grupo) = 'PROTEÇÃO INTEGRAL' OR upper(sigla_snuc) = 'RESEX'
),
qui AS (
    SELECT nm_comunid, nm_municip, nr_familia, ST_Transform(geom, 5880) AS g
    FROM quilombolas
),
 
-- ===== SOBREPOSIÇÃO DIRETA (>= 5% da área do processo) =====
ti_dir AS (
    SELECT p.processo,
           string_agg(DISTINCT upper(ti.terrai_nom), ' | ') AS ti_nomes,
           string_agg(DISTINCT ti.etnia_nome, ' | ')        AS ti_etnias,
           1 AS ti_ov
    FROM p JOIN ti ON ST_Intersects(p.g, ti.g)
    GROUP BY p.processo, p.area_ha
    HAVING SUM(ST_Area(ST_Intersection(p.g, ti.g)))/10000.0 >= 0.05 * p.area_ha
),
uc_dir AS (
    SELECT p.processo,
           string_agg(DISTINCT uc.nome_uc, ' | ')    AS uc_nomes,
           string_agg(DISTINCT uc.sigla_snuc, ' | ') AS uc_tipos,
           string_agg(DISTINCT uc.categoria, ' | ')  AS uc_categorias,
           1 AS uc_ov
    FROM p JOIN uc ON ST_Intersects(p.g, uc.g)
    GROUP BY p.processo, p.area_ha
    HAVING SUM(ST_Area(ST_Intersection(p.g, uc.g)))/10000.0 >= 0.05 * p.area_ha
),
qui_dir AS (
    SELECT p.processo,
           string_agg(DISTINCT qui.nm_comunid, ' | ') AS qui_nomes,
           string_agg(DISTINCT qui.nm_municip, ' | ') AS qui_municipios,
           1 AS qui_ov
    FROM p JOIN qui ON ST_Intersects(p.g, qui.g)
    GROUP BY p.processo, p.area_ha
    HAVING SUM(ST_Area(ST_Intersection(p.g, qui.g)))/10000.0 >= 0.05 * p.area_ha
),
 
-- ===== ÁREA DE INFLUÊNCIA (distância; só TI e UC) =====
-- TI: 10 km
ti_inf AS (
    SELECT p.processo,
           string_agg(DISTINCT upper(ti.terrai_nom), ' | ') AS ti_inf_nomes,
           1 AS ti_influencia
    FROM p JOIN ti ON ST_DWithin(p.g, ti.g, 10000)
    GROUP BY p.processo
),
-- UC: 10 km se pl_manejo='SIM', senão 2 km
uc_inf AS (
    SELECT p.processo,
           string_agg(DISTINCT uc.nome_uc, ' | ') AS uc_inf_nomes,
           1 AS uc_influencia
    FROM p JOIN uc
      ON ST_DWithin(p.g, uc.g,
                    CASE WHEN upper(trim(uc.pl_manejo))='SIM' THEN 10000 ELSE 2000 END)
    GROUP BY p.processo
)
 
SELECT
    p.processo, p.subspmagrp, p.fase, p.area_ha,
    -- sobreposição direta
    COALESCE(ti_dir.ti_ov, 0)   AS ti_ov,
    COALESCE(uc_dir.uc_ov, 0)   AS uc_ov,
    COALESCE(qui_dir.qui_ov, 0) AS qui_ov,
    -- influência
    COALESCE(ti_inf.ti_influencia, 0) AS ti_influencia,
    COALESCE(uc_inf.uc_influencia, 0) AS uc_influencia,
    -- nomes e infos (direta)
    ti_dir.ti_nomes, ti_dir.ti_etnias,
    uc_dir.uc_nomes, uc_dir.uc_tipos, uc_dir.uc_categorias,
    qui_dir.qui_nomes, qui_dir.qui_municipios,
    -- nomes (influência)
    ti_inf.ti_inf_nomes,
    uc_inf.uc_inf_nomes,
    -- geometria (de volta em 4326 p/ mapa)
    ST_Transform(p.g, 4326)::geometry(MultiPolygon,4326) AS geom
FROM p
LEFT JOIN ti_dir  USING (processo)
LEFT JOIN uc_dir  USING (processo)
LEFT JOIN qui_dir USING (processo)
LEFT JOIN ti_inf  USING (processo)
LEFT JOIN uc_inf  USING (processo);
 
CREATE INDEX ix_mv_sobre_processo  ON mv_sobreposicoes (processo);
CREATE INDEX ix_mv_sobre_ti_ov     ON mv_sobreposicoes (ti_ov);
CREATE INDEX ix_mv_sobre_uc_ov     ON mv_sobreposicoes (uc_ov);
CREATE INDEX ix_mv_sobre_qui_ov    ON mv_sobreposicoes (qui_ov);
CREATE INDEX ix_mv_sobre_ti_inf    ON mv_sobreposicoes (ti_influencia);
CREATE INDEX ix_mv_sobre_uc_inf    ON mv_sobreposicoes (uc_influencia);
CREATE INDEX gix_mv_sobre_geom     ON mv_sobreposicoes USING GIST (geom);
 
-- ----------------------------------------------------------------------------
-- VIEW 4: HISTÓRICO DE TITULARES
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
-- VIEW 5: HISTÓRICO DE EVENTOS (linha do tempo administrativa do processo)
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