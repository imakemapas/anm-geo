-- =============================================================================
-- MATERIALIZED VIEW 1: CFEM anual por processo e substância
-- Substitui o cfem_anual.rds do Shiny
-- =============================================================================
CREATE MATERIALIZED VIEW mv_cfem_anual AS
SELECT
    c.processo,
    c.ano,
    c.subs_arr_sim                          AS subs_grp,
    c.subs_arr,
    c.cpf_cnpj_arr,
    c.nome_arr,
    c.abbrev_state,
    c.name_muni,
    c.fase,
    c.titular,
    SUM(c.valor_arr)                        AS valor_arr_total,
    SUM(c.valor_tot)                        AS valor_tot_total,
    SUM(c.peso_kg_final)                    AS peso_kg_total,
    SUM(c.peso_g_final)                     AS peso_g_total,
    COUNT(*)                                AS n_declaracoes,
    -- geometria do centróide do processo
    p.geometry,
    p.area_ha,
    p.subs_grp                              AS subs_grp_pma
FROM cfem_eventos c
LEFT JOIN processos p USING (processo)
GROUP BY
    c.processo, c.ano, c.subs_arr_sim, c.subs_arr,
    c.cpf_cnpj_arr, c.nome_arr, c.abbrev_state,
    c.name_muni, c.fase, c.titular,
    p.geometry, p.area_ha, p.subs_grp;

CREATE INDEX ON mv_cfem_anual (processo);
CREATE INDEX ON mv_cfem_anual (ano);
CREATE INDEX ON mv_cfem_anual (subs_grp);
CREATE INDEX ON mv_cfem_anual USING GIST (geometry);

-- =============================================================================
-- MATERIALIZED VIEW 2: CFEM mensal — série temporal
-- Substitui o cfem_mensal.rds do Shiny
-- =============================================================================
CREATE MATERIALIZED VIEW mv_cfem_mensal AS
SELECT
    c.processo,
    c.ano,
    c.mes,
    MAKE_DATE(c.ano::int, c.mes::int, 1)    AS data,
    c.subs_arr_sim                          AS subs_grp,
    c.subs_arr,
    c.cpf_cnpj_arr,
    c.nome_arr,
    c.abbrev_state,
    c.fase,
    SUM(c.valor_arr)                        AS valor_arr_total,
    SUM(c.peso_kg_final)                    AS peso_kg_total,
    SUM(c.peso_g_final)                     AS peso_g_total,
    COUNT(*)                                AS n_declaracoes
FROM cfem_eventos c
GROUP BY
    c.processo, c.ano, c.mes, c.subs_arr_sim,
    c.subs_arr, c.cpf_cnpj_arr, c.nome_arr,
    c.abbrev_state, c.fase;

CREATE INDEX ON mv_cfem_mensal (processo);
CREATE INDEX ON mv_cfem_mensal (data);
CREATE INDEX ON mv_cfem_mensal (subs_grp);

-- =============================================================================
-- MATERIALIZED VIEW 3: Sobreposições TI/UC/QUI por processo
-- Substitui as colunas TIov, UCov, QUIov do shapefile antigo
-- =============================================================================
CREATE MATERIALIZED VIEW mv_sobreposicoes AS
SELECT
    p.processo,
    p.subs_grp,
    p.fase,
    p.area_ha,
    -- Sobreposição direta
    EXISTS (
        SELECT 1 FROM terras_indigenas ti
        WHERE ST_Intersects(p.geometry, ti.geometry)
    ) AS ti_ov,
    EXISTS (
        SELECT 1 FROM unidades_conservacao uc
        WHERE ST_Intersects(p.geometry, uc.geometry)
    ) AS uc_ov,
    EXISTS (
        SELECT 1 FROM quilombolas qu
        WHERE ST_Intersects(p.geometry, qu.geometry)
    ) AS qui_ov,
    -- Nomes das TI sobrepostas
    (
        SELECT STRING_AGG(DISTINCT ti.terrai_nom, ' | ')
        FROM terras_indigenas ti
        WHERE ST_Intersects(p.geometry, ti.geometry)
    ) AS ti_nomes,
    -- Nomes das UC sobrepostas
    (
        SELECT STRING_AGG(DISTINCT uc.nome_uc, ' | ')
        FROM unidades_conservacao uc
        WHERE ST_Intersects(p.geometry, uc.geometry)
    ) AS uc_nomes,
    -- Buffer 10km — intersecta TI?
    EXISTS (
        SELECT 1 FROM terras_indigenas ti
        WHERE ST_DWithin(
            p.geometry::geography,
            ti.geometry::geography,
            10000
        )
    ) AS ti_ov_10km,
    p.geometry
FROM processos p;

CREATE INDEX ON mv_sobreposicoes (processo);
CREATE INDEX ON mv_sobreposicoes (ti_ov);
CREATE INDEX ON mv_sobreposicoes (uc_ov);
CREATE INDEX ON mv_sobreposicoes USING GIST (geometry);