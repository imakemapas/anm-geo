-- ============================================================================
-- 03_indexes.sql
-- ----------------------------------------------------------------------------
-- Roda DEPOIS de 02_constraints.sql. Conectar ao anm_geo.
-- ============================================================================
 
-- ----------------------------------------------------------------------------
-- A) ÍNDICES ESPACIAIS (GiST) - mapa e interseções
-- ----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS gix_processos_geom            ON processos            USING GIST (geom);
CREATE INDEX IF NOT EXISTS gix_terras_indigenas_geom     ON terras_indigenas     USING GIST (geom);
CREATE INDEX IF NOT EXISTS gix_unidades_conservacao_geom ON unidades_conservacao USING GIST (geom);
CREATE INDEX IF NOT EXISTS gix_quilombolas_geom          ON quilombolas          USING GIST (geom);
 
-- ----------------------------------------------------------------------------
-- B) COLUNAS DE JUNÇÃO (processo / dsprocesso)
--    A PK já indexa processos.processo e micro_processo.dsprocesso. Aqui vão
--    as colunas de junção do LADO "muitos" (que não são PK).
-- ----------------------------------------------------------------------------
-- CFEM -> processos
CREATE INDEX IF NOT EXISTS ix_cfem_eventos_processo   ON cfem_eventos   (processo);
CREATE INDEX IF NOT EXISTS ix_cfem_autuacoes_processo ON cfem_autuacoes (processo);
 
-- ponte: processos.processo (PK) <-> micro_processo.processo (UNIQUE) já indexados.
 
-- fatos de microdados -> micro_processo (dsprocesso)
CREATE INDEX IF NOT EXISTS ix_mpe_dsprocesso  ON micro_processo_evento            (dsprocesso);
CREATE INDEX IF NOT EXISTS ix_mpp_dsprocesso  ON micro_processo_pessoa            (dsprocesso);
CREATE INDEX IF NOT EXISTS ix_mps_dsprocesso  ON micro_processo_substancia        (dsprocesso);
-- micro_processo_municipio: dsprocesso já é parte da PK composta (indexado)
CREATE INDEX IF NOT EXISTS ix_mpt_dsprocesso  ON micro_processo_titulo            (dsprocesso);
CREATE INDEX IF NOT EXISTS ix_mpd_dsprocesso  ON micro_processo_documentacao      (dsprocesso);
CREATE INDEX IF NOT EXISTS ix_mpa_dsprocesso  ON micro_processo_associacao        (dsprocesso);
CREATE INDEX IF NOT EXISTS ix_mpps_dsprocesso ON micro_processo_propriedade_solo  (dsprocesso);
 
-- chaves para juntar fatos aos lookups
CREATE INDEX IF NOT EXISTS ix_mpe_idevento       ON micro_processo_evento     (idevento);
CREATE INDEX IF NOT EXISTS ix_mpp_idpessoa       ON micro_processo_pessoa     (idpessoa);
CREATE INDEX IF NOT EXISTS ix_mps_idsubstancia   ON micro_processo_substancia (idsubstancia);
CREATE INDEX IF NOT EXISTS ix_mpm_idmunicipio    ON micro_processo_municipio  (idmunicipio);
 
-- ----------------------------------------------------------------------------
-- C) COLUNAS DE FILTRO FREQUENTE
-- ----------------------------------------------------------------------------
-- processos: substância, UF, município, fase, fonte do município
CREATE INDEX IF NOT EXISTS ix_processos_subspmagrp  ON processos (subspmagrp);
CREATE INDEX IF NOT EXISTS ix_processos_uf          ON processos (uf);
CREATE INDEX IF NOT EXISTS ix_processos_munic       ON processos (munic);
CREATE INDEX IF NOT EXISTS ix_processos_fase        ON processos (fase);
CREATE INDEX IF NOT EXISTS ix_processos_munic_fonte ON processos (munic_fonte);
 
-- cfem_eventos: substância, UF, ano, município, grupo
CREATE INDEX IF NOT EXISTS ix_cfem_subsarr      ON cfem_eventos (subsarr);
CREATE INDEX IF NOT EXISTS ix_cfem_subsarrsim   ON cfem_eventos (subsarrsim);
CREATE INDEX IF NOT EXISTS ix_cfem_uf           ON cfem_eventos (abbrev_state);
CREATE INDEX IF NOT EXISTS ix_cfem_ano          ON cfem_eventos (ano);
CREATE INDEX IF NOT EXISTS ix_cfem_code_muni    ON cfem_eventos (code_muni);
CREATE INDEX IF NOT EXISTS ix_cfem_data         ON cfem_eventos (data);
 
-- índice composto p/ filtros combinados comuns (substância + ano)
CREATE INDEX IF NOT EXISTS ix_cfem_subs_ano ON cfem_eventos (subsarrsim, ano);
 
-- cfem_autuacoes: substância, UF, ano
CREATE INDEX IF NOT EXISTS ix_aut_subsaut ON cfem_autuacoes (subsaut);
CREATE INDEX IF NOT EXISTS ix_aut_uf      ON cfem_autuacoes (abbrev_state);
CREATE INDEX IF NOT EXISTS ix_aut_ano     ON cfem_autuacoes (ano);
 
-- micro_processo: chave limpa (p/ a ponte) e fase
CREATE INDEX IF NOT EXISTS ix_micro_processo_processo ON micro_processo (processo);
CREATE INDEX IF NOT EXISTS ix_micro_processo_fase     ON micro_processo (idfaseprocesso);
 
-- ----------------------------------------------------------------------------
-- D) ATUALIZA ESTATÍSTICAS
-- ----------------------------------------------------------------------------
ANALYZE;
 
-- Fim. Próximo: 04_views.sql