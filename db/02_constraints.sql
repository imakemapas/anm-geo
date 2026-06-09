-- ============================================================================
-- 02_constraints.sql  —  quarentena única + chaves estrangeiras
-- ----------------------------------------------------------------------------
-- Roda DEPOIS da carga (01_load_postgis.py e 02_load_microdados.py).
--
-- um processo só fica em 'processos' se tiver microdado (existe em micro_processo). 
-- Sem microdado (ex.: requerimentos 2026 ainda não cadastrados) -> quarentena.
--  CFEM que aponta para processo que não fica -> quarentena também.
--  Ao final, FKs forçadas; tudo fecha.
-- ============================================================================
 
-- ----------------------------------------------------------------------------
-- 1) QUARENTENA DE PROCESSOS (sem microdado)
-- ----------------------------------------------------------------------------
DROP TABLE IF EXISTS processos_sem_microdado;
CREATE TABLE processos_sem_microdado (LIKE processos INCLUDING ALL);
 
INSERT INTO processos_sem_microdado
SELECT p.*
FROM processos p
WHERE NOT EXISTS (
    SELECT 1 FROM micro_processo mp WHERE mp.processo = p.processo
);
 
-- ----------------------------------------------------------------------------
-- 2) QUARENTENA DA CFEM
--    "permanece" = existe em processos E tem microdado
-- ----------------------------------------------------------------------------
 
-- 2.1 cfem_eventos
DROP TABLE IF EXISTS cfem_eventos_sem_processo;
CREATE TABLE cfem_eventos_sem_processo (LIKE cfem_eventos INCLUDING ALL);
 
INSERT INTO cfem_eventos_sem_processo
SELECT c.*
FROM cfem_eventos c
WHERE NOT EXISTS (
    SELECT 1
    FROM processos p
    JOIN micro_processo mp ON mp.processo = p.processo
    WHERE p.processo = c.processo
);
 
DELETE FROM cfem_eventos c
WHERE NOT EXISTS (
    SELECT 1
    FROM processos p
    JOIN micro_processo mp ON mp.processo = p.processo
    WHERE p.processo = c.processo
);
 
-- 2.2 cfem_autuacoes
DROP TABLE IF EXISTS cfem_autuacoes_sem_processo;
CREATE TABLE cfem_autuacoes_sem_processo (LIKE cfem_autuacoes INCLUDING ALL);
 
INSERT INTO cfem_autuacoes_sem_processo
SELECT a.*
FROM cfem_autuacoes a
WHERE NOT EXISTS (
    SELECT 1
    FROM processos p
    JOIN micro_processo mp ON mp.processo = p.processo
    WHERE p.processo = a.processo
);
 
DELETE FROM cfem_autuacoes a
WHERE NOT EXISTS (
    SELECT 1
    FROM processos p
    JOIN micro_processo mp ON mp.processo = p.processo
    WHERE p.processo = a.processo
);
 
-- ----------------------------------------------------------------------------
-- 3) REMOVE DA PRINCIPAL OS PROCESSOS SEM MICRODADO
-- ----------------------------------------------------------------------------
DELETE FROM processos p
WHERE NOT EXISTS (
    SELECT 1 FROM micro_processo mp WHERE mp.processo = p.processo
);
 
-- ----------------------------------------------------------------------------
-- 4) CHAVES ESTRANGEIRAS
-- ----------------------------------------------------------------------------
 
-- 4.0 Pré-requisito: a ponte aponta para micro_processo.processo (chave limpa),
ALTER TABLE micro_processo
    ADD CONSTRAINT uq_micro_processo_processo UNIQUE (processo);
 
-- 4.1 Ponte: processos -> micro_processo (todo processo do principal tem micro)
ALTER TABLE processos
    ADD CONSTRAINT fk_processos_microdado
    FOREIGN KEY (processo) REFERENCES micro_processo (processo);
 
-- 4.2 CFEM -> processos
ALTER TABLE cfem_eventos
    ADD CONSTRAINT fk_cfem_eventos_processo
    FOREIGN KEY (processo) REFERENCES processos (processo);
 
ALTER TABLE cfem_autuacoes
    ADD CONSTRAINT fk_cfem_autuacoes_processo
    FOREIGN KEY (processo) REFERENCES processos (processo);
 
-- 4.3 Fatos de microdados -> micro_processo (via dsprocesso, PK do mestre)
ALTER TABLE micro_processo_evento
    ADD CONSTRAINT fk_mpe_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
ALTER TABLE micro_processo_pessoa
    ADD CONSTRAINT fk_mpp_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
ALTER TABLE micro_processo_substancia
    ADD CONSTRAINT fk_mps_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
ALTER TABLE micro_processo_municipio
    ADD CONSTRAINT fk_mpm_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
ALTER TABLE micro_processo_titulo
    ADD CONSTRAINT fk_mpt_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
ALTER TABLE micro_processo_documentacao
    ADD CONSTRAINT fk_mpd_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
ALTER TABLE micro_processo_associacao
    ADD CONSTRAINT fk_mpa_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
ALTER TABLE micro_processo_propriedade_solo
    ADD CONSTRAINT fk_mpps_dsprocesso FOREIGN KEY (dsprocesso) REFERENCES micro_processo (dsprocesso);
 
-- ----------------------------------------------------------------------------
-- 5) CONFERÊNCIA (rode após o script; valores esperados anotados)
-- ----------------------------------------------------------------------------
-- SELECT
--   (SELECT COUNT(*) FROM processos) AS processos,
--   (SELECT COUNT(*) FROM processos_sem_microdado) AS proc_quarentena,
--   (SELECT COUNT(*) FROM cfem_eventos) AS cfem,
--   (SELECT COUNT(*) FROM cfem_eventos_sem_processo) AS cfem_quarentena,
--   (SELECT COUNT(*) FROM micro_processo) AS micro_processo;

--  SELECT COUNT(*) AS total_fks
-- 	FROM pg_constraint
-- 	WHERE contype = 'f';

-- Fim. Próximo: 03_indexes.sql