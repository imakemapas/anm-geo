-- ============================================================================
-- 00_recriar_banco.sql 
-- ----------------------------------------------------------------------------
-- APAGA o banco anm_geo atual e cria um novo vazio.
--
-- COMO RODAR: conecte-se a OUTRO banco (ex.: 'postgres') para rodar isto, pois
-- não dá para dropar o banco ao qual você está conectado.
--   No psql:   \c postgres
--   Depois:    \i 00_recriar_banco.sql
-- ============================================================================

-- Encerra conexões abertas ao anm_geo (senão o DROP falha "database in use").
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE datname = 'anm_geo' AND pid <> pg_backend_pid();

DROP DATABASE IF EXISTS anm_geo;

CREATE DATABASE anm_geo
  WITH ENCODING = 'UTF8'
       TEMPLATE = template0;