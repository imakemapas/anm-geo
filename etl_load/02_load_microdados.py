"""
ETL Load — Microdados SCM completo
Carrega TODOS os arquivos TXT dos microdados no PostGIS
"""

import pandas as pd
from sqlalchemy import create_engine
from pathlib import Path

DB_URL = "postgresql://postgres:0101@localhost:5432/anm_geo"
engine = create_engine(DB_URL)

ROOT      = Path("C:/GP/anm-geo")
MICRO_DIR = ROOT / "data" / "raw_data" / "anm_microdados" / "microdados-scm"

def log(msg):
    print(f"[MICRO] {msg}")

def read_txt(filename):
    path = MICRO_DIR / filename
    log(f"Lendo {filename} ({path.stat().st_size / 1024 / 1024:.1f} MB)...")
    for enc in ["Windows-1252", "utf-8", "latin-1", "cp850"]:
        try:
            df = pd.read_csv(path, sep=";", encoding=enc,
                           dtype=str, low_memory=False)
            log(f"  encoding OK: {enc} | {len(df)} registros")
            return df
        except UnicodeDecodeError:
            continue

    return pd.read_csv(path, sep=";", encoding="utf-8",
                      errors="ignore", dtype=str, low_memory=False)

def load(filename, tablename):
    try:
        df = read_txt(filename)
        df.columns = [c.lower() for c in df.columns]
        df.to_sql(tablename, engine, if_exists="replace",
                  index=False, chunksize=10000)
        log(f"  ✅ {tablename}: {len(df)} registros carregados")
    except Exception as e:
        log(f"  ❌ {tablename} ERRO: {e}")

# =============================================================================
# Tabelas principais (grandes)
# =============================================================================
load("Processo.txt",               "micro_processo")
load("ProcessoEvento.txt",         "micro_processo_evento")
load("ProcessoPessoa.txt",         "micro_processo_pessoa")
load("ProcessoSubstancia.txt",     "micro_processo_substancia")
load("ProcessoMunicipio.txt",      "micro_processo_municipio")
load("ProcessoTitulo.txt",         "micro_processo_titulo")
load("ProcessoDocumentacao.txt",   "micro_processo_documentacao")
load("ProcessoAssociacao.txt",     "micro_processo_associacao")
load("ProcessoPropriedadeSolo.txt","micro_processo_propriedade_solo")

# =============================================================================
# Tabelas de pessoas
# =============================================================================
load("Pessoa.txt",                 "micro_pessoa")

# =============================================================================
# Tabelas de lookup (pequenas — dicionários)
# =============================================================================
load("Municipio.txt",                    "micro_municipio")
load("Evento.txt",                       "micro_evento")
load("FaseProcesso.txt",                 "micro_fase_processo")
load("Substancia.txt",                   "micro_substancia")
load("TipoRequerimento.txt",             "micro_tipo_requerimento")
load("TipoAssociacao.txt",               "micro_tipo_associacao")
load("TipoDocumento.txt",                "micro_tipo_documento")
load("TipoDocumentoLegal.txt",           "micro_tipo_documento_legal")
load("TipoRelacao.txt",                  "micro_tipo_relacao")
load("TipoRepresentacaoLegal.txt",       "micro_tipo_representacao_legal")
load("TipoResponsabilidadeTecnica.txt",  "micro_tipo_responsabilidade_tecnica")
load("TipoUsoSubstancia.txt",            "micro_tipo_uso_substancia")
load("CondicaoPropriedadeSolo.txt",      "micro_condicao_propriedade_solo")
load("MotivoEncerramentoSubstancia.txt", "micro_motivo_encerramento_substancia")
load("SituacaoDocumentoLegal.txt",       "micro_situacao_documento_legal")
load("DocumentoLegal.txt",               "micro_documento_legal")
load("UnidadeAdministrativaRegional.txt","micro_unidade_administrativa")
load("UnidadeProtocolizadora.txt",       "micro_unidade_protocolizadora")

log("=== TODOS OS MICRODADOS CARREGADOS ===")