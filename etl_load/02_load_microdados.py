"""
02_load_microdados.py
  Lê os .parquet de data/result_db/microdados/ (gerados pelo 04_microdados.R)
  e INSERE nas tabelas já criadas pelo 01_schema.sql (if_exists="append").
"""

import os
import pandas as pd
from sqlalchemy import create_engine

DB_URL = "postgresql://postgres:0101@localhost:5432/anm_geo"
engine = create_engine(DB_URL)

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
MICRO_DIR = os.path.join(ROOT, "data", "result_db", "microdados")

MAPA = {
    # mestre
    "micro_processo.parquet":                 "micro_processo",
    # fatos
    "micro_processo_evento.parquet":          "micro_processo_evento",
    "micro_processo_pessoa.parquet":          "micro_processo_pessoa",
    "micro_processo_substancia.parquet":      "micro_processo_substancia",
    "micro_processo_municipio.parquet":       "micro_processo_municipio",
    "micro_processo_titulo.parquet":          "micro_processo_titulo",
    "micro_processo_documentacao.parquet":    "micro_processo_documentacao",
    "micro_processo_associacao.parquet":      "micro_processo_associacao",
    "micro_processo_propriedade_solo.parquet":"micro_processo_propriedade_solo",
    # lookups
    "micro_municipio.parquet":                "micro_municipio",
    "micro_pessoa.parquet":                   "micro_pessoa",
    "micro_evento.parquet":                   "micro_evento",
    "micro_fase_processo.parquet":            "micro_fase_processo",
    "micro_substancia.parquet":               "micro_substancia",
    "micro_tipo_requerimento.parquet":        "micro_tipo_requerimento",
    "micro_tipo_associacao.parquet":          "micro_tipo_associacao",
    "micro_tipo_documento.parquet":           "micro_tipo_documento",
    "micro_tipo_documento_legal.parquet":     "micro_tipo_documento_legal",
    "micro_tipo_relacao.parquet":             "micro_tipo_relacao",
    "micro_tipo_representacao_legal.parquet": "micro_tipo_representacao_legal",
    "micro_tipo_responsabilidade_tecnica.parquet": "micro_tipo_responsabilidade_tecnica",
    "micro_tipo_uso_substancia.parquet":      "micro_tipo_uso_substancia",
    "micro_condicao_propriedade_solo.parquet":"micro_condicao_propriedade_solo",
    "micro_motivo_encerramento_substancia.parquet": "micro_motivo_encerramento_substancia",
    "micro_situacao_documento_legal.parquet": "micro_situacao_documento_legal",
    "micro_documento_legal.parquet":          "micro_documento_legal",
    "micro_unidade_administrativa.parquet":   "micro_unidade_administrativa",
    "micro_unidade_protocolizadora.parquet":  "micro_unidade_protocolizadora",
}

# --- Carga -------------------------------------------------------------------
def carregar(arquivo, tabela):
    caminho = os.path.join(MICRO_DIR, arquivo)
    if not os.path.exists(caminho):
        print(f"   AVISO: {arquivo} não encontrado. Pulando.")
        return
    print(f"\n[micro] {arquivo} -> {tabela}")
    df = pd.read_parquet(caminho)

    df.columns = [c.lower() for c in df.columns]

    # NaN/NaT -> None (NULL no banco). object_cols evita estragar datas.
    df = df.astype(object).where(pd.notnull(df), None)

    df.to_sql(tabela, engine, if_exists="append", index=False, chunksize=10000)
    print(f"   {len(df)} linhas inseridas.")


if __name__ == "__main__":
    total = len(MAPA)
    i = 0
    for arquivo, tabela in MAPA.items():
        i += 1
        print(f"[{i}/{total}]", end=" ")
        carregar(arquivo, tabela)

    print("\n=== Carga de microdados concluída ===")