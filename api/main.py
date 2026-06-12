from fastapi import FastAPI, HTTPException
import psycopg

app = FastAPI()

DB_URL = "postgresql://postgres:0101@localhost:5432/anm_geo"

@app.get("/")
def home():
    return {"mensagem": "API do ANM-GEO funcionando!"}

@app.get("/processos")
def listar_processos():
    with psycopg.connect(DB_URL) as conn:
        with conn.cursor() as cur:
            cur.execute("SELECT processo, titular, subspmagrp, fase FROM processos LIMIT 5;")
            colunas = [desc[0] for desc in cur.description]
            linhas = cur.fetchall()
    return [dict(zip(colunas, linha)) for linha in linhas]


@app.get("/processos/{processo:path}")
def detalhe_processo(processo: str):
    with psycopg.connect(DB_URL) as conn:
        with conn.cursor() as cur:
            # 1) dados base
            cur.execute("""
                SELECT processo, titular, cpf_cnpjcm, subs, subspmagrp, fase,
                       tipo_reqcm, area_orig, area_ha, munic, uf, n_munic, munic_fonte
                FROM processos
                WHERE processo = %s;
            """, (processo,))
            row = cur.fetchone()
            if row is None:
                raise HTTPException(status_code=404, detail="Processo não encontrado")
            colunas = [desc[0] for desc in cur.description]
            base = dict(zip(colunas, row))

            # 2) flags de sobreposição
            cur.execute("""
                SELECT ti_ov, uc_ov, qui_ov, ti_influencia, uc_influencia, qui_influencia
                FROM mv_sobreposicoes
                WHERE processo = %s;
            """, (processo,))
            row = cur.fetchone()
            colunas = [desc[0] for desc in cur.description]
            sobreposicao = dict(zip(colunas, row)) if row else {}

    return {
        "dados": base,
        "sobreposicao": sobreposicao
    }
