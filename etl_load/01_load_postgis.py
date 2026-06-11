"""
01_load_postgis.py
 
  Lê os arquivos do R (data/result_db/) e INSERE nas tabelas já criadas pelo
  01_schema.sql (if_exists="append"), preservando tipos fortes e PKs.
"""
 
import os
 
# ----------------------------------------------------------------------------
# Conserto do PROJ: o Windows tem uma variável apontando para o proj.db do
# PostgreSQL (incompatível). Forçamos o proj.db do conda, que sabemos existir,
# ANTES de importar qualquer biblioteca geográfica.
# ----------------------------------------------------------------------------
_PROJ_CONDA = r"C:\GP\Py\my-gee-python-project\.conda\Library\share\proj"
os.environ["PROJ_LIB"]  = _PROJ_CONDA
os.environ["PROJ_DATA"] = _PROJ_CONDA
 
import pyproj  # noqa: E402
pyproj.datadir.set_data_dir(_PROJ_CONDA) 
 
import geopandas as gpd  # noqa: E402
import pandas as pd  # noqa: E402
from sqlalchemy import create_engine  # noqa: E402
 
# --- Conexão -----------------------------------------------------------------
DB_URL = "postgresql://postgres:0101@localhost:5432/anm_geo"
engine = create_engine(DB_URL)
 
# --- Caminhos ----------------------------------------------------------------
ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
RESULT_DB = os.path.join(ROOT, "data", "result_db")
 
# --- Helpers -----------------------------------------------------------------
def carregar_geojson(arquivo, tabela):
    caminho = os.path.join(RESULT_DB, arquivo)
    print(f"\n[espacial] {arquivo} -> {tabela}")
    gdf = gpd.read_file(caminho, engine="fiona")
    gdf.columns = [c.lower() for c in gdf.columns]
 
    # O R já salvou em 4326. Marca o CRS; converte só se vier outro.
    if gdf.crs is None:
        gdf = gdf.set_crs(4326, allow_override=True)
    elif gdf.crs.to_epsg() != 4326:
        gdf = gdf.to_crs(4326)
 
    gdf = gdf.rename_geometry("geom")
 
    cols = [c for c in gdf.columns if c != "geom"]
    
    # # converte as decimais geradas pelo Pandas de volta para "Inteiros que aceitam Nulos" (Int64)
    # for col in cols:
    #     if gdf[col].dtype == 'float64':
    #         try:
    #             gdf[col] = gdf[col].astype('Int64')
    #         except TypeError:
    #             pass
    
    cols_inteiras = ["n_munic", "arr_ndcl", "arr_nbuy"]
    for col in cols_inteiras:
        if col in gdf.columns:
            gdf[col] = gdf[col].astype('Int64')

    gdf[cols] = gdf[cols].where(pd.notnull(gdf[cols]), None)
 
    gdf.to_postgis(tabela, engine, if_exists="append", index=False)
    print(f"   {len(gdf)} linhas inseridas.")
 
 
def carregar_csv(arquivo, tabela):
    caminho = os.path.join(RESULT_DB, arquivo)
    print(f"\n[csv] {arquivo} -> {tabela}")
    df = pd.read_csv(caminho)
    df.columns = [c.lower() for c in df.columns]
    df = df.where(pd.notnull(df), None)
    df.to_sql(tabela, engine, if_exists="append", index=False)
    print(f"   {len(df)} linhas inseridas.")
 
# --- Execução ----------------------------------------------------------------
if __name__ == "__main__":
    carregar_geojson("pma_amzl_ALLminerals_final.geojson", "processos")
    carregar_geojson("ti_amzl.geojson",  "terras_indigenas")
    carregar_geojson("uc_amzl.geojson",  "unidades_conservacao")
    carregar_geojson("qui_amzl.geojson", "quilombolas")
    carregar_csv("cfem_amzl_ALLminerals_GOLD_CASScorrected.csv", "cfem_eventos")
    carregar_csv("cfem_aut_all_min_amzl.csv", "cfem_autuacoes")
 
    print("\n=== Carga espacial + CFEM concluída ===")