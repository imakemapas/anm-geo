"""
ETL Load — carrega outputs do pipeline R no PostGIS
"""

import geopandas as gpd
import pandas as pd
from sqlalchemy import create_engine, text
from pathlib import Path

# =============================================================================
# Conexão
# =============================================================================
DB_URL = "postgresql://postgres:0101@localhost:5432/anm_geo"
engine = create_engine(DB_URL)

# Raiz do projeto
ROOT   = Path("C:/GP/anm-geo")
RESULT = ROOT / "data" / "result"
CLEAN  = ROOT / "data" / "clean_data"
PRE_PROC = ROOT / "data" / "pre_proc_data"

def log(msg):
    print(f"[ETL] {msg}")

# =============================================================================
# 1. Processos (PMA shapefile)
# =============================================================================
log("Carregando processos...")
pma = gpd.read_file(RESULT / "pma_amzl_ALLminerals_final.shp")

# Renomeia colunas para o schema
pma = pma.rename(columns={
    "PROCESSO":    "processo",
    "FASE":        "fase",
    "ULT_EVENTO":  "ult_evento",
    "ULT_EV_ID":   "ult_ev_id",
    "ULT_EV_DAT":  "ult_ev_dat",
    "ULT_EV_DES":  "ult_ev_des",
    "TITULAR":     "titular",
    "SUBS":        "subs",
    "SUBSpmaGRP":  "subs_grp",
    "AREA_HA":     "area_ha",
    "AREA_orig":   "area_orig",
    "TIPO_REQcm":  "tipo_req",
    "CPF_CNPJcm":  "cpf_cnpj_cm",
    "code_muni":   "code_muni",
    "name_muni":   "name_muni",
    "abbrev_stat": "abbrev_state",
    "name_state":  "name_state",
    "name_regio":  "name_region",
    "arr_kg_T":    "arr_kg_t",
    "arr_kg_L":    "arr_kg_l",
    "arr_g_T":     "arr_g_t",
    "arr_g_L":     "arr_g_l",
})

pma = pma.to_crs("EPSG:4326")

pma.to_postgis(
    "processos", engine,
    if_exists="replace",
    index=False,
    chunksize=5000
)
log(f"Processos carregados: {len(pma)} registros")

# =============================================================================
# 2. CFEM eventos
# =============================================================================
log("Carregando CFEM eventos...")
cfem = pd.read_csv(RESULT / "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv")

cfem = cfem.rename(columns={
    "PROCESSO":      "processo",
    "ANO":           "ano",
    "MES":           "mes",
    "SUBSarr":       "subs_arr",
    "SUBSarrSIM":    "subs_arr_sim",
    "CPF_CNPJarr":   "cpf_cnpj_arr",
    "NOME_arr":      "nome_arr",
    "code_muni":     "code_muni",
    "name_muni":     "name_muni",
    "abbrev_state":  "abbrev_state",
    "name_state":    "name_state",
    "name_region":   "name_region",
    "QTD_MINERIO":   "qtd_minerio",
    "UM":            "um",
    "PESO_KG":       "peso_kg",
    "PESO_G":        "peso_g",
    "VALORarr":      "valor_arr",
    "VALORtot":      "valor_tot",
    "ALIQUOTA_PCT":  "aliquota_pct",
    "PESO_KG_final": "peso_kg_final",
    "PESO_G_final":  "peso_g_final",
    "preco_g_orig":  "preco_g_orig",
    "preco_g_final": "preco_g_final",
    "corr":          "corr",
    "data":          "data",
    "AREA_HA":       "area_ha",
    "FASE":          "fase",
    "ULT_EVENTO":    "ult_evento",
    "ULT_EV_ID":     "ult_ev_id",
    "ULT_EV_DAT":    "ult_ev_dat",
    "ULT_EV_DES":    "ult_ev_des",
    "TITULAR":       "titular",
    "SUBS":          "subs",
    "TIPO_REQcm":    "tipo_req",
    "CPF_CNPJcm":    "cpf_cnpj_cm",
})

cfem.to_sql(
    "cfem_eventos", engine,
    if_exists="replace",
    index=False,
    chunksize=10000
)
log(f"CFEM eventos carregados: {len(cfem)} registros")

# =============================================================================
# 3. Terras Indígenas
# =============================================================================
log("Carregando Terras Indígenas...")
ti = gpd.read_file(RESULT / "ti_amzl.shp", engine="fiona").to_crs("EPSG:4326")
ti.columns = [c.lower() for c in ti.columns]
ti.to_postgis("terras_indigenas", engine, if_exists="replace", index=False)
log(f"TI carregadas: {len(ti)} registros")

# =============================================================================
# 4. Unidades de Conservação
# =============================================================================
log("Carregando Unidades de Conservação...")
uc = gpd.read_file(RESULT / "uc_amzl.shp", engine="fiona").to_crs("EPSG:4326")
uc.columns = [c.lower() for c in uc.columns]
uc.to_postgis("unidades_conservacao", engine, if_exists="replace", index=False)
log(f"UC carregadas: {len(uc)} registros")

# =============================================================================
# 5. Quilombolas
# =============================================================================
log("Carregando Quilombolas...")
qui = gpd.read_file(RESULT / "qui_amzl.shp", engine="fiona").to_crs("EPSG:4326")
qui.columns = [c.lower() for c in qui.columns]
qui.to_postgis("quilombolas", engine, if_exists="replace", index=False)
log(f"Quilombolas carregados: {len(qui)} registros")

# =============================================================================
# 6. CFEM autuações
# =============================================================================
log("Carregando CFEM autuações...")
aut = pd.read_csv(CLEAN / "cfem_aut_all_min_amzl.csv")
aut.columns = [c.lower() for c in aut.columns]
aut = aut.rename(columns={
    "suBSaut":      "subs_aut",
    "cpf_cnpjaut":  "cpf_cnpj_aut",
    "titularaut":   "titular_aut",
    "valoraut":     "valor_aut",
})
aut.to_sql("cfem_autuacoes", engine, if_exists="replace", index=False, chunksize=5000)
log(f"Autuações carregadas: {len(aut)} registros")

log("=== CARGA COMPLETA ===")

# =============================================================================
# 7. Embargos e Infrações
# =============================================================================
log("Carregando Embargos e Infrações...")

PRE_PROC = ROOT / "data" / "pre_proc_data"

# IBAMA embargos
log("Carregando IBAMA embargos...")
emb_ib = gpd.read_file(PRE_PROC / "ibama_embargos.shp", engine="fiona").to_crs("EPSG:4326")
emb_ib.columns = [c.lower() for c in emb_ib.columns]
emb_ib.to_postgis("embargos_ibama", engine, if_exists="replace", index=False)
log(f"IBAMA embargos carregados: {len(emb_ib)} registros")

# IBAMA infrações
log("Carregando IBAMA infrações...")
inf_ib = pd.read_csv(PRE_PROC / "ibama_infracoes.csv", encoding="latin-1")
inf_ib.columns = [c.lower() for c in inf_ib.columns]
inf_ib.to_sql("infracoes_ibama", engine, if_exists="replace", index=False, chunksize=5000)
log(f"IBAMA infrações carregadas: {len(inf_ib)} registros")

# ICMBio embargos
log("Carregando ICMBio embargos...")
emb_ic = gpd.read_file(PRE_PROC / "icmbio_embargos.shp", engine="fiona").to_crs("EPSG:4326")
emb_ic.columns = [c.lower() for c in emb_ic.columns]
emb_ic.to_postgis("embargos_icmbio", engine, if_exists="replace", index=False)
log(f"ICMBio embargos carregados: {len(emb_ic)} registros")

# ICMBio infrações
log("Carregando ICMBio infrações...")
inf_ic = gpd.read_file(PRE_PROC / "icmbio_infracoes.shp", engine="fiona").to_crs("EPSG:4326")
inf_ic.columns = [c.lower() for c in inf_ic.columns]
inf_ic.to_postgis("infracoes_icmbio", engine, if_exists="replace", index=False)
log(f"ICMBio infrações carregadas: {len(inf_ic)} registros")

# SEMA-MT embargos
log("Carregando SEMA-MT embargos...")
emb_mt = gpd.read_file(PRE_PROC / "sema_mt_embargos.shp", engine="fiona").to_crs("EPSG:4326")
emb_mt.columns = [c.lower() for c in emb_mt.columns]
emb_mt.to_postgis("embargos_sema_mt", engine, if_exists="replace", index=False)
log(f"SEMA-MT embargos carregados: {len(emb_mt)} registros")

# SEMA-MT SIGA embargos
log("Carregando SEMA-MT SIGA embargos...")
emb_siga = gpd.read_file(PRE_PROC / "sema_mt_embargos_siga.shp", engine="fiona").to_crs("EPSG:4326")
emb_siga.columns = [c.lower() for c in emb_siga.columns]
emb_siga.to_postgis("embargos_sema_mt_siga", engine, if_exists="replace", index=False)
log(f"SEMA-MT SIGA embargos carregados: {len(emb_siga)} registros")

# SEMA-MT SIGA infrações
log("Carregando SEMA-MT SIGA infrações...")
inf_siga = gpd.read_file(PRE_PROC / "sema_mt_infracoes_siga.shp", engine="fiona").to_crs("EPSG:4326")
inf_siga.columns = [c.lower() for c in inf_siga.columns]
inf_siga.to_postgis("infracoes_sema_mt_siga", engine, if_exists="replace", index=False)
log(f"SEMA-MT SIGA infrações carregadas: {len(inf_siga)} registros")

log("=== CARGA COMPLETA ===")