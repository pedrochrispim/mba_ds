-- ============================================================
  -- 1. JUNTA OS ARQUIVOS DE TODOS OS ANOS
  -- 1.1. O código foi cosntruído para DuckSB, mas não foi testado
  -- modificações podem ser necessárias
  -- 1.2 Ele considera que todos os arquivos de dados estão em
  -- ./data/raw no formato *.csv. Para isso, utilize o código
  -- em 01_00_ImportaDBC_ConverteCSV.R
-- ============================================================
  
  CREATE OR REPLACE TABLE obitos AS
SELECT
*,
regexp_extract(filename, 'DOBR([0-9]{4})\.csv', 1) AS ano
FROM read_csv(
  'data/raw/DOBR*.csv',
  union_by_name = true,
  filename = true
);


-- ============================================================
  -- 2. CALCULA O PREENCHIMENTO DAS COLUNAS
-- ============================================================
  
  CREATE OR REPLACE TABLE preenchimento AS
WITH dados_longos AS (
  
  UNPIVOT obitos
  ON COLUMNS(*)
  INTO
  NAME coluna
  VALUE valor
  
)

SELECT
coluna,

COUNT(*) AS total_registros,

COUNT(valor) AS registros_preenchidos,

COUNT(*) - COUNT(valor) AS registros_vazios,

ROUND(
  100.0 * COUNT(valor) / COUNT(*),
  2
) AS percentual_preenchimento

FROM dados_longos

GROUP BY coluna

ORDER BY percentual_preenchimento DESC;


-- ============================================================
  -- 3. VISUALIZA O PREENCHIMENTO
-- ============================================================
  
  SELECT *
  FROM preenchimento;


-- ============================================================
  -- 4. IDENTIFICA COLUNAS COM MAIS DE 80% DE PREENCHIMENTO
-- ============================================================
  
  CREATE OR REPLACE TEMP TABLE colunas_80 AS

SELECT coluna
FROM preenchimento
WHERE percentual_preenchimento > 80;


-- ============================================================
  -- 5. MONTA LISTA DINÂMICA DE COLUNAS
-- ============================================================
  
  SET VARIABLE lista_colunas = (
    
    SELECT string_agg(
      '"' || replace(coluna, '"', '""') || '"',
      ', '
    )
    FROM colunas_80
    
  );


-- ============================================================
  -- 6. EXPORTA APENAS AS COLUNAS SELECIONADAS
-- ============================================================
  
  SET VARIABLE sql_exportacao = (
    
    SELECT
    'COPY (SELECT ' ||
      getvariable('lista_colunas') ||
      ' FROM obitos) ' ||
      'TO ''data/raw/obitos.csv'' ' ||
      '(HEADER, DELIMITER '','');'
    
  );


-- Executa a instrução COPY gerada
FROM query(getvariable('sql_exportacao'));