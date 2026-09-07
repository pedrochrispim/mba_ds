# Carrega pacotes ####
library(data.table)
library(lubridate)
library(stringr)
library(arrow)
library(readxl)
library(zoo)
source("./script/funcoes.R")

# Manipulação e Limpeza dos dados ####

#######################################
########## DADOS DE SUPORTE ###########
#######################################

# dados UF e ano
estados <- data.table(
  estado = c(
    "Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal",
    "Espírito Santo","Goiás","Maranhão","Mato Grosso","Mato Grosso do Sul",
    "Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
    "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia",
    "Roraima","Santa Catarina","São Paulo","Sergipe","Tocantins"
  ),
  uf = c(
    "AC","AL","AP","AM","BA","CE","DF",
    "ES","GO","MA","MT","MS",
    "MG","PA","PB","PR","PE","PI",
    "RJ","RN","RS","RO",
    "RR","SC","SP","SE","TO"
  ),
  coduf = as.integer(c(
    12, 27, 16, 13, 29, 23, 53,
    32, 52, 21, 51, 50,
    31, 15, 25, 41, 26, 22,
    33, 24, 43, 11,
    14, 42, 35, 28, 17
  ))
)[, Estado := str_to_upper(estado)]

mestxt <- data.table(
  mest = toupper(month.abb),
  mes = as.integer(1:12)
)

load("./data/raw/cid10.rda")

#######################################
############# POPULACAO ###############
#######################################

# Baixe a tabela9606 do SIDRA/IBGE
# será necessário baixar várias tabelas pelo 
# limite de extração de dados.

pop <- list.files(path = "./data/raw/", 
                  pattern = "tabela9606", 
                  full.names = T)

populacao <- rbindlist(lapply(pop, function(x){
  a = fread(x)
  return(a)
}))

populacao[,":="(
  codmun6 = str_sub(Cód., end = 6),
  uf = str_sub(as.character(Município), start = -3, end = -2),
  coduf = str_sub(Cód., end = 2),
  Municipio = str_sub(Cód., end = -6),
  raca = `Cor ou raça`,
  sexo = Sexo,
  Idade = Idade,
  Ano2010 = as.numeric(`2010`),
  Ano2022 = as.numeric(`2022`))][, c("Cód.", "Cor ou raça", "Sexo", "2010", "2022") := NULL]

# atribui populacao média
populacao[,Populacao := fcase(!is.na(Ano2010) & !is.na(Ano2022), (Ano2010+Ano2022)/2,
                              is.na(Ano2010) & !is.na(Ano2022), 0,
                              !is.na(Ano2010) & is.na(Ano2022), 0,
                              is.na(Ano2010) & is.na(Ano2022), 0,
                              default = 0)]

# aribui populacao por faixa etaria
populacao[,":="(
  # faixa etaria para emprego
  i_e = factor(fcase(Idade >=14 & Idade <=17, "14 a 17 anos",
                     Idade >=18 & Idade <=24, "18 a 24 anos",
                     Idade >=25 & Idade <=39, "25 a 39 anos",
                     Idade >=40 & Idade <=59, "40 a 59 anos",
                     Idade >=60, "60 anos ou mais"),
               levels = c(
                 "14 a 17 anos",
                 "18 a 24 anos",
                 "25 a 39 anos",
                 "40 a 59 anos",
                 "60 anos ou mais")),
  
  # sociodemografia
  i_sd = factor(fcase(Idade < 18, "adolescentes (10a-|18a)",
                      Idade >= 18 & Idade < 30, "jovens (18a-|29a)",
                      Idade >= 30 & Idade < 60, "adultos (30a|-59a)",
                      Idade >= 60 & Idade < 80, "idosos (60a|-79a)",
                      Idade >= 80, "muito idosos (80a-)"),
                levels = c("infantil (10a-|18a)",
                           "jovens (18a-|29a)",
                           "adultos (30a|-59a)",
                           "idosos (60a|-79a)",
                           "muito idosos (80a-)")))]



populacao <- populacao[Populacao !=0]
populacao[, ":=" (MUNICIPIO = str_to_upper(str_sub(Município, end = -6)),
                  municipio = str_sub(Município, end = -6))]
populacao[,key := str_c(uf,MUNICIPIO)]

saveRDS(populacao,
        file = "./data/raw/populacao.rds")



#######################################
############## OBITOS #################
#######################################


obitos <- read_parquet(file = "./data/raw/obitos.parquet")


##################################################
# Aplica criterios de selecao e classificacao ####
##################################################

# CORRIGE CLASSE DE DADOS
obitos[,":="(CODMUNRES = as.character(CODMUNRES),
             CODMUNOCOR = as.character(CODMUNOCOR),
             DTOBITO = as.character(DTOBITO),
             DTNASC = as.character(DTNASC),
             DTCADASTRO = as.character(DTCADASTRO),
             DTRECEBIM = as.character(DTRECEBIM),
             DTATESTADO = as.character(DTATESTADO),
             HORAOBITO = as.character(HORAOBITO))]

obitos[, DTNASC := iconv(DTNASC, from = "", to = "UTF-8", sub = "") # corrige caracteres estranhos (não UTF-8)
                       ][, DTNASC := fifelse(DTNASC=="27085193", "27081953", DTNASC)] # corrige erro de digitacao
                         
# ATRIBUI VARIAVEIS SEXO, DATAS, RACA, ESCOLARIDADE, LOCAL DE OBITO
obitos[,":="(CAUSABAS = fcase(str_length(CAUSABAS) == 3, paste0(CAUSABAS,"0"),
                              default = CAUSABAS),
             DTOBITO2 = fcase(str_length(DTOBITO) == 8, DTOBITO,
                              str_length(DTOBITO) == 7, paste0('0',DTOBITO), # aproxima datas incompletas pelo dia
                              str_length(DTOBITO) == 6, paste0('15',DTOBITO), # aproxima datas incompletas pelo dia
                              str_length(DTOBITO) == 4, DTOBITO),  # datas com ano somente serão descartadas
             DTNASC2  = fcase(str_length(DTNASC) == 8, DTNASC,
                              str_length(DTNASC) == 7, paste0('0',DTNASC), # aproxima datas incompletas pelo dia
                              str_length(DTNASC) == 6, paste0('15',DTNASC), # aproxima datas incompletas pelo dia
                              str_length(DTNASC) == 4, DTNASC),  # datas com ano somente serão descartadas
             DTCADASTRO2  = fcase(str_length(DTCADASTRO) == 8, DTCADASTRO,
                                  str_length(DTCADASTRO) == 7, paste0('0',DTCADASTRO)), # datas com ano somente serão descartadas
             DTRECEBIM2  = fcase(str_length(DTRECEBIM) == 8, DTRECEBIM,
                                 str_length(DTRECEBIM) == 7, paste0('0',DTRECEBIM)), # datas com ano somente serão descartadas
             DTATESTADO2 = fcase(str_length(DTATESTADO) == 8, DTATESTADO,
                                 str_length(DTATESTADO) == 7, paste0('0',DTATESTADO)), # datas com ano somente serão descartadas
             HORAOBITO2 = fcase(str_length(HORAOBITO) == 4, HORAOBITO,
                                str_length(HORAOBITO) > 4, paste(str_sub(HORAOBITO,end = 2),str_sub(HORAOBITO,start = -2)),
                                str_length(HORAOBITO) < 4, paste(str_sub(HORAOBITO,end = 2),"00")), # datas com ano somente serão descartadas
             obito_home = CODMUNRES == CODMUNOCOR, # identifica se o obito ocorreu no município de residência
             codmun6 = str_sub(CODMUNRES,1,6), # fica apenas com 6 dígitos de codificação
             codmunocor6 = str_sub(CODMUNOCOR,1,6),
             sexo = fcase(SEXO == 1, "Homens",
                          SEXO == 2, "Mulheres",       # Define sexo
                          SEXO %in% c(0,9), "Ignorado",    
                          default = NA),
             raca = fcase(RACACOR == 1, "Branca",
                          RACACOR == 2, "Preta",
                          RACACOR == 3, "Amarela",      # Define raca
                          RACACOR == 4, "Parda",
                          RACACOR == 5, "Indígena",
                          RACACOR == 9, "Ignorado",
                          default = NA),
             escolaridade = fcase(ESC == 1, "Nenhuma",
                                  ESC == 2, "1 a 3 anos",
                                  ESC == 3, "4 a 7 anos",   # Define escolaridade
                                  ESC == 4, "8 a 11 anos",
                                  ESC == 5, "12 ou mais anos",
                                  ESC  %in% c(0,9), "Ignorado",
                                  default = NA),
             estcivil = fcase(ESTCIV == 1, "Solteiro",
                              ESTCIV == 2, "Casado",
                              ESTCIV == 3, "Viúvo",   # Define escolaridade
                              ESTCIV == 4, "Separado/Divorciado",
                              ESTCIV == 5, "União Estável",
                              ESTCIV  %in% c(0,9,6), "Ignorado",
                              default = NA),
             local = fcase(LOCOCOR == 1, "Hospital",
                           LOCOCOR == 2, "Outro Est Saúde",
                           LOCOCOR == 3, "Domicílio",       #  Define local de obito
                           LOCOCOR == 4, "Via Pública",
                           LOCOCOR == 5, "Outros",
                           LOCOCOR == 6, "Aldeia Indígena",
                           LOCOCOR == 9, "Ignorado",
                           default = NA))] # fica apenas com 6 dígitos de codificação



# CALCULA A IDADE APROXIMADA EM ANOS QUANDO DA OCORRÊNCIA DO ÓBITO
obitos[,":="(dt_obito = as.IDate(DTOBITO2, format = "%d%m%Y"), # Converte formato de data
             dt_nasc = as.IDate(DTNASC2, format = "%d%m%Y"), # Converte formato de data
             dt_cadastro = as.IDate(DTCADASTRO2, format = "%d%m%Y"), # Covnerte formato de data
             dt_recebim = as.IDate(DTRECEBIM2, format = "%d%m%Y"), # Covnerte formato de data
             dt_atestado = as.IDate(DTATESTADO2, format = "%d%m%Y"), # Covnerte formato de data
             dt_hora = as.ITime(HORAOBITO2, format = "%H%M"), # Covnerte formato de data
             Idade = as.numeric(as.IDate(DTOBITO2, format = "%d%m%Y") - as.IDate(DTNASC2, format = "%d%m%Y"))/365.25)      # calcula a idade em anos aproximados
][,c("DTOBITO","DTOBITO2","DTNASC","DTNASC2","CODMUNOCOR","CODMUNRES","IDADE","TIPOBITO","SEXO","RACACOR","ESC","LOCOCOR",
     "DTCADASTRO","DTRECEBIM","DTATESTADO","HORAOBITO","ESTCIV") := NULL]


# CALCULA O PREENCHIMENTO DAS VARIAVEIS
Preenchimento <- data.table(
  variavel = names(obitos),
  Preench = obitos[,unlist(lapply(.SD,meanfill))]
)[order(-Preench)]

# SELECIONA APENAS VARIAVEIS COM MAIS DE 80% DE PREENCHIMENTO (<20% MISSING)
Preenchimento80 <- Preenchimento[Preench >80]
  
# SELECIONA SUBSET 
# COM VAR > 80% DE PREENCHMENTO
obitos <- obitos[,.SD,.SDcols = Preenchimento80$variavel]
#rm(obitos)

# ELIMINA INCONSISTÊNCIAS
obitos_sub <- obitos[str_length(dt_nasc)==10]                           # elimina preenchimento incompleto de data
obitos_sub <- obitos_sub[Idade>=10 | is.na(Idade)]                      # elimina menores de 10 anos de idade 
obitos_sub <- obitos_sub[!str_detect(codmun6,"0000$")]                  # elimina municípios indefinidos 
obitos_sub[,sexo :=fifelse(sexo == "Ignorado",NA,sexo)]                 # Transforma sexo ignorado em NA para imputação posterior
obitos_sub[,escolaridade :=fifelse(escolaridade == "Ignorado",NA,escolaridade)] # Transforma escolaridade ignorada em NA para imputação posterior


# cria vairavel no cid10
cid10 <- readRDS( "./data/raw/cid10.rds")
acidente <- cid10[str_detect(Grupo_desc, "acidente"),Subcategoria] |> unique()


# dataset agressoes
obitos_sub[,CAUSABAS := str_to_upper(CAUSABAS)]
obitos_sub[,":="(# agressoes totais
    agressoesT = fifelse(str_detect(CAUSABAS, "^X(6|7|8[0-9])|^X9|^Y[0-2]|^Y3[0-4]"),1,0),
    # agressoes indeterminadas em relacao aa intencao
    agressoesI = fifelse(str_detect(CAUSABAS, "^Y1|^Y2|^Y3[0-4]"),1,0),
    # agressoes intencionais
    agressoes_3os = fifelse(str_detect(CAUSABAS, "^X8[5-9]|^X9|^Y0"),1,0),
    # agressoes auto-infligidas(suicidio)
    agressoes_auto = fifelse(str_detect(CAUSABAS, "^X(6|7|8[0-4])"),1,0),
    # acidentes
    acidentes = fifelse(CAUSABAS %in% acidente,1,0))]
  

# adiciona ao dataset
obitos_sub[# adiciona motivos das causas externas
  cid10,":="(agressao_desc = i.Cat_desc,
             Grupo = i.Grupo,
             Grupo_desc = Grupo_desc), on = .(CAUSABAS = Subcategoria)]


write_parquet(obitos_sub,
              sink = "./data/raw/obitos_sub.parquet")

  
# SUBSET OBITOS POR AGRESSOES
agressoes_base <- obitos_sub[agressoesT == 1 | acidentes == 1
                             ][,agg_tipo := fifelse(Grupo_desc %in% c("Agressões",
                                                                   "Eventos (fatos) cuja intenção é indeterminada",
                                                                   "Lesões autoprovocadas intencionalmente"), 
                                                   Grupo_desc,
                                                   "Acidentes")]
  

write_parquet(agressoes_base,
              sink = "./data/raw/agressoes_base.parquet")
  
  
  
# DESCARTA OBJETOS GRANDES
obitos_N <- obitos[,.N]
obitos_SemDataNasc <- obitos[str_length(dt_nasc)!=10,.N]
obitos_MenorDeSemIdade <- obitos[Idade < 18 | is.na(Idade),.N]
obitos_MunicipiosIndefinidos <- obitos[str_detect(codmun6,"0000$"),.N]
  
save(obitos_N,
     obitos_SemDataNasc,
     obitos_MenorDeSemIdade,
     obitos_MunicipiosIndefinidos,
     file = "./data/obitos_st.rda")
  
rm(obitos,obitos_sub)
gc()
  
  




#######################################
########## ATENCAO BASICA #############
#######################################

# atencao basica
# 2010 - 2020
ab <- fread("./data/raw/br_ms_atencao_basica_municipio.csv.gz")

ab[,":="(codmun6 = id_municipio_6,
         uf = sigla_uf)]

abuf <- ab[,.(coduf= str_sub(id_municipio_6, end = 2),
              uf,
              populacao,
              populacao_coberta_total_atencao_basica,
              ano, 
              mes)]


abuf <- abuf[,.(populacao = sum(populacao),
                popab = sum(populacao_coberta_total_atencao_basica)), by = .(coduf, uf, ano, mes)]

abuf[,":="(cobab = round(popab/populacao*100,2))]


# 2021 - 2023
pns_files <- list.files(path = "./data/raw",
                       pattern = "cobertura-pns-01-09-2026",
                       full.names = T)

ab2023 <- rbindlist(lapply(pns_files,function(x){
  a = read_xlsx(x,
                sheet = "Dados")
  return(a)
}))

ab2023 <- ab2023[,.(uf = UF,
                    Estado = Estado,
                    Regiao = Região,
                    ano = as.integer(str_sub(`Competência CNES`,start = -4)),
                    mest = str_sub(`Competência CNES`,end = 3),
                    cobab = as.numeric(str_sub(str_replace(`Cobertura APS`,",", "."),end=-2)))]

ab2023 <- mestxt[ab2023, on = "mest"]
ab2023[,key := str_c(uf,)]

ab2023 <- unique(populacao[,.(uf, coduf = as.integer(str_sub(codmun6,end=2)))])[ab2023, on = "uf"]


# UNIR AS BASES
ab <- rbindlist(list(ab2023[,.(coduf, uf, ano, mes, cobab)],
                     abuf[,.(coduf, uf, ano, mes, cobab)]))[ano %in% c(2010:2023)]

ab <- estados[ab, on = "uf"][,i.coduf := NULL]

saveRDS(ab, "./data/raw/ab.rds")



#######################################
############ DESEMPREGO ###############
#######################################


desemprego <- read_xlsx("./data/raw/DesocupacaoTabela4094.xlsx",
                        sheet = "dataset") |> setDT()

desemprego[,":="(coduf = nafill(as.integer(coduf),type = "locf"),
                 uf = na.locf(uf),
                 Trimestre = na.locf(Trimestre),
                 i_e = factor(i_e,
                              levels = c(
                                "14 a 17 anos",
                                "18 a 24 anos",
                                "25 a 39 anos",
                                "40 a 59 anos",
                                "60 anos ou mais")))]

desemprego[,":="(ano = as.integer(str_sub(Trimestre, start = -4)),
                 mes = as.integer(fcase(str_detect(Trimestre, "^1"), 1,
                                        str_detect(Trimestre, "^2"), 4,
                                        str_detect(Trimestre, "^3"), 7,
                                        str_detect(Trimestre, "^4"), 10)),
                 Desemprego = as.numeric(Desemprego))]


saveRDS(desemprego, "./data/raw/desemprego.rds")


#######################################
############### GINI ##################
#######################################


gini <- melt(
  read_xlsx("./data/raw/Gini_Tabela7435.xlsx",
            sheet = "dataset") |> setDT(),
  id.vars = "estado",
  variable.name = "ano",
  value.name = "gini")

gini <- estados[gini, on = "estado"]
gini[,ano := as.integer(as.character(ano))]

saveRDS(gini, "./data/raw/gini.rds")


##########################
######### IDH ############
##########################

idhuf <- readxl::read_xlsx("./data/raw/idh_uf.xlsx",
                           sheet = "dataset") %>% 
  setDT() %>%
  .[-1] %>% 
  .[,":="(Estado = str_to_upper(Territorialidades))] %>% 
  .[estados, on = "Estado"] 


saveRDS(idhuf,
        file = "./data/raw/idhuf.rds")
