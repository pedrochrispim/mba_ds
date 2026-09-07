# Carrega pacotes ####
library(data.table)
library(stringr)
library(arrow)
source("./script/funcoes.R")

####################################
######### CARREGA DADOS ###########
####################################


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


# obitos
load("./data/imputados/agressoes_imp.rda")
agressoes_subset <- read_parquet("./data/dataset/agressoes_subset.parquet")

# populacao
populacao <- readRDS("./data/raw/populacao.rds")

# IDH
idhuf <- readRDS("./data/raw/idhuf.rds")

# Cobertura AB
ab <- readRDS("./data/raw/ab.rds")

# Desemprego
desemprego <- readRDS("./data/raw/desemprego.rds")

# GINI
gini <- readRDS("./data/raw/gini.rds")

###############################################
######## carregar as 11 bases do mice #########
###############################################
vars_imputadas <- c(
  "sexo",
  "raca",
  "estcivil",
  "escolaridade",
  "DIFDATA"
)
# 
path <- "./data/imputados"
parquetfiles <- list.files("./data/imputados",
                           pattern = ".parquet",
                           full.names = T)

base_original <- agressoes_subset



############################################################################################################
############################################################################################################
############################################################################################################


###############################################
##### montar lista de datasets imputados ######
################## bases_mice #################
###############################################

# ler as bases
# montar lista de datasets
bases_mice <- lapply(
  1:10,
  function(i) {
    as.data.table(
      read_parquet(parquetfiles[i])
        )
      }
  )

names(bases_mice) <- sprintf("imp_%02d", 1:10)
names(bases_mice)

# manter somente campos imputados
bases_mice <- lapply(
  bases_mice,
  function(x) {
    x[, c("id", vars_imputadas), with = FALSE]
  }
)

# N# por id
sapply(
  bases_mice,
  function(x) uniqueN(x[["id"]]) == nrow(x)
)

sapply(
  bases_mice,
  function(x) anyDuplicated(x[["id"]])
)

# correspondencia com base original
sapply(
  bases_mice,
  function(x) {
    setequal(
      x[["id"]],
      base_original[["id"]]
    )
  }
)

# CIRAR BASES COMPLETAS
# CAMPOS DA BASE ORIGINAL
# CAMPOS IMPUTADOS

bases_completas <- lapply(
  bases_mice,
  function(imp) {
    
    dados <- copy(base_original)
    
    # adiciona colunas da base original às bases imputadas
    dados[
      imp,
      on = "id",
      (vars_imputadas) := mget(
        paste0("i.", vars_imputadas)
      )
    ]
    
    dados[]
  }
)

names(bases_completas) <- paste0("imp_", 1:10)

# completude
sapply(bases_completas, function(x){x[,sapply(.SD,meanfill)]})

saveRDS(bases_completas, file = "./data/imputados/completos/bases_completas.rds")

# juntar as bases
bases_todas <- c(
  list(original = base_original),
  bases_mice
)
names(bases_todas)


# Num de registros nas bases
sapply(
  bases_todas,
  nrow
)

# Num de colunes nas bases
sapply(
  bases_todas,
  ncol
)


# salvando arquivos completos
for (i in 1:10) {
  
  write_parquet(
    bases_completas[[i]],
    file.path(
      "./data/imputados/completos",
      paste0("base_mice_", i, ".parquet")
    )
  )
}

saveRDS(bases_completas,
        file = "./data/imputados/completos/bases_completas.rds")

saveRDS(bases_todas,
        file = "./data/dataset/bases_todas.rds")


############################################################################################################
############################################################################################################
############################################################################################################


####################################
######## DATASET ECONOMICO #########
############ CONTAGEM ##############
####################################
agressoes_subset[,coduf := as.integer(str_sub(codmun6, end = 2))]

agg_e_grid <- setDT(expand.grid(ano = c(2012:2026),
                          mes = c(1:12),
                          sexo = c("Homens", "Mulheres"),
                          i_e = c("14 a 17 anos", "18 a 24 anos", "25 a 39 anos", "40 a 59 anos", "60 anos ou mais"),
                          raca = c("Branca","Parda","Preta","Amarela","Indígena", "Ignorado"),
                          Grupo_desc = c("Agressões", "Lesões autoprovocadas intencionalmente", "Acidentes","Eventos (fatos) cuja intenção é indeterminada"),
                          uf = c( "AC", "AL", "AM", "AP", "BA",
                                  "CE", "DF", "ES", "GO", "MA",
                                  "MG", "MS", "MT", "PA", "PB",
                                  "PE", "PI", "PR", "RJ", "RN",
                                  "RO", "RR", "RS", "SC", "SE",
                                  "SP", "TO")))[order(uf,ano,mes,Grupo_desc,sexo,i_e),.(uf,ano,mes,Grupo_desc,sexo,raca,i_e)]

agg_e_i <- estados[,.(coduf,uf)][agressoes_subset[,.(id,i_e,coduf,ano,mes,Grupo_desc,raca,sexo)], on = "coduf"]
agg_est <- agg_e_i[,.N, by = .(Grupo_desc,sexo,i_e,raca,uf,ano,mes)][!is.na(i_e)][,.(uf,ano,mes,Grupo_desc,sexo,raca,i_e,N)]

setkey(agg_e_grid,uf,ano,mes,Grupo_desc,sexo,i_e)
setkey(agg_est,uf,ano,mes,Grupo_desc,sexo,i_e)  

agg_e <- agg_e_grid[agg_est, N := i.N, on = .(uf,ano,mes,Grupo_desc,sexo,i_e)]
agg_e[,.N]
# sem imputacao
# Desemprego
desemprego[1:5]
desemprego[estados, uf:=i.uf, on = "coduf"]
dt <- setDT(expand.grid(ano = c(2012:2026),
                        mes = c(1:12),
                        i_e = c("14 a 17 anos", "18 a 24 anos", "25 a 39 anos", "40 a 59 anos", "60 anos ou mais"),
                        uf = c( "AC", "AL", "AM", "AP", "BA",
                                "CE", "DF", "ES", "GO", "MA",
                                "MG", "MS", "MT", "PA", "PB",
                                "PE", "PI", "PR", "RJ", "RN",
                                "RO", "RR", "RS", "SC", "SE",
                                "SP", "TO")))[order(ano,mes)]

dt <- desemprego[dt, on = .(ano,mes,uf,i_e)
                 ][order(uf,i_e,ano,mes),":="(Desemprego = nafill(Desemprego, "locf"),
                         coduf = nafill(coduf, "locf")), by = .(uf, i_e, ano)]

dt[uf == "AC" & ano == 2012 & i_e =="14 a 17 anos"]

setkey(dt, ano, mes, uf, i_e)
setkey(agg_e,ano,mes,uf,i_e)

agg_e <- dt[,.(ano,mes,uf,i_e,Desemprego)][agg_e, on = .(uf, ano, mes, i_e)]


# Atencao Basica
ab[,.(max(as.IDate(paste0(ano,"-",mes,"-01"))))]
setkey(ab, ano, mes, uf)
agg_e <- ab[agg_e]


# GINI
head(gini)
setkey(gini, ano, uf)
agg_e <- gini[,.(uf,ano,gini)][agg_e, on = .(ano,uf)]

# IDH
head(idhuf)
setkey(idhuf,uf)
agg_e <- idhuf[,.(uf,idh = IDHM2010)][agg_e, on = .(uf)]

# POPULACAO
head(populacao)
pop_e <- populacao[,.(Populacao = sum(Populacao, na.rm = T)), by = .(uf, raca, sexo, i_e)]
setkey(pop_e, uf,sexo,raca,i_e)

agg_e <- pop_e[agg_e, on = .(uf, sexo, raca, i_e)]
agg_e[,N := fifelse(is.na(N),0,N)]

agg_e[,lapply(.SD,meanfill)]

# salva base original para analise socioeconomica
write_parquet(agg_e, 
              sink = "./data/dataset/agg_e.parquet")


###################################
### AGREGAR PARA IMPUTADOS ########
###################################

bases_completas_e <- lapply(bases_completas, function(x){
  a = x[Grupo_desc %in% c("Agressões", "Lesões autoprovocadas intencionalmente", "Acidentes","Eventos (fatos) cuja intenção é indeterminada")]
  
  b <- estados[,.(coduf,uf)][a[,.(id,i_e,coduf,ano,mes,Grupo_desc,raca,sexo)], on = "coduf"]
  c <- b[,.N, by = .(Grupo_desc,sexo,i_e,raca,uf,ano,mes)][!is.na(i_e)][,.(uf,ano,mes,Grupo_desc,sexo,raca,i_e,N)]
  
  setkey(agg_e_grid,uf,ano,mes,Grupo_desc,sexo,i_e)
  setkey(c,uf,ano,mes,Grupo_desc,sexo,i_e)  
  
  agg_ee <- agg_e_grid[c, N := i.N, on = .(uf,ano,mes,Grupo_desc,sexo,i_e)]
  
 
  # sem imputacao
  # Desemprego
  dt <- setDT(expand.grid(ano = c(2012:2026),
                          mes = c(1:12),
                          i_e = c("14 a 17 anos", "18 a 24 anos", "25 a 39 anos", "40 a 59 anos", "60 anos ou mais"),
                          uf = c( "AC", "AL", "AM", "AP", "BA",
                                  "CE", "DF", "ES", "GO", "MA",
                                  "MG", "MS", "MT", "PA", "PB",
                                  "PE", "PI", "PR", "RJ", "RN",
                                  "RO", "RR", "RS", "SC", "SE",
                                  "SP", "TO")))[order(ano,mes)]
  
  dt <- desemprego[dt, on = .(ano,mes,uf,i_e)
  ][,":="(Desemprego = nafill(Desemprego, "locf"),
          coduf = nafill(coduf, "locf")), by = .(uf, i_e, ano)]
  
  setkey(dt, ano, mes, uf, i_e)
  setkey(agg_ee,ano,mes,uf,i_e)
  
  agg_ee <- dt[,.(ano,mes,uf,i_e,Desemprego)][agg_ee, on = .(uf, ano, mes, i_e)]
  
  
  # Atencao Basica
  ab[,.(max(as.IDate(paste0(ano,"-",mes,"-01"))))]
  setkey(ab, ano, mes, uf)
  agg_ee <- ab[agg_ee]
  
  
  # GINI
  head(gini)
  setkey(gini, ano, uf)
  agg_ee <- gini[,.(uf,ano,gini)][agg_ee, on = .(ano,uf)]
  
  # IDH
  head(idhuf)
  setkey(idhuf,uf)
  agg_ee <- idhuf[,.(uf,idh = IDHM2010)][agg_ee, on = .(uf)]
  
  # POPULACAO
  head(populacao)
  pop_e <- populacao[,.(Populacao = sum(Populacao, na.rm = T)), by = .(uf, raca, sexo, i_e)]
  setkey(pop_e, uf,sexo,raca,i_e)
  
  agg_ee <- pop_e[agg_ee, on = .(uf, sexo, raca, i_e)]
  agg_ee <- agg_ee[ano > 2011 & ano < 2024]
  agg_ee[,N := fifelse(is.na(N),0,N)]
})

names(bases_completas_e)

# completude
sapply(bases_completas_e, function(x){x[,sapply(.SD,meanfill)]})

saveRDS(bases_completas_e,
              file = "./data/imputados/completos/bases_completas_e.rds")



####################################
## BASES ECONOMICAS PARA MODELAGEM #
####################################

agg_edt <- agg_e[ano > 2011 & ano < 2024]

bases_e <- c(
  list(original = agg_edt),
  bases_completas_e
) 

sapply(bases_e, function(x){
  x[,summary(ano)]
})

# seleciona apenas os CID de interesse
bases_e <- lapply(bases_e, function(x)
{
  a = x[Grupo_desc %in% c("Agressões", "Lesões autoprovocadas intencionalmente", "Acidentes","Eventos (fatos) cuja intenção é indeterminada")]
  return(a)
}
)

sapply(bases_e, function(x){
  data.table(ncol = ncol(x),
             nrow = nrow(x))
})

saveRDS(bases_e,
        file = "./data/dataset/bases_e.rds")

# salva as bases organizadas para modelagem posterior
 for (nome in names(bases_e)) {
  arrow::write_parquet(
    bases_e[[nome]],
    sink = paste0("./data/dataset/completos/", nome, "_e.parquet")
  )
}


############################################################################################################
############################################################################################################
############################################################################################################

####################################
######## DATASET DEMOGRAFICO #######
####################################

# original
agressoes_subset[,coduf := as.integer(str_sub(codmun6, end = 2))]


agg_d_grid <- setDT(expand.grid(ano = c(2010:2026),
                                mes = c(1:12),
                                sexo = c("Homens", "Mulheres"),
                                i_sd = c("infantil (10a-|18a)",
                                         "jovens (18a-|29a)",
                                         "adultos (30a|-59a)",
                                         "idosos (60a|-79a)",
                                         "muito idosos (80a-)"),
                                raca = c("Branca","Parda","Preta","Amarela","Indígena", "Ignorado"),
                                Grupo_desc = c("Agressões", "Lesões autoprovocadas intencionalmente", "Acidentes","Eventos (fatos) cuja intenção é indeterminada"),
                                uf = c( "AC", "AL", "AM", "AP", "BA",
                                        "CE", "DF", "ES", "GO", "MA",
                                        "MG", "MS", "MT", "PA", "PB",
                                        "PE", "PI", "PR", "RJ", "RN",
                                        "RO", "RR", "RS", "SC", "SE",
                                        "SP", "TO")))[order(uf,ano,mes,Grupo_desc,sexo,i_sd),.(uf,ano,mes,Grupo_desc,sexo,raca,i_sd)]



agg_d_i <- agressoes_subset[,.(coduf = as.integer(str_sub(codmun6, end=2)), Grupo_desc, i_sd, sexo, raca, estcivil, escolaridade, DIFDATA, agg_tipo, ano, mes)]
agg_d <- agg_d_i[estados, uf:=i.uf, on= "coduf"]


# Atencao Basica
setkey(agg_d, ano, mes, coduf)
setkey(ab, ano, mes, coduf)
agg_d <- ab[,.(coduf,ano,mes,cobab)][agg_d]


# GINI
setkey(gini, ano, coduf)
agg_d <- gini[,.(coduf,ano,gini)][agg_d, on = .(ano,coduf)]

# IDH
head(idhuf,3)
setkey(idhuf,coduf)
agg_d <- idhuf[,.(coduf,idh = IDHM2010)][agg_d, on = .(coduf)]

# POPULACAO
head(populacao,3)
populacao[,i_sd := fifelse(i_sd == "infantil (10a-|18a)", "adolescentes (10a-|18a)", as.character(i_sd))]

pop_sd <- populacao[,.(Populacao = sum(Populacao, na.rm = T)), by = .(uf, raca, sexo, i_sd)]
setkey(pop_sd, uf,sexo,raca,i_sd)
setkey(agg_d, uf,sexo,raca,i_sd)
agg_d <- pop_sd[agg_d, on = .(uf, sexo, raca, i_sd)]

agg_d <- agg_d[ano > 2011 & ano < 2024]
agg_d <- agg_d[Grupo_desc %in% c("Agressões", "Lesões autoprovocadas intencionalmente", "Acidentes","Eventos (fatos) cuja intenção é indeterminada")]


# salva base original para analise sociodemografica
write_parquet(agg_d, 
              sink = "./data/dataset/agg_d.parquet")



####################################
### AGREGAR PARA IMPUTADOS #########
####################################

bases_completas_d <- lapply(bases_completas, function(x){
  a = x[Grupo_desc %in% c("Agressões", "Lesões autoprovocadas intencionalmente", "Acidentes","Eventos (fatos) cuja intenção é indeterminada")]
  
  agg_dd <- estados[,.(coduf,uf)][a[,.(coduf, Grupo_desc, i_sd, sexo, raca, estcivil, escolaridade, DIFDATA, agg_tipo, ano, mes)], on = "coduf"]
  
  
  # Atencao Basica
  setkey(agg_dd, ano, mes, coduf)
  setkey(ab, ano, mes, coduf)
  agg_dd <- ab[,.(coduf,ano,mes,cobab)][agg_dd]
  
  
  # GINI
  setkey(gini, ano, coduf)
  agg_dd <- gini[,.(coduf,ano,gini)][agg_dd, on = .(ano,coduf)]
  
  # IDH
  head(idhuf,3)
  setkey(idhuf,coduf)
  agg_dd <- idhuf[,.(coduf,idh = IDHM2010)][agg_dd, on = .(coduf)]
  
  # POPULACAO
  head(populacao,3)
  populacao[,i_sd := fifelse(i_sd == "infantil (10a-|18a)", "adolescentes (10a-|18a)", as.character(i_sd))]
  
  pop_sd <- populacao[,.(Populacao = sum(Populacao, na.rm = T)), by = .(uf, raca, sexo, i_sd)]
  setkey(pop_sd, uf,sexo,raca,i_sd)
  setkey(agg_dd, uf,sexo,raca,i_sd)
  agg_dd <- pop_sd[agg_dd, on = .(uf, sexo, raca, i_sd)]
  agg_dd[,coduf :=NULL]
  agg_dd <- agg_dd[ano > 2011 & ano < 2024]
  
})

names(bases_completas_d)

# completude
sapply(bases_completas_d, function(x){x[,sapply(.SD,meanfill)]})


saveRDS(bases_completas_d,
        file = "./data/imputados/completos/bases_completas_d.rds")



#######################################
## BASES DEMOGRAFICAS PARA MODELAGEM ##
#######################################

agg_ddt <- agg_d

bases_d <- c(
  list(original = agg_ddt),
  bases_completas_d
) 

sapply(bases_d, function(x){
  x[,summary(ano)]
})

sapply(bases_d, function(x){
  data.table(ncol = ncol(x),
             nrow = nrow(x))
})

saveRDS(bases_d,
        file = "./data/dataset/bases_d.rds")


# salva as bases organizadas para modelagem posterior
for (nome in names(bases_d)) {
  arrow::write_parquet(
    bases_e[[nome]],
    sink = paste0("./data/dataset/completos/", nome, "_d.parquet")
  )
}


