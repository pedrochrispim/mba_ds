# Carrega pacotes ####
library(data.table)
library(lubridate)
library(ggplot2)
library(stringr)
library(mice)
library(rstatix)
# library(DescTools)
library(arrow)
source("./script/funcoes.R")

agressoes_base <- readRDS("./data/raw/agressoes_base.rds")

################################################################
# Imputa dados para variaveis que serão utilizadas no modelo ###
################################################################

# CONFIRMA TIPO DE DADOS
# SELECIONA VARIAVEIS
agressoes_subset <- agressoes_base[,.(id = .I,
                                      Grupo = as.factor(Grupo),
                                      obito_home = as.logical(obito_home),
                                      codmun6 = as.factor(codmun6),
                                      sexo = as.factor(sexo),
                                      Idade = as.numeric(Idade),
                                      raca = as.factor(raca),
                                      estcivil = as.factor(estcivil),
                                      escolaridade = as.factor(escolaridade),
                                      local = as.factor(local),
                                      dt_obito = as.IDate(dt_obito),
                                      DIFDATA = as.integer(DIFDATA),
                                      Grupo = Grupo,
                                      Grupo_desc = Grupo_desc,
                                      agg_tipo = agg_tipo)
                                   ][,":="(ano = year(dt_obito),
                                           mes = month(dt_obito),
                                           trimestre = quarter(dt_obito))]

setkey(agressoes_subset,id)

preenchimento <- data.table(
  campo = names(agressoes_subset),
  Tipo = agressoes_subset[,unlist(lapply(.SD, \(x) class(x)[1]))],
  Preench = agressoes_subset[,unlist(lapply(.SD, meanfill))]
)[order(-Preench)]

preenchimento
fatores <- preenchimento[Tipo=="factor",campo]


cramer_matrix <- function(dt, vars) {
  
  mat <- matrix(
    NA_real_,
    length(vars),
    length(vars),
    dimnames = list(vars, vars)
  )
  
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      
      tab <- table(dt[[vars[i]]], dt[[vars[j]]])
      mat[i, j] <- DescTools::CramerV(tab)
    }
  }
  
  mat
}

mat_associacao <- cramer_matrix(
  agressoes_subset,
  fatores
)

# transformar matriz em formato longo
mat_long <- as.data.table(as.table(mat_associacao))

setnames(
  mat_long,
  c("V1", "V2", "N"),
  c("Variavel1", "Variavel2", "Associacao")
)

# heatmap
ggplot(
  mat_long,
  aes(
    x = Variavel1,
    y = Variavel2,
    fill = Associacao
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.2f", Associacao)),
    size = 4
  ) +
  scale_fill_gradient(
    low = "white",
    high = "steelblue",
    limits = c(0, 1)
  ) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL,
    fill = "Associação"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )


matriz_fatores <- round(mat_associacao, 3)

# SELEACAO DE MAIORES ASSOCIACOES
big_assoc <- mat_long[order(-Associacao)
         ][Variavel1 != Variavel2
           ][!((Variavel1 == "codmunocor6" & Variavel2 == "codmun6") | (Variavel1 == "codmun6" & Variavel2 == "codmunocor6")) 
             ][1:50]

# principais associações
big_assoc[order(Variavel1,-Associacao)]




# IMPUTACAO DOS DADOS
agg_mice <- agressoes_subset[,.(id,
                              Grupo,
                              obito_home,
                              sexo,
                              Idade,
                              local,
                              DIFDATA,
                              ano_q = quarter(dt_obito,with_year = TRUE),
                              raca,
                              estcivil,
                              escolaridade)]


meth <- make.method(agg_mice)
pred <- make.predictorMatrix(agg_mice)

# pred <- make.predictorMatrix(agg_mice) # considerar em datasets menores
pred[,] <- 0

pred["sexo", c(
  "Grupo", "estcivil", "local"
)] <- 1

pred["DIFDATA", c(
  "Grupo", "raca", "sexo", "local","escolaridade"
)] <- 1

pred["raca", c(
  "Grupo", "sexo", "codmun6", "DIFDATA"
)] <- 1

pred["estcivil", c(
  "Grupo", "sexo", "codmun6"
)] <- 1

pred["escolaridade", c(
  "Grupo", "sexo", "DIFDATA",  "Idade", "local"
)] <- 1



# variaveis completas
meth["id"]           <- ""
meth["Grupo"]        <- ""
meth["obito_home"]   <- ""
meth["local"]        <- ""
meth["Idade"]        <- ""
meth["ano_q"]        <- ""

# variaveis incompletas
meth["sexo"]         <- "logreg"
meth["raca"]         <- "polyreg"
meth["estcivil"]     <- "polyreg"
meth["escolaridade"] <- "polyreg"
meth["DIFDATA"]      <- "pmm"


# não incluir na matriz
pred["id", ] <- 0
pred[, "id"] <- 0

# utiliza MICE para imputar dados
agg_imp <- mice(
  agg_mice,
  method = meth,
  predictorMatrix = pred,
  m = 10,
  maxit = 20,
  seed = 123
)

# criando objeto lista com 10 datasets de dados imputados
agg_mice_imp <- lapply(1:10, function(i) {
  complete(agg_imp, i)
})



# acrescentando grupo de idade para futuras analises

agressoes_subset[,":="(
  # faixa etaria para emprego
  i_e = factor(fcase(Idade >=14 & Idade < 18, "14 a 17 anos",
                     Idade >=18 & Idade < 25, "18 a 24 anos",
                     Idade >=25 & Idade < 40, "25 a 39 anos",
                     Idade >=40 & Idade < 60, "40 a 59 anos",
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
                           "muito idosos (80a-)"))
)]


# salvar objetos de imputacao
save(agressoes_subset,
     agg_mice,
     agg_imp,
     agg_mice_imp,
     file = "./data/imputados/agressoes_imp.rda")

write_parquet(agressoes_subset,
              sink = "./data/raw/agressoes_subset.parquet")


# salvando cada dataset imputado em parquet

for (i in 1:10) {
  write_parquet(
    agg_mice_imp[[i]],
    paste0("data/imputados/imp_", i, ".parquet")
  )
}
