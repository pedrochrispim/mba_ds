# Importacao de dados
library(data.table)
library(stringr)
source("./script/funcoes.R")
library(read.dbc)

# DADOS EXTERNOS ####

# Os dados brutos do DATASUS são muito volumosos para este repositório.
# Os arquivos originais *.dbc são obtidos de ftp://ftp.datasus.gov.br
# Foram utilizados os arquivos *.dbc de consolidacao anual da Declaração de Óbito, por local de residência
# Os arquivos têm o formato BRAAAA.dbc, sendo BR para Brasil, AAAA para o ano de consolidação dos dados.


# fucao ChatGPT get data from datasus
ftp_read_and_bind_rcurl <- function(
    base_url,
    subdir = "",
    user,
    password,
    pattern = NULL,
    subset_func = function(dt) dt,
    add_filename_column = TRUE
) {
  if (!requireNamespace("RCurl", quietly = TRUE)) stop("Instale o pacote 'RCurl'")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Instale o pacote 'data.table'")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Instale o pacote 'stringr'")
  if (!requireNamespace("read.dbc", quietly = TRUE)) stop("Instale o pacote 'read.dbc'")
  
  # Monta URL completa
  full_url <- paste0(base_url, subdir)
  
  # Lista arquivos no subdiretório autenticado
  res <- tryCatch({
    RCurl::getURL(
      full_url,
      userpwd = paste0(user, ":", password),
      dirlistonly = FALSE
    )
  }, error = function(e) {
    stop("Erro ao acessar FTP: ", e$message)
  })
  
  files <- strsplit(res, "\r*\n")[[1]]
  filesdbc <- str_sub(files,start = -12)[str_detect(files,"DOBR201|DOBR202")]
  filesurl <- paste0(full_url,filesdbc)
  
  # baixa os arquivos
  
  lapply(filesurl, function(d){
    
    a = str_sub(d,start = -12)
    
    download.file(d,
                  destfile = paste0("./data/raw/",a),
                  mode = "wb"
                  )
  })
  
# Junta os arquivos filtrados
data.raw.lista <- list.files(path = './data/raw',recursive = T,full.names = T, pattern = ".dbc")

obitos <- rbindlist(
  lapply(data.raw.lista,
         function(x){
           a = read.dbc(x) %>% setDT()
           return(a)
         }),
  fill = TRUE
)
}


# le e consolida os dados
obitos <- ftp_read_and_bind_rcurl(
  base_url = "ftp://ftp.datasus.gov.br/",
  subdir = "dissemin/publicos/sim/cid10/dores/",
  user = "anonymous",
  password = "",
  pattern = NULL
)

# fwrite(obitos, 
       # file = "./data/raw/dores.csv")

write_parquet(obitos,
              sink = "./data/raw/obitos.parquet")

# rm(list = ls())  # Utilize essa linha se o consumo de RAM for crítico para sua máquina





