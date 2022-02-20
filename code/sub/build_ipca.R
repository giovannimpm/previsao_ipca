# Build IPCA
# Giovanni Machado
# Prof Ricardo Dahis

# Nesse script pretendo baixar todas as 
# desagregacoes do ipca que serao previstas

# Lista de desagregacoes a serem baixadas (codigos do SGS entre parenteses)
#  - IPCA (433)
#  - alimentacao e bebidas  (1635)
#  - habitacao (1636)
#  - artigos de residencia  (1637)
#  - vestuario (1638)
#  - transporte (1639)
#  - comunicacao (1640)
#  - saude e cuidados pessoais (1641)
#  - despesas pessoais (1642)
#  - educacao (1643)

# limpar o environment
rm(list = ls())

# diretorios
out = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\data"

# Pacotes
library(jsonlite)
library(lubridate)
library(tidyverse)

# baixar as desagregacoes
# correspondencia entre desagregacoes e codigos

corresp = data.frame(
  
  nomes = c("ipca","alimentacao","habitacao","residencia",
          "vestuario","transporte","comunicacao","saude",
          "despesas_pessoais","educacao"),
  
  codigos = c(433,1635,1636,1637,1638,1639,1640,1641,1642,1643)
)

# API - BCB
# vamos construir os urls de cada uma dessas variaveis
# e adiciona-los ao data frame corresp

# data em que estamos
# mes = ifelse(month(Sys.Date())-1 == 0, 1, month(Sys.Date())-1)
# ano = year(Sys.Date()) - 1
# 
# data_final = paste0("01/",
#                     ifelse(mes< 10, paste0("0", mes), mes),
#                     "/",
#                     ifelse(ano < 10, paste0("0",ano), ano))

corresp = 
  corresp %>% 
  mutate(url = paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",codigos,"/dados?formato=json&dataInicial=01/01/2012&dataFinal=01/01/2022"))

# baixando as variaveis
for (i in 1:nrow(corresp)) {
  assign(corresp[i,1], as.numeric(fromJSON(corresp[i,3])$valor))
}

# agrupando-as
desagrega = data.frame(
  ipca, alimentacao, habitacao, residencia,
  vestuario, transporte, comunicacao, saude,
  despesas_pessoais, educacao
)

# adicionando as datas
desagrega =
  desagrega %>% 
  mutate(data = seq.Date(from = dmy("01012012"),
                         length.out = nrow(desagrega),
                         by = "month")) %>% 
  relocate(data)

# exportar csv
write.csv(desagrega, file.path(out,"desagregacoes_ipca.csv"), row.names = !T)
