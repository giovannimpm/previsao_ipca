# Build covariates
# Giovanni Machado
# Prof Ricardo Dahis

# Nesse script pretendo baixar todas as 
# covariadas que nos ajudarao a prever o ipca

# limpar o environment
rm(list = ls())

# diretorios
out = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\data"

# Pacotes
library(jsonlite)
library(lubridate)
library(tidyverse)

## data frame com preditores, seus codigos e transformacoes necessarias

nomes = c("ipca","igpm","ipca15","bm_broad","m1","icbr",
          "ibcbr","pimpf","tcu","aggreg_wage","elec",
          "brl_usd","selic","saving","cred","net_debt_gdp",
          "primary","current_accoutn","trade_balance","imports")

codigos = c(433,189,7478,1833,27788,27574,24363,21859,24352,22078,1406,
            3695,4390,1835,20539,4513,4649,22701,22704,22709)

# para tratar cada serie corretamente, faremos modificacoes 
# em cada uma delas para que elas se tornem estacionarias
# o vetor a seguir indica qual transformacao faremos, onde
# none == nenhuma
# per == variacao percentual
# dif == primeira diferenca

transformacoes = c("none","none","none","per","per","per","per","per",
                   "dif","per","per","per","none","per","per",
                   "dif","per","per","per","per")

# para reproduzir o trabalho do econometrista, vamos 
# usar apenas as informacoes que teriamos a disposicao
# para previsao do ipca em determinado mes, portanto,
# o vetor a seguir indica qual o tamanho da defasagem 
# de cada uma das series analisadas

atraso = c(1,1,1,2,2,1,3,2,1,2,3,1,1,2,2,2,2,2,2,2)

df = data.frame(nomes,codigos,transformacoes,atraso)

# API - BCB
## urls
df = df %>%
  mutate(url = paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",codigos,"/dados?formato=json&dataInicial=01/08/2011&dataFinal=01/01/2022"))

## baixando variaveis
for (i in 1:nrow(df)) {
  assign(df[i,1], as.numeric(fromJSON(df[i,5])$valor))
}


## transformacoes
### funcao transformacao
transform = function(serie, tipo){
  # vetor transformado
  transformado = c()
  
  # tipos de transformacao
  if (tipo == "dif"){
    for (i in 2:length(serie)){transformado[i] = serie[i] - serie[i-1]}
  } else if (tipo == "per"){
    for (i in 2:length(serie)){transformado[i] = ((serie[i] - serie[i-1])/serie[i-1])*100}
  } else{transformado = serie}
  
  return(transformado)
}

## transformacoes
for (i in 1:nrow(df)){
  assign(df[i,1], transform(get(df[i,1]),df[i,3]))
}

## corrigindo os atrasos
for (i in 1:nrow(df)){
  assign(df[i,1], get(df[i,1])[(6 - df[i,4]):(126 - df[i,4])])
}

# a linha 6 representa o mes 01/2012 e 
# a linha 126 representa o mes 01/2022

# data frame 
X = data.frame(ipca,igpm,ipca15,bm_broad,m1,icbr,
               ibcbr,pimpf,tcu,aggreg_wage,elec,
               brl_usd,selic,saving,cred,net_debt_gdp,primary,
               current_accoutn,trade_balance,imports)

# datas
X = X %>% 
  mutate(data = seq.Date(from = dmy("01012012"),
                         length.out = nrow(X),
                         by = "month")) %>% 
  relocate(data)

# exportar o csv
write.csv(X, file.path(out,"covariadas.csv"), row.names = !T)
