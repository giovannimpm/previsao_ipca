# analysis regression
# Giovanni Machado
# Prof Ricardo Dahis

# Nesse script pretendo rodar os modelos de ML

# limpar o environment
rm(list = ls())

# diretorios
out = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\data"

# Pacotes
library(tidyverse)
library(HDeconometrics)
library(randomForest)

# importando os data sets
alimentacao = read.csv(file.path(out,"alimentacao.csv"))
comunicacao = read.csv(file.path(out,"comunicacao.csv"))
covariadas = read.csv(file.path(out,"covariadas.csv"))
desagrega = read.csv(file.path(out,"desagregacoes_ipca.csv"))
despesas_pessoais = read.csv(file.path(out,"despesas_pessoais.csv"))
educacao = read.csv(file.path(out,"educacao.csv"))
habitacao = read.csv(file.path(out,"habitacao.csv"))
ipca = read.csv(file.path(out,"ipca.csv"))
residencia = read.csv(file.path(out,"saude.csv"))
transporte = read.csv(file.path(out,"transporte.csv"))
vestuario = read.csv(file.path(out,"vestuario.csv"))


############ Alimentacao ###############
# variável a ser prevista
y_real = desagrega[,c(1,3)]

# data frame com variável de interesse e preditores
df = 

# objetos necessários para o loop
previsao_media_historica = rep(NA, 24)
previsao_ar2 = rep(NA, 24)
previsao_lasso = rep(NA, 24)
previsao_rf = rep(NA, 24)
erro_media_historica = rep(NA, 24)
erro_ar2 = rep(NA, 24)
erro_lasso = rep(NA, 24)
erro_rf = rep(NA, 24)

# loop
for (i in 1:24){
  ########### indices ###########
  inicio = i
  fim = 87 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(y ~ `semi(-1)` + `semi(-2)`, data = data_ar2)
  X_oos = X[fim+1,]
  
  ## previsao
  p = predict(ar2,as.vector(X_oos))
  previsao_ar2[i] = p[1]
  
  ## erro
  erro_ar2[i] = y[fim + 1] - previsao_ar2[i]
  
  ############ LASSO ############
  ## modelo
  y_lasso = y[inicio:fim]
  X_lasso = X[inicio:fim,]
  lasso = ic.glmnet(X_lasso,y_lasso, crit = "bic", alpha = 1)
  X_oos = X[fim+1,]
  
  ## previsao
  p = predict(lasso, as.vector(X_oos))
  previsao_lasso[i] = p[1]
  
  ## erro
  erro_lasso[i] = y[fim+1] - previsao_lasso[i]
  
  ######## random forest ########
  ## modelo
  y_rf = y[inicio:fim]
  X_rf = X[inicio:fim,]
  rf = randomForest(X_rf,y_rf)
  X_oos = X[fim+1,]
  
  ## previsao
  p = predict(rf, as.vector(X_oos))
  previsao_rf[i] = p[1]
  
  ## erro
  erro_rf[i] = y[fim+1] - previsao_rf[i]
}








