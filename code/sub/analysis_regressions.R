# analysis regression
# Giovanni Machado
# Prof Ricardo Dahis

# Nesse script pretendo rodar os modelos de ML

# limpar o environment
rm(list = ls())

# diretorios
out = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\data"
tab = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\tables"

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
residencia = read.csv(file.path(out,"residencia.csv"))
transporte = read.csv(file.path(out,"transporte.csv"))
vestuario = read.csv(file.path(out,"vestuario.csv"))
saude = read.csv(file.path(out,"saude.csv"))


############ Alimentacao ###############
colnames(alimentacao)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,3)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, alimentacao, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-alimentacao)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(alimentacao ~ `alimentacao..1.` + `alimentacao..2.`, data = data_ar2)
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

realizado = y[96:119]

alimentacao_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                  previsao_media_historica, previsao_rf)

alimentacao_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ Comunicacao ###############
colnames(comunicacao)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,8)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, comunicacao, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-comunicacao)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(comunicacao ~ `comunicacao..1.` + `comunicacao..2.`, data = data_ar2)
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

realizado = y[96:119]

comunicacao_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                              previsao_media_historica, previsao_rf)

comunicacao_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ Despesas Pessoais ###############
colnames(despesas_pessoais)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,10)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, despesas_pessoais, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-despesas_pessoais)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(despesas_pessoais ~ `despesas_pessoais..1.` + `despesas_pessoais..2.`, data = data_ar2)
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

realizado = y[96:119]

despesas_pessoais_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                              previsao_media_historica, previsao_rf)

despesas_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ Educacao ###############
colnames(educacao)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,11)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, educacao, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-educacao)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(educacao ~ `educacao..1.` + `educacao..2.`, data = data_ar2)
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

realizado = y[96:119]

educacao_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                                    previsao_media_historica, previsao_rf)

educacao_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ Habitacao ###############
colnames(habitacao)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,4)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, habitacao, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-habitacao)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(habitacao ~ `habitacao..1.` + `habitacao..2.`, data = data_ar2)
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

realizado = y[96:119]

habitacao_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                            
                           previsao_media_historica, previsao_rf)
habitacao_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ ipca ###############
colnames(ipca)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,2)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, ipca, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-ipca)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(ipca ~ `ipca..1.` + `ipca..2.`, data = data_ar2)
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

realizado = y[96:119]

ipca_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                            previsao_media_historica, previsao_rf)

ipca_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ residencia ###############
colnames(residencia)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,5)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, residencia, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-residencia)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(residencia ~ `residencia..1.` + `residencia..2.`, data = data_ar2)
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

realizado = y[96:119]

residencia_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                       previsao_media_historica, previsao_rf)

residencia_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ transporte ###############
colnames(transporte)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,7)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, transporte, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-transporte)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(transporte ~ `transporte..1.` + `transporte..2.`, data = data_ar2)
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

realizado = y[96:119]

transporte_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                             previsao_media_historica, previsao_rf)

transporte_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ vestuario ###############
colnames(vestuario)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,6)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, vestuario, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-vestuario)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(vestuario ~ `vestuario..1.` + `vestuario..2.`, data = data_ar2)
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

realizado = y[96:119]

vestuario_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                             previsao_media_historica, previsao_rf)

vestuario_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)


############ saude ###############
colnames(saude)[3] = "data"

# variável a ser prevista
y_real = desagrega[,c(1,9)]
y = y_real[3:121,2]

# data frame com variável de interesse e preditores
df = inner_join(y_real, saude, by = "data")

# excluindo a coluna de datas
df = df %>% select(-data)

# apenas as covariadas
X = df %>% select(-saude)

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
  fim = 94 + i
  
  ####### média histórica #######
  ## previsao
  previsao_media_historica[i] = mean(y[inicio:fim])
  
  ## erro
  erro_media_historica[i] = y[fim + 1] - previsao_media_historica[i]
  
  ############ AR(2) ############
  ## modelo
  data_ar2 = df[inicio:fim,]
  ar2 = lm(saude ~ `saude..1.` + `saude..2.`, data = data_ar2)
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

realizado = y[96:119]

saude_prev = data.frame(realizado, previsao_ar2, previsao_lasso,
                            previsao_media_historica, previsao_rf)

saude_erro = data.frame(erro_ar2,erro_lasso,
                              erro_media_historica,
                              erro_rf)

# escrevendo csv
# previsao
write.csv(alimentacao_prev, file.path(tab,"alimentacao_prev.csv"), row.names = F)
write.csv(comunicacao_prev, file.path(tab,"comunicacao_prev.csv"), row.names = F)
write.csv(despesas_pessoais_prev, file.path(tab,"despesas_pessoais_prev.csv"), row.names = F)
write.csv(educacao_prev, file.path(tab,"educacao_prev.csv"), row.names = F)
write.csv(habitacao_prev, file.path(tab,"habitacao_prev.csv"), row.names = F)
write.csv(ipca_prev, file.path(tab,"ipca_prev.csv"), row.names = F)
write.csv(residencia_prev, file.path(tab,"residencia_prev.csv"), row.names = F)
write.csv(saude_prev, file.path(tab,"saude_prev.csv"), row.names = F)
write.csv(transporte_prev, file.path(tab,"transporte_prev.csv"), row.names = F)
write.csv(vestuario_prev, file.path(tab,"vestuario_prev.csv"), row.names = F)

#erros
write.csv(alimentacao_erro, file.path(tab,"alimentacao_erro.csv"), row.names = F)
write.csv(comunicacao_erro, file.path(tab,"comunicacao_erro.csv"), row.names = F)
write.csv(despesas_erro, file.path(tab,"despesas_pessoais_erro.csv"), row.names = F)
write.csv(educacao_erro, file.path(tab,"educacao_erro.csv"), row.names = F)
write.csv(habitacao_erro, file.path(tab,"habitacao_erro.csv"), row.names = F)
write.csv(ipca_erro, file.path(tab,"ipca_erro.csv"), row.names = F)
write.csv(residencia_erro, file.path(tab,"residencia_erro.csv"), row.names = F)
write.csv(saude_erro, file.path(tab,"saude_erro.csv"), row.names = F)
write.csv(transporte_erro, file.path(tab,"transporte_erro.csv"), row.names = F)
write.csv(vestuario_erro, file.path(tab,"vestuario_erro.csv"), row.names = F)
