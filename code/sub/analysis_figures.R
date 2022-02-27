# analysis figures
# Giovanni Machado
# Prof Ricardo Dahis

# Nesse script pretendo gerar os gráficos que serão usados no relatório

# limpar o environment
rm(list = ls())

# diretorios
tab = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\tables"
graf = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\figures"

# Pacotes
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggsci)

# tema para os gráficos
tema = theme(panel.background = element_rect(fill = "white"),
             panel.grid = element_line(color = "grey95"),
             legend.key = element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 9),
)

# Importação de dados
alimentacao = read.csv(file.path(tab,"alimentacao_prev.csv"))
comunicacao = read.csv(file.path(tab,"comunicacao_prev.csv"))
despesas_pessoais = read.csv(file.path(tab,"despesas_pessoais_prev.csv"))
educacao = read.csv(file.path(tab,"educacao_prev.csv"))
habitacao = read.csv(file.path(tab,"habitacao_prev.csv"))
ipca = read.csv(file.path(tab,"ipca_prev.csv"))
residencia = read.csv(file.path(tab,"residencia_prev.csv"))
transporte = read.csv(file.path(tab,"transporte_prev.csv"))
vestuario = read.csv(file.path(tab,"vestuario_prev.csv"))
saude = read.csv(file.path(tab,"saude_prev.csv"))


# Gráficos
# Alimentação
# organizar data frame - datas + pivot_longer

alimentacao = 
  alimentacao %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

alimentacao_graf = 
  pivot_longer(alimentacao, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(alimentacao_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Alimentação e Bebidas",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3)) 

ggsave("alimentacao.pdf", path = graf, plot = last_plot())


# Comunicacao
# organizar data frame - datas + pivot_longer

comunicacao = 
  comunicacao %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

comunicacao_graf = 
  pivot_longer(comunicacao, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(comunicacao_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Comunicação",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3)) 

ggsave("comunicacao.pdf", path = graf, plot = last_plot())

# Despesas Pessoais
# organizar data frame - datas + pivot_longer

despesas_pessoais = 
  despesas_pessoais %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

despesas_pessoais_graf = 
  pivot_longer(despesas_pessoais, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(despesas_pessoais_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Despesas Pessoais",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3)) 

ggsave("despesas.pdf", path = graf, plot = last_plot())

# Educacao
# organizar data frame - datas + pivot_longer

educacao = 
  educacao %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

educacao_graf = 
  pivot_longer(educacao, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(educacao_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Educação",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3)) 

ggsave("educacao.pdf", path = graf, plot = last_plot())

# IPCA
# organizar data frame - datas + pivot_longer

ipca = 
  ipca %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

ipca_graf = 
  pivot_longer(ipca, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(ipca_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "Índice de Preços ao Consumidor Amplo",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3))

ggsave("ipca.pdf", path = graf, plot = last_plot())

# Habitacao
# organizar data frame - datas + pivot_longer

habitacao = 
  habitacao %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

habitacao_graf = 
  pivot_longer(habitacao, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(habitacao_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Habitação",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3))

ggsave("habitacao.pdf", path = graf, plot = last_plot())

# Residencia
# organizar data frame - datas + pivot_longer

residencia = 
  residencia %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

residencia_graf = 
  pivot_longer(residencia, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(residencia_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Artigos de Residência",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3))

ggsave("residencia.pdf", path = graf, plot = last_plot())

# Saude
# organizar data frame - datas + pivot_longer

saude = 
  saude %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

saude_graf = 
  pivot_longer(saude, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(saude_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Saúde e Cuidados Pessoais",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3))

ggsave("saude.pdf", path = graf, plot = last_plot())

# Transporte
# organizar data frame - datas + pivot_longer

transporte = 
  transporte %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

transporte_graf = 
  pivot_longer(transporte, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(transporte_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Transporte",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3))

ggsave("transporte.pdf", path = graf, plot = last_plot())

# Vestuario
# organizar data frame - datas + pivot_longer

vestuario = 
  vestuario %>% 
  mutate(data = seq.Date(from = dmy("01022020"),
                         length.out = 24,
                         by = "month"))

vestuario_graf = 
  pivot_longer(vestuario, cols = 1:5,
               names_to = "Modelo",
               values_to = "Valores") %>% 
  mutate(Modelo = case_when(Modelo == "previsao_ar2" ~ "AR(2)",
                            Modelo == "previsao_lasso" ~ "Lasso",
                            Modelo == "previsao_media_historica" ~ "Média Histórica",
                            Modelo == "previsao_rf" ~ "Random Forest",
                            Modelo == "realizado" ~ "Realizado"))


# gráfico
ggplot(vestuario_graf, aes(x = data, y = Valores))+
  geom_line(aes(color = Modelo)) + tema +
  labs(title = "IPCA - Vestuário",
       subtitle = "Comparação das previsões out of sample",
       x = NULL, y = "%") +
  scale_color_igv(guide = guide_legend(ncol = 3))

ggsave("vestuario.pdf", path = graf, plot = last_plot())


# Erros

# importacao
alimentacao_erro = read.csv(file.path(tab,"alimentacao_erro.csv"))
comunicacao_erro = read.csv(file.path(tab,"comunicacao_erro.csv"))
despesas_erro = read.csv(file.path(tab,"despesas_pessoais_erro.csv"))
educacao_erro = read.csv(file.path(tab,"educacao_erro.csv"))
habitacao_erro = read.csv(file.path(tab,"habitacao_erro.csv"))
ipca_erro = read.csv(file.path(tab,"ipca_erro.csv"))
residencia_erro = read.csv(file.path(tab,"residencia_erro.csv"))
saude_erro = read.csv(file.path(tab,"saude_erro.csv"))
transporte_erro = read.csv(file.path(tab,"transporte_erro.csv"))
vestuario_erro = read.csv(file.path(tab,"vestuario_erro.csv"))

# Alimentacao
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(alimentacao_erro$erro_ar2^2)/sum(alimentacao_erro$erro_media_historica^2)
R2_lasso = 1 - sum(alimentacao_erro$erro_lasso^2)/sum(alimentacao_erro$erro_media_historica^2)
R2_rf = 1 - sum(alimentacao_erro$erro_rf^2)/sum(alimentacao_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Alimentação e Bebida",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("alimentacao_erro.pdf", path = graf, plot = last_plot())

# Comunicacao
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(comunicacao_erro$erro_ar2^2)/sum(comunicacao_erro$erro_media_historica^2)
R2_lasso = 1 - sum(comunicacao_erro$erro_lasso^2)/sum(comunicacao_erro$erro_media_historica^2)
R2_rf = 1 - sum(comunicacao_erro$erro_rf^2)/sum(comunicacao_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Comunicação",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("comunicacao_erro.pdf", path = graf, plot = last_plot())

# Despesas Pessoais
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(despesas_erro$erro_ar2^2)/sum(despesas_erro$erro_media_historica^2)
R2_lasso = 1 - sum(despesas_erro$erro_lasso^2)/sum(despesas_erro$erro_media_historica^2)
R2_rf = 1 - sum(despesas_erro$erro_rf^2)/sum(despesas_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Despesas Pessoais",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("despesas_pessoais_erro.pdf", path = graf, plot = last_plot())

# Educação
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(educacao_erro$erro_ar2^2)/sum(educacao_erro$erro_media_historica^2)
R2_lasso = 1 - sum(educacao_erro$erro_lasso^2)/sum(educacao_erro$erro_media_historica^2)
R2_rf = 1 - sum(educacao_erro$erro_rf^2)/sum(educacao_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Educação",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("educacao_erro.pdf", path = graf, plot = last_plot())

# Habitacao
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(habitacao_erro$erro_ar2^2)/sum(habitacao_erro$erro_media_historica^2)
R2_lasso = 1 - sum(habitacao_erro$erro_lasso^2)/sum(habitacao_erro$erro_media_historica^2)
R2_rf = 1 - sum(habitacao_erro$erro_rf^2)/sum(habitacao_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Habitacão",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("habitacao_erro.pdf", path = graf, plot = last_plot())

# ipca
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(ipca_erro$erro_ar2^2)/sum(ipca_erro$erro_media_historica^2)
R2_lasso = 1 - sum(ipca_erro$erro_lasso^2)/sum(ipca_erro$erro_media_historica^2)
R2_rf = 1 - sum(ipca_erro$erro_rf^2)/sum(ipca_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "Índice de Preço ao Consumidor Amplo",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("ipca_erro.pdf", path = graf, plot = last_plot())

# residencia
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(residencia_erro$erro_ar2^2)/sum(residencia_erro$erro_media_historica^2)
R2_lasso = 1 - sum(residencia_erro$erro_lasso^2)/sum(residencia_erro$erro_media_historica^2)
R2_rf = 1 - sum(residencia_erro$erro_rf^2)/sum(residencia_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Artigos de Residência",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("residencia_erro.pdf", path = graf, plot = last_plot())

# saude
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(saude_erro$erro_ar2^2)/sum(saude_erro$erro_media_historica^2)
R2_lasso = 1 - sum(saude_erro$erro_lasso^2)/sum(saude_erro$erro_media_historica^2)
R2_rf = 1 - sum(saude_erro$erro_rf^2)/sum(saude_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Saúde e Cuidados Pessoais",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("saude_erro.pdf", path = graf, plot = last_plot())

# transporte
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(transporte_erro$erro_ar2^2)/sum(transporte_erro$erro_media_historica^2)
R2_lasso = 1 - sum(transporte_erro$erro_lasso^2)/sum(transporte_erro$erro_media_historica^2)
R2_rf = 1 - sum(transporte_erro$erro_rf^2)/sum(transporte_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Transporte",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("transporte_erro.pdf", path = graf, plot = last_plot())

# vesturio
# vamos calcular o R^2 fora da amostra para cada modelo
R2_ar2 = 1 - sum(vestuario_erro$erro_ar2^2)/sum(vestuario_erro$erro_media_historica^2)
R2_lasso = 1 - sum(vestuario_erro$erro_lasso^2)/sum(vestuario_erro$erro_media_historica^2)
R2_rf = 1 - sum(vestuario_erro$erro_rf^2)/sum(vestuario_erro$erro_media_historica^2)

alimentacao_r2 = data.frame(R2_ar2,R2_lasso,R2_rf) %>% 
  pivot_longer(cols = 1:3,
               names_to = "Modelos",
               values_to = "Valores") %>% 
  mutate(Modelos = case_when(Modelos == "R2_ar2" ~ "AR(2)",
                             Modelos == "R2_lasso" ~ "LASSO",
                             Modelos == "R2_rf" ~ "Random Forest"))

# Gráficos
ggplot(alimentacao_r2, aes(x = Modelos, y = Valores))+
  geom_col() + tema +
  labs(title = "IPCA - Vestuário",
       subtitle = "R-squared out of sample",
       y = NULL)

ggsave("vestuario_erro.pdf", path = graf, plot = last_plot())

