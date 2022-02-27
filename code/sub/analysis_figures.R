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





