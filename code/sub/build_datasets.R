# Build datasets
# Giovanni Machado
# Prof Ricardo Dahis

# Nesse script pretendo organizar os datasets 
# baixados para rodar os modelos

# limpar o environment
rm(list = ls())

# diretorios
out = "C:\\Users\\Giovanni Machado\\previsao_ipca\\output\\data"

# Pacotes
library(tidyverse)

# chamando os data sets
desagrega = read.csv(file.path(out,"desagregacoes_ipca.csv"))
covariadas = read.csv(file.path(out,"covariadas.csv"))

# Covariadas
# adicionando defasagens
names = names(covariadas)
names = paste0(names,"(-",rep(1:2, each = ncol(covariadas)),")")
covariadas_defasadas = as.data.frame(embed(as.matrix(covariadas),2))
colnames(covariadas_defasadas) = names

covariadas_defasadas =
  covariadas_defasadas %>% select(-`data(-2)`)

# Desagregacoes 
# adicionando defasagens
names = names(desagrega)
names = paste0(names,"(-",rep(1:2, each = ncol(desagrega)),")")
desagrega_defasadas = as.data.frame(embed(as.matrix(desagrega),2))
colnames(desagrega_defasadas) = names

desagrega_defasadas =
  desagrega_defasadas %>% select(-`data(-2)`)

# datasets por desagregacao
desagregacoes_nomes = names(desagrega)

# covariadas sem data
X = covariadas_defasadas %>% 
  select(-`data(-1)`)


# construindo os datasets
for (i in 2:11){
  assign(desagregacoes_nomes[i],
         cbind(desagrega_defasadas[,i],
               desagrega_defasadas[,i+10],
               covariadas_defasadas))
}

# consertando os nomes
nomes = names(alimentacao) #apenas um exemplo

names(alimentacao) = c("alimentacao(-1)","alimentacao(-2)",
                       nomes[3:43])

names(comunicacao) = c("comunicacao(-1)","comunicacao(-2)",
                       nomes[3:43])

names(despesas_pessoais) = c("despesas_pessoais(-1)","despesas_pessoais(-2)",
                             nomes[3:43])

names(educacao) = c("educacao(-1)","educacao(-2)",
                    nomes[3:43])

names(habitacao) = c("habitacao(-1)","habitacao(-2)",
                     nomes[3:43])

# ipca tem uma coluna a mais
ipca = ipca[,-4]

names(ipca) = c("ipca(-1)","ipca(-2)",
                nomes[c(3, 5:43)])

names(residencia) = c("residencia(-1)","residencia(-2)",
                      nomes[3:43])

names(saude) = c("saude(-1)","saude(-2)",
                 nomes[3:43])

names(transporte) = c("transporte(-1)","transporte(-2)",
                      nomes[3:43])

names(vestuario) = c("vestuario(-1)","vestuario(-2)",
                     nomes[3:43])

# escrevendo os csvs
write.csv(alimentacao, file.path(out,"alimentacao.csv"), row.names = F)
write.csv(comunicacao, file.path(out,"comunicacao.csv"), row.names = F)
write.csv(despesas_pessoais, file.path(out,"despesas_pessoais.csv"), row.names = F)
write.csv(educacao, file.path(out,"educacao.csv"), row.names = F)
write.csv(habitacao, file.path(out,"habitacao.csv"), row.names = F)
write.csv(ipca, file.path(out,"ipca.csv"), row.names = F)
write.csv(residencia, file.path(out,"residencia.csv"), row.names = F)
write.csv(saude, file.path(out,"saude.csv"), row.names = F)
write.csv(transporte, file.path(out,"transporte.csv"), row.names = F)
write.csv(vestuario, file.path(out,"vestuario.csv"), row.names = F)
