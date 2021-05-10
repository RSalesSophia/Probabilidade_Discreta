library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(extrafont)
library(ggthemes)

#Configuracao 
options(scipen=999)

windowsFonts(Times=windowsFont("TT Times New Roman"))


#Extraindo a base de dados: 
base <- readRDS("ex_1_4_vad_maternidade.rds")

#Interessa-se saber a probabilidade de nascimento de crianças por dia de semana, 
#estratificando por tipo de parto.
#Para isso, e necessario tratar a coluna DTNASC para descobrir qual dia da semana
#esta associado aquela data. 

#Conforme e observado falta-se um 0 para esquerda para todas as datas. Portanto:
base$DTNASC <- str_pad(base$DTNASC, 8 , pad = "0")

#Transformando a coluna em data
base$DTNASC <- dmy(base$DTNASC)

#Associando o dia da semana com a data
base$dia_semana <- format.Date(as.Date(base$DTNASC), "%A") 

#Primeiramente atribuindo uma coluna com o numero total de nascimento para calcular as probabilidades

base %<>%
  mutate(total = n()) 

#Calculando a probabilidade de nascer por parto normal ou cesário
base %<>%
  group_by(PARTO) %>%
  mutate(total_tipo = n(),
         prob_parto = (total_tipo/total)) %>%
  ungroup()

#Calculando a probabilidade de nascer em determinado dia da semana
base %<>% 
  group_by(dia_semana) %>%
  mutate(total_dia = n(),
         prob_dia = (total_dia/total)) %>%
  ungroup()


#Diante a independencia desses eventos, calcula-se a probabilidade conjunta
#Exemplo: nascer de parto normal e na sexta - RESULTADO: 8,4376%
base %<>% 
  mutate(prob_DiaeParto = prob_dia*prob_parto)

#Conferindo os resultados 
teste <- base %>%
  group_by(PARTO, dia_semana) %>%
  summarise(media = mean(prob_DiaeParto))

#A soma de todos os caminhos de probabilidades tem que ser igual a 1
sum(teste$media)

#Dividindo os dias em turnos: dia e noite 

#Primeiramete transformando a variavel HORANASC em número

base$HORANASC <-  as.numeric(base$HORANASC)

base %<>% mutate(turno = if_else(base$HORANASC >= 0600 & base$HORANASC <= 1759, "Dia", "Noite"))

#Calculando a probabilidade de nascer em determinado turno
base %<>% 
  group_by(turno) %>%
  mutate(total_turno = n(),
         prob_turno = (total_turno/total)) %>%
  ungroup()

#Diante a independencia desses eventos, calcula-se a probabilidade conjunta do parto e turno.
#Exemplo: probabilidade de nascer por parto normal e no turno Noite. RESULTADO: 23,51% 

base %<>% 
  mutate(prob_ParteTurno = prob_parto*prob_turno)

#Conferindo os resultados 
teste <- base %>%
  group_by(turno, PARTO) %>%
  summarise(media = mean(prob_ParteTurno))
#A soma desses caminhos de probabilidade tem que ser igual a 1 
sum(teste$media)

#Para descobrir qual a probabilidade de ocorrência de x nascimentos por dia, necessita utilizar o conceito de um modelo de distribuição de probabilidade discreta.
#Atentando que ao tamanho da base de dados, a distribuição mais adequada é a distribuição de Poisson. 

#Para descobrir a taxa media de ocorrencia do evento em uma unidade medida,necessita-se descobrir a médio de nascimento a depender do tipo de parto e turno. 
#Observacao: o ano de 2016 teve 366 dias 

#Com o codigo abaixo, e respondido a pergunta:
#Por dia, quantos nascimentos ocorrem em media no turno da manha por parto cesario?

base %>%
  group_by(turno, PARTO) %>% 
  summarise(media = n()/366)

#RESPOSTA: em media no turno da manhã nascem 2,43 criancas por parto cesario. 


#Obtendo essas informações das medias e atribuindo a variavel aleatoria discreta como
#o numero de ocorrencia de x nascimentos/dia, podemos calcular a probabilidade de ter zero, um, dois e assim por diante nascimentos por dia . 

#Abaixo segue o cálculo da  distribuicao de Poisson, considerando que o valor medio do turno do dia por parto cesario e igual 2.43:

poisson = function(x, lambda){
  p = exp(-lambda)*lambda^x/factorial(x)
  return(p)
}

x = 0:5 
poisson(x, lambda = 2.43)


#Considerando o valor medio do turno do dia para parto normal (2.40)
poisson = function(x, lambda){
  p = exp(-lambda)*lambda^x/factorial(x)
  return(p)
}

x = 0:5 
poisson(x, lambda = 2.4)

#Mostrando graficamente a distribuicao de probabilidade tendo em vista até 5 nascimentos por tipo de parto no turno Dia 

#Construbindo uma tabela com os dados das probabilidades de parto cesareo
x <- c(0,1,2,3,4,5)
prob <- c(0.0880, 0.2139, 0.2599, 0.2105, 0.1279, 0.0621)
y <- c("Cesáreo")

tabela_1 <- cbind(x, prob, y)
tabela_1 <- as_tibble(tabela_1)

tabela_1$prob <- as.numeric(tabela_1$prob)

tabela_1 %<>% mutate(prob = prob*100,
                     porc = paste0(prob, "%"))

#Construbindo uma tabela com os dados das probabilidades de parto normal

prob <- c(0.0907, 0.2177, 0.2612, 0.2090, 0.1255, 0.0601)
y <- c("Normal")


tabela_2 <- cbind(x, prob, y)
tabela_2 <- as_tibble(tabela_2)

tabela_2$prob <- as.numeric(tabela_2$prob)

tabela_2 %<>% mutate(prob = prob*100,
                     porc = paste0(prob, "%"))

tabela_uniao <- rbind(tabela_1, tabela_2)


#Transformando o tipo de algumas variaveis 
tabela_uniao$x <- as.numeric(tabela_uniao$x)
tabela_uniao$y <- as.factor(tabela_uniao$y)

tabela_uniao$porc<- gsub("\\.", ",", tabela_uniao$porc)

#Grafico: 

png("Graf_1.png", width = 7, height = 4, units = 'in', res = 300)

ggplot(tabela_uniao, aes(x=x, y=prob)) + 
  geom_bar(stat = "identity", width=0.9, fill = "#2d6d66") +
  facet_wrap( ~y ) +
  labs(x = "Nascimentos/Dia", y = "Probabilidade (%)") + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  geom_text(aes( label = porc),
            vjust = -0.4, size = 2.8, color = "black") +
  theme_bw(base_family = "Times New Roman", base_size = 12) + 
  theme( axis.text.x = element_text(  hjust = 1 )) 

dev.off()  

####################################################################
#Agora considerando o turno da noite para parto cesareo (1.66)
poisson = function(x, lambda){
  p = exp(-lambda)*lambda^x/factorial(x)
  return(p)
}

x = 0:5 
poisson(x, lambda = 1.66)


#Considerando o valor medio do turno da noite para parto normal (2.20)
poisson = function(x, lambda){
  p = exp(-lambda)*lambda^x/factorial(x)
  return(p)
}

x = 0:5 
poisson(x, lambda = 2.20)

#Mostrando graficamente a distribuicao de probabilidade tendo em vista ate 5 nascimentos por tipo de parto no turno Noite 

#Construbindo uma tabela com os dados das probabilidades de parto cesareo
x <- c(0,1,2,3,4,5)
prob <- c(0.190, 0.3156, 0.2619, 0.1449, 0.060, 0.0199)
y <- c("Cesário")


tabela_1 <- cbind(x, prob, y)
tabela_1 <- as_tibble(tabela_1)
tabela_1$prob <- as.numeric(tabela_1$prob)

tabela_1 %<>% mutate(prob = prob*100,
                     porc = paste0(prob, "%"))


#Construbindo uma tabela com os dados das probabilidades de parto normal

prob <- c(0.110, 0.2437, 0.2681, 0.1966, 0.1081, 0.0475)
y <- c("Normal")

tabela_2 <- cbind(x, prob, y)
tabela_2 <- as_tibble(tabela_2)

tabela_2$prob <- as.numeric(tabela_2$prob)

tabela_2 %<>% mutate(prob = prob*100,
                     porc = paste0(prob, "%"))

tabela_uniao <- rbind(tabela_1, tabela_2)

#Transformando o tipo de algumas variaveis 
tabela_uniao$x <- as.numeric(tabela_uniao$x)
tabela_uniao$y <- as.factor(tabela_uniao$y)

tabela_uniao$porc<- gsub("\\.", ",", tabela_uniao$porc)


#Gráfico 
png("Graf_2.png", width = 7, height = 4, units = 'in', res = 300)

ggplot(tabela_uniao, aes(x=x, y=prob)) + 
  geom_bar(stat = "identity", width=0.9, fill = "#7F0303") +
  facet_wrap( ~y ) +
  labs(x = "Nascimentos/Dia", y = "Probabilidade (%)") + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  geom_text(aes( label = porc),
            vjust = -0.4, size = 2.8, color = "black") +
  theme_bw(base_family = "Times New Roman", base_size = 12) + 
  theme( axis.text.x = element_text(  hjust = 1 )) 

dev.off()  

#################################
#Probabilidades de x nascimentos por dia da semana
#Descobrindo a taxa de ocorrencia desses eventos 

base %>%
  group_by(PARTO, dia_semana) %>% 
  summarise(media = n()/366)

#Domingo tem uma taxa media de parto cesario igual a 0,628. #Portanto a funcao de probabilidade e igual a: 

poisson = function(x, lambda){
  p = exp(-lambda)*lambda^x/factorial(x)
  return(p)
}

x = 0:5 
poisson(x, lambda = 4.09)*100


#Para segunda: 0.749, Terca: 0,716, Quarta: 0,451, Quinta: 0,495, 
#Sexta: 0,664, Sábado: 0,388
poisson(x, lambda = 0.749)*100
poisson(x, lambda = 0.716)*100
poisson(x, lambda = 0.451)*100
poisson(x, lambda = 0.495)*100
poisson(x, lambda = 0.664)*100
poisson(x, lambda = 0.388)*100

#Fazendo a analise para parto normal de acordo com os dias, respectivamente sao:
#domingo, segunda, terca, quarta, quinta, sexta e sabado
poisson(x, lambda = 0.664)*100
poisson(x, lambda = 0.489)*100
poisson(x, lambda = 0.637)*100
poisson(x, lambda = 0.596)*100
poisson(x, lambda = 0.721)*100
poisson(x, lambda = 0.721)*100
poisson(x, lambda = 0.773)*100

