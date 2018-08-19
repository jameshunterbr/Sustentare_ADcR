## Arquivo Master de Programas para Materia ADcR
## Semana 1: 17-18/08
## Agosto 2018
## James R. Hunter
## 15/08/2018

## Programa 1
## Instalação dos Pacotes
## vem de "instalar_pacotes.R"

pacotes <- c("tidyverse", "car", "caret", "caTools", "data.table", 
             "DescTools", "devtools", "gapminder", "ggpubr", "ggvis", 
             "gmodels", "Hmisc", "kableExtra", "knitr", "lattice", 
             "lpSolve", "lubridate", "magrittr", "mice", "nortest", 
             "nycflights13", "outliers", "pROC", "psych", 
             "RColorBrewer", "Rcpp", "readxl", "ROCR", "shiny", "styler", 
             "titanic", "yarrr")

install.packages(pacotes)

## Programa 2
## Assignments

#assign1
x <- 5
x

## Assignment como Resultado da Operação
x <- 3 + 2
x

## Assignment com Resultado Lógico
x <- 1 == 0
x


## Assignment como Resultado de uma Função
x <- sqrt(675.3)
x


## Ainda Mais Complicada

set.seed(42)
dados <- runif(100, min = 0, max = 1000)
m <- sum(dados)/length(dados)
m
mean(dados)


## Programa 3
## Estilo dos Nomes das Variáveis


## 1ª Versão
peso <- 55  ## Pessoa pesa 55 kg.

## 2ª Versão
peso_kg <- 55 ## Mais claro

## Pode Converter à Libra
peso_lb <- peso_kg * 2.2
peso_lb

## Anote comentários com #

## Programa 4
## messy_prog.R

## Programa 5
## Dataframe de Inflamação

# Carregar tidyverse
library(tidyverse)
sessionInfo() ## Comando para mostrar o estado do sistema R neste momento

## Carregar dados (já fez download para working directory)

dados <- read_csv(file = "inflamacao.csv", col_names = FALSE)

## Determinar class de dados e olhar na estrutura

class(dados)

str(dados[, 1:5])
glimpse(dados[, 1:5])
dim(dados)

## Subsets dos dados

# subset de colchetes
dados[1,1]
dados[1, 1:5] # 1º paciente, 1º cinco dias
dados[1:5, 1] # 1º 5 pacientes, 1º dia
dados[50, ] # paciente 50, todos os dias

# Subsets usando tidyverse (dplyr)
# equivalente a dados[1,1]

dados %>% 
  slice(1) %>% 
  select(X1)

# equivalente a dados[1, 1:5]

dados %>% 
  slice(1) %>% 
  select(X1:X5)

# equivalente a dados[1:5, 1]

dados %>% 
  slice(1:5) %>% 
  select(X1)

# equivalente a dados[50, ]
dados %>% 
  slice(50)

## Dar nomes dos dias para variáveis

# Utilizar for loop
nome <- vector(mode = "character") 
for (i in seq_along(dados)) {
  nome <- c(nome, paste0("dia", i))
}
colnames(dados) <- nome
str(dados[, 1:5])


## EDA 1

sub_dados <- dados %>% 
  select(dia20:dia25)
glimpse(sub_dados)
summary(sub_dados)
sub_dados %>% map_dbl(sd)

## map_dbl e plot

dados %>% 
  map_dbl(mean) %>% plot(type = "p", pch = 20, 
                         main = "Inflamação por Dia\ncom Novo Tratamento",
                         xlab = "Dia",
                         ylab = "Grau de Inflamação") 

## Todos os tipos de caracteres de plotagem

generateRPointShapes<-function(){
  oldPar<-par()
  par(font=2, mar=c(0.5,0,0,0))
  y=rev(c(rep(1,6),rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
  x=c(rep(1:5,5),6)
  plot(x, y, pch = 0:25, cex=1.5, ylim=c(1,5.5), xlim=c(1,6.5), 
       axes=FALSE, xlab="", ylab="", bg="blue")
  text(x, y, labels=0:25, pos=3)
  par(mar=oldPar$mar,font=oldPar$font )
}
generateRPointShapes()

## Preço de Ação Importação

acoes <- readr::read_csv("stockprice.csv")
str(acoes)

## Pacientes de HIV -- readxl

pac_data <- readxl::read_excel("pac_demo.xlsx", sheet = "pac_demo")
tibble::glimpse(pac_data)

## Ortografia Conta

r_designacao <- 26 * 37
# agora quero ver o valor
r.designacao
R_designacao
r_designacao

## Tidy Data
## TB Dados de OMS
# dados de pacote tidyr

data(who)
who

## Passo - gather

who_mod1 <- who %>% gather(new_sp_m014:newrel_f65, 
                           key = "chave", value = "casos", 
                           na.rm = TRUE)
who_mod1
who_mod1 %>% 
  distinct(chave) %>% 
  slice(1:20)

## Agrupar as variáveis
## operador %in%

brics <- c("Brazil", "Russian Federation", "India", "China", "South Africa")
who_mod2 <- who_mod1 %>% 
  group_by(country, year) %>% 
  summarize(casos = sum(casos)) %>% 
  filter(country %in% brics) %>% 
  filter(year %in% 2002:2011)
who_mod2

## Resumo dos Dados

library(DescTools)
library(Hmisc)
DescTools::Desc(who_mod2, plotit = TRUE)
Hmisc::describe(who_mod2)

## Usar dplyr para construir uma função para fazer a mesma coisa

who_mod2 %>% 
  group_by(country) %>% 
  summarise(número = n())
who_mod2 %>% 
  group_by(year) %>% 
  summarise(número = n())

## Gráfico dos Dados com ggpubr

library(ggpubr)
who_mod2 %>% 
  ggline(x = "year", y = "casos", color = "country", # Variáveis
         plot_type = "b",                         # "both" linhas e pontos
         title = "Novos Casos de TB por País BRICS",
         subtitle = "2002 - 2011",
         xlab = "Ano",                            # Rótulo do eixo x
         ylab = "Novos Casos")                    # Rótulo de eixo y

## Lei de Benford

library(tibble)
p <- tibble(digit = 1:9, prob = 0)
for (i in 1:9)   p$prob[i] <-  log10((i + 1)/i)
problabel <- round(p$prob, 3)
p %>% mutate(digit = as.character(digit)) %>% 
  ggbarplot(x = "digit", y = "prob",
            palette = "aaas",
            fill = "dark blue",
            color = "dark blue",
            label = problabel,
            lab.pos = "out",
            title = "A Lei de Benford -- Primeiro Dígito",
            subtitle = "Tamanho Relativo da Probabilidade do Digito",
            xlab = "Digito",
            ylab = "Probabilidade",
            ggtheme = theme_gray())

## Funcão da Lei de Benford

benford_law <- function(variavel) {
  casos_ch <- as.character(variavel)
  dig1 <- as.integer(str_sub(casos_ch, 1, 1))
  dist_dig1 <- as.data.frame(table(dig1))
  dist_dig1 <- dist_dig1 %>% 
    mutate(prob = Freq/length(casos_ch)) 
  return(dist_dig1)
}

## Lei aplicada a # de casos
benford_law(who_mod2$casos) 

# Para replicar completamente precisa mais de 100 casos não os 49 aqui

## Problemas dos Dados
## Rússia 2005
who_mod2 %>% filter(country == "Russian Federation")

## Imputar novo valor para Rússia 2005

r2005 <- mean(who_mod2$casos[who_mod2$country == "Russian Federation" &
                               who_mod2$year %in% 2004:2006])
# criar novo linha no who_mod3 para este valor
who_mod3 <- who_mod2 %>% 
  ungroup() %>% 
  add_row(country = "Russian Federation", year = 2005, casos = r2005) %>% 
  arrange(country, year)
knitr::kable(who_mod3[who_mod3$country == "Russian Federation",])

print(paste("Sem dado de 2005: média =", 
            round(mean(who_mod2$casos[who_mod3$country == "Russian Federation"]), 1)))
print(paste("Com dado de 2005: média =", 
            round(mean(who_mod3$casos[who_mod3$country == "Russian Federation"]), 1)))

## Problema de Índia: Outlier

who_mod3 %>% 
  filter(country == "India") %>% 
  mutate(deltapct = 100 * (lead(casos) - casos)/casos) %>% 
  knitr::kable()
who_mod3 %>% 
  filter(country == "India") %>% 
  ggboxplot(x = "country", y = "casos",
            palette = "aaas",
            add = "point",
            title = "Casos de TB em Índia 2002 - 2011",
            xlab = "",
            ylab = "Casos")

# Média dos Outros Valores

india <- (who_mod3 %>% 
            filter(country == "India"))
india <- india$casos

## Média de todos os casos

mean(india)

## Tirar o caso outlier
india_noout <- india[-6] # tirar o sexto caso (2007)
mean(india_noout)

## Mudar o 6o caso para o valor mais próximo

india_sort <- sort(india, decreasing = TRUE) # arrumar valores em ordem
novo_valor <- india_sort[2] # próximo valor
who_mod4 <- who_mod3 # criar novo tibble para a modificação
who_mod4$casos[who_mod3$country == "India" & 
                 who_mod3$year == 2007] <- novo_valor
who_mod4 %>% 
  filter(country == "India") %>%
  knitr::kable()
who_mod4 %>% 
  filter(country == "India") %>% 
  ggboxplot(x = "country", y = "casos",
            palette = "aaas",
            add = "point",
            title = "Casos de TB em Índia 2002 - 2011",
            subtitle = "Novo Valor para 2007",
            xlab = "",
            ylab = "Casos")

## Ajustar para tamanho de população

popTB <- readRDS("popTB.rds") # arquivo que eu scraped de OMS
knitr::kable(popTB)
who_mod5 <- who_mod4 %>% 
  left_join(., popTB, by = c("country", "year")) %>% 
  mutate(taxa = 100000 * casos/pop)
knitr::kable(who_mod5, caption = "who_mod5 com População")

## Gráfico 1 - com Ajuste e RSA

who_mod5 %>% 
  ggline(x = "year", y = "taxa", color = "country", # Variáveis
         plot_type = "b",                         # "both" linhas e pontos
         title = "Taxa de Incidência de TB por País BRICS",
         subtitle = "2002 - 2011",
         xlab = "Ano",                            # Rótulo do eixo x
         ylab = "Casos por 100.000 população")    # Rótulo de eixo y

## Gráfico 2 - Com Ajuste e sem RSA

who_mod5 %>% 
  filter(country != "South Africa") %>% 
  ggline(x = "year", y = "taxa", color = "country", # Variáveis
         plot_type = "b",                         # "both" linhas e pontos
         title = "Taxa de Incidência de TB por País BRICS",
         subtitle = "2002 - 2011",
         xlab = "Ano",                            # Rótulo do eixo x
         ylab = "Casos por 100.000 população")    # Rótulo de eixo y

## Binary Search

# Open Data Base

jogadores <- readRDS("players_list.rds")
str(jogadores)

## Traduzir o Programa em Código R 
jogadores <- sort(jogadores)

## Bloco 3 -- Achar a Entrada de Meio

tamanho <- length(jogadores)
limite_baixo <- 1 # para uso tardio
limite_cima <- tamanho # para uso tardio
entrada_meio <- tamanho %/% 2

### Bloco 4 -- Testar a Entrada contra o Nome "Mantle"

#  Se a entrada igual "Mantle", relatar o resultado
#  e terminar o programa
#  Definir Mickey Mantle como o  `alvo`
alvo <- "Mantle"
terminado <- FALSE # variável lógica para marcar terminação
if(jogadores[entrada_meio] == "Mantle") {
  print(paste("Mantle é entrada número", entrada_meio, "na lista."))
  terminado = TRUE
}

### Bloco 5 -- Determinar a Nova Metade da Lista

# A. Se "Mantle" > que o item escolhido, definir nova lista 
#    como item escolhido + 1 até o final da lista
# B. Se "Mantle" < que o item escolhido, definir nova lista 
#    como início da lista até item escolhido - 1
ifelse(alvo > jogadores[entrada_meio], 
       limite_baixo <- entrada_meio + 1,
       limite_cima <- entrada_meio - 1)
tamanho_novo <- limite_cima - limite_baixo + 1 
# precisa 1 para incluir ambos os finais
entrada_meio <- tamanho_novo %/% 2

### Bloco 4 Revisado -- Estrutura de *Loop*

# Initialização das variáveis 
tamanho <- length(jogadores)
limite_baixo <- 1 # para uso tardio
limite_cima <- tamanho # para uso tardio
entrada_meio <- tamanho %/% 2
terminado <- FALSE
iter <- 1
tentativa <- entrada_meio

# loop
while (!terminado) { # repetir até terminado == TRUE
  if (jogadores[entrada_meio] == alvo) { # passa teste; vá para final
    print(paste(alvo, "é entrada", entrada_meio, "da lista."))
    print(paste("Usamos", iter, "iterações para achar o alvo."))
    terminado <- TRUE
  }
  else { # precisa iteração adicional
    iter <- iter + 1 # aumentar contagem das iterações
    ifelse(alvo > jogadores[entrada_meio],                # teste
           limite_baixo <- entrada_meio + 1,  # TRUE
           limite_cima <- entrada_meio - 1)   # FALSE
    tamanho_novo <- limite_cima - limite_baixo + 1 
    # precisa 1 para incluir ambos os finais
    entrada_meio <- limite_baixo + (tamanho_novo %/% 2)
    tentativa <- c(tentativa, entrada_meio) # quais entradas escolhidas
  }
}

