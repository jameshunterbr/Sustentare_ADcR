## Arquivo Master de Programas para Materia ADcR
## Semana 2: 24-25/08
## Agosto 2018
## James R. Hunter
## 22/08/2018

suppressMessages(library(tidyverse))
options(scipen = 10)

## Programa 1 -- Exemplos de Funções de Vetores e Coerção

# vetor lógico

x <- 1:10
x
x %% 2 == 0

# vetor numérico

x1 <- 1:10
x1
class(x1)
x2 <- 1.5:10.5
x2
class(x2)
typeof(x2)
5/0
-5/0

# vetor caractere

x <- "a"
x
xx <- "Este é uma cadeia de caracteres"
xx
length(x) # número de elementos no objeto
length(xx)
nchar(x) # número de caracteres no objeto
nchar(xx) 

# teste dos tipos dos vetores

log_var <- c(TRUE, FALSE, TRUE)
int_var <- c(1L, 20L, 2000L)
doub_var <- c(10.5, 20.8, 30)
car_var <- c("gato", "cao", "y")

is_logical(log_var)
is_character(log_var)
is_integer(int_var)
is_double(int_var)


# coerção 

x <- as.integer(log_var)
x
sum(x) # contexto que usamos esta coerção antes

y <- as.double(int_var)
y
yy <- int_var/2.5 # coerção implicito
yy
class(yy)

z <- as.character(c(log_var, int_var, doub_var))
z   # anote as aspas; são todas caracteres agora
zz <- c(car_var, int_var)
class(zz)

car_var
as.logical(car_var)
doub_var
as.integer(doub_var)

## Programa 2 - Simulação

# função VPL

vpl <- function(i, cf, t = seq(along = cf)) {
  sum(cf/(1+i)^t) 
}

# entrada de variáveis

taxa_cap <- 0.0825 #também usada para calcular o desconto de VPL
cust_init <- 1500000
preco <- 310000
cust_fixo <- 150000
deprec <- 100000
cust_var <- 0.78 # porcentagem da receita bruta da venda
t_imp <- 0.34
anos <- 4

## Função para os retornos
projeto <- function(cust_init, preco, cust_fixo, deprec,
                    cust_var, t_imp, anos, demanda) {
  proj_dados <- tibble::tibble(ano = 0:anos, demanda,
                               renda = demanda * preco,
                               custos = ifelse(ano == 0, -cust_init, 
                                               -(renda * cust_var + cust_fixo + deprec)),
                               margem = renda + custos,
                               lucro = margem * ifelse(margem > 0, (1 - t_imp), 1),
                               ncf = lucro + ifelse(ano == 0, 0, deprec))
  return(proj_dados)
}

# Teste estático

stat <- projeto(cust_init, preco, cust_fixo, deprec, 
                cust_var, t_imp, anos, c(0, rep(10, 4)))
knitr::kable(stat, caption = "Venda de 10 Unidades")
vpl(taxa_cap, stat$ncf)

# Distribuição Uniforme

x <- dunif(seq(from = -5, to = 5, by = 0.01), min = -2, max = 2)
plot(x, type = "l", main = "Distribuição Uniforme")

set.seed(42)
runif(10, min = 0, max = 10)
# com `floor`
set.seed(42)
floor(runif(10, min = 8, max = 12.999))

# Distribuição Normal

x <- dnorm(seq(from = -5, to = 5, by = 0.01), mean = 0, sd = 1)
plot(x, type = "l", main = "Distribuição Normal")

set.seed(43)
floor(rnorm(10, mean = mean(8:12), sd = sd(8:12)))

# Distribuição Triangular

x <- triangle::dtriangle(seq(from = -5, to = 5, by = 0.01), a = -2, b = 2)
plot(x, type = "l", main = "Distribuição Triangular")

set.seed(42)
floor(triangle::rtriangle(10, a = 8, b = 12.999))

# Simulação

exper <- 10000 # repetições da simulação
sim_resultado <- vector(mode = "numeric", length = exper) # vetor para resultados

# loop
set.seed(42)
for (i in 1:exper) {
  # definir demanda aleatória para cada simulação; 1o ano sempre 0
  demanda_alea <- c(0, floor(triangle::rtriangle(anos, a = 8, b = 12.999)))
  proj <- projeto(cust_init, preco, cust_fixo, deprec, cust_var, 
                  t_imp, anos, demanda_alea)
  # Colocar o VPL da simulação no vetor dos resultados
  sim_resultado[i] <- vpl(taxa_cap, proj)
}

# resultados

# Teste simples: % dos experimentos que teve VPL positivo
library(DescTools)
pct_exito <- sum(sim_resultado > 0)/exper
scales::percent(pct_exito)
desc_res <- Desc(sim_resultado)
desc_res

# Teste com timing

start_time <- Sys.time()
x <- vector(mode = "numeric", length = exper)
for (i in 1:exper) {
  # definir demanda aleatória para cada simulação; 1o ano sempre 0
  demanda_alea <- c(0, floor(triangle::rtriangle(anos, a = 8, b = 12.999)))
  proj <- projeto(cust_init, preco, cust_fixo, deprec, cust_var, 
                  t_imp, anos, demanda_alea)
  # Colocar o VPL da simulação no vetor dos resultados
  x[i] <- vpl(taxa_cap, proj)
}
end_time <- Sys.time()

tempo_gasto = end_time - start_time

## Programa 3 SLR - Sacramento

suppressPackageStartupMessages(library(caret))
data(Sacramento)
tibble::glimpse(Sacramento)

suppressMessages(library(tidyverse))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(ggpubr)) # fazer gráficos
sac_mod <- Sacramento %>% 
  select(sqft, price)
glimpse(sac_mod)

# Exploração dos Dados

Desc(sac_mod, plotit = TRUE)
gr_price_sf <- ggplot(data = sac_mod, aes(x = sqft, y = price)) 
gr_price_sf <- gr_price_sf + geom_point(shape = 20) 
gr_price_sf <- gr_price_sf + labs(x = "Tamanho em pés quadrados", 
                                  y = "Preço em US$", 
                                  title = "Sacramento - Preços de Habitação")
gr_price_sf
cor(sac_mod$sqft, sac_mod$price)

# train e test sets

set.seed(42)
indice <- createDataPartition(sac_mod$price, p = 0.5, list = FALSE)
train_data <- sac_mod[indice, ]
test_data <- sac_mod[-indice, ]

# test split

rbind(data.frame(group = "train", train_data),
      data.frame(group = "test", test_data)) %>% 
  ggplot(aes(x = price, color = group, fill = group)) +
  geom_density(alpha = 0.3) 

# LM simples - com todos os dados não só train_data

fit <- lm(price ~ sqft, data = sac_mod)
summary(fit)

# modelo de treinamento

set.seed(42)
model1 <- caret::train(price ~ sqft,
                       method = "lm",
                       data = train_data,
                       trControl = trainControl(method = "repeatedcv",
                                                number = 5,
                                                repeats = 10,
                                                savePredictions = "final",
                                                verboseIter = FALSE))
model1
summary(model1)

# Minha casa de 2000 sqft

casa <- 2000
# extrair os parâmetros do modelo model1
betas <- coef(summary(model1))[,1]
beta0 <- betas[1] 
beta1 <- betas[2]
valor <- unname(beta0 + beta1 * casa)
valor

# Fazer previsões com `test_data`
library(hrbrthemes)
# previsões
prv <- predict(model1, test_data)
# comparar para preços observados
data.frame(obs = test_data$price,
           previs = prv) %>% 
  ggplot(aes(x = obs, y = previs)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  theme_ipsum_rc() +
  labs(x = "Preço observado", y = "Preço previsto")

# Gráfico de diferença em preço x sqft

res <- data.frame(obs = test_data$price, 
                  previs = prv,
                  sqft = test_data$sqft) %>% 
  mutate(dif = (obs - previs)) # data frame 
(summ <- summary(res$dif)) 
ggplot(data = res, aes(x = sqft, y = dif)) +
  geom_jitter() +
  geom_smooth(method = "lm")  +
  theme_ipsum_rc() +
  labs(x = "Tamanho em pés quadrados", y = "Diferença")

paste("Diferenças entre observados e previstos > $50000 =", 
      100 * sum(abs(res$dif) > 50000)/nrow(res))
paste("R^2 do modelo =", prettyNum(model1$results$Rsquared, digits = 4))

## Programa 4 - MLR

suppressMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(nortest))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(broom))
suppressMessages(library(mosaic))
data(Sacramento)
# Tirar de Sacramento longitude e latitude
sac_mod <- Sacramento %>% 
  select(-c(longitude, latitude, zip))
glimpse(sac_mod)

# Factor x Character

x <- c("a", "b", "c")
str(x)
x_fct <- factor(x)
str(x_fct)

# Cidade como candidato para transformação em `factor`

table(sac_mod$city)

# mod para reduzir # das cidades

sac_mod2 <- sac_mod %>% 
  mutate(city = forcats::fct_lump(city, prop = 15/nrow(sac_mod), other_level = "OUTRO"))
forcats::fct_count(sac_mod2$city)

# quartos e banheiros - criar categorias

sac_mod3 <- sac_mod2 %>% 
  mutate(beds2 = factor(as.character(beds), 
                        levels = c("1", "2", "3", "4", "5", "6", "8")), 
         baths2 = factor(as.character(baths), 
                         levels = c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"))) 
sac_mod4 <- sac_mod3 %>%   
  mutate(beds = fct_other(beds2, drop = c("5", "6", "8"), other_level = "mais_de_4"),
         baths = fct_other(baths2, drop = c("4", "4.5", "5"), other_level = "4_ou_mais")) %>% 
  select(-beds2, -baths2)
print("Quartos")
fct_count(sac_mod4$beds)
print("Banheiros")
fct_count(sac_mod4$baths)

# Variável type já em factor

fct_count(sac_mod4$type, sort = TRUE)
table(sac_mod4$type)


gr_price_sf <- sac_mod4 %>% 
  ggscatter(x = "sqft", y = "price", color = "type",
            palette = "aaas",
            title = "Sacramento - Preços de Habitação",
            xlab = "Tamanho em pés quadrados", 
            ylab = "Preço em US$",
            shape = 20,
            add = "reg.line",
            cor.coef = TRUE,
            ggtheme = theme_ipsum_rc())
gr_price_sf

# teste de normalidade

options(scipen = 10)
densPlot <- function(numvar) {
  titulo <- paste0("Variável ", numvar)
  sac_mod4 %>% 
    ggdensity(x = numvar, 
              title = titulo,
              xvar = FALSE,
              add = "mean",
              rug = TRUE,
              ggtheme = theme_ipsum_rc())
}
densPlot("price")
densPlot("sqft")
shapiro.test(sac_mod4$sqft)
shapiro.test(sac_mod4$price)
nortest::ad.test(sac_mod4$sqft)
nortest::ad.test(sac_mod4$price)

# model.matrix

knitr::kable(with(sac_mod4, model.matrix(~ type)[c(1:10, 105:115),]))

# modelo MLR

# criar train_data e test_data
set.seed(42)
indice <- createDataPartition(sac_mod4$price, p = 0.5, list = FALSE)
train_data <- sac_mod4[indice, ]
test_data <- sac_mod4[-indice, ]

# modelo
modelo2 <- caret::train(price ~ sqft + city + beds + baths + type,
                        method = "lm",
                        data = train_data,
                        preProc = c("center", "scale"),
                        trControl = trainControl(method = "repeatedcv",
                                                 number = 5,
                                                 repeats = 10,
                                                 savePredictions = "none",
                                                 verboseIter = FALSE))
summary(modelo2)

# Previsões com modelo2

# previsões
prv <- predict(modelo2, test_data)
# comparar para preços observados
data.frame(obs = test_data$price,
           previs = prv,
           type = test_data$type) %>% 
  ggplot(aes(x = obs, y = previs, color = type)) +
  geom_jitter(shape = 20) +
  geom_smooth(method = "lm") +
  theme_ipsum_rc() +
  labs(x = "Preço observado", y = "Preço previsto")

# preços com sqft nas previsões

res <- data.frame(obs = test_data$price, 
                  previs = prv,
                  sqft = test_data$sqft) %>% 
  mutate(dif = (obs - previs)) # data frame 
(summ <- summary(res$dif)) 
ggplot(data = res, aes(x = sqft, y = dif)) +
  geom_jitter(shape = 20) +
  geom_smooth(method = "lm")  +
  theme_ipsum_rc() +
  labs(x = "Tamanho em pés quadrados", y = "Diferença")

# Quantos valores de teste ficou dentro de 50000 do preço observado?

res <- res %>%
  mutate(bom = ifelse(abs(dif) <=50000, "bom", "ruim"))
table(res$bom)
(pct <- 100 * sum(res$bom == "bom")/nrow(res))

# Importância das variáveis

caret::varImp(modelo2)

# premissas de regressão -- normalidade
# qqPlot vem de `car`
car::qqPlot(lm(price ~ sqft + city + beds + baths + type, data = sac_mod4), 
       main = "qqPlot do Modelo2")
ggplot(res, aes(sample=previs)) + 
  stat_qq() + stat_qq_line() + 
  theme_ipsum_rc() +
  labs(title = "qqplot dos Preços Previstos")


## Programa 5 - Regressão Lógistica

suppressMessages(library(tidyverse))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(nortest))
suppressPackageStartupMessages(library(hrbrthemes))
suppressMessages(library(mosaic))

chdage <- read_csv("chdage.csv")

glimpse(chdage)
Desc(chd ~ idade, data = chdage, plotit = FALSE)
chdscat <- ggplot(data = chdage, aes(y = chd, x = idade)) + 
  geom_point() + theme_ipsum_rc()
chdscat
chdbox <- ggplot(data = chdage, aes(x = chd, y = idade, group = chd))
chdbox <- chdbox + geom_boxplot() + theme_ipsum_rc()
chdbox

## Conditional Density Plot

cdplot(factor(chd) ~ idade, data = chdage, 
       main = "Densidade Condicional de Idade sobre CHD",
       xlab = "Idade", ylab = "Presença (1) ou Ausência (0) de CHD")


## Refazer idade

chdage$idgrp <- Recode(chdage$idade, "20:29 = '20-29'; 30:34 = '30-34'; 
                       35:39 = '35-39'; 40:44 = '40-44'; 45:49 = '45-49';
                       50:54 = '50-54'; 55:59 = '55-59'; 60:69 = '60-69'",
                       as.factor = TRUE) 

table(chdage$idgrp, chdage$chd)
gmodels::CrossTable(chdage$idgrp, chdage$chd, chisq = TRUE, 
                    prop.c = FALSE, prop.t = FALSE, 
                    prop.chisq = FALSE, format = "SPSS")

# Modelo Básico

chdfit1 <- glm(chd ~  idade, data = chdage, 
               family = binomial(link = "logit"))

summary(chdfit1)

## invlogit funcão

invlogit <- function(x) {
  1/(1 + exp(-x))
}

invlogit(coef(chdfit1)[2])


# Modelo 2 - 1 ind. variável

chdfit2 <- glm(chd ~  idgrp, data = chdage, 
               family = binomial(link = "logit"))
summary(chdfit2)

invlogit(coef(chdfit2)[6:8])

## Modelo 2 Multivariada RL

load("riscochd.RData")
Desc(riscochd$chd, plotit = FALSE)
Desc(riscochd$idade, plotit = TRUE)
Desc(riscochd$bmi, plotit = TRUE)
Desc(riscochd$genero, plotit = FALSE)
cdplot(factor(chd) ~ idade, data = riscochd, 
       main = "Densidade Condicional de Idade sobre CHD",
       xlab = "Idade", ylab = "Presença (1) ou Ausência (0) de CHD")
cdplot(factor(chd) ~ bmi, data = riscochd, 
       main = "Densidade Condicional de IMC sobre CHD",
       xlab = "IMC", ylab = "Presença (1) ou Ausência (0) de CHD")

## Modelo 1 todos as variáveis

chdfit3 <- glm(chd ~ idade + bmi + genero, data = riscochd)
summary(chdfit3)

# Modelo 2 só idade

chdfit4 <- glm(chd ~ idade, data = riscochd)
summary(chdfit4)

## Modelo 3 idade e bmi

chdfit5 <- glm(chd ~ idade + bmi, data = riscochd)
summary(chdfit5)

## Relações dos Odds

paste("Relação de Odds:")
exp(coef(chdfit5)) # Calculate the odds
paste("Intervalo de Confiança dos Odds:")
exp(confint(chdfit5))
paste("Probabilidade de Ocorrência:")
invlogit(chdfit5$coefficients)

## Exemplo 3 

load(file = "studdat.Rda")
actmodfit <- glm(tropismo ~ totr + cd4, 
                 data = dat2, family = "binomial")
summary(actmodfit)
paste("Relação de Odds:")
exp(coef(actmodfit)) # Calculate the odds
paste("Intervalo de Confiança dos Odds:")
exp(confint(actmodfit))
paste("Probabilidade de Ocorrência:")
invlogit(actmodfit$coefficients)
