## Código R do Webinar de Reegressão
## Autor: James Hunter
## Data: 09/08/18

## Bloco 1

library(caret)
data(Sacramento)
tibble::glimpse(Sacramento)


## Bloco 2

suppressMessages(library(tidyverse))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(ggpubr)) # fazer gráficos
sac_mod <- Sacramento %>% 
  select(sqft, price)
glimpse(sac_mod)


##Bloco 3

Desc(sac_mod, plotit = TRUE)
gr_price_sf <- ggplot(data = sac_mod, aes(x = sqft, y = price)) 
gr_price_sf <- gr_price_sf + geom_point(shape = 20) 
gr_price_sf <- gr_price_sf + labs(x = "Tamanho em pés quadrados", 
                                  y = "Preço em US$", 
                                  title = "Sacramento - Preços de Habitação")
gr_price_sf

## Bloco 4

cor(sac_mod$sqft, sac_mod$price)

## Bloco 5

set.seed(42)
indice <- createDataPartition(sac_mod$price, p = 0.5, list = FALSE)
train_data <- sac_mod[indice, ]
test_data <- sac_mod[-indice, ]

## Bloco 6

rbind(data.frame(group = "train", train_data),
      data.frame(group = "test", test_data)) %>% 
  ggplot(aes(x = price, color = group, fill = group)) +
  geom_density(alpha = 0.3) 

## Bloco 7

fit <- lm(price ~ sqft, data = sac_mod)
summary(fit)

## Bloco 8

set.seed(42)
model1 <- caret::train(price ~ sqft,
                       method = "lm",
                       data = sac_mod,
                       trControl = trainControl(method = "repeatedcv",
                                                number = 5,
                                                repeats = 10,
                                                savePredictions = "final",
                                                verboseIter = FALSE))
model1
summary(model1)

## Bloco 9

# previsões
prv <- predict(model1, test_data)
# comparar para preços observados
data.frame(obs = test_data$price,
           previs = prv) %>% 
  ggplot(aes(x = obs, y = previs)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(x = "Preço observado", y = "Preço previsto")

## Bloco 10 (novo)

# Calcular Diferença entre tamanho da residência e valor previsto

res <- data.frame(obs = test_data$price, 
                  previs = prv,
                  sqft = test_data$sqft) %>% 
  mutate(dif = (obs - previs), 
         pctdif = 100 * dif/previs)
Desc(res$dif)
ggplot(data = res, aes(x = sqft, y = dif)) +
  geom_jitter() +
  geom_smooth(method = "lm")  +
  labs(x = "Tamanho em pés quadrados", y = "Diferença(%)")
