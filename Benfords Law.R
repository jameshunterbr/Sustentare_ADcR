library(tibble)
library(dplyr)
library(ggpubr)
p <- tibble(digit = 1:9, prob = 0)
for (i in 1:9)   p$prob[i] <-  log10((i + 1)/i)
problabel <- round(p$prob, 3)
library(ggpubr)
p %>% mutate(digit = as.character(digit)) %>% 
  ggbarplot(x = "digit", y = "prob",
                palette = "aaas",
                fill = "dark blue",
                color = "dark blue",
                label = problabel,
                lab.pos = "out",
                title = "A Lei de Benford -- Primeiro Digito",
                subtitle = "Tamanho Relativo da Probabilidade do Digito",
                xlab = "Digito",
                ylab = "Probabilidade",
                ggtheme = theme_gray())
