## Webscraping & Limpeza dos Dados
## de Tutorial "Web Scraping and Parsing Data in R: Exploring H-1b Data"
## https://www.datacamp.com/community/tutorials/exploring-h-1b-data-with-r#getting
## Adaptado James Hunter 19/8/18

##---(WebScrape)-----------------------------------------------##
library(jsonlite)
library(rvest)
library(pbapply)
library(data.table)
json_cities<-paste0('http://h1bdata.info/cities.php?term=', 
                    c("a", "b", "f", "l"))
all_cities<-unlist(pblapply(json_cities,fromJSON))
all_cities <- c("ANN ARBOR", "BOSTON", "FARGO", "LOS ANGELES")
city_year<-expand.grid(city=all_cities,yr=seq(2012,2016))
city_year$city<-urltools::url_encode(as.character(city_year$city))
all_urls<-paste0('http://h1bdata.info/index.php?em=&job=&city=', 
                 city_year[,1],'&year=', city_year[,2])

main<-function(url_x){
  x<-read_html(url_x)
  x<-html_table(x)
  x<-data.table(x[[1]])
  return(x)
  Sys.sleep(5)
}

all_h1b<-pblapply(all_urls, main)
all_h1b<-rbindlist(all_h1b)
saveRDS(all_h1b, file = "all_h1b.rds")

##---(Limpeza)-----------------------------------------------##

library(lubridate)
library(stringr)
library(forcats)

options(scipen = 999) # tirar os valores em notação ciêntfico

## col names

names(all_h1b)
# tem espaços e em maiúsculas
# usar gsub() para substituir "_" para " ")
colnames(all_h1b) <- tolower(names(all_h1b))
colnames(all_h1b) <- gsub(" ", "_", names(all_h1b))

## olhar na cabeça e final dos dados

head(all_h1b)
tail(all_h1b)

## verificar classes das colunas (variáveis)

purrr::map_chr(all_h1b, class)

## Precisa mudar `submit_date`, `start_date` ao formato "POSIXct"
## Precisa mudar `location`, `case_status` a classe "factor"
## Precisa tirar a vírgula da `base_salary`
library(dplyr)
h1b_data <- all_h1b %>%  # 1º - criar novo objeto 
  mutate(., submit_ano = as.factor(as.character(year(mdy(submit_date))))) %>% 
  mutate(., submit_mes = as.factor(as.character(month(mdy(submit_date))))) %>% 
  mutate(., start_ano = as.factor(as.character(year(mdy(start_date))))) %>% 
  mutate(., start_mes = as.factor(as.character(month(mdy(start_date))))) %>%
  mutate(., location = as.factor(location)) %>% 
  mutate(., case_status = as.factor(case_status)) %>% 
  mutate(., base_salary = as.numeric(str_replace_all(base_salary, ",", "")))

## Mostrar as locais distintos; mais que Ann Arbor, MI, Boston, MA 
## Fargo, ND e LA, CA

h1b_data %>% distinct(., location)

# Usar filtar para limitar para as 2 cidades
cities <- c("ANN ARBOR, MI", "BOSTON, MA", 
            "FARGO, ND", "LOS ANGELES, CA")
h1b_data <- h1b_data %>% 
  filter(location %in% cities) %>% 
  mutate(location = forcats::fct_drop(location))

## Tirar colunas desnecessárias

h1b_data2 <- h1b_data %>% 
  select(-c(submit_date, start_date))
saveRDS(h1b_data2, file = "h1b_data.rds") #guardar em arquivo

## Dados Agora Tidy

##---(Explorar)-----------------------------------------------##

h1b_data <- readRDS("h1b_data.rds")
## locais

library(DescTools)
Desc(h1b_data$location)

# location x year
library(ggpubr)

locyr <- h1b_data %>% 
  group_by(location, submit_ano) %>% 
  summarise(tot = n())

locyr %>% ggbarplot(x = "location", y = "tot",
                    color = "submit_ano",
                    fill = "submit_ano",
                    palette = "aaas", 
                    ylab = "Total Cases",
                    ggtheme = theme_classic())

## Gráfico -- Base Salary x Status 

gg_sal_stat_box <-  h1b_data %>% 
  ggboxplot(., x = "case_status", y = "base_salary", 
            palette = "aaas",
            fill = "case_status",
            title = "Vistos H1b -- Salário x Status",
            xlab = "Status de Pedido",
            ylab = "Salário",
            ylim = c(0, 150000),
            add = c("mean"),
            ggtheme = theme_gray()) 

gg_sal_stat_box

## Acrescentar local para o gráfico

gg_sal_stat_box + facet_grid(rows = vars(location), scales = "free_y")

summ_salario_local <- h1b_data %>% 
  group_by(location, submit_ano) %>% 
  summarise(salário_médio = mean(base_salary),
            sal_desvio_padrao = sd(base_salary),
            salário_mínimo = min(base_salary),
            salário_máximo = max(base_salary),
            tot_admitido = sum(case_status == "CERTIFIED"))

knitr::kable(summ_salario_local, caption = "Resumo de Salários & Éxito por Cidade")

## Gráfico de Salário nos Anos por Cidade

gg_sal_local_ano <- h1b_data %>% 
  ggboxplot(x = "submit_ano", y = "base_salary",
            palette = "aaas",
            fill = "submit_ano",
            title = "Vistos H1b -- Salário por Ano por Cidade",
            xlab = "",
            ylab = "Salário",
            ylim = c(0, 200000),
            add = "mean",
            facet.by = "location",
            ggtheme = theme_gray())
gg_sal_local_ano
