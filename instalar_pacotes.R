## Programa para automatizar o download dos pacotes
## necessários para o curso Sustentare Análise de Dados com R
## Author: James Hunter
## Date: 18/5/18
## Version: 2.0

pacotes <- c("tidyverse", "car", "caret", "caTools", "data.table", 
             "DescTools", "devtools", "gapminder", "ggpubr", "ggvis", 
             "gmodels", "kableExtra", "knitr", "lattice", "lpSolve", 
             "lubridate", "magrittr", "nortest", "nycflights13", "outliers", 
             "pROC", "psych", "RColorBrewer", "Rcpp", "readxl", "ROCR",   
             "shiny", "titanic", "yarrr")

install.packages(pacotes)
