## Programa para automatizar o download dos pacotes
## necessários para o curso Sustentare Análise de Dados com R
## Author: James Hunter
## Date: 28/6/18
## Version: 3.0

pacotes <- c("tidyverse", "car", "caret", "caTools", "data.table", 
             "DescTools", "devtools", "gapminder", "ggpubr", "ggvis", 
             "gmodels", "Hmisc", "kableExtra", "knitr", "lattice", 
             "lpSolve", "lubridate", "magrittr", "mice", "nortest", 
             "nycflights13", "outliers", "pROC", "psych", 
             "RColorBrewer", "Rcpp", "readxl", "ROCR", "shiny", "styler", 
             "titanic", "yarrr")

install.packages(pacotes)
