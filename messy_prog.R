dependent = "differ.factor"

# Specify explanatory variables of interest
explanatory = c("age", "sex.factor", "extent.factor", "obstruct.factor", "nodes")
colon_s %>% select(age, sex.factor, 
extent.factor, obstruct.factor, nodes) %>% 
names() -> explanatory

colon_s %>% 
summary_factorlist(dependent, explanatory, 
p=TRUE, na_include=TRUE)

Hmisc::label(colon_s$nodes) = "Lymph nodes involved"
explanatory = c("age", "sex.factor","extent.factor", "nodes")

colon_s %>% 
summary_factorlist(dependent, explanatory, p=TRUE, na_include=TRUE, add_dependent_label=TRUE) -> table1
table1; explanatory = c("age", "sex.factor", "extent.factor", "nodes", "differ.factor")
dependent = "mort_5yr"
colon_s %>% 
finalfit(dependent, explanatory, dependent_label_prefix = "") -> table2
table2
colon_s %>% 
or_plot(dependent, explanatory, 
          breaks = c(0.5, 1, 5, 10, 20, 30))
save(table1, table2, dependent, explanatory, file = "out.rda")
