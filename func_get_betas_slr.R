## Extract betas from a caret Train model for SLR
## Produces a vector with 2 elements: [1] intercept-beta1 [2] slope-beta2
## James R. Hunter
## 10/08/18

get_beta_slr <- function(model) {
  betas <- coef(summary(model1))[,1]
} 