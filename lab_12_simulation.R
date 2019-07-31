
library(tidyverse)

generate_data <- function(n, p){
  covariates <- rnorm(n*p) %>% matrix(nrow=n)
  responses <- rnorm(n)
  return(list(covariates = covariates,
              responses = responses))
}

model_select <- function(covariates, responses, cutoff){
  full_model <- lm(responses ~ covariates)
  full_model_ps <- 
    summary(full_model)$coefficients[, "Pr(>|t|)"][-1] # ignore the intercept p
  significant_covars <- full_model_ps <= cutoff
  if (sum(significant_covars) == 0){
    return(vector())   # return empty vector
  } else {
    reduced_model <- lm(responses ~ covariates[, significant_covars])
    reduced_model_ps <-
      summary(reduced_model)$coefficients[, "Pr(>|t|)"][-1] # ignore the intercept p
    return(reduced_model_ps)
  }
}
