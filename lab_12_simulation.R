
library(tidyverse)

generate_data <- function(n, p){
  covariates <- rnorm(n*p) %>% matrix(nrow=n)
  responses <- rnorm(n)
  return(list(covariates = covariates,
              responses = responses))
}

