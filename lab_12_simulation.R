
library(tidyverse)
library(gridExtra)

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

run_trial <- function(n, p, cutoff){
  data_ls <- generate_data(n, p)
  model_select(data_ls$covariates, data_ls$responses, cutoff)
}

run_simulation <- function(n_trials=1000, n, p, cutoff=0.05){
  # both n and p should be vectors of numbers
  np_combos_df <- expand.grid(n=n, p=p)
  np_combos_ls <- split(np_combos_df, seq(nrow(np_combos_df)))
  np_combos_df$ps <- lapply(np_combos_ls, function(np_vec){
    replicate(n_trials, run_trial(np_vec$n, np_vec$p, cutoff)) %>%
      reduce(c)
  })
  cat("Finished running trials, start plotting now.")
  np_plots <- list()
  for (r_i in seq(nrow(np_combos_df))){
    curr_n <- np_combos_df$n[r_i]
    curr_p <- np_combos_df$p[r_i]
    curr_ps <- np_combos_df$ps[r_i] %>% unlist
    np_plots[[r_i]] <- data.frame(p_vals = curr_ps) %>%
      ggplot(aes(x=p_vals)) +
      geom_histogram(bins=30) +
      labs(title=sprintf("n=%s, p=%s, n_trails=%s", curr_n, curr_p, n_trials),
           x="P Values", y ="Count") +
      theme_minimal()
  }
  grid.arrange(grobs=np_plots)
}

# right-skewed histograms (with model selection)
#run_simulation(n=c(100, 1000, 10000), p=c(10, 20, 50))

# uniform distribution from 0-1 (without model selection)
#run_simulation(n=c(100, 1000, 10000), p=c(10, 20, 50), cutoff=1)




