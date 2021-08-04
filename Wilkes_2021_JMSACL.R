## Required packages ----
library(brms)
library(dplyr)
library(readxl)
library(tidyr)

## Information ----
# This section creates the models for the data with the following form, where t represents the Student-t distribution; y_ij represents the relative deviation value for the given sample j in run i; \beta_0 represents the overall intercept; \beta_1 represents the overall slope; x_ij represents the run position number for sample j in run i; u_i represents the identifier for run i; and \epsilon_ij represents the residual error (which is assumed to be Student t-distributed, with the degrees of freedom parameter \nu); median(x) represents the median; and MAD represents the median absolute deviation. Each parameter is given an weakly informative prior so as to allow the data to dominate the estimates. This approach allows the incorporation of prior knowledge to constrain the parameter values and allows easy production of prediction intervals on which to base IS acceptance criteria.

# y_ij ~ t(\nu, \mu_ij, \sigma^2)
# \mu_ij = \beta_0 + \beta_1*x_i + u_i + \epsilon_ij
# \beta_0 ~ t(3, median(y), MAD(y))
# \beta_1 ~ N(0, 1)
# u ~ t(3, 0, MAD(y))
# \nu ~ Gamma(2, 0.1)
  
 ## Modelling ----
# Here it is assumed that the dependent variable in your data is entitled `Deviation`, run position is called `Number`, and independent assay run number is called `Run`.

model <- brm(
  formula = bf(Deviation ~ Number + (1|Run)) # predict deviation by run position, grouped by "Run"
  ,data = df # your data
  ,prior = prior(normal(0, 1), class = "b") # weakly informative prior on the slope
  ,seed = 1234 # important for reproducibility
  ,cores = 1 # can increase depending on your machine
  ,iter = 10000
  ,family = student # t-distributed likelihood
)


## Results ----
gatherPredictions <- function(model
                              ,newdata
                              ,seed = 1234
                              ,lower_q99 = 0.01
                              ,lower_q95 = 0.025
                              ,upper_q95 = 0.975
                              ,upper_q99 = 0.99) {
  
  # Set random seed
  set.seed(seed)
  
  # Gather posterior predictions
  df_pred <- posterior_predict(model, newdata = newdata, allow_new_levels = TRUE) %>%
    t() %>%
    as.data.frame() %>%
    bind_cols(newdata) %>%
    pivot_longer(cols = -c(Number, Run), values_to = "prediction") %>%
    group_by(Number) %>%
    summarise(
      lower_99 = quantile(prediction, lower_q99)
      ,lower_95 = quantile(prediction, lower_q95)
      ,median = median(prediction)
      ,upper_95 = quantile(prediction, upper_q95)
      ,upper_99 = quantile(prediction, upper_q99)
    ) %>%
    mutate(
      lower_99 = smooth(lower_99) # smooth once
      ,lower_95 = smooth(lower_95)
      ,median = smooth(median)
      ,upper_95 = smooth(upper_95)
      ,upper_99 = smooth(upper_99)
    )
  return(df_pred)
  
}

df_new <- data.frame(Number = 1:99, Run = "Run1") # define new data
df_pred <- gatherPredictions(model = model, newdata = df_new) # gather prediction intervals
