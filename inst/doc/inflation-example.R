## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7,
  fig.height = 5,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(sparseDFM)
data <- inflation 

## -----------------------------------------------------------------------------
# n = 135, p = 36
dim(data)
# Names of inflation variables 
colnames(data)
# Plot of data (standardised to mean 0 sd 1)
matplot(scale(data), type = 'l', ylab = 'Standardised Values', xlab = 'Observations')

## -----------------------------------------------------------------------------
# Take first differences 
new_data = transformData(data, stationary_transform = rep(2,ncol(data)))
# Plot new_data (standardised to mean 0 sd 1)
matplot(scale(new_data), type = 'l', ylab = 'Standardised Values', xlab = 'Observations')

## -----------------------------------------------------------------------------
missing_data_plot(data)

## -----------------------------------------------------------------------------
tuneFactors(new_data)

## -----------------------------------------------------------------------------
fit.dfm <- sparseDFM(new_data, r=3, alg = 'EM', err = 'IID', kalman = 'univariate')
summary(fit.dfm)


## -----------------------------------------------------------------------------
# Plot all of the estimated factors 
plot(fit.dfm, type = 'factor')


## -----------------------------------------------------------------------------
# Plot a heatmap of the loadings for all factors 
plot(fit.dfm, type = 'loading.heatmap', use.series.names = TRUE)

## -----------------------------------------------------------------------------
# Plot a line plot for the loadings for factor 1 
plot(fit.dfm, type = 'loading.lineplot', loading.factor = 1, use.series.names = TRUE)

## -----------------------------------------------------------------------------
# Plot boxplots for the residuals of each variable 
plot(fit.dfm, type = 'residual', use.series.names = TRUE)

## -----------------------------------------------------------------------------
# Did the EM algorithm converge?
fit.dfm$em$converged
# How many iterations did the EM take to converge?
fit.dfm$em$num_iter
# What were the log-likelihood values at each EM iteration?
fit.dfm$em$loglik
# Plot these log-likelihood values 
plot(fit.dfm, type = 'em.convergence')

## -----------------------------------------------------------------------------
# Forecast 3 steps ahead of the sample 
my_forecast <- predict(fit.dfm, h = 3)
my_forecast

## -----------------------------------------------------------------------------
fit.sdfm <- sparseDFM(new_data, r = 3, alphas = logspace(-2,3,100), alg = 'EM-sparse', err = 'IID', kalman = 'univariate')
summary(fit.sdfm)

## -----------------------------------------------------------------------------
# Grid of alpha values used before a column of Lambda was set entirely to 0 
fit.sdfm$em$alpha_grid
# The best alpha chosen 
fit.sdfm$em$alpha_opt
# Plot the BIC values for each alpha 
plot(fit.sdfm, type = 'lasso.bic')

## -----------------------------------------------------------------------------
plot(fit.sdfm, type = 'loading.heatmap', use.series.names = TRUE)

