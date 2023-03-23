## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7,
  fig.height = 5,
  comment = "#>"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(sparseDFM)
library(gridExtra)
data <- exports 

## -----------------------------------------------------------------------------
# Dimension of the data: n = 226, p = 445.
dim(data)

# Plot the 9 target series using ts.plot with a legend on the right 
def.par <- par(no.readonly = TRUE) # initial graphic parameters 
goods <- data[,1:9]
layout(matrix(c(1,2),nrow=1), width=c(4,3)) 
par(mar=c(5,4,4,0)) 
ts.plot(goods, gpars= list(col=10:1,lty=1:10))
par(mar=c(5,0,4,2)) 
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("center", legend = colnames(goods), col = 10:1, lty = 1:10, cex = 0.7)
par(def.par) # reset graphic parameters to initial

## -----------------------------------------------------------------------------
# last 12 months 
data_last12 = tail(data, 12)

# Missing data plot. Too many variable names so use.names is set to FALSE for clearer output.
missing_data_plot(data_last12, use.names = FALSE)

## -----------------------------------------------------------------------------
# first-differences correspond to stationary_transform set to 2 for each series
new_data = transformData(data, stationary_transform = rep(2,ncol(data)))

## -----------------------------------------------------------------------------
tuneFactors(new_data)

## -----------------------------------------------------------------------------
# Regular DFM fit - takes around 18 seconds 
fit.dfm <- sparseDFM(new_data, r = 4, alg = 'EM')

# Sparse DFM fit - takes around 2 mins to tune 
# set q = 9 as the first 9 variables (targets) should not be regularised
# L1 penalty grid set to logspace(0.4,1,15) after exploration
fit.sdfm <- sparseDFM(new_data, r = 4, q = 9, alg = 'EM-sparse', alphas = logspace(0.4,1,15))

## -----------------------------------------------------------------------------
# Number of iterations the DFM took to converge
fit.dfm$em$num_iter

# Number of iterations the Sparse DFM took to converge at each L1 norm penalty 
fit.sdfm$em$num_iter

# Optimal L1 norm penalty chosen
fit.sdfm$em$alpha_opt

# Plot of BIC values for each L1 norm penalty 
plot(fit.sdfm, type = 'lasso.bic')

## -----------------------------------------------------------------------------
## Plot the estimated factors for the DFM
plot(fit.dfm, type = 'factor')

## Plot the estimated loadings for each of the 4 factors in a grid 

# Specify the name of the group each indicator belongs too
groups = c(rep('TiG',9), rep('IoP',89), rep('CPI',166), rep('PPI',153),
           rep('Exch',12), rep('Conf',2), rep('GT',14))

# Specify the colours for each of the groups 
group_cols = c('black','blue','red','pink','green','navy','brown')

# Plot the group lineplot in a 2 x 2 grid 
p1 = plot(fit.dfm, type = 'loading.grouplineplot', loading.factor = 1, group.names = groups, group.cols = group_cols)
p2 = plot(fit.dfm, type = 'loading.grouplineplot', loading.factor = 2, group.names = groups, group.cols = group_cols)
p3 = plot(fit.dfm, type = 'loading.grouplineplot', loading.factor = 3, group.names = groups, group.cols = group_cols)
p4 = plot(fit.dfm, type = 'loading.grouplineplot', loading.factor = 4, group.names = groups, group.cols = group_cols)

grid.arrange(p1, p2, p3, p4, nrow = 2)


## -----------------------------------------------------------------------------
## Plot the estimated factors for the Sparse DFM
plot(fit.sdfm, type = 'factor')

## Plot the estimated loadings for each of the 4 factors in a grid 

# Plot the group lineplot in a 2 x 2 grid 
p1 = plot(fit.sdfm, type = 'loading.grouplineplot', loading.factor = 1, group.names = groups, group.cols = group_cols)
p2 = plot(fit.sdfm, type = 'loading.grouplineplot', loading.factor = 2, group.names = groups, group.cols = group_cols)
p3 = plot(fit.sdfm, type = 'loading.grouplineplot', loading.factor = 3, group.names = groups, group.cols = group_cols)
p4 = plot(fit.sdfm, type = 'loading.grouplineplot', loading.factor = 4, group.names = groups, group.cols = group_cols)

grid.arrange(p1, p2, p3, p4, nrow = 2)


## -----------------------------------------------------------------------------
## DFM nowcasts (on the differenced data)

# directly from fit.dfm 
dfm.nowcasts = tail(fit.dfm$data$fitted.unscaled[,1:9],2)

# is the same as from fitted()
dfm.nowcasts = tail(fitted(fit.dfm)[,1:9],2)

## Sparse DFM nowcasts (on the differenced data)

sdfm.nowcasts = tail(fit.sdfm$data$fitted.unscaled[,1:9],2)

## -----------------------------------------------------------------------------
## August 2022 figures for targets 

obs_aug22 = tail(data,3)[1,1:9]

## DFM nowcast for original level 

dfm_sept_nowcast = obs_aug22 + dfm.nowcasts[1,]
dfm_oct_nowcast = dfm_sept_nowcast + dfm.nowcasts[2,]

## Sparse DFM nowcast for original level 

sdfm_sept_nowcast = obs_aug22 + sdfm.nowcasts[1,]
sdfm_oct_nowcast = sdfm_sept_nowcast + sdfm.nowcasts[2,]

# Print 
cbind(dfm_sept_nowcast,
dfm_oct_nowcast,
sdfm_sept_nowcast,
sdfm_oct_nowcast)

