rm(list=ls())
library("ggplot2")
library(patchwork)
library(rjags)
library(coda)
source("./utils_functions.R")
df = readRDS(file="./data/wine.Rda")

set.seed(46)
keep_size = 0.2 * nrow(df)
indexes = sample(1:nrow(df), size=keep_size)
df <- df[+indexes,]

indexes  = sample(1:keep_size, size=0.5 * keep_size)
train <- df[-indexes,]
test <- df[+indexes,]

Y_train = as.vector(train$quality)
X_train = as.matrix(train[,!names(df) %in% c("quality"), drop=F])

model_string <- textConnection("model{
  # Likelihood
  for (i in 1:N){
    Y[i] ~ dcat(pi[i, 1:C])
    for (c in 1:C) {
      exp_z[i,c] <- exp(z[i, c])                  # softmax function
      pi[i, c]    <- exp_z[i, c]/sum(exp_z[i, ])   # softmax function
      z[i, c]    <- beta[c, ] %*% X[i, ]          # linear model
    }
  }
  # Prior
  for (c in 1: C){
    for (k in 1: K){
      beta[c, k] ~ dnorm(0, 0.01)       # Gaussian prior
    }
  }
}")

N <- dim(X_train)[1]  # number of observations
K <- dim(X_train)[2]  # number of covariates
C <- 10         # number of categories
data <-list(Y=Y_train, X=X_train, N=N, C=C, K=K)
burn     <- 500
n.iter   <- 5000
thin     <- 100
n.chains <- 2
n.samples<- 100000

model <- jags.model(model_string,data = data, n.chains=n.chains,quiet=FALSE)

update(model, burn, n.iter=n.iter)

samples <- coda.samples(model, variable.names=c("beta"), thin=thin, n.iter=n.samples)

save(samples, file="chains/categorical_standardized_gaussian_thin100.dat")

load(file="chains/categorical_standardized_gaussian_thin100.dat")

betas <- c()
for (i in 1: K){
  for(j in 1:10)
    betas <- append(betas, paste("beta.", as.character(j), ".", as.character(i), ".", sep=""))
}
thin(samples)
# SAVE THE PLOTS
for (i in 1:(K * 10)){
  print(i)
  x <-ggplot_traceplot(samples, betas[i])
  ggsave(paste("pictures/categorical/traces_thin100/trace_beta", as.character(i), ".png", sep=""), plot=x)
  x <-ggplot_density_MC(samples, betas[i])
  ggsave(paste("pictures/categorical/densities_thin100/density_beta", as.character(i), ".png", sep=""), plot=x)
}

for (i in 1:(K * 10)){
  print(i)
  x <-ggplot_autocorr(samples, betas[i], thinning=thin(samples))
  ggsave(paste("pictures/categorical/corr_plts_thin100/corr_beta", as.character(i), ".png", sep=""), plot=x)
}