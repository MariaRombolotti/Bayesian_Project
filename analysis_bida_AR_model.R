#analisi bida and AR models
library(LearnBayes)
library(mvtnorm)
library(coda)  
library(ggplot2)
library(ggpubr)
library(rstan)
library(loo)
library(rethinking)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(RColorBrewer)


load("model48_base.RData")
pairs(model_base, pars = c('beta'), condition = 0.5)

stan_dens(model_base,pars=c("beta"),col='royalblue4',bg='royalblue4')


#analysis of Gamma-Exp model
load("model2_bida.RData")
coda_bida_lambda <- As.mcmc.list(model2_bida, pars = c("lambda"))
summary(coda_bida_lambda)
acfplot(coda_bida_lambda, lag.max = 100,sub="Gamma-Exp model")

coda_bida_beta <- As.mcmc.list(model2_bida, pars = c("beta"))
summary(coda_bida_beta)
acfplot(coda_bida_beta, lag.max = 100,sub="Gamma-Exp model")

rstan::traceplot(model2_bida,pars=c("beta"))

stan_dens(model2_bida,pars=c("lambda"),col='royalblue4',bg='royalblue4')

stan_dens(model2_bida,pars=c("beta"),col='royalblue4',bg='royalblue4')

pairs(model2_bida, pars = c('beta'), condition = 0.5)

#analysis of AR model
load("model_AR.RData")
coda_AR_lambda <- As.mcmc.list(model_AR, pars = c("lambda"))
summary(coda_AR_lambda)
acfplot(coda_AR_lambda, lag.max = 100,sub="AR model")

coda_AR_beta <- As.mcmc.list(model_AR, pars = c("beta"))
summary(coda_AR_beta)
acfplot(coda_AR_beta, lag.max = 100,sub="AR model")

rstan::traceplot(model_AR,pars=c("beta"))

stan_dens(model_AR,pars=c("lambda"),col='royalblue4',bg='royalblue4')

stan_dens(model_AR,pars=c("beta"),col='royalblue4',bg='royalblue4')

pairs(model_AR, pars = c('beta'), condition = 0.5)



#analysis DP model
load("model0_DP.RData")
