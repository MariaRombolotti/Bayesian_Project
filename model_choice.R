
library(rstan)
library(loo)
library(rethinking)

## Gamma Prior
# lambda_k ~ Gamma(alpha, beta) k=1:10
# model_BASE: alpha=2=beta
View(model_base)
posterior_base <- extract(model_base)
log_lik_base <- extract(model_base,'log_lik')$log_lik
loo(log_lik_base)
waic_model_BASE <- WAIC(model_base)
traceplot(model_base)
stan_dens(model_base,col='royalblue4',bg='royalblue4')
i=1
while (i <= 10){
  plot(density(posterior_base$lambda[,i]), main = "Lambda")
  plot(posterior_base$lambda[,i], type='l', main = "Lambda")
  i=i+1
}

## Gamma prior + Exponential hyperprior
# lambda_k ~ Gamma(lambda* r, r) with lambda* ~ Exp(base) k=1:10
# model_BIDA: base=1 r=2
View(model_BIDA)
posterior_BIDA <- extract(model_BIDA)
log_lik_BIDA <- extract(model_BIDA,'log_lik')$log_lik
loo(log_lik_BIDA)
waic_model_BIDA <- WAIC(model2_bida)
traceplot(model2_bida)
stan_dens(model2_bida,col='royalblue4',bg='royalblue4')
i=1
while (i <= 10){
  plot(density(posterior_BIDA$lambda[,i]), main = "Lambda")
  plot(posterior_BIDA$lambda[,i], type='l', main = "Lambda")
  i=i+1
}

## Autoregressive Prior
# log(lambda_k) ~ N(log(lambda_k-1),v) k=1:10
# model_AR: v=1/4 lambda0=0
View(model_AR)
posterior_AR <- extract(model_AR)
log_lik_AR <- extract(model_AR,'log_lik')$log_lik
waic_model_AR <- WAIC(model_AR)
traceplot(model_AR)
stan_dens(model_AR,col='royalblue4',bg='royalblue4')
i=1
while (i <= 10){
  plot(density(posterior_AR$lambda[,i]), main = "Lambda")
  plot(posterior_AR$lambda[,i], type='l', main = "Lambda")
  i=i+1
}
