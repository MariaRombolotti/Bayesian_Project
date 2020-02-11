library(rstan)
library(devtools)
library(coda)
library(rjags)
library(rethinking)
 	
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#load("donazioni_full.RData")

### Gamma Prior
# lambda_k ~ Gamma(alpha, beta) k=1:10
dati<- read_rdump("modello48.R")
model_base<-stan(file="recurrent.stan",data=dati, chains=2, seed=42,iter=100000,warmup=50000,thin=25,verbose = TRUE)


### try various models of Gamma prior + Exp hyperprior
# lambda_k ~ Gamma(lambda* r, r) with lambda* ~ Exp(base) k=1:10
#modello 1: base=1 r=0.01
dati0.01_bida<- read_rdump("modello48_bida_0.01.R")
model0.01_bida<-stan(file = "recurrent_bida.stan",data=dati0.01_bida, chains=2, seed=42, iter=30000,warmup=15000,
                     thin=25,verbose=TRUE)
#it says that there are a lot of problems, 4 divergent transitions after warmup

#modello 2: base=1 r=1.5
dati1.5_bida<- read_rdump("modello48_bida_1.5.R")
model1.5_bida<-stan(file = "recurrent_bida.stan",data=dati1.5_bida, chains=2, seed=42, iter=30000,warmup=15000,
                     thin=25,verbose=TRUE)

#modello 3: base=1 r=0.5
dati0.5_bida<- read_rdump("modello48_bida_0.5.R")
model0.5_bida<-stan(file = "recurrent_bida.stan",data=dati0.5_bida, chains=2, seed=42, iter=30000,warmup=15000,
                    thin=25,verbose=TRUE)

#modello 4: base=1 r=3
dati3_bida<- read_rdump("modello48_bida_3.R")
model3_bida<-stan(file = "recurrent_bida.stan",data=dati3_bida, chains=2, seed=42, iter=30000,warmup=15000,
                    thin=25,verbose=TRUE)

#modello 5: base=1 r=20
dati20_bida<- read_rdump("modello48_bida_20.R")
model20_bida<-stan(file = "recurrent_bida.stan",data=dati20_bida, chains=2, seed=42, iter=30000,warmup=15000,
                  thin=25,verbose=TRUE)
#warning message also for this model

#modello 6: base=1, r=2
dati2_bida<-read_rdump("modello48_bida_2.R")
model2_bida<-stan(file = "recurrent_bida.stan",data=dati2_bida, chains=2, seed=42, iter=30000,warmup=15000,
                   thin=25,verbose=TRUE)
model_bida<-stan(file="recurrent_bida.stan",data=dati2_bida, chains=2, seed=42,iter=100000,warmup=50000,thin=25,verbose = TRUE)


### try various models of AR prior
# log(lambda_k) ~ N(log(lambda_k-1),v) k=1:10
# modello 0: v=1/4 log(lambda0)=0
dati48_AR<- read_rdump("modello48_AR.R")
model_AR<-stan(file="recurrent_AR.stan",data=dati48_AR, chains=2, seed=42,iter=30000,warmup=15000,thin=25,verbose = TRUE)
model_AR<-stan(file="recurrent_AR.stan",data=dati48_AR, chains=2, seed=42,iter=100000,warmup=50000,thin=25,verbose = TRUE)

#modello 1: v=1/2 log(lambda0)=10
dati1_AR<- read_rdump("modello1_AR.R")
model1_AR<-stan(file="recurrent_AR.stan",data=dati1_AR, chains=2, seed=42,iter=30000,warmup=15000,thin=25,verbose = TRUE)

#model 2: v=1  log(lambda0)=0
dati2_AR<- read_rdump("modello2_AR.R")
model2_AR<-stan(file="recurrent_AR.stan",data=dati2_AR, chains=2, seed=42,iter=30000,warmup=15000,thin=25,verbose = TRUE)

#model 3: v=10  log(lambda0)=1
dati3_AR<- read_rdump("modello3_AR.R")
model3_AR<-stan(file="recurrent_AR.stan",data=dati3_AR, chains=2, seed=42,iter=30000,warmup=15000,thin=25,verbose = TRUE)

#model 4: v=100  log(lambda0)=0
dati4_AR<- read_rdump("modello4_AR.R")
model4_AR<-stan(file="recurrent_AR.stan",data=dati4_AR, chains=2, seed=42,iter=30000,warmup=15000,thin=25,verbose = TRUE)

#model 5: v=1/2 log(lambda0)=1
dati5_AR<- read_rdump("modello5_AR.R")
model5_AR<-stan(file="recurrent_AR.stan",data=dati5_AR, chains=2, seed=42,iter=30000,warmup=15000,thin=25,verbose = TRUE)
#Warning messages:
#  1: There were 75 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
#2: Examine the pairs() plot to diagnose sampling problems



#generating the model with Dirichlet Process mixture 
dati0<- read_rdump("modello48_DP.R")
model0_DP<-stan(file="recurrent_DP.stan",data=dati0, chains=2, seed=134889,iter=30000,warmup=15000,thin=2,verbose = TRUE,control=list(adapt_delta=0.99))

#generating the model with the spline con ord 2 ind 1 (lineare)
dati6 <- read_rdump("modello48_splineok.R)
model_spline <- stan(file= "recurrent_spline.stan", dati=dati6, chains=2, iter= 15000, warmup=5000)  
#vedendo che molte iterazioni divergevano abbiamo provato a compilarlo nel seguente modo:
#model_spline2<-stan(file="recurrent_spline1.stan",data=dati6, chains=2, seed=134889,iter=30000,warmup=15000,thin=2,verbose = TRUE,control=list(adapt_delta=0.99))
#richiedeva però troppo tempo
 
# ord 3 ind 2 (quadratico)
dati7 <- read_rdump("modello48_spline_quad.R")
modello_spline <- stan("recurrent_spline1.stan", data=dati7, seed=134889, chains=2, iter=30000 , warmup=15000, thin=2, verbose = TRUE)
         

