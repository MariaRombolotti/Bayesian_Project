data{
  int<lower=1> M; // numero individui 
  int<lower=1> Nmax; // numero massimo eventi individuo 
  int<lower=1> Smax; // numero massimo intervalli non osservabilità
  int<lower=2> K; // numero nodi 
  int<lower=0> p; // numero covariate - intero
  
  matrix[M,Nmax] times; // tempi eventi (negativo se non avvenuta)
  vector[K] nodi; // nodi 
  matrix[M,p] X; // design matrix 
  matrix[M,Smax] inizio; // inizio non osservabilit?
  matrix[M,Smax] fine; // fine non osservabilit?
  
  real<lower=0> a_eta; // shape parameter eta
  real<lower=0> base; // lambda_star exp parameter
  real<lower=0> b_eta; // scale parameter eta
  vector[K] r; // scale parameter lambda_k
  real<lower=0> sigma_b; // varianza beta
  }

transformed data{
 matrix[K-1,M] n;  // numero eventi in ogni intervallo di ogni individuo
 vector[K-1] npunto;  // numero di eventi totale per ogni intervallo
 int nind[M]; // numero eventi per ogni individuo
 matrix[M,(K-1)] tau; // tempo di osservazione in ogni intervallo
 
 for(i in 1:M){
   for(k in 1:(K-1)){
     tau[i,k] = nodi[k+1]-nodi[k];
     for(j in 1:Smax){
       tau[i,k] = tau[i,k] - fmax(fmin(nodi[k+1],fine[i,j])-fmax(nodi[k],inizio[i,j]),0); 
     }
   }
 }
 
 
// inizializzo nind
 for(i in 1:M){
   nind[i] = 0;
   for(t in 1:Nmax){
     if(times[i,t]>=0) nind[i] +=1;
   }
 }
 
 // inizializzo n
 for(i in 1:M){
   
     for(k in 1:(K-1)){
       n[k,i] = 0;
     
     for(j in 1:nind[i]){
       
       if((times[i,j] > nodi[k]) && (times[i,j] <= nodi[k+1])){
          n[k,i] += 1;
          }
     }
   }
 }
 // inizializzo npunto
 for(k in 1:(K-1)) {
   npunto[k] = sum(n[k,]);
}

}

parameters {
  vector<lower=0>[K-1] lambda; // logaritmo valori dei rates
  vector[p] beta; // regression parameters
  vector<lower=0>[M] w; // log-frailties
  real<lower=0> eta; // variance of w[i] i=1,...,M
  real<lower=0> w_new; 
}

transformed parameters {
vector[M] coeffs;
coeffs = X*beta;
}

model{
  target += sum(npunto .* log(lambda)) + sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda)); //log-likelihood processo, vedi cook-lawless 3.3
 
 target += gamma_lpdf(eta | a_eta, b_eta);
 for (i in 1:K-1){
    target += gamma_lpdf(lambda[k] | lambda_star*r[k], r[k]);
 }
 target += exponential_lpdf(lambda_star | base);
 target += gamma_lpdf(w | 1/eta, 1/eta);
 target += normal_lpdf( beta | 0, sigma_b);
 target += gamma_lpdf(w_new | 1/eta, 1/eta);
}

generated quantities{
  real sum_log_lik;
  vector[M] log_lik;
  
  for(i in 1:M){
     log_lik[i] = sum(n[,i])*(log(w_new)+coeffs[i]);
      for(k in 1:(K-1)){
   log_lik[i] += n[k,i]*log(lambda[k]) - w_new*exp(coeffs[i])*tau[i,k]*lambda[k];
}
}
  sum_log_lik = sum(npunto .* log(lambda)) + sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda));
}
