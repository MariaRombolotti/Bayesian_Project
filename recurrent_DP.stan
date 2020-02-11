data{
  int<lower=1> M; // numero individui 
  int<lower=1> Nmax; // numero massimo eventi individuo 
  int<lower=1> Smax; // numero massimo intervalli non osservabilità
  int<lower=2> K; // numero nodi 
  int<lower=0> p; // numero covariate - intero
  int<lower=0> C;//num of cluster    *****
  
  matrix[M,Nmax] times; // tempi eventi (negativo se non avvenuta)
  vector[K] nodi; // nodi 
  matrix[M,p] X; // design matrix 
  matrix[M,Smax] inizio; // inizio non osservabilità
  matrix[M,Smax] fine; // fine non osservabilità
  
  //real<lower=0> a_eta; // shape parameter eta
  real<lower=0> a_lambda; // shape parameter lambda_k
  //real<lower=0> b_eta; // scale parameter eta
  real<lower=0> b_lambda; // scale parameter lambda_k
  real<lower=0> sigma_b;// varianza beta    
  real <lower=0> a_sigma; //parameter alpha of the gamma for sigma   ****
  real <lower=0> b_sigma; //parameter beta of the gamma for sigma   ****
  real <lower=0> mu_mu;// alfa of the gamma G_0   *****
  real <lower=0> sigma_mu;//beta of the gamma G_0  *****
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
  vector<lower=0>[M] w; //log-frailties
  //vector<lower=0>[C] w_star; // log-frailties
  //real<lower=0> eta; // variance of w[i] i=1,...,M
  //real<lower=0> w_new; 
  
 
  real <lower=0> mu_cl[C]; //cluster mean  ****
  real <lower=0,upper=1> v[C];  //***
  real<lower=0> sigma_cl[C]; // error scale
  real<lower=0> alpha; // hyper prior DP(alpha,base)
}

transformed parameters {
vector[M] coeffs;
simplex[C] pi;

coeffs = X*beta;
  pi[1] = v[1];
  // stick-break process based on The BUGS book Chapter 11 (p.294)
  for(j in 2:(C-1)){
      pi[j]= v[j]*(1-v[j-1])*pi[j-1]/v[j-1]; 
  }
  pi[C]=1-sum(pi[1:(C-1)]); // to make a simplex.
}

model{
  real ps[C];
  sigma_cl ~ gamma(a_sigma, b_sigma);
  mu_cl ~ gamma(mu_mu, sigma_mu);
  alpha ~ gamma(6, 1);
  v ~ beta(1, alpha); 

  target += sum(npunto .* log(lambda)) + sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda)); //log-likelihood processo, vedi cook-lawless 3.3
  //target += sum(npunto .* log(lambda)) + sum( n * (X * beta)); //log-likelihood processo, vedi cook-lawless 3.3
 for(i in 1:M){
    for(c in 1:C){
      ps[c]=log(pi[c])+gamma_lpdf(w[i]|mu_cl[c],sigma_cl[c]);
      //ps[c] = log(pi[c])+n * log(w_star[c]) - sum(w_star[c]*exp(X[i,:] * beta) * (tau[i,:] * lambda)); 
    
    }
    target += log_sum_exp(ps);
  }
 //target += gamma_lpdf(eta | a_eta, b_eta);
 target += gamma_lpdf(lambda | a_lambda, b_lambda);
 //target += gamma_lpdf(w | 1/eta, 1/eta);
 
 target += normal_lpdf( beta | 0, sigma_b);
 //target += gamma_lpdf(w_new | 1/eta, 1/eta);
 

   
}
generated quantities{
  real sum_log_lik;
  vector[M] log_lik;
  
  for(i in 1:M){
     log_lik[i] = sum(n[,i])*(log(w[i])+coeffs[i]);
      for(k in 1:(K-1)){
   log_lik[i] += n[k,i]*log(lambda[k]) - w[i]*exp(coeffs[i])*tau[i,k]*lambda[k];
}
}
  sum_log_lik = sum(npunto .* log(lambda)) + sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda));
}

