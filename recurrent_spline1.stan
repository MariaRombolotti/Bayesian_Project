functions {
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
    // INPUTS:
    //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t)) // B-splines of order 1 are piece-wise constant
        b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]);
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) /
             (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) /
                 (ext_knots[ind+order] - ext_knots[ind+1]);
      // Calculating the B-spline recursively as linear interpolation of two lower-order splines
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) +
                 w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
}

data{
  int<lower=1> M; // numero individui 
  int<lower=1> Nmax; // numero massimo eventi individuo 
  int<lower=1> Smax; // numero massimo intervalli non osservabilità
  int<lower=2> K; // numero nodi 
  int<lower=0> p; // numero covariate - intero
  
  matrix[M,Nmax] times; // tempi eventi (negativo se non avvenuta)
  vector[K] nodi; // nodi 
  matrix[M,p] X; // design matrix 
  matrix[M,Smax] inizio; // inizio non osservabilità
  matrix[M,Smax] fine; // fine non osservabilità
  
  real<lower=0> a_eta; // shape parameter eta
  real<lower=0> a_lambda; // shape parameter lambda_k
  real<lower=0> b_eta; // scale parameter eta
  real<lower=0> b_lambda; // scale parameter lambda_k
  real<lower=0> sigma_b; // varianza beta
  
  // B splines function inputs
  // int num_data;             // number of data points
  int num_knots;            // num of knots
  vector[num_knots] knots;  // the sequence of knots
  int spline_degree;        // the degree of spline (is equal to order - 1)
  vector[num_knots+2] ext_knots;
  int ind;
  int order;
  //vector<lower=0>[K-1] lambda;
  
  
  // real Y[num_data];
  // real X[num_data];
  }


transformed data{
 matrix[K-1,M] n;  // numero eventi in ogni intervallo di ogni individuo
 vector[K-1] npunto;  // numero di eventi totale per ogni intervallo
 int nind[M]; // numero eventi per ogni individuo
 matrix[M,(K-1)] tau; // tempo di osservazione in ogni intervallo
 matrix[M,(K-1)] tau2; // tempo di osservazione in ogni intervallo
// real molt;
// real molt2;
// real somma;
// int i;
// int k;
// int j;
//vector<lower=0>[K-1] lambda;
// matrix[M,Nmax] b_spline;
//
 for(i in 1:M){
   for(k in 1:(K-1)){
     tau[i,k] = (nodi[k+1])^2/2-(nodi[k])^2/2;
     for(j in 1:Smax){
       tau[i,k] = tau[i,k] - fmax((fmin(nodi[k+1],fine[i,j]))^2/2-(fmax(nodi[k],inizio[i,j]))^2/2,0); 
     }
   }
 }

// 
//
//// termine likelihood
//for (c in 1:M){
//  b_spline[c,:] = to_row_vector(build_b_spline(to_array_1d(times[c,:]),  to_array_1d(ext_knots), ind, order));
//}
//
//molt = 1;
//molt2 = 1;
//k = 1;
//i = 1;
//j = 1;
//while(i <= 5){
//  j = 1;
//  while(j <= Nmax){
//    somma = 0;
//    k = 1;
//    while(k <= K-1){
//      if (times[i,j] > 0){
//        somma = somma + lambda[k]*b_spline[i,j];
//      }
//      k = k+1;
//    }
//    if(somma>0){
//    molt = molt * somma;
//    }
//    j = j+1;
//  }
//  molt2 = molt2 * molt;
//  i = i+1;
//}
 
// inizializzo nind
 for(d in 1:M){
   nind[d] = 0;
   for(t in 1:Nmax){
     if(times[d,t]>=0) nind[d] +=1;
   }
 }
 
 // inizializzo n
 for(d in 1:M){
   
     for(t in 1:(K-1)){
       n[t,d] = 0;
     
     for(c in 1:nind[d]){
       
       if((times[d,c] > nodi[t]) && (times[d,c] <= nodi[t+1])){
          n[t,d] += 1;
          }
     }
   }
 }
 // inizializzo npunto
 for(c in 1:(K-1)) {
   npunto[c] = sum(n[c,]);
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
 real molt;
 real molt2;
 real somma;
 int i;
 int k;
 int j;
 //vector<lower=0>[K-1] lambda;
 matrix[M,Nmax] b_spline;
 

// termine likelihood
for (c in 1:M){
  b_spline[c,:] = to_row_vector(build_b_spline(to_array_1d(times[c,:]),  to_array_1d(ext_knots), ind, order));
}

molt = 1;
molt2 = 1;
k = 1;
i = 1;
j = 1;
while(i <= 5){
  j = 1;
  while(j <= Nmax){
    somma = 0;
    k = 1;
    while(k <= K-1){
      if (times[i,j] > 0){
        somma = somma + lambda[k]*b_spline[i,j];
      }
      k = k+1;
    }
    if(somma>0){
    molt = molt * somma;
    }
    j = j+1;
  }
  molt2 = molt2 * molt;
  i = i+1;
}
 target += log(molt2) + sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda)); //log-likelihood processo, vedi cook-lawless 3.3
 //target += sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda)); //log-likelihood processo, vedi cook-lawless 3.3

 target += gamma_lpdf(eta | a_eta, b_eta);
 target += gamma_lpdf(lambda | a_lambda, b_lambda);
 target += gamma_lpdf(w | 1/eta, 1/eta);
 target += normal_lpdf( beta | 0, sigma_b);
 target += gamma_lpdf(w_new | 1/eta, 1/eta);
}

//generated quantities{
//  real sum_log_lik;
//  vector[M] log_lik;
//  
//  for(i in 1:M){
//     log_lik[i] = sum(n[,i])*(log(w_new)+coeffs[i]);
//      for(k in 1:(K-1)){
//   log_lik[i] += n[k,i]*log(lambda[k]) - w_new*exp(coeffs[i])*tau[i,k]*lambda[k];
//}
//}
//  //sum_log_lik = sum(npunto .* log(lambda)) + sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda));
//    sum_log_lik= log(molt2) + sum(n * log(w)) + sum( n * (X * beta)) - sum((w .* exp(X * beta)) .* (tau * lambda))
//}
