data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  // priors
  mu ~ normal(95, 20);
  sigma ~ inv_chi_square(10);

  // likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  real ypred;
  vector[N] log_lik;
  // Compute predictive distribution
  ypred = normal_rng(mu, sigma);
  
  for (i in 1:N){
    log_lik[i] = normal_lpdf(y[i] | mu, sigma);
  }
  
}
