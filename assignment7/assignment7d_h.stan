data {
  int<lower=0> N;
  int<lower=0> J;
  vector[J] y[N];
}

parameters {
  vector[J] mu;
  vector<lower=0>[J] sigma;
  real mu_p;
  real<lower=0> tau;
}

model {
  mu_p ~ normal(0, 10);
  tau ~ gamma(1, 1);
  // priors
  for (j in 1:J){
    mu[j] ~ normal(mu_p, tau);
    sigma[j] ~ gamma(1, 1);
  }

  // likelihood
  for (j in 1:J)
    y[,j] ~ normal(mu[j], sigma[j]);
}

generated quantities {
  real ypred1;
  real ypred2;
  real ypred3;
  real ypred4;
  real ypred5;
  real ypred6;
  real ypred7;
  real mu_pred7;
  real sigma_pred7;
  // Compute predictive distribution
  ypred1 = normal_rng(mu[1], sigma[1]);
  ypred2 = normal_rng(mu[2], sigma[2]);
  ypred3 = normal_rng(mu[3], sigma[3]);
  ypred4 = normal_rng(mu[4], sigma[4]);
  ypred5 = normal_rng(mu[5], sigma[5]);
  ypred6 = normal_rng(mu[6], sigma[6]);
  mu_pred7 = normal_rng(mu_p, tau);
  sigma_pred7 = inv_chi_square_rng(10);
  ypred7 = normal_rng(mu_pred7, sigma_pred7);
}
