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
  mu ~ normal(0, 10);
  sigma ~ gamma(1, 1);

  // likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  real ypred;
  // Compute predictive distribution
  ypred = normal_rng(mu, sigma);
}
