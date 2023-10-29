data {
  int<lower=0> N;
  int<lower=0> J;
  vector[J] y[N];
}

parameters {
  vector[J] mu;
  real<lower=0> sigma;
  real theta;
  real<lower=0> alpha;
}

model {
  theta ~ normal(95, 20);
  alpha ~ student_t(4, 0, 20);
  // priors
  for (j in 1:J){
    mu[j] ~ normal(theta, alpha);
    sigma ~ inv_chi_square(10);
  }

  // likelihood
  for (j in 1:J){
    y[,j] ~ normal(mu[j], sigma);
  }
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
  vector[J] log_lik[N];
  // Compute predictive distribution
  ypred1 = normal_rng(mu[1], sigma);
  ypred2 = normal_rng(mu[2], sigma);
  ypred3 = normal_rng(mu[3], sigma);
  ypred4 = normal_rng(mu[4], sigma);
  ypred5 = normal_rng(mu[5], sigma);
  ypred6 = normal_rng(mu[6], sigma);
  mu_pred7 = normal_rng(theta, alpha);
  ypred7 = normal_rng(mu_pred7, sigma);
  // assignment 8 loo
  for (j in 1:J) {
    for (n in 1:N) {
      log_lik[n, j] = normal_lpdf(y[n,j] | mu[j], sigma);
    }
  }
}
