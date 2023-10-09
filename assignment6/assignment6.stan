
data {
  // Biossay Data from BDA3
  int<lower=0> N;
  int<lower=0> y[N];
  vector[N] x;
  int<lower=0> n[N];
  
  // prior data
  vector[2] mu;
  matrix<lower=0>[2, 2] Sigma;
}


parameters {
  vector[2] beta;
}


transformed parameters {
  vector[N] theta = beta[1] + beta[2] * x;
}

model {
  y ~ binomial_logit(n, theta);
  beta ~ multi_normal(mu, Sigma);
}
