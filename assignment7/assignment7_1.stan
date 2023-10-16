data {
    int<lower=0> N;  // number of data points
    vector[N] x;     // observation year
    vector[N] y;     // observation number of drowned
    real xpred;      // prediction year
}
parameters {
    real alpha;
    real beta;
    real<lower=0> sigma; // fixed upper to lower, sigma can only take values>0 
}
transformed parameters {
    vector[N] mu = alpha + beta*x;
}
model {
    y ~ normal(mu, sigma); // fixed by adding ;
    beta ~ normal(0, 27); // added the beta prior here 
    alpha ~ normal(134, 35); // added the alpha prior here
}
generated quantities {
    real ypred = normal_rng(alpha + beta*xpred, sigma); //fixed using xpred instead of x
}
