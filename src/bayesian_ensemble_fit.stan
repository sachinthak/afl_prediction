data{
  int<lower=2> num_models;
  vector<lower=0>[num_models] alpha;
  int<lower=1> N;
  real<lower=0,upper=1> simple_elo[N];
  real<lower=0,upper=1> weighted_expert_tips[N];
  int<lower=0,upper=1>  team1_won[N]; // dependent variable
}

parameters{
  simplex[num_models] theta;
}

model {
  real p[N];
  
  theta ~ dirichlet(alpha);
  
  
  
  for (n in 1:N){
    p[n] = theta[1]*simple_elo[n] + theta[2]*weighted_expert_tips[n];
    team1_won[n] ~ bernoulli_logit(p[n]);
  }
  
}