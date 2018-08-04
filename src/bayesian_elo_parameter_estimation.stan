data{
  int n_matches;
  int n_rounds;
  int n_teams;
  int round_ids[n_matches];
  int team1_ids[n_matches];
  int team2_ids[n_matches];
  int team1_win_indictr[n_matches];
}

parameters{
  real<lower=0> K; // K factor of ELO
  real<lower=0> xi; // logsistic parameter of ELO
  vector[n_teams] elo_pre_season; // pre season elo
}

model{
  
  vector[n_teams] elo_score[n_rounds]; // each round is a vector of elo scores
  
  /* define the priors  */
  
  /* pre season elo */
  for (n in 1:n_teams){
    elo_pre_season[n] ~ normal(1500,100);
  }
  
  /* pre populate elo_score array with elo_pre_season to handle a situatation 
  that not all teams start in round 1 */
  for (n in 1:n_rounds)
    elo_score[n] = elo_pre_season;
  
  
  K ~ normal(100,50);
  xi ~ normal(400,100);
  
  for (match in 1:n_matches){
    int team1_id = team1_ids[match];
    int team2_id = team2_ids[match];
    int rnd_id = round_ids[match];
    
    // some variable declarations
    real prev_elo_team1;
    real prev_elo_team2;
    real elo_diff;
    real d;
    real logit_inv_d;
    real elo_delta_team1;
    
    
    
    // get the previous elo for each team
    if (rnd_id == 1){
       prev_elo_team1 = elo_pre_season[team1_id];
       prev_elo_team2 = elo_pre_season[team2_id];
    }else {
       prev_elo_team1 = elo_score[rnd_id-1][team1_id];
       prev_elo_team2 = elo_score[rnd_id-1][team2_id];
    }
    
    elo_diff = prev_elo_team1-prev_elo_team2; 
    d = elo_diff/xi;
    logit_inv_d = 1/(1+ 10^(-d));
    
    // modify the elo score for team 1
    elo_delta_team1 = K*(team1_win_indictr[match]-logit_inv_d);
    elo_score[rnd_id][team1_id] = prev_elo_team1 + elo_delta_team1;
    elo_score[rnd_id][team2_id] = prev_elo_team2 - elo_delta_team1;
    
    /* increase the log probability */
    target +=  bernoulli_lpmf(team1_win_indictr[match] | logit_inv_d);

  }
}

generated quantities {
  vector[n_teams] elo_score[n_rounds]; // each round is a vector of elo scores
  
  for (n in 1:n_rounds)
    elo_score[n] = elo_pre_season;
    
  for (match in 1:n_matches){
    int team1_id = team1_ids[match];
    int team2_id = team2_ids[match];
    int rnd_id = round_ids[match];
    
    // some variable declarations
    real prev_elo_team1;
    real prev_elo_team2;
    real elo_diff;
    real d;
    real logit_inv_d;
    real elo_delta_team1;
    
    
    
    // get the previous elo for each team
    if (rnd_id == 1){
       prev_elo_team1 = elo_pre_season[team1_id];
       prev_elo_team2 = elo_pre_season[team2_id];
    }else {
       prev_elo_team1 = elo_score[rnd_id-1][team1_id];
       prev_elo_team2 = elo_score[rnd_id-1][team2_id];
    }
    
    elo_diff = prev_elo_team1-prev_elo_team2; 
    d = elo_diff/xi;
    logit_inv_d = 1/(1+ 10^(-d));
    
    // modify the elo score for team 1
    elo_delta_team1 = K*(team1_win_indictr[match]-logit_inv_d);
    elo_score[rnd_id][team1_id] = prev_elo_team1 + elo_delta_team1;
    elo_score[rnd_id][team2_id] = prev_elo_team2 - elo_delta_team1;
  }
}