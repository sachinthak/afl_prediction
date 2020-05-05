functions {
  
  // sort by the points ladder and return the top 8 teams. Incase of a tie use the for_against_ratio to break the tie
  int[] return_final_8_teams(int[] points_arr, real[] for_against_ratio_arr, int n_teams){
    int sorted_team_ids[n_teams];
    int points_arr_tmp[n_teams] = points_arr;
    real for_against_ratio_arr_tmp[n_teams] = for_against_ratio_arr;
    int final_8[8];
    
    for (n in 1:n_teams)
      sorted_team_ids[n] = n;
    
    for (t1 in 1:n_teams-1)
      for (t2 in t1+1:n_teams)
        if (points_arr_tmp[t1]<points_arr_tmp[t2] || (points_arr_tmp[t1] == points_arr_tmp[t2] && for_against_ratio_arr_tmp[t1] < for_against_ratio_arr_tmp[t2])){
          // tmp variables used for swapping;
          int t_ind;
          real t_real;
          
          // swap the team indices
          t_ind = sorted_team_ids[t1];
          sorted_team_ids[t1] = sorted_team_ids[t2];
          sorted_team_ids[t2] = t_ind;
          
          // swap the points
          t_ind = points_arr_tmp[t1];
          points_arr_tmp[t1] = points_arr_tmp[t2];
          points_arr_tmp[t2] = t_ind;
          
          // swap the  for_against_ratio
          t_real = for_against_ratio_arr_tmp[t1];
          for_against_ratio_arr_tmp[t1] = for_against_ratio_arr_tmp[t2];
          for_against_ratio_arr_tmp[t2] = t_real;
        }
    
    // generate the output to return
    for (n in 1:8)
      final_8[n] = sorted_team_ids[n];
    
    return(final_8);
  }
  
  int[] return_team_ids_for_a_final_series_game(int[] final_8_arr,int[] semi_final_4_arr,int[] prelim_final_4_arr,int[] final_2_arr,int round_type){
    int two_team_ids[2];  
    
    if (round_type == -1){ // QF 1 
      two_team_ids[1] = final_8_arr[1];
      two_team_ids[2] = final_8_arr[4];
    }
    
    if (round_type == -2){ // QF 2
      two_team_ids[1] = final_8_arr[2];
      two_team_ids[2] = final_8_arr[3];
    }
    
    if (round_type == -3){ // EF 1 
      two_team_ids[1] = final_8_arr[5];
      two_team_ids[2] = final_8_arr[8];
    }
    
    if (round_type == -4){ // EF 2
      two_team_ids[1] = final_8_arr[6];
      two_team_ids[2] = final_8_arr[7];
    }
    
    if (round_type == -5){ // SF 1
      two_team_ids[1] = semi_final_4_arr[1];
      two_team_ids[2] = semi_final_4_arr[2];
    }
    
    if (round_type == -6){ // SF 2
      two_team_ids[1] = semi_final_4_arr[3];
      two_team_ids[2] = semi_final_4_arr[4];
    }
    
    if (round_type == -7){ // PF 1
      two_team_ids[1] = prelim_final_4_arr[1];
      two_team_ids[2] = prelim_final_4_arr[2];
    }
    
    if (round_type == -8){ // PF 2
      two_team_ids[1] = prelim_final_4_arr[3];
      two_team_ids[2] = prelim_final_4_arr[4];
    }
    
    if (round_type == -9) // GF
      two_team_ids = final_2_arr;
    
    return(two_team_ids); 
  }
}

data{
  
  int n_matches;
  int n_rounds;
  int n_teams;
  int round_ids[n_matches];
  int team1_ids[n_matches];
  int team2_ids[n_matches];
  int team1_win_indictr[n_matches];

  /* futr_ variables are used to simulate the upcoming scheduled matches */
  int futr_n_matches;
  int futr_team1_ids[futr_n_matches];
  int futr_team2_ids[futr_n_matches];
  int futr_n_rounds;
  int first_futr_round;
  int futr_round_ids[futr_n_matches];
  
  int final_8_fixed; // used in simulating the future matches
  int semi_final_4_fixed; // used in simulating the future matches
  int prelim_final_4_fixed; // used in simulating the future matches
  int final_2_fixed; // used in simulating the future matches
  int premiership_team_fixed; // used in simulating the future matches
  
  int final_8_team_ids_input[8]; // if the final 8 are determined we pass in the team ids
  int semi_final_4_team_ids_input[4]; // if the semi final 4 are determined we pass in the team ids
  int prelim_final_4_team_ids_input[4]; // if the prelim final 4 are determined we pass in the team ids
  int final_2_team_ids_input[2]; // if the final 2 are determined we pass in the team ids
  int premiership_team_id_input; // if the final 8 are determined we pass in the team ids
  
  int futr_rnd_type[futr_n_matches];
  int points_ladder[n_teams];
  real for_against_ratio[n_teams];
  
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
  
  # default 
  K ~ normal(100,50);
  xi ~ normal(400,100);
  
  # sensitiyivity analysis case 1 - try with  log normal prior 
   
   #K ~ lognormal(log(100^2/sqrt(100^2+50^2)),sqrt(log(1 + 50^2/100^2)));
   #xi ~ lognormal(log(400^2/sqrt(400^2+100^2)),sqrt(log(1 + 100^2/400^2))); 

  # sensitiyivity analysis case 2 - student t with same mean
  #K ~ student_t(3,100,50);
  #xi ~ student_t(3,400,100); 
  

  # sensitiyivity analysis case 3 - student t with shifted mean and wide tail
  #K ~ student_t(3,80,100);
  #xi ~ student_t(3,200,200); 

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
    
    /* modify the elo score for team 1 and team 2. Will assign the modified value for all 
    the future rounds. This is to handle the situations where some teams may not play in 
    the imediate next rounds(s) */
    elo_delta_team1 = K*(team1_win_indictr[match]-logit_inv_d);
    for (rnd in rnd_id:n_rounds){
      elo_score[rnd][team1_id] = prev_elo_team1 + elo_delta_team1;
      elo_score[rnd][team2_id] = prev_elo_team2 - elo_delta_team1;
    }
    
    
    /* increase the log probability */
    target +=  bernoulli_lpmf(team1_win_indictr[match] | logit_inv_d);

  }
}

generated quantities {
 
  vector[n_teams] elo_score[n_rounds]; // each round is a vector of elo scores
  vector[n_teams] last_elo_score; // used to store 
  int futr_match_outcome[futr_n_matches]; // 1 if team 1 wins 0 otherwise
  vector[n_teams] futr_elo_score[futr_n_rounds]; // to store upcoming simulated elo scores
  
  int final8_sim[8]; // structure to store the team ids of final 8 (ordered by highest to lowest points)
  int semi_final4_sim[4]; // structure to store the team ids of semi final 4
  int prelim_final4_sim[4]; // structure to store the team ids of prelim final 4
  int final2_sim[2]; // structure to store the team ids of final 2
  int premiership_sim; // structure to store the premiership winner
  
  int final8_fixed_var = final_8_fixed; // dummy variable
  int futr_points_ladder[n_teams]; // to store the points for the simulated matches
  
  // if we have already supplied finals series teams then pre-poluate the above data structures
  if (final_8_fixed == 1)
    final8_sim = final_8_team_ids_input;
  if (semi_final_4_fixed == 1)
    semi_final4_sim = semi_final_4_team_ids_input;
  if (prelim_final_4_fixed == 1)
    prelim_final4_sim = prelim_final_4_team_ids_input;    
  if (final_2_fixed == 1)
    final2_sim = final_2_team_ids_input;  
  if (premiership_team_fixed == 1)
    premiership_sim = premiership_team_id_input;
  
  
  
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
    
    /* modify the elo score for team 1 and team 2. Will assign the modified value for all 
    the future rounds. This is to handle the situations where some teams may not play in 
    the imediate next rounds(s) */
    elo_delta_team1 = K*(team1_win_indictr[match]-logit_inv_d);
    for (rnd in rnd_id:n_rounds){
      elo_score[rnd][team1_id] = prev_elo_team1 + elo_delta_team1;
      elo_score[rnd][team2_id] = prev_elo_team2 - elo_delta_team1;
    }
  }
  
  /* simulate the rest of the tournament */
  last_elo_score = elo_score[n_rounds];
   
  // initialise the team points to the accumulated points from the actual matches played
  futr_points_ladder = points_ladder;
   
  for (n in 1:futr_n_rounds)
   futr_elo_score[n] = last_elo_score;
    
  for (match in 1:futr_n_matches){
    
    // some variable declarations 
    int team1_id;
    int team2_id;
    int winner_team_id;
    int loser_team_id;
  
    real prev_elo_team1;
    real prev_elo_team2;
    real elo_diff;
    real d;
    real logit_inv_d;
    real elo_delta_team1;
    
    int rnd_id = futr_round_ids[match]-first_futr_round+1; // offseting the rounds that have happends so far 
    
    // check if this is this is the first week of the finals series (i.e. qualifying finals
    // or elimination finals). if so if we havent already supplied the final 8 then determine the final 8 now.
    if (futr_rnd_type[match] < 0 && futr_rnd_type[match] > -5  && final8_fixed_var == 0){
      final8_sim = return_final_8_teams(futr_points_ladder,for_against_ratio,n_teams);
      final8_fixed_var = 1;
    }
    
    
    // get the two team ids
    if (futr_rnd_type[match] == 0){ // regular round
     team1_id = futr_team1_ids[match];
     team2_id = futr_team2_ids[match];
    } else { // a final series match
      int two_team_ids[2] = return_team_ids_for_a_final_series_game(final8_sim,semi_final4_sim,
      prelim_final4_sim,final2_sim,futr_rnd_type[match]);
      team1_id = two_team_ids[1];
      team2_id = two_team_ids[2];
    }
    
   
    // get the previous elo for each team
    if (rnd_id == 1){
       prev_elo_team1 = last_elo_score[team1_id];
       prev_elo_team2 = last_elo_score[team2_id];
    }else {
       prev_elo_team1 = futr_elo_score[rnd_id-1][team1_id];
       prev_elo_team2 = futr_elo_score[rnd_id-1][team2_id];
    }
    
    elo_diff = prev_elo_team1-prev_elo_team2; 
    d = elo_diff/xi;
    logit_inv_d = 1/(1+ 10^(-d));
    
    // simulate a random match outcome
    futr_match_outcome[match] = bernoulli_rng(logit_inv_d);
    
    /* modify the elo score for team 1 and team 2. Will assign the modified value for all 
    the future rounds. This is to handle the situations where some teams may not play in 
    the imediate next rounds(s) */
    elo_delta_team1 = K*(futr_match_outcome[match]-logit_inv_d);
    for (rnd in rnd_id:futr_n_rounds){
      futr_elo_score[rnd][team1_id] = prev_elo_team1 + elo_delta_team1;
      futr_elo_score[rnd][team2_id] = prev_elo_team2 - elo_delta_team1;
    }
   
    // for convinience work out the winner/loser team ids
    if (futr_match_outcome[match]){
      winner_team_id = team1_id;
      loser_team_id = team2_id;
    } else {
      winner_team_id = team2_id;
      loser_team_id = team1_id;
    }
    
    // accumulate points
    futr_points_ladder[winner_team_id] += 4; 
      
  
    // update the placeholders if it is  QF1
    if (futr_rnd_type[match] == -1){ 
      prelim_final4_sim[1] = winner_team_id;
      semi_final4_sim[1]  = loser_team_id;
    }
    
    // update the placeholders if it is  QF2
    if (futr_rnd_type[match] == -2){ 
      prelim_final4_sim[3] = winner_team_id;
      semi_final4_sim[3]  = loser_team_id;
    }
    
    // update the placeholders if it is  EF1
    if (futr_rnd_type[match] == -3)
      semi_final4_sim[2] = winner_team_id;
    
    // update the placeholders if it is  EF2
    if (futr_rnd_type[match] == -4)
      semi_final4_sim[4] = winner_team_id;
    
    // update the placeholders if it is  SF1
    if (futr_rnd_type[match] == -5)
      prelim_final4_sim[4] = winner_team_id;
    
    // update the placeholders if it is  SF2
    if (futr_rnd_type[match] == -6)
      prelim_final4_sim[2] = winner_team_id;
    
    // update the placeholders if it is  PF1
    if (futr_rnd_type[match] == -7)
      final2_sim[1] = winner_team_id;
    
    // update the placeholders if it is  PF2
    if (futr_rnd_type[match] == -8)
      final2_sim[2] = winner_team_id;
    
    // update the placeholders if it is  GF
    if (futr_rnd_type[match] == -9)
      premiership_sim = winner_team_id;
   
}
}