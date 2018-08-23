
# Main data sources

- AFL tables for the past results https://afltables.com/afl/afl_index.html
- current schedule from https://fixturedownload.com
- Expert tipster data from Herald Sun [No longer used anymore]


# Updating the model data weekly

- Download the csv file from https://fixturedownload.com and copy it to the data folder. Filename looks like afl.2018.AUSEasternStandardTime
- As of lately herald sun expert tips data is not available publicly so the following is not applicable.
- Get the herald sun expert tipsters data (google for herald sun expert tips with the round number)
    - Edit the download_expert_tips_run_script.R in src folder and add the new link and edit the round and link_str. 
    - Run the script and make sure that the expert_tips variable looks OK. The script would save the results to an RDS file.
- Run clear.cache() and (re)run 
    - library(ProjectTemplate)
    - load.project()


# Run retro analysis for the previous rounds
 - Run src/reto_score_weighted_expert_tips.R
 - Run src/retro_score_simple_elo.R (make sure to update the round to the most recent completed round)
 - Run the ensemble models
    - logistic: Run src/retro_score_ensemble_logistic.R after editing the round
    - Bayesian: Run src/retro_score_ensemble_bayesian.R after editing the round
    
# Plot graphs to compare different models for the current season
  - Run profiling/compare_models_historical_head_to_head.R

# Run the predictions for the current round 
  - Run src/season_2018_predictions.R after editing the round