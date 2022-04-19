
## Bayesian Elo

For the implementation of Bayesian Elo as described in the [blog](https://blog.sach-k.com/2020/04/11/Bayesian-ELO-Part1/) post(s) , refer to src/bayesian_elo_parameter_estimation.R 

You would need ProjectTemplate installed to easily load data sets and source functions. Once the package is installed run
    - library(ProjectTemplate)
    - load.project() 
to set up the environment.

## Steps I followed during running the model(s) on a weekly basis during the 2018 season

###  Main data sources

- AFL tables for the past results https://afltables.com/afl/afl_index.html
- current schedule from https://fixturedownload.com
- Expert tipster data from Herald Sun [No longer used anymore]


### Updating the model data weekly

- Download the csv file from https://fixturedownload.com and copy it to the data folder. Filename looks like afl.2018.AUSEasternStandardTime
- As of lately herald sun expert tips data is not available publicly so the following is not applicable.
- Get the herald sun expert tipsters data (google for herald sun expert tips with the round number)
    - Edit the download_expert_tips_run_script.R in src folder and add the new link and edit the round and link_str. 
    - Run the script and make sure that the expert_tips variable looks OK. The script would save the results to an RDS file.
- Run clear.cache() and (re)run 
    - library(ProjectTemplate)
    - load.project()


### Run retro analysis for the previous rounds
 - Run src/reto_score_weighted_expert_tips.R
 - Run src/retro_score_simple_elo.R (make sure to update the round to the most recent completed round)
 - Run the ensemble models
    - logistic: Run src/retro_score_ensemble_logistic.R after editing the round
    - Bayesian: Run src/retro_score_ensemble_bayesian.R after editing the round
    
###  Plot graphs to compare different models for the current season
  - Run profiling/compare_models_historical_head_to_head.R

### Run the predictions for the current round 
  - Run src/season_2018_predictions.R after editing the round
  
### update 2022
  - Once a year the data/past_results_xxxx.rds and data/schedules_xxxx.rds (where xxxx is the previous year) need to be updated
  - use src/generatre_past_results.rds to do that. 
  - you would need to download the afl-xxxx-AUSEasternStandardTime file from https://fixturedownload.com and copy it to the data folder
  - would need slight edits to src/generatre_past_results.rds but the changes should be obvious
