
# load retro scores from differenet models for comparison -----------------

files <- list.files('data/',pattern = '^retro_score.*.RDS')
retro_scores <- rbindlist(lapply(files, function(file){readRDS(paste0('data/',file))}))

