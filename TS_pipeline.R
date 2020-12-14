source("TS_functions_v2.R") # only experimental data

# SETTINGS ---------------------------------------------------------------------
filesDir <- "DIRECTORY/OF/RAW/DATA/FILES"

wt <- c("mouse1","mouse2")
ko <- c("mouse3","mouse4")
ki <- NULL
het <- NULL

exportName <- "my-favourite-pal"

# WHEN ALL SETTINGS ARE FILLED IN, PRESS "SOURCE"
# A BEEP WILL SIGNAL WHEN THE PROCESSING IS FINISHED

# PIPELINE ---------------------------------------------------------------------
tic()
# IMPORT AND SIMPLIFY
sample <- format2essential(filesDir = filesDir, 
                           wt = wt, 
                           ko = ko,
                           het = het,
                           ki = ki)
sample <- format2dataframe(sample)
names(sample)

# View(sample$details)
# View(sample$df)

# EXPORT MASTER FILE
saveRDS(sample, paste0(exportName, ".RDS"))

# PARAMETERS
accuracy <- extract_accuracy(sample)
activity <- extract_activity(sample)
latency <- extract_latency(sample)
collectionerror <- extract_rewardCollectionError(sample)
screentouches <- extract_screenTouches(sample)
trialtime <- extract_trialTime(sample)

sequence <- extract_sequence(sample)

# grid.arrange(
#     process_sequence(seq = sequence, details = sample, test = "Mouse dPAL v3", start = 1, end = 5, title = "Acquisition 1-5"),
#     process_sequence(seq = sequence, details = sample, test = "Mouse dPAL v3", start = 15, end = 20, title = "Acquisition 15-20"),
#     process_sequence(seq = sequence, details = sample, test = "Mouse dPAL reversal", start = 1, end = 5, title = "Reversal 1-5"),
#     process_sequence(seq = sequence, details = sample, test = "Mouse dPAL reversal", start = 25, end = 30, title = "Reversal 25-30"),
#     ncol = 2, nrow = 2
# )
    
# EXPORT SUMMARY FILE
sample_summary <- sample$details %>% 
    arrange(desc(protocol), animalID, day) %>% 
    left_join(accuracy, by = "fileName") %>% 
    left_join(activity, by = "fileName") %>% 
    left_join(latency, by = "fileName") %>% 
    left_join(collectionerror, by = "fileName") %>% 
    left_join(screentouches, by = "fileName") %>% 
    left_join(trialtime, by = "fileName") %>% 
    left_join(sequence$seqData, by = "fileName")

writexl::write_xlsx(sample_summary, paste0(exportName, ".xlsx"))
beep(2)
toc()