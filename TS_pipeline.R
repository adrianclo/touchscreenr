source("TS_functions_v2.R")

# example ----------------------------------------------------------------------
tic()

# IMPORT AND SIMPLIFY
sample <- format2essential(filesDir = file.path(getwd(), "pal_data"), 
                           wt = c("341-blue"), 
                           ko = c("341-red"))
sample <- format2dataframe(sample)
names(sample)

# View(sample$details)
# View(sample$df)

# EXPORT MASTER FILE
saveRDS(sample, "sample-pal-data.RDS")

# PARAMETERS
accuracy <- extract_accuracy(sample)
activity <- extract_activity(sample)
latency <- extract_latency(sample)
collectionerror <- extract_rewardCollectionError(sample)
screentouches <- extract_screenTouches(sample)
trialtime <- extract_trialTime(sample)

sequence <- extract_sequence(sample)

grid.arrange(
    process_sequence(seq = sequence, details = sample, test = "Mouse dPAL v3", start = 1, end = 5, title = "Acquisition 1-5"),
    process_sequence(seq = sequence, details = sample, test = "Mouse dPAL v3", start = 15, end = 20, title = "Acquisition 15-20"),
    process_sequence(seq = sequence, details = sample, test = "Mouse dPAL reversal", start = 1, end = 5, title = "Reversal 1-5"),
    process_sequence(seq = sequence, details = sample, test = "Mouse dPAL reversal", start = 25, end = 30, title = "Reversal 25-30"),
    ncol = 2, nrow = 2
)
    
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

writexl::write_xlsx(sample_summary, "sample-pal-summary.xlsx")
View(sample_summary)

toc() # n = 2; 2 x 50 csv files ~ 134 s


