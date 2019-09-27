suppressMessages(library(dplyr))     # 0.7.4
suppressMessages(library(readxl))    # 1.1.0
suppressMessages(library(writexl))   # 0.2
suppressMessages(library(magrittr))  # 1.5.0
suppressMessages(library(ggplot2))   # 2.2.1
suppressMessages(library(tibble))    # 1.4.2
suppressMessages(library(stringr))   # 1.3.0
suppressMessages(library(gridExtra)) # 2.2.1
suppressMessages(library(extrafont)) # 0.17
suppressMessages(library(reshape2))  # 1.4.2
suppressMessages(library(readr))     # 1.3.1
suppressMessages(library(lubridate)) # 1.7.4
suppressMessages(library(tidyr))     # 0.8.3

dummy_dir = "G:/RESEARCH/CBA/PRIVATE/Adrian/data_Touchscreen/test files r script/"
dummy_wt = c(paste0("801-", c("blue","red","green","blue2","red2")),paste0("802-", c("blue","red","green","blue2","red2")),paste0("803-", c("blue","red","green","blue2")))
dummy_ko = c(paste0("804-", c("blue","red","green","blue2","red2")),paste0("805-", c("blue","red","green","blue2","red2")))

latest_file = function(machine = "work") {
  if(tolower(machine) == "work") {
    prefix = "C:/Users/Alo1/"
  } else if(tolower(machine) == "mac") {
    prefix = "/Users/adrianlo/"
  } else { prefix = "C:/Users/Adrian/" }
  
  latest = list.files(path = paste0(prefix,"Dropbox/work/DNF/data_touchscreen/2019-06-fmr1fvb/"),
                      pattern = ".RDS$") %>% tail(1)
  dat = readRDS(paste0(prefix,"Dropbox/work/DNF/data_touchscreen/2019-06-fmr1fvb/",latest))
  
  return(list(data = c(dat$ml1$data, dat$ml2$data),
              details = bind_rows(dat$ml1$details,dat$ml2$details),
              df = bind_rows(dat$df1,dat$df2)))
} # machine = c("home","work","mac")

se = function(x) sd(x, na.rm = T) / sqrt(length(x))

format2essential = function(filesDir = dummy_dir, 
                            wt = NULL, het = NULL, ko = NULL, ki = NULL,
                            exclude_files = NULL, include_files = NULL, exclude_mice = NULL) {
  
  if(!is.null(exclude_files)) {
    files = list.files(filesDir) %>% setdiff(exclude_files) # files = files[!(files %in% exclude_files)]
    nFiles = length(files)
  } else if(!is.null(include_files)) {
    files = list.files(filesDir) %>% intersect(include_files)
    nFiles = length(files)
  } else {
    files = list.files(filesDir)
    nFiles = length(files)
  }
  
  ## placeholder details and data
  details = tibble(animalID = character(nFiles), genotype = character(nFiles), 
                   protocol = character(nFiles), machineID = character(nFiles),
                   sessionID = numeric(nFiles), fileName = character(nFiles), 
                   date = character(nFiles))
  data = setNames(replicate(nFiles, tibble()), files)
  
  # ii = 1
  for(ii in 1:nFiles) {
    print(paste0("Simplifying ",ii,"/",nFiles," files: ",
                 files[ii],
                 ". Progress: ",round(ii / nFiles*100, 2),"%"))
    
    ## different raw file alignment depending on protocol
    temp = suppressWarnings(readLines(paste0(filesDir, files[ii]))) %>% str_detect("----------")
    identifier = which(cumsum(temp) == 1)[1]
    
    details_t = suppressMessages(read_csv(paste0(filesDir, files[ii]), n_max = identifier - 3))
    data_t = suppressMessages(read_csv(paste0(filesDir, files[ii]), skip = identifier))
    
    if(details_t %>% filter(Name == "Animal ID") %>% pull(Value) %in% exclude_mice) {
      next
    } else {
      ## format details item
      if(details_t %>% filter(Name == "Animal ID") %>% pull(Value) %>% length() == 0) {
        details$animalID[ii] = NA
      } else { details$animalID[ii] = details_t %>% filter(Name == "Animal ID") %>% pull(Value) }
      
      details$genotype[ii] = ifelse(details$animalID[ii] %in% wt, "WT",
                                    ifelse(details$animalID[ii] %in% het, "HET",
                                           ifelse(details$animalID[ii] %in% ko, "KO",
                                                  ifelse(details$animalID[ii] %in% ki, "KI", "No genotype"))))
      details$protocol[ii] = details_t %>% filter(Name == "Schedule") %>% pull(Value)
      details$machineID[ii] = details_t %>% filter(Name == "MachineName") %>% pull(Value)
      details$sessionID[ii] = details_t %>% filter(Name == "SessionId") %>% pull(Value)
      details$fileName[ii] = files[ii]
      details$date[ii] = details_t %>% filter(Name == "Date/Time") %>% pull(Value)
      
      ## format data item
      data[[ii]] =
        data_t %>%
        select(Evnt_Time, Evnt_Name, Item_Name, Alias_Name, Arg1_Value, Arg2_Name, Arg3_Value, Arg4_Value) %>%
        filter( 
          ## start of trial
          (Evnt_Name == "Condition Event" & Item_Name %in% c("Start Trial", "Start Correction Trial")) | 
            ## which images shown in which position 
            Alias_Name %in% c("Images", "Background") |
            ## images pressed at which coordinates / screen touches
            (Evnt_Name == "Touch Down Event" & Arg2_Name %in% c("Image 1", "Image 2", "Image 3")) | 
            ## trial outcome
            (Evnt_Name == "Condition Event" & Item_Name %in% c("Correct", "Incorrect")) | 
            ## entries/exits reward tray
            (Evnt_Name %in% c("Input Transition On Event", "Input Transition Off Event") & Item_Name == "Tray #1") | 
            ## end of time-out period
            (Evnt_Name == "Condition Event" & Item_Name == "Time Out End") | 
            ## front/back beamcrossing
            (Evnt_Name %in% c("Input Transition On Event", "Input Transition Off Event") & Item_Name %in% c("FIRBeam #1", "BIRBeam #1")) |
            ## Schedule shutdown time
            (Evnt_Name == "Schedule Shutdown Event") )
    }
  }
  details %<>%
    # mutate(protocol = case_when(str_detect(tolower(protocol), pattern = "punish incorrect") ~ "punish incorrect",
    #                             str_detect(tolower(protocol), pattern = "reversal") ~ "reversal",
    #                             str_detect(tolower(protocol), pattern = "acquisition") ~ "acquisition")) %>%
    mutate(sessionID = as.numeric(sessionID),
           date = dmy_hms(date)) %>%
    group_by(protocol, animalID) %>%
    arrange(date) %>%
    mutate(day = 1:n()) %>%
    arrange(day) %>%
    ungroup() %>% 
    select(animalID, genotype, protocol, day, machineID, sessionID, fileName, date) %>% 
    filter(animalID != "")
  
  return(list(details = details,
              data = data))
}

format2dataframe = function(data = ml) {
  details = data$details
  data = data$data
  
  ## placeholder tibble for all summary data
  fData = tibble(fileName = character(0), 
                 trialOrder = numeric(0), trialType = character(0), 
                 startTime = numeric(0), choiceRT = numeric(0),
                 outcome = logical(0), postChoiceVisit = logical(0),
                 firBeam = numeric(0), birBeam = numeric(0), postScreenTouch = numeric(0),
                 choice_location = numeric(0), choice_image = character(0), choice_X = numeric(0), choice_Y = numeric(0),
                 tBeam = numeric(0), endTime = numeric(0))
  
  # ii = 622
  for(ii in 1:nrow(details)) {
    print(paste0("file ",ii," / ",nrow(details),": ",details$fileName[ii]))
    ## load single .csv-related essential data from list
    ssData = data[ names(data) == details$fileName[ii] ][[1]] 
    
    ## identify start and end indices for trials
    startIndices = which(ssData$Item_Name %in% c("Start Trial", "Start Correction Trial"))
    endIndices = c(startIndices[-1] - 1, nrow(ssData))
    trialLength = length(startIndices)
    
    ## placeholder tibble for single .csv-related summary data
    data_t = tibble(trialOrder = numeric(trialLength), trialType = character(trialLength), 
                    startTime = numeric(trialLength), choiceRT = numeric(trialLength),
                    outcome = character(trialLength), postChoiceVisit = character(trialLength),
                    firBeam = numeric(trialLength), birBeam = numeric(trialLength),
                    postScreenTouch = numeric(trialLength), 
                    choice_location = numeric(trialLength), choice_image = character(trialLength),
                    choice_X = numeric(trialLength), choice_Y = numeric(trialLength))
    
    # jj = 1
    for(jj in 1:trialLength) {
      # print(paste("--trial",jj))
      ## load single trial
      ssTrial = ssData[startIndices[jj]:endIndices[jj],]
      
      image_specs = ssTrial[which(ssTrial$Item_Name %in% c("Correct","Incorrect"))-1,] %>% select(Arg1_Value:Arg4_Value)
      
      data_t$trialOrder[jj] = jj
      data_t$trialType[jj] = gsub("Start ", "", ssTrial$Item_Name[1])
      data_t$startTime[jj] = ssTrial$Evnt_Time[1]
      data_t$choiceRT[jj] = filter(ssTrial, Item_Name %in% c("Correct","Incorrect"))$Evnt_Time[1] - data_t$startTime[jj]
      data_t$outcome[jj] = filter(ssTrial, Item_Name %in% c("Correct","Incorrect"))$Item_Name[1]
      
      ## check whether a choice was made during this trial:
      ## if not: then no choice outcomes
      if(nrow(image_specs) == 0) {
        data_t$choice_location[jj] = NA
        data_t$choice_image[jj] = NA
        data_t$choice_X[jj] = NA
        data_t$choice_Y[jj] = NA
      } else {
        data_t$choice_location[jj] = image_specs$Arg1_Value
        data_t$choice_image[jj] = image_specs$Arg2_Name
        data_t$choice_X[jj] = image_specs$Arg3_Value 
        data_t$choice_Y[jj] = image_specs$Arg4_Value
      }
      
      postOutcome_start = filter(ssTrial, Item_Name %in% c("Correct","Incorrect"))$Evnt_Time
      ## check whether a choice was made during this trial:
      ## if not: then no postOutcomeVisit
      if( length(postOutcome_start) == 0 ) { 
        data_t$postChoiceVisit[jj] = NA
      } else {
        postOutcome_end = postOutcome_start + 5 # s
        postOutcomeVisit_df = filter(ssTrial, between(Evnt_Time, postOutcome_start, postOutcome_end))
        data_t$postChoiceVisit[jj] = ifelse(sum(postOutcomeVisit_df$Item_Name == "Tray #1") > 0, T, F)
      }
      
      data_t$firBeam[jj] = 
        ssTrial %>%
        filter(Evnt_Name == "Input Transition On Event" & Item_Name == "FIRBeam #1") %>%
        nrow()
      data_t$birBeam[jj] = 
        ssTrial %>%
        filter(Evnt_Name == "Input Transition On Event" & Item_Name == "BIRBeam #1") %>%
        nrow()
      
      data_t$postScreenTouch[jj] = 
        ssTrial %>% 
        filter(Evnt_Name == "Touch Down Event" & str_detect(Item_Name, pattern = "Bussey Mouse")) %>%
        slice(2:n()) %>% count() %>% pull()
    }
    
    data_t %<>%
      mutate(fileName = details$fileName[ii],
             outcome = ifelse(outcome == "Correct", T, F),
             tBeam = firBeam + birBeam,
             endTime = lead(startTime)) %>%
      select(fileName, trialOrder:startTime, endTime, choiceRT:outcome, 
             choice_location:choice_Y, postChoiceVisit, firBeam:birBeam, tBeam, postScreenTouch)
    data_t$endTime[nrow(data_t)] = tail(ssData$Evnt_Time, 1)
    
    fData %<>% bind_rows(data_t)
  }
  return(list(details = details,
              data = data,
              df = fData
  ))
}

extract_trialTime = function(data = ml, unit = "day") {} # in progress

extract_accuracy = function(data = ml, unit = "day") {
  details = data$details
  df = data$df
  
  n = nrow(details)
  accuracy_df = tibble(animalID = character(n), genotype = character(n),
                       protocol = character(n), day = numeric(n),
                       nTrial_n = numeric(n), nTrial_correct = numeric(n), nTrial_perc = numeric(n),
                       tTrial_n = numeric(n), cTrial_n = numeric(n), cTrial_perc = numeric(n), 
                       perseveration_index = numeric(n),
                       file = character(n), date = character(n))
  
  # ii = 1
  for(ii in 1:nrow(details)) {
    print(paste0("file ", ii, " / ", nrow(details),": ",details$fileName[ii]))
    
    accuracy_df$file[ii] = details$fileName[ii]
    accuracy_df$day[ii] = details$day[ii]
    accuracy_df[ii,c("nTrial_n","nTrial_correct","nTrial_perc")] =
      df %>% filter(fileName == details$fileName[ii]) %>%
      filter(trialType == "Trial") %>% filter(!is.na(outcome)) %>%
      summarise(nTrial_n = n(), nTrial_correct = sum(outcome)) %>%
      mutate(nTrial_perc = (nTrial_correct / nTrial_n) * 100) %>% as.matrix()
    tTrial = df %>% filter(fileName == details$fileName[ii]) %>% nrow()
    accuracy_df$tTrial_n[ii] = tTrial
    accuracy_df[ii,c("cTrial_n","cTrial_perc")] =
      df %>% filter(fileName == details$fileName[ii]) %>%
      filter(trialType == "Correction Trial") %>%
      filter(!is.na(outcome)) %>%
      summarise(cTrial_n = length(outcome)) %>%
      mutate(cTrial_perc = (cTrial_n / tTrial) * 100) %>% as.matrix()
    accuracy_df$animalID[ii] = details$animalID[ii]
    accuracy_df$genotype[ii] = details$genotype[ii]
    accuracy_df$protocol[ii] = details$protocol[ii]
    accuracy_df$date[ii] = as.character(details$date[ii])
  }
  
  accuracy_df %<>% 
    mutate(perseveration_index = (tTrial_n-nTrial_n) / (nTrial_n-nTrial_correct)) %>%
    select(animalID,genotype,protocol,day,
           tTrial_n,
           nTrial_n,nTrial_correct,nTrial_perc,
           cTrial_n,cTrial_perc,perseveration_index,
           file,date)
  return(accuracy_df)
} # unit = c("day","bin","both")

extract_sequence = function(data = ml, type = "Correction Trial", unit = "day") {
  details = data$details
  df = data$df
  
  sequence_list = setNames(replicate(nrow(details), tibble()), details$fileName)
  sequence_df = tibble(fileName = character(nrow(details)), 
                       trialType = rep(type, nrow(details)),
                       nSeq = numeric(nrow(details)), 
                       minSeq = numeric(nrow(details)), medSeq = numeric(nrow(details)), 
                       aveSeq = numeric(nrow(details)), maxSeq = numeric(nrow(details)),
                       sdSeq = numeric(nrow(details)), seSeq = numeric(nrow(details)))
  
  # ii = 1
  for(ii in 1:nrow(details)) {
    print(paste0("file ", ii, " / ", nrow(details)))
    ssFile = filter(df, fileName == pull(details[ii, "fileName"]))
    identifier = rle(as.character(ssFile$trialType)); identifier = tibble(values = identifier$values, lengths = identifier$lengths)
    sequence_list[[ii]] = identifier %>% filter(values == type) %>% pull(lengths)
    sequence_df[ii,] = 
      identifier %>%
      filter(values == type) %>%
      summarise(fileName = details$fileName[ii],
                trialType = type,
                nSeq = n(),
                minSeq = min(lengths),
                medSeq = median(lengths),
                aveSeq = mean(lengths),
                maxSeq = max(lengths),
                sdSeq = sd(lengths),
                seSeq = se(lengths))
  }
  return(seqSummary = list(seqList = sequence_list,
                           seqData = sequence_df))
} # type = c("Trial","Correction Trial"); unit = c("day","bin")

extract_latency = function(data = ml, unit = "day") {
  details = data$details
  data = data$data
  
  # reports the latency to make a (in)correct response
  latency_df = tibble(Animal_ID = rep("dummy", nrow(details)), Genotype = rep("dummy", nrow(details)), 
                      Protocol = rep("dummy", nrow(details)), Day = numeric(nrow(details)),
                      Correct_latency = numeric(nrow(details)), Incorrect_latency = numeric(nrow(details)), General_latency = numeric(nrow(details)))
  
  # ii = 1
  for(ii in 1:nrow(details)) {
    print(paste0("Response latency ", ii, "/", nrow(details), " files. Progress: ", round(ii / nrow(details)*100, 2), "%"))
    
    outcome =
      data[[details$fileName[ii]]] %>%
      select(Evnt_Time, Item_Name) %>%
      filter(Item_Name %in% c("Correct", "Incorrect")) 
    response_all =
      data[[details$fileName[ii]]] %>%
      select(Evnt_Time, Item_Name) %>% 
      filter(Item_Name %in% c("Start Trial", "Start Correction Trial")) %>%
      slice(1:nrow(outcome)) %>%
      bind_cols(outcome) %>%
      mutate(Response_Latency = Evnt_Time1 - Evnt_Time,
             Item_Name = gsub("Start ", "", Item_Name))
    response_c_ic =
      response_all %>% 
      group_by(Item_Name1) %>%
      summarise(Response_Latency = mean(Response_Latency)) %>% pull()
    latency_df$Animal_ID[ii] = pull(details[ii, "animalID"])
    latency_df$Genotype[ii] = pull(details[ii, "genotype"])
    latency_df$Protocol[ii] = ifelse(details[ii, "protocol"] %>% pull() %>% str_detect("reversal"), "Reversal", "Acquisition")
    latency_df$Day[ii] = pull(details[ii, "day"])
    latency_df[ii, c("Correct_latency", "Incorrect_latency")] = response_c_ic
    latency_df[ii, "General_latency"] = mean(response_all$Response_Latency)
  }
  return(latency_df)
} # unit = c("day","bin")

extract_rewardCollectionError = function(data = ml, unit = "day") {
  details = data$details
  data = data$data
  
  # reports % entries during tone 5 s post-stimulus choice
  prop2tray = tibble(Animal_ID = rep("dummy", nrow(details)), Genotype = rep("dummy", nrow(details)),
                     Machine_ID = rep("dummy", nrow(details)), Session_ID = numeric(nrow(details)), 
                     Protocol = rep("dummy", nrow(details)),
                     propCorrect = numeric(nrow(details)), propIncorrect = numeric(nrow(details)),
                     Day = numeric(nrow(details)))
  
  # ii = 1
  for(ii in 1:nrow(details)) {
    print(paste0("Reward collection error ", ii, "/", nrow(details), " files. Progress: ", round(ii / nrow(details)*100, 2), "%"))
    
    prop2tray$Animal_ID[ii] = pull(details[ii, "animalID"])
    prop2tray$Genotype[ii] = pull(details[ii, "genotype"])
    prop2tray$Machine_ID[ii] = pull(details[ii, "machineID"])
    prop2tray$Session_ID[ii] = pull(details[ii, "sessionID"])
    prop2tray$Protocol[ii] = ifelse(details[ii, "protocol"] %>% pull() %>% str_detect("reversal"), "Reversal", "Acquisition")
    prop2tray[ii, c("propCorrect", "propIncorrect")] =
      data[[details$fileName[ii]]] %>%
      filter(Item_Name %in% c("Correct", "Incorrect", "Tray #1")) %>%
      mutate(Evnt_Time2 = lead(Evnt_Time)) %>%
      filter(Item_Name %in% c("Correct", "Incorrect")) %>%
      mutate(within_5s = Evnt_Time2 - Evnt_Time <= 5) %>%
      select(Evnt_Time, Item_Name, Evnt_Time2, within_5s) %>%
      group_by(Item_Name) %>%
      summarise(Prop2tray = mean(within_5s, na.rm = T)) %>% pull()
    prop2tray$Day[ii] = details$day[ii] }
  return(prop2tray) 
} # unit = c("day","bin")

extract_activity = function(data = ml, option = "all", unit = "day") {
  details = data$details
  data = data$data
  
  # reports the total number of back-front-total beam crossings
  activity_df <- tibble(Animal_ID = rep("dummy", nrow(details)), Genotype = rep("dummy", nrow(details)),
                        Machine_ID = rep("dummy", nrow(details)), Session_ID = numeric(nrow(details)), 
                        Protocol = rep("dummy", nrow(details)), Day = numeric(nrow(details)),
                        TrayEntries = numeric(nrow(details)), TrayTotalDuration = numeric(nrow(details)), TrayMeanDuration = numeric(nrow(details)),
                        BackBeams = numeric(nrow(details)), FrontBeams = numeric(nrow(details)))
  
  # ii = 1
  for(ii in 1:nrow(details)) {
    print(paste0("Activity ", ii, "/", nrow(details), " files. Progress: ", round(ii / nrow(details)*100, 2), "%"))
    
    activity_df$Animal_ID[ii] <- pull(details[ii, "animalID"])
    activity_df$Genotype[ii] <- pull(details[ii, "genotype"])
    activity_df$Machine_ID[ii] <- pull(details[ii, "machineID"])
    activity_df$Session_ID[ii] <- pull(details[ii, "sessionID"])
    activity_df$Protocol[ii] <- pull(details[ii, "protocol"])
    activity_df$Day[ii] <- pull(details[ii, "day"])
    
    if(option == "trial") {
      start = data[[details$fileName[ii]]] %>% 
        filter(grepl("^Start", Item_Name)) %>% pull(Evnt_Time)
      end = data[[details$fileName[ii]]] %>% 
        filter(grepl("orrect$", Item_Name)) %>% pull(Evnt_Time)
      if(length(start) != length(end)) { start = start[-1] }
      progress = tibble(id = 1:length(start),start,end, front_beam = rep(NA,length(start)), back_beam = rep(NA,length(start)))
      
      # jj = 1
      for(jj in 1:nrow(progress)) {
        beams = data[[details$fileName[ii]]] %>% 
          filter(between(Evnt_Time, progress$start[jj], progress$end[jj])) %>% 
          filter(Evnt_Name == "Input Transition On Event",
                 Item_Name %in% c("FIRBeam #1", "BIRBeam #1")) %>% 
          group_by(Item_Name) %>%
          summarize(n()) %>%
          pull(`n()`)
        if(length(beams) == 1) beams = c(beams,NA)
        
        progress[jj, c("front_beam", "back_beam")] = beams
      }
      activity_df[ii,"BackBeams"] = sum(progress$back_beam, na.rm = T)
      activity_df[ii,"FrontBeams"] = sum(progress$front_beam, na.rm = T)
    } else if(option == "iti") {
      
      start = data[[details$fileName[ii]]] %>% 
        filter(grepl("orrect$", Item_Name)) %>% pull(Evnt_Time)
      end = data[[details$fileName[ii]]] %>% 
        filter(grepl("^Start", Item_Name)) %>% pull(Evnt_Time)
      if(length(start) != length(end)) { start = start[-1] }
      progress = tibble(id = 1:length(start),start,end, front_beam = rep(NA,length(start)), back_beam = rep(NA,length(start)))
      
      # jj = 1
      for(jj in 1:nrow(progress)) {
        beams = data[[details$fileName[ii]]] %>% 
          filter(between(Evnt_Time, progress$start[jj], progress$end[jj])) %>% 
          filter(Evnt_Name == "Input Transition On Event",
                 Item_Name %in% c("FIRBeam #1", "BIRBeam #1")) %>% 
          group_by(Item_Name) %>%
          summarize(n()) %>%
          pull(`n()`)
        if(length(beams) == 1) beams = c(beams,NA)
        
        progress[jj, c("front_beam", "back_beam")] = beams
      }
      activity_df[ii,"BackBeams"] = sum(progress$back_beam, na.rm = T)
      activity_df[ii,"FrontBeams"] = sum(progress$front_beam, na.rm = T)
    } else {
      activity_df[ii, c("BackBeams", "FrontBeams")] <-
        data[[details$fileName[ii]]] %>% 
        filter(Evnt_Name == "Input Transition On Event",
               Item_Name %in% c("FIRBeam #1", "BIRBeam #1")) %>%
        group_by(Item_Name) %>%
        summarize(n()) %>%
        pull(`n()`)
      
      tray_entries <-
        data[[details$fileName[ii]]] %>% 
        filter(Evnt_Name %in% c("Input Transition On Event", "Input Transition Off Event"),
               Item_Name == "Tray #1")
      if(tray_entries$Evnt_Name[1] == "Input Transition Off Event") tray_entries <- tray_entries[-1,]
      activity_df$TrayEntries[ii] <-
        tray_entries %>% 
        filter(Evnt_Name == "Input Transition On Event") %>%
        count() %>% pull
      
      tray_in <-
        tray_entries %>%
        filter(Evnt_Name == "Input Transition On Event") %>%
        select(In = Evnt_Time)
      tray_out <-
        tray_entries %>%
        filter(Evnt_Name == "Input Transition Off Event") %>%
        select(Out = Evnt_Time)
      
      if(nrow(tray_in) != nrow(tray_out)) tray_in <- tray_in[-nrow(tray_in),]
      
      tray_time <-
        tray_out %>% 
        bind_cols(tray_in) %>%
        mutate(TrayTime = Out - In)
      activity_df[ii, c("TrayTotalDuration", "TrayMeanDuration")] <-
        tray_time %>%
        summarise(total = sum(TrayTime),
                  mean = mean(TrayTime)) %>% unlist %>% unname
    }
  } 
  
  activity_df %<>% mutate(TotalBeams = BackBeams + FrontBeams)
  
  return(activity_df)
}  # option = c("all","trial","iti"); unit = c("day","bin")

extract_screenTouches = function(data = ml, unit = "day") {
  details = data$details
  df = data$df
  
  screenActivity_df =
    df %>% 
    left_join(details, by = "fileName") %>% 
    filter(!is.na(postScreenTouch)) %>% 
    group_by(animalID,fileName,genotype,protocol,day) %>% 
    summarise(postScreenTouch_sum = sum(postScreenTouch),
              postScreenTouch_mean = mean(postScreenTouch)) %>% 
    mutate(bin = ceiling(day / 5)) %>% 
    ungroup()
  
  return(screenActivity_df)
  
} # unit = c("day","bin")
