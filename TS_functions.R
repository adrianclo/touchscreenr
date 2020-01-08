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
suppressMessages(library(purrr))     # 0.3.2

se = function(x) sd(x, na.rm = T) / sqrt(length(x))

`%not_in%` = purrr::negate(`%in%`)

format2essential = function(filesDir = NULL, 
                            wt = NULL, het = NULL, ko = NULL, ki = NULL,
                            exclude_files = NULL, include_files = NULL, exclude_mice = NULL) {
  
  if(!is.null(exclude_files)) {
    files = list.files(filesDir) %>% dplyr::setdiff(exclude_files) # files = files[!(files %in% exclude_files)]
    nFiles = length(files)
  } else if(!is.null(include_files)) {
    files = list.files(filesDir) %>% dplyr::intersect(include_files)
    nFiles = length(files)
  } else {
    files = list.files(filesDir)
    nFiles = length(files)
  }
  
  ## placeholder details and data
  details = dplyr::tibble(animalID = character(nFiles), genotype = character(nFiles), 
                   protocol = character(nFiles), machineID = character(nFiles),
                   sessionID = numeric(nFiles), fileName = character(nFiles), 
                   date = character(nFiles))
  data = setNames(replicate(nFiles, dplyr::tibble()), files)
  
  # ii = 1
  for(ii in 1:nFiles) {
    print(paste0("Simplifying ",ii,"/",nFiles," files: ",
                 files[ii],
                 ". Progress: ", round(ii / nFiles*100, 2),"%"))
    
    ## different raw file alignment depending on protocol
    temp = suppressWarnings(readLines(paste0(filesDir, files[ii]))) %>% stringr::str_detect("----------")
    identifier = which(cumsum(temp) == 1)[1]
    
    details_t = suppressMessages(readxl::read_csv(paste0(filesDir, files[ii]), n_max = identifier - 3))
    data_t = suppressMessages(readxl::read_csv(paste0(filesDir, files[ii]), skip = identifier))
    
    if(details_t %>% filter(Name == "Animal ID") %>% dplyr::pull(Value) %in% exclude_mice) {
      next()
    } else {
      ## format details item
      if(details_t %>% dplyr::filter(Name == "Animal ID") %>% dplyr::pull(Value) %>% length() == 0) {
        details$animalID[ii] = NA
      } else { details$animalID[ii] = details_t %>% dplyr::filter(Name == "Animal ID") %>% dplyr::pull(Value) }
      
      details$genotype[ii] = ifelse(details$animalID[ii] %in% wt, "WT",
                                    ifelse(details$animalID[ii] %in% het, "HET",
                                           ifelse(details$animalID[ii] %in% ko, "KO",
                                                  ifelse(details$animalID[ii] %in% ki, "KI", "No genotype"))))
      details$protocol[ii] = details_t %>% dplyr::filter(Name == "Schedule") %>% dplyr::pull(Value)
      details$machineID[ii] = details_t %>% dplyr::filter(Name == "MachineName") %>% dplyr::pull(Value)
      details$sessionID[ii] = details_t %>% dplyr::filter(Name == "SessionId") %>% dplyr::pull(Value)
      details$fileName[ii] = files[ii]
      details$date[ii] = details_t %>% dplyr::filter(Name == "Date/Time") %>% dplyr::pull(Value)
      
      ## format data item
      data[[ii]] =
        data_t %>%
        dplyr::select(Evnt_Time, Evnt_Name, Item_Name, Alias_Name, Arg1_Value, Arg2_Name, Arg3_Value, Arg4_Value) %>%
        dplyr::filter( 
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
    dplyr::mutate(sessionID = as.numeric(sessionID),
           date = lubridate::dmy_hms(date)) %>%
    dplyr::group_by(protocol, animalID) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(day = 1:dplyr::n()) %>%
    dplyr::arrange(day) %>%
    dplyr::ungroup() %>% 
    dplyr::select(animalID, genotype, protocol, day, machineID, sessionID, fileName, date) %>% 
    dplyr::filter(animalID != "")
  
  return(list(details = details,
              data = data))
}

format2dataframe = function(data = ml) {
  details = data$details
  data = data$data
  
  ## placeholder tibble for all summary data
  fData = dplyr::tibble(fileName = character(0), 
                 trialOrder = numeric(0), trialType = character(0), 
                 startTime = numeric(0), choiceRT = numeric(0),
                 outcome = logical(0), postChoiceVisit = logical(0),
                 firBeam = numeric(0), birBeam = numeric(0), postScreenTouch = numeric(0),
                 choice_location = numeric(0), choice_image = character(0), choice_X = numeric(0), choice_Y = numeric(0),
                 tBeam = numeric(0), endTime = numeric(0))
  
  # ii = 622
  for(ii in 1:nrow(details)) {
    print(paste0("file ",ii," / ", nrow(details),": ",details$fileName[ii]))
    ## load single .csv-related essential data from list
    ssData = data[ names(data) == details$fileName[ii] ][[1]] 
    
    ## identify start and end indices for trials
    startIndices = which(ssData$Item_Name %in% c("Start Trial", "Start Correction Trial"))
    endIndices = c(startIndices[-1] - 1, nrow(ssData))
    trialLength = length(startIndices)
    
    ## placeholder tibble for single .csv-related summary data
    data_t = dplyr::tibble(trialOrder = numeric(trialLength), trialType = character(trialLength), 
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
      
      image_specs = ssTrial[which(ssTrial$Item_Name %in% c("Correct","Incorrect"))-1,] %>% dplyr::select(Arg1_Value:Arg4_Value)
      
      data_t$trialOrder[jj] = jj
      data_t$trialType[jj] = gsub("Start ", "", ssTrial$Item_Name[1])
      data_t$startTime[jj] = ssTrial$Evnt_Time[1]
      data_t$choiceRT[jj] = dplyr::filter(ssTrial, Item_Name %in% c("Correct","Incorrect"))$Evnt_Time[1] - data_t$startTime[jj]
      data_t$outcome[jj] = dplyr::filter(ssTrial, Item_Name %in% c("Correct","Incorrect"))$Item_Name[1]
      
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
      
      postOutcome_start = dplyr::filter(ssTrial, Item_Name %in% c("Correct","Incorrect"))$Evnt_Time
      ## check whether a choice was made during this trial:
      ## if not: then no postOutcomeVisit
      if( length(postOutcome_start) == 0 ) { 
        data_t$postChoiceVisit[jj] = NA
      } else {
        postOutcome_end = postOutcome_start + 5 # s
        postOutcomeVisit_df = dplyr::filter(ssTrial, dplyr::between(Evnt_Time, postOutcome_start, postOutcome_end))
        data_t$postChoiceVisit[jj] = ifelse(sum(postOutcomeVisit_df$Item_Name == "Tray #1") > 0, T, F)
      }
      
      data_t$firBeam[jj] = 
        ssTrial %>%
        dplyr::filter(Evnt_Name == "Input Transition On Event" & Item_Name == "FIRBeam #1") %>%
        dplyr::nrow()
      data_t$birBeam[jj] = 
        ssTrial %>%
        dplyr::filter(Evnt_Name == "Input Transition On Event" & Item_Name == "BIRBeam #1") %>%
        dplyr::nrow()
      
      data_t$postScreenTouch[jj] = 
        ssTrial %>% 
        dplyr::filter(Evnt_Name == "Touch Down Event" & stringr::str_detect(Item_Name, pattern = "Bussey Mouse")) %>%
        dplyr::slice(2:dplyr::n()) %>% dplyr::count() %>% dplyr::pull()
    }
    
    data_t %<>%
      dplyr::mutate(fileName = details$fileName[ii],
             outcome = ifelse(outcome == "Correct", T, F),
             tBeam = firBeam + birBeam,
             endTime = dplyr::lead(startTime)) %>%
      dplyr::select(fileName, trialOrder:startTime, endTime, choiceRT:outcome, 
             choice_location:choice_Y, postChoiceVisit, firBeam:birBeam, tBeam, postScreenTouch)
    data_t$endTime[nrow(data_t)] = tail(ssData$Evnt_Time, 1)
    
    fData %<>% dplyr::bind_rows(data_t)
  }
  return(list(details = details,
              data = data,
              df = fData
  ))
}

extract_accuracy = function(data = ml, exclude = NULL) {
  details = data$details %>% dplyr::filter(animalID %not_in% exclude)
  df = data$df
  n = nrow(details)
  accuracy_df = dplyr::tibble(fileName = character(n),
                       nTrial_n = numeric(n), nTrial_correct = numeric(n), nTrial_perc = numeric(n),
                       tTrial_n = numeric(n), cTrial_n = numeric(n), cTrial_perc = numeric(n), 
                       perseveration_index = numeric(n))
  
  for(ii in 1:n) {
    print(paste0("Accuracy - file ", ii, " / ", n,": ",details$fileName[ii]))
    
    accuracy_df$fileName[ii] = details$fileName[ii]
    accuracy_df[ii,c("nTrial_n","nTrial_correct","nTrial_perc")] =
      df %>% dplyr::filter(fileName == details$fileName[ii]) %>%
      dplyr::filter(trialType == "Trial") %>% dplyr::filter(!is.na(outcome)) %>%
      dplyr::summarise(nTrial_n = dplyr::n(), nTrial_correct = sum(outcome)) %>%
      dplyr::mutate(nTrial_perc = (nTrial_correct / nTrial_n) * 100) %>% as.matrix()
    tTrial = df %>% dplyr::filter(fileName == details$fileName[ii]) %>% nrow()
    accuracy_df$tTrial_n[ii] = tTrial
    accuracy_df[ii,c("cTrial_n","cTrial_perc")] =
      df %>% dplyr::filter(fileName == details$fileName[ii]) %>%
      dplyr::filter(trialType == "Correction Trial") %>%
      dplyr::filter(!is.na(outcome)) %>%
      dplyr::summarise(cTrial_n = length(outcome)) %>%
      dplyr::mutate(cTrial_perc = (cTrial_n / tTrial) * 100) %>% as.matrix()
  }
  accuracy_df %<>% dplyr::mutate(perseveration_index = (tTrial_n-nTrial_n) / (nTrial_n-nTrial_correct))
  return(accuracy_df)
}

extract_activity = function(data = ml, option = "all", exclude = NULL) {
  ## option = c("all","trial","iti")
  
  details = data$details %>% dplyr::filter(animalID %not_in% exclude)
  data = data$data
  n = nrow(details)
  # reports the total number of back-front-total beam crossings
  activity_df = dplyr::tibble(fileName = character(n),
                       trayEntries = numeric(n), trayTotalDuration = numeric(n), trayMeanDuration = numeric(n),
                       backBeams = numeric(n), frontBeams = numeric(n))
  
  for(ii in 1:n) {
    print(paste0("Activity - file ", ii, " / ", n,": ",details$fileName[ii]))
    
    activity_df$fileName[ii] = details$fileName[ii]
    
    if(option == "trial") {
      start = data[[details$fileName[ii]]] %>% 
        dplyr::filter(grepl("^Start", Item_Name)) %>% dplyr::pull(Evnt_Time)
      end = data[[details$fileName[ii]]] %>% 
        dplyr::filter(grepl("orrect$", Item_Name)) %>% dplyr::pull(Evnt_Time)
      if(length(start) != length(end)) { start = start[-1] }
      progress = dplyr::tibble(id = 1:length(start),start,end, front_beam = rep(NA,length(start)), back_beam = rep(NA,length(start)))
      
      # jj = 1
      for(jj in 1:nrow(progress)) {
        beams = data[[details$fileName[ii]]] %>% 
          dplyr::filter(dplyr::between(Evnt_Time, progress$start[jj], progress$end[jj])) %>% 
          dplyr::filter(Evnt_Name == "Input Transition On Event",
                 Item_Name %in% c("FIRBeam #1", "BIRBeam #1")) %>% 
          dplyr::group_by(Item_Name) %>%
          dplyr::summarize(dplyr::n()) %>%
          dplyr::pull(`n()`)
        if(length(beams) == 1) beams = c(beams,NA)
        
        progress[jj, c("front_beam", "back_beam")] = beams
      }
      activity_df[ii,"backBeams"] = sum(progress$back_beam, na.rm = T)
      activity_df[ii,"frontBeams"] = sum(progress$front_beam, na.rm = T)
      
    } else if(option == "iti") {
      start = data[[details$fileName[ii]]] %>% 
        dplyr::filter(grepl("orrect$", Item_Name)) %>% dplyr::pull(Evnt_Time)
      end = data[[details$fileName[ii]]] %>% 
        dplyr::filter(grepl("^Start", Item_Name)) %>% dplyr::pull(Evnt_Time)
      if(length(start) != length(end)) { start = start[-1] }
      progress = dplyr::tibble(id = 1:length(start),start,end, front_beam = rep(NA,length(start)), back_beam = rep(NA,length(start)))
      
      # jj = 1
      for(jj in 1:nrow(progress)) {
        beams = data[[details$fileName[ii]]] %>% 
          dplyr::filter(dplyr::between(Evnt_Time, progress$start[jj], progress$end[jj])) %>% 
          dplyr::filter(Evnt_Name == "Input Transition On Event",
                 Item_Name %in% c("FIRBeam #1", "BIRBeam #1")) %>% 
          dplyr::group_by(Item_Name) %>%
          dplyr::summarize(dplyr::n()) %>%
          dplyr::pull(`n()`)
        if(length(beams) == 1) beams = c(beams,NA)
        
        progress[jj, c("front_beam", "back_beam")] = beams
      }
      activity_df[ii,"backBeams"] = sum(progress$back_beam, na.rm = T)
      activity_df[ii,"frontBeams"] = sum(progress$front_beam, na.rm = T)
      
    } else {
      values = data[[details$fileName[ii]]] %>% 
        dplyr::filter(Evnt_Name == "Input Transition On Event",
               Item_Name %in% c("FIRBeam #1", "BIRBeam #1")) %>%
        dplyr::group_by(Item_Name) %>%
        dplyr::summarize(dplyr::n()) %>%
        dplyr::pull(`n()`)
      
      if(length(values) == 0) { values = c(0,0) } # added line (test phase)
      activity_df[ii, c("backBeams", "frontBeams")] = values
        
      
      tray_entries =
        data[[details$fileName[ii]]] %>% 
        dplyr::filter(Evnt_Name %in% c("Input Transition On Event", "Input Transition Off Event"),
               Item_Name == "Tray #1")
      if(tray_entries$Evnt_Name[1] == "Input Transition Off Event") tray_entries <- tray_entries[-1,]
      activity_df$trayEntries[ii] =
        tray_entries %>% 
        dplyr::filter(Evnt_Name == "Input Transition On Event") %>%
        dplyr::count() %>% dplyr::pull()
      
      tray_in =
        tray_entries %>%
        dplyr::filter(Evnt_Name == "Input Transition On Event") %>%
        dplyr::select(In = Evnt_Time)
      tray_out =
        tray_entries %>%
        dplyr::filter(Evnt_Name == "Input Transition Off Event") %>%
        dplyr::select(Out = Evnt_Time)
      
      if(nrow(tray_in) != nrow(tray_out)) tray_in = tray_in[-nrow(tray_in),]
      
      tray_time =
        tray_out %>% 
        dplyr::bind_cols(tray_in) %>%
        dplyr::mutate(TrayTime = Out - In)
      activity_df[ii, c("trayTotalDuration", "trayMeanDuration")] =
        tray_time %>%
        summarise(total = sum(TrayTime),
                  mean = mean(TrayTime)) %>% unlist %>% unname
    }
  } 
  
  activity_df %<>% dplyr::mutate(totalBeams = backBeams + frontBeams)
  
  return(activity_df)
}

extract_latency = function(data = ml, exclude = NULL) {
  details = data$details %>% dplyr::filter(animalID %not_in% exclude)
  data = data$data
  n = nrow(details)
  latency_df = dplyr::tibble(fileName = character(n),
                      correctLatency = numeric(n), incorrectLatency = numeric(n), generalLatency = numeric(n))
  
  for(ii in 1:n) {
    print(paste0("Response latency - file ", ii, " / ", n, ": ", details$fileName[ii]))
    
    latency_df$fileName[ii] = details$fileName[ii]
    
    outcome =
      data[[details$fileName[ii]]] %>%
      dplyr::select(Evnt_Time, Item_Name) %>%
      dplyr::filter(Item_Name %in% c("Correct", "Incorrect")) 
    response_all =
      data[[details$fileName[ii]]] %>%
      dplyr::select(Evnt_Time, Item_Name) %>% 
      dplyr::filter(Item_Name %in% c("Start Trial", "Start Correction Trial")) %>%
      dplyr::slice(1:nrow(outcome)) %>%
      dplyr::bind_cols(outcome) %>%
      dplyr::mutate(Response_Latency = Evnt_Time1 - Evnt_Time,
             Item_Name = gsub("Start ", "", Item_Name))
    response_c_ic =
      response_all %>% 
      dplyr::group_by(Item_Name1) %>%
      dplyr::summarise(Response_Latency = mean(Response_Latency)) %>% dplyr::pull()
    
    latency_df[ii, c("correctLatency", "incorrectLatency")] = response_c_ic
    latency_df[ii, "generalLatency"] = mean(response_all$Response_Latency)
  }
  
  return(latency_df)
}

extract_rewardCollectionError = function(data = ml, exclude = NULL) {
  details = data$details %>% dplyr::filter(animalID %not_in% exclude)
  data = data$data
  n = nrow(details)
  prop2tray = dplyr::tibble(fileName = character(n),
                     propCorrect = numeric(n), propIncorrect = numeric(n))
  
  for(ii in 1:n) {
    print(paste0("Reward collection error - file ", ii, " / ", n, ": ", details$fileName[ii]))
    
    prop2tray$fileName[ii] = details$fileName[ii]
    prop2tray[ii, c("propCorrect", "propIncorrect")] =
      data[[details$fileName[ii]]] %>%
      dplyr::filter(Item_Name %in% c("Correct", "Incorrect", "Tray #1")) %>%
      dplyr::mutate(Evnt_Time2 = dplyr::lead(Evnt_Time)) %>%
      dplyr::filter(Item_Name %in% c("Correct", "Incorrect")) %>%
      dplyr::mutate(within_5s = Evnt_Time2 - Evnt_Time <= 5) %>%
      dplyr::select(Evnt_Time, Item_Name, Evnt_Time2, within_5s) %>%
      dplyr::group_by(Item_Name) %>%
      dplyr::summarise(Prop2tray = mean(within_5s, na.rm = T)) %>% dplyr::pull()
  }
  
  return(prop2tray) 
}

extract_screenTouches = function(data = ml, exclude = NULL) {
  details = data$details %>% dplyr::filter(animalID %not_in% exclude)
  df = data$df %>% dplyr::semi_join(details, by = "fileName")
  
  print("Initiate processing of screen touches\n")
  
  screenActivity_df =
    df %>% 
    dplyr::left_join(details, by = "fileName") %>% 
    dplyr::filter(!is.na(postScreenTouch)) %>% 
    dplyr::group_by(animalID,fileName,genotype,protocol,day) %>% 
    dplyr::summarise(postScreenTouch_sum = sum(postScreenTouch),
              postScreenTouch_mean = mean(postScreenTouch)) %>% 
    # mutate(bin = ceiling(day / 5)) %>% 
    dplyr::ungroup()
  
  return(screenActivity_df)
}

extract_sequence = function(data = ml, type = "Correction Trial", exclude = NULL) {
  ## type = c("Trial","Correction Trial")
  
  details = data$details %>% dplyr::filter(animalID %not_in% exclude)
  df = data$df
  n = nrow(details)
  sequence_list = setNames(replicate(n, dplyr::tibble()), details$fileName)
  sequence_df = dplyr::tibble(fileName = character(n), 
                       trialType = rep(type, n),
                       nSeq = numeric(n), 
                       minSeq = numeric(n), medSeq = numeric(n), 
                       aveSeq = numeric(n), maxSeq = numeric(n),
                       sdSeq = numeric(n), seSeq = numeric(n))
  
  for(ii in 1:n) {
    print(paste0("Sequence - file ", ii, " / ", n, ": ", details$fileName[ii]))
    
    ssFile = dplyr::filter(df, fileName == dplyr::pull(details[ii, "fileName"]))
    identifier = rle(as.character(ssFile$trialType)); identifier = dplyr::tibble(values = identifier$values, lengths = identifier$lengths)
    sequence_list[[ii]] = identifier %>% dplyr::filter(values == type) %>% dplyr::pull(lengths)
    sequence_df[ii,] = 
      identifier %>%
      dplyr::filter(values == type) %>%
      dplyr::summarise(fileName = details$fileName[ii],
                trialType = type,
                nSeq = dplyr::n(),
                minSeq = min(lengths),
                medSeq = median(lengths),
                aveSeq = mean(lengths),
                maxSeq = max(lengths),
                sdSeq = sd(lengths),
                seSeq = se(lengths))
  }
  return(seqSummary = list(seqList = sequence_list,
                           seqData = sequence_df))
}

extract_trialTime = function(data = ml, exclude = NULL) {
  # details = data$details %>% filter(animalID %not_in% exclude)
  df = data$df
  # data = data$data
  # n = nrow(details)
  
  trialduration =
    df %>% 
    dplyr::mutate(trialDuration = endTime - startTime) %>% 
    dplyr::filter(!is.na(outcome)) %>% 
    dplyr::select(fileName,outcome,trialDuration)
  trialtime_df =
    trialduration %>% 
    dplyr::group_by(fileName,outcome) %>% 
    dplyr::summarise(trialDuration = mean(trialDuration)) %>% 
    tidyr::spread(outcome,trialDuration) %>% 
    dplyr::rename(durationTrialIncorrect = `FALSE`, durationTrialCorrect = `TRUE`) %>% 
    dplyr::left_join(
      trialduration %>% 
        dplyr::group_by(fileName) %>% 
        dplyr::summarise(durationTrialMean = mean(trialDuration)),
      by = "fileName"
    )
  
  return(trialtime_df)
}

process_sequence = function(seq = seq, details = ml, test = NULL, start = NULL, end = NULL, title = NULL, max_length = 40) {
  details = details$details
  details %<>%
    dplyr::mutate(genotype = factor(genotype, levels = c("WT","KO"))) %>% 
    dplyr::filter(protocol == test) %>% 
    dplyr::filter(dplyr::between(day,start,end)) %>% 
    dplyr::select(genotype,fileName) %>% 
    dplyr::group_by(genotype) %>% tidyr::nest()
  n = nrow(details)
  
  seq_list = rep(list(NULL), times = n)
  names(seq_list) = details %>% dplyr::pull(genotype)
  for(ii in 1:n) { 
    seq_list[[ii]] = 
      seq$seqList[details %>% 
                    dplyr::ungroup() %>% 
                    dplyr::slice(ii) %>% tidyr::unnest(c(data)) %>% 
                    dplyr::pull(fileName)] %>% 
      unlist() %>% unname() %>% sort()
  }

  print(ks.test(seq_list[[1]],
                seq_list[[2]]))
  
  seq_distribution =
    dplyr::tibble(
      genotype = rep(names(seq_list), times = c(length(seq_list[[1]]),length(seq_list[[2]]))),
      seq_length = c(seq_list[[1]], seq_list[[2]])
    )
  
  seqPlot =
    ggplot2::ggplot(seq_distribution, aes(seq_length, color = genotype)) +
    ggplot2::stat_ecdf(geom = "step", size = 1) +
    ggplot2::ggtitle(title) +
    ggplot2::xlim(c(0,max_length)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Sequence length", y = "Cumulative proportion")
  print(seqPlot)
}