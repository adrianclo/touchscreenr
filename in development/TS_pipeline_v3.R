source("TS_functions_v3.R")

sample <- format2essential(filesDir = file.path(getwd(), "other_pal"), 
                           wt = "341-blue", 
                           ko = "341-red",
                           het = het,
                           ki = ki)

sample$details %>% View()

