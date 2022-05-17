# fitbitToken <- commandArgs(TRUE)
# user <- args
# fitbitID <- commandArgs(TRUE)
# user <- args

# library
library(httr)
library(RCurl)
library(stringr)
library(dplyr)

source("/home/hnbgenomics/Fitbit/fitbitCommonFx.R")
fitbitID = '9B43LS'
fitbitToken = 'eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiIyMkM4UEYiLCJzdWIiOiI5QjQzTFMiLCJpc3MiOiJGaXRiaXQiLCJ0eXAiOiJhY2Nlc3NfdG9rZW4iLCJzY29wZXMiOiJ3aHIgd3BybyB3bnV0IHdzbGUgd3dlaSB3c29jIHdzZXQgd2FjdCB3bG9jIiwiZXhwIjoxNjQ4Mjc5NzAxLCJpYXQiOjE2MTY3NDM3MDF9.g7MXO06eo2fOUZzriy108Fdx2rzmivrOMQMPqtgeJyE'
# sleep
getSleep <- function(date = NULL) {
  url_sleep <- paste0("https://api.fitbit.com/1.2/user/",fitbitID,"/sleep/")
  GET(url = paste0(url_sleep, sprintf("date/%s.json", date)),
      add_headers(Authorization = paste0("Bearer ", fitbitToken)), content_type_json())
}

date <- lubridate::today()
logSleep <- tidy_output(extract_content(check_response(getSleep(date))), simplify = FALSE)

sleepLevel.df <- data.frame(logSleep$sleep$levels$data)
sleepLevel.df$dateTime <- substring(sleepLevel.df$dateTime, 12,16)

# output
cat("###asleep",logSleep$summary$totalMinutesAsleep,
    "###graph(numOfLevel)",nrow(sleepLevel.df),
    "###graph(time)",paste0(sleepLevel.df$dateTime,collapse = "|"),
    "###graph(level)",paste0(sleepLevel.df$level,collapse = "|"),
    "###graph(duration)",paste0(sleepLevel.df$seconds,collapse = "|"),
    "###tossTurnTime(m,s)",paste0(sleepLevel.df$seconds[1]%/%60,"|",sleepLevel.df$seconds[1]%%60),
    "###wakeupInSleep",logSleep$summary$stages$wake-sleepLevel.df$seconds[1]%/%60,
    "###rem",logSleep$summary$stages$rem,
    "###light",logSleep$summary$stages$light,
    "###deep",logSleep$summary$stages$deep)