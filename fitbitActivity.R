# activity fx (https://api.fitbit.com/1/user/[user-id]/activities/date/[date].json)
getActivity <- function(date = NULL){
  url_api  <- paste0("https://api.fitbit.com/1/user/", fitbitID,'/') # Use "-" (dash) for current logged-in user.
  url_activity <- paste0(url_api, 'activities/')
  GET(url = paste0(url_activity, sprintf('date/%s.json', date)),
      add_headers(Authorization = paste0('Bearer ', fitbitToken)), content_type_json())
}

options(digits = 2)
# execution
p <- as.numeric(today()-Total_meal$date[1]) #변경
date <- as.character.Date(today() - days(p:0))

fbAct.tb <- data.table()
for(i in 1:(p+1) ) {
  logActivity <- tidy_output(extract_content(check_response(getActivity(date[i]))), simplify = FALSE)  # check - str(logActivity)
  if(length(logActivity$activities)==0) next
  
  t <- data.table(
    User_Data$`_id`,
    logActivity$activities$startDate,
    case_when(logActivity$activities$name=="Walk" ~ "걷기",
              logActivity$activities$name=="Run"  ~ "달리기",
              logActivity$activities$name=="Swim" ~ "수영",
              logActivity$activities$name=="Bike" ~ "자전거타기"),
    substring(logActivity$activities$startTime,1,2),
    substring(logActivity$activities$startTime,4,5),
    as.character(logActivity$activities$duration/60000),
    logActivity$activities$calories/(3.5*User_Data$weight*0.005*(logActivity$activities$duration/60000)),
    case_when(logActivity$activities$name=="Walk" ~ "aero",
              logActivity$activities$name=="Run"  ~ "aero",
              logActivity$activities$name=="Swim" ~ "mix",
              logActivity$activities$name=="Bike" ~ "mix"),
    as.numeric(logActivity$activities$calories) )
  
  fbAct.tb <- rbind(fbAct.tb, t)
}

if(nrow(fbAct.tb)!=0){
  names(fbAct.tb) <- c("user_ID","date","name","start hour","start minute","duration","mets","types","calories_consumed")
  Exercise_Data <- rbind(Exercise_Data, fbAct.tb)
}

