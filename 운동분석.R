############ setting ############
# 오입력 방지를 위한 데이터 복사
df = data.frame(Exercise_Data)
n = nrow(df)

############ extract exercise ############
if(n==0) {
  exercise_routine <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA)
} else {
  exercise_routine <- data.table()
  for (i in 1:n){
    ex_data <- df$data[[i]]
    if (length(ex_data)==0)  next
    if (ex_data$name=="")  next
    ex_date <- df$date[i]
    ex_name <- ex_data$name
    ex_time <- t(data.frame(ex_data$time))
    ex_met <- ex_data$mets
    ex_type <- ex_data$type
    ex <- cbind(user_id, ex_date, ex_name, ex_time, ex_met, ex_type)
    exercise_routine <- rbind(exercise_routine,ex)
  }
  # empty control, clear useless
  if(nrow(exercise_routine)==0) {
    exercise_routine <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    rm(i, ex_data)
  } else{
    rm(i, ex_data, ex_date, ex_name, ex_time, ex_met, ex_type, ex)
  }
}
names(exercise_routine) <- c("user_ID", "date", "name", "start hour","start minute","duration","mets", "types")
Exercise_Data <- exercise_routine %>% mutate(calories_consumed = as.numeric(mets)*3.5 * User_Data$weight * 0.005 * as.numeric(duration))
rm(df,n,exercise_routine)

############ fitbit activity log ############
if(!is.na(fitbitID) & !is.na(fitbitToken))  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\fitbit\\fitbitActivity.R")

############ 일자별 칼로리 소모 계산 ############
total_exercise_Data <- Exercise_Data %>%
  group_by(date,types) %>%
  summarise(calories_total = sum(calories_consumed), .groups = 'drop')
total_exercise_Data$date <- as.Date(total_exercise_Data$date)
if(length(total_exercise_Data[which(total_exercise_Data$date == Today & total_exercise_Data$types == "aero"), ]$calories_total)==0){
  total_exercise_Data <- rbind(total_exercise_Data,data.table(Today,"aero",0),use.names=FALSE)}
if(length(total_exercise_Data[which(total_exercise_Data$date == Today & total_exercise_Data$types == "anaero"), ]$calories_total)==0){
  total_exercise_Data <- rbind(total_exercise_Data,data.table(Today,"anaero",0),use.names=FALSE)}
if(length(total_exercise_Data[which(total_exercise_Data$date == Today & total_exercise_Data$types == "mix"), ]$calories_total)==0){
  total_exercise_Data <- rbind(total_exercise_Data,data.table(Today,"mix",0),use.names=FALSE)}

Exercise_Data <- Exercise_Data %>%
  group_by(date) %>%
  summarise(Exercise_calories_consumed_sum = sum(calories_consumed))
Exercise_Data$date <- as.Date(Exercise_Data$date)

Total <- merge(Total_meal, Exercise_Data, by="date", all.x=TRUE)
Total[is.na(Total)] <- 0

Total <- Total %>% mutate(calories_remained = calorie_total - User_Data$BMR - Exercise_calories_consumed_sum)
