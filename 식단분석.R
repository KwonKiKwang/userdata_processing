############ setting ############
options(digits=4) # 유효숫자 조정 option

# time setting
Today <- today()  # 오늘

# 오입력 방지를 위한 데이터 복사
df = data.frame(Meal_Data)
n = nrow(df)

############ extract breakfast ############
if(n==0) {
  Meal_breakfast <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
} else {
  Meal_breakfast <- data.table()  # 아침 식단 Data가 저장될 데이터테이블
  for (i in 1:n){
    breakfast_menu <- df$breakfast$menus[[i]]
    if(length(breakfast_menu)==0 )  next
    breakfast_date <- df$date[i]
    breakfast_time <- df$breakfast$time[[i]]
    if(grepl("cholesterol_mg", paste(dQuote(colnames(breakfast_menu)), collapse = ", "), fixed = FALSE)){
      breakfast_menu <- breakfast_menu[,c("name", "category", "calorie", "protein_g", "fat_g", "carbo_g", "sugars_g", "cholesterol_mg", "saturatedFat_g", "transFat_g", "triglyceride_g")]
      breakfast_menu$cholesterol_mg <- breakfast_menu$cholesterol_mg / 1000
      names(breakfast_menu) <- c("name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride")
    }
    breakfast_amount <- df$breakfast$amount[[i]]
    breakfast <- cbind(user_id, breakfast_date, breakfast_time, breakfast_menu, breakfast_amount)
    Meal_breakfast <- rbind(Meal_breakfast,breakfast)
  }
  # empty control, clear useless
  if(nrow(Meal_breakfast)==0) {
    Meal_breakfast <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    rm(breakfast_menu)
  } else{
    rm(breakfast_menu,breakfast_date,breakfast_time,breakfast_amount,breakfast)
  }
}
names(Meal_breakfast) <- c("user_ID", "date", "breakfast_time", "name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride", "amount")
Meal_breakfast$amount[is.na(Meal_breakfast$amount)] <- 1

############ extract lunch ############
if(n==0) {
  Meal_lunch <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
} else {
  Meal_lunch <- data.table()  # 아침 식단 Data가 저장될 데이터테이블
  for (i in 1:n){
    lunch_menu <- df$lunch$menus[[i]]
    if (length(lunch_menu)==0 )  next
    lunch_date <- df$date[i]
    lunch_time <- df$lunch$time[[i]]
    if(grepl("cholesterol_mg", paste(dQuote(colnames(lunch_menu)), collapse = ", "), fixed = FALSE)){
      lunch_menu <- lunch_menu[,c("name", "category", "calorie", "protein_g", "fat_g", "carbo_g", "sugars_g", "cholesterol_mg", "saturatedFat_g", "transFat_g", "triglyceride_g")]
      lunch_menu$cholesterol_mg <- lunch_menu$cholesterol_mg / 1000
      names(lunch_menu) <- c("name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride")
    }
    lunch_amount <- df$lunch$amount[[i]]
    lunch <- cbind(user_id, lunch_date, lunch_time, lunch_menu, lunch_amount)
    Meal_lunch <- rbind(Meal_lunch,lunch)
  }
  # empty control, clear useless
  if(nrow(Meal_lunch)==0) {
    Meal_lunch <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    rm(lunch_menu)
  } else{
    rm(lunch_menu,lunch_date,lunch_time,lunch_amount,lunch)
  }
}
names(Meal_lunch) <- c("user_ID", "date", "lunch_time", "name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride", "amount")
Meal_lunch$amount[is.na(Meal_lunch$amount)] <- 1

############ extract dinner ############
if(n==0) {
  Meal_dinner <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
} else {
  Meal_dinner <- data.table()  # 아침 식단 Data가 저장될 데이터테이블
  for (i in 1:n){
    dinner_menu <- df$dinner$menus[[i]]
    if (length(dinner_menu)==0 )  next
    dinner_date <- df$date[i]
    dinner_time <- df$dinner$time[[i]]
    if(grepl("cholesterol_mg", paste(dQuote(colnames(dinner_menu)), collapse = ", "), fixed = FALSE)){
      dinner_menu <- dinner_menu[,c("name", "category", "calorie", "protein_g", "fat_g", "carbo_g", "sugars_g", "cholesterol_mg", "saturatedFat_g", "transFat_g", "triglyceride_g")]
      dinner_menu$cholesterol_mg <- dinner_menu$cholesterol_mg / 1000
      names(dinner_menu) <- c("name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride")
    }
    dinner_amount <- df$dinner$amount[[i]]
    dinner <- cbind(user_id, dinner_date, dinner_time, dinner_menu, dinner_amount)
    Meal_dinner <- rbind(Meal_dinner,dinner)
  }
  # empty control, clear useless
  if(nrow(Meal_dinner)==0) {
    Meal_dinner <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    rm(dinner_menu)
  } else{
    rm(dinner_menu,dinner_date,dinner_time,dinner_amount,dinner)
  }
}
names(Meal_dinner) <- c("user_ID", "date", "dinner_time", "name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride", "amount")
Meal_dinner$amount[is.na(Meal_dinner$amount)] <- 1

############ extract snack ############
if(n==0) {
  Meal_snack <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
} else {
  Meal_snack <- data.table()  # 아침 식단 Data가 저장될 데이터테이블
  for (i in 1:n){
    snack_menu <- df$snack$menus[[i]]
    if (length(snack_menu)==0 )  next
    snack_date <- df$date[i]
    snack_time <- df$snack$time[[i]]
    if(grepl("cholesterol_mg", paste(dQuote(colnames(snack_menu)), collapse = ", "), fixed = FALSE)){
      snack_menu <- snack_menu[,c("name", "category", "calorie", "protein_g", "fat_g", "carbo_g", "sugars_g", "cholesterol_mg", "saturatedFat_g", "transFat_g", "triglyceride_g")]
      snack_menu$cholesterol_mg <- snack_menu$cholesterol_mg / 1000
      names(snack_menu) <- c("name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride")
    }
    snack_amount <- df$snack$amount[[i]]
    snack <- cbind(user_id, snack_date, snack_time, snack_menu, snack_amount)
    Meal_snack <- rbind(Meal_snack,snack)
  }
  # empty control, clear useless
  if(nrow(Meal_snack)==0) {
    Meal_snack <- data.table(User_Data$`_id`,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    rm(snack_menu)
  } else{
    rm(snack_menu,snack_date,snack_time,snack_amount,snack)
  }
}
names(Meal_snack) <- c("user_ID", "date", "snack_time", "name", "category", "calorie", "protein", "fat", "carbo", "sugars", "cholesterol", "sat_fat", "trans_fat", "triglyceride", "amount")
Meal_snack$amount[is.na(Meal_snack$amount)] <- 1

rm(df,n)

############ fitbit food log ############
#if(!is.na(fitbitID) & !is.na(fitbitToken))
#  {source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\fitbit\\fitbitFood.R")}

############ 영양성분 계산 ############
# 아침으로 섭취한 식단의 일자별 총 영양성분
total_Meal_breakfast <- Meal_breakfast %>%
  group_by(date) %>%
  summarise(calorie_sum = sum(calorie*amount, na.rm = TRUE),carbo_sum = sum(carbo*amount, na.rm = TRUE), protein_sum = sum(protein*amount, na.rm = TRUE), fat_sum = sum(fat*amount, na.rm = TRUE), sugars_sum = sum(sugars*amount, na.rm = TRUE), cholesterol_sum = sum(cholesterol*amount, na.rm = TRUE), sat_fat_sum = sum(sat_fat*amount, na.rm = TRUE), trans_fat_sum = sum(trans_fat*amount, na.rm = TRUE), triglyceride_sum = sum(triglyceride*amount, na.rm = TRUE))
# 점심으로 섭취한 식단의 일자별 총 영양성분
total_Meal_lunch <- Meal_lunch %>%
  group_by(date) %>%
  summarise(calorie_sum = sum(calorie*amount, na.rm = TRUE),carbo_sum = sum(carbo*amount, na.rm = TRUE), protein_sum = sum(protein*amount, na.rm = TRUE), fat_sum = sum(fat*amount, na.rm = TRUE), sugars_sum = sum(sugars*amount, na.rm = TRUE), cholesterol_sum = sum(cholesterol*amount, na.rm = TRUE), sat_fat_sum = sum(sat_fat*amount, na.rm = TRUE), trans_fat_sum = sum(trans_fat*amount, na.rm = TRUE), triglyceride_sum = sum(triglyceride*amount, na.rm = TRUE))
# 저녁으로 섭취한 식단의 일자별 총 영양성분
total_Meal_dinner <- Meal_dinner %>%
  group_by(date) %>%
  summarise(calorie_sum = sum(calorie*amount, na.rm = TRUE),carbo_sum = sum(carbo*amount, na.rm = TRUE), protein_sum = sum(protein*amount, na.rm = TRUE), fat_sum = sum(fat*amount, na.rm = TRUE), sugars_sum = sum(sugars*amount, na.rm = TRUE), cholesterol_sum = sum(cholesterol*amount, na.rm = TRUE), sat_fat_sum = sum(sat_fat*amount, na.rm = TRUE), trans_fat_sum = sum(trans_fat*amount, na.rm = TRUE), triglyceride_sum = sum(triglyceride*amount, na.rm = TRUE))
# 간식으로 섭취한 식단의 일자별 총 영양성분
total_Meal_snack <- Meal_snack %>%
  group_by(date) %>%
  summarise(calorie_sum = sum(calorie*amount, na.rm = TRUE),carbo_sum = sum(carbo*amount, na.rm = TRUE), protein_sum = sum(protein*amount, na.rm = TRUE), fat_sum = sum(fat*amount, na.rm = TRUE), sugars_sum = sum(sugars*amount, na.rm = TRUE), cholesterol_sum = sum(cholesterol*amount, na.rm = TRUE), sat_fat_sum = sum(sat_fat*amount, na.rm = TRUE), trans_fat_sum = sum(trans_fat*amount, na.rm = TRUE), triglyceride_sum = sum(triglyceride*amount, na.rm = TRUE))
# 일자별 총 영양성분 Total_meal에 저장됨
Total_meal <- rbind(total_Meal_breakfast, total_Meal_lunch, total_Meal_dinner, total_Meal_snack) %>%
  group_by(date) %>%
  summarise(calorie_total = sum(calorie_sum, na.rm = TRUE),carbo_total = sum(carbo_sum, na.rm = TRUE), protein_total = sum(protein_sum, na.rm = TRUE), fat_total = sum(fat_sum, na.rm = TRUE), sugars_total = sum(sugars_sum, na.rm = TRUE), cholesterol_total = sum(cholesterol_sum, na.rm = TRUE), sat_fat_total = sum(sat_fat_sum, na.rm = TRUE), trans_fat_total = sum(trans_fat_sum, na.rm = TRUE), triglyceride_total = sum(triglyceride_sum, na.rm = TRUE))

# date column의 형식이 factor로 변환된 것 date로 고치기
Meal_breakfast$date <- as.Date(Meal_breakfast$date)
Meal_lunch$date <- as.Date(Meal_lunch$date)
Meal_dinner$date <- as.Date(Meal_dinner$date)
Meal_snack$date <- as.Date(Meal_snack$date)

total_Meal_breakfast$date <- as.Date(total_Meal_breakfast$date)
total_Meal_lunch$date <- as.Date(total_Meal_lunch$date)
total_Meal_dinner$date <- as.Date(total_Meal_dinner$date)
total_Meal_snack$date <- as.Date(total_Meal_snack$date)

Total_meal$date <- as.Date(Total_meal$date)
Total_meal <- Total_meal[order(Total_meal$date), ]

############ 영양 섭취 기준 값 설정 ############
# 기준 값
trans_fat_standard <-2.2  # 권장 섭취량 2000kcal인 사람 기준 트랜스지방 권장섭취량 2.2g
sat_fat_standard <-17.6   # 권장 섭취량 2000kcal인 사람 기준 포화지방 권장섭취량 17.6g
chol_standard <-0.3       # 권장 콜레스테롤 섭취량 300mg
sugar_standard <- 50      # 권장 섭취량 2000kcal인 사람 기준 당 권장 섭취량 50g
triglyceride_standard <- 25 # 권장 섭취량 2000kcal인 사람 기준 당 권장 섭취량 25g
# user 기초 대사량 기반 기준 값
User_trans_fat_standard <- (trans_fat_standard*User_Data$BMR/2000) 
User_sat_fat_standard <- (sat_fat_standard*User_Data$BMR/2000) 
User_sugar_standard <- (sugar_standard*User_Data$BMR/2000)
User_triglyceride_standard <- (triglyceride_standard*User_Data$BMR/2000) #
standard <- c(User_trans_fat_standard, User_sat_fat_standard, chol_standard, User_sugar_standard, User_triglyceride_standard) 


############ 기준 값에 따른 섭취량 ############
Total_meal <- Total_meal %>% mutate(calorie_ratio = round(calorie_total/User_Data$BMR*100,1),   # 기초대사량 대비 섭취 칼로리 비율
                                    carbo_ratio = round(carbo_total*4/(carbo_total*4+protein_total*4+fat_total*9)*100,1),     # 탄수화물 섭취 비율
                                    protein_ratio = round(protein_total*4/(carbo_total*4+protein_total*4+fat_total*9)*100,1), # 단백질 섭취 비율
                                    fat_ratio = 100-carbo_ratio-protein_ratio,                  # 지방 섭취 비율
                                    sugars_ratio = round(sugars_total/standard[4]*100,2),       # 권장 당 섭취량 대비 당 섭취 비율
                                    chol_ratio = round(cholesterol_total/standard[3]*100,2),    # 권장 콜레스테롤 섭취량 대비 콜레스테롤 섭취 비율
                                    sat_fat_ratio = round(sat_fat_total/standard[2]*100,2),     # 권장 포화지방 섭취량 대비 포화지방 섭취 비율
                                    trans_fat_ratio = round(trans_fat_total/standard[1]*100,2),  # 권장 트랜스지방 섭취량 대비 트랜스지방 섭취 비율
                                    triglyceride_ratio = round(triglyceride_total/standard[5]*100,2)
)
Total_meal <- na.omit(Total_meal) # 아무 값도 입력되지 않았을 경우, 빈 테이블로 만든다

############ 식단 분석 - 오늘 ############
# d1_meal
d1_meal_breakfast <- subset(Meal_breakfast, Meal_breakfast$date==Today)
# if(nrow(d1_meal_breakfast)==0)  d1_meal_breakfast <- data.table(User_Data$`_id`,NA,NA," ",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
d1_meal_lunch <- subset(Meal_lunch, Meal_lunch$date==Today)
# if(nrow(d1_meal_lunch)==0)  d1_meal_lunch <- data.table(User_Data$`_id`,NA,NA," ",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
d1_meal_dinner <- subset(Meal_dinner, Meal_dinner$date==Today)
# if(nrow(d1_meal_dinner)==0)  d1_meal_dinner <- data.table(User_Data$`_id`,NA,NA," ",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
d1_meal_snack <- subset(Meal_snack, Meal_snack$date==Today)
# if(nrow(d1_meal_snack)==0)  d1_meal_snack <- data.table(User_Data$`_id`,NA,NA," ",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
# d1 영양섭취량
d1_total <- subset(Total_meal, Total_meal$date==Today)

############ 식단 분석 - 이번주(일주일) ############
# d7 영양섭취량
d7_total <- subset(Total_meal, Today - days(7) < Total_meal$date & Total_meal$date <= Today)
d7absense <- as.character.Date(today() - days(6:0))
for(i in d7absense) {
  if(sum(d7_total$date==i)==0)
    d7_total <- rbind(d7_total,c(i,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
}
d7_total <- arrange(d7_total,date)
rm(d7absense,i)

############ 식단 분석 - 이번달(30일) ############
# d30 영양섭취량
# d30_total <- subset(Total_meal, Today - days(30) < Total_meal$date & Total_meal$date <= Today)

# 한달동안 아침, 점심, 저녁, 간식 끼니 별 영양성분의 평균
# 전체를 평균내면 기록하지 않은 것까지 포함되어 수치가 작아짐. 정확한 월 평균을 내기 위해 끼니별 평균을 구함.
d30_mean_breakfast <- subset(total_Meal_breakfast, Today - days(30) < total_Meal_breakfast$date & total_Meal_breakfast$date <= Today) %>%
  summarize(calorie_mean = mean(calorie_sum, na.rm = TRUE),carbo_mean = mean(carbo_sum, na.rm = TRUE), protein_mean = mean(protein_sum, na.rm = TRUE), fat_mean = mean(fat_sum, na.rm = TRUE), sugars_mean = mean(sugars_sum, na.rm = TRUE), cholesterol_mean = mean(cholesterol_sum, na.rm = TRUE), sat_fat_mean = mean(sat_fat_sum, na.rm = TRUE), trans_fat_mean = mean(trans_fat_sum, na.rm = TRUE), triglyceride_mean = mean(triglyceride_sum, na.rm = TRUE))
d30_mean_lunch <- subset(total_Meal_lunch, Today - days(30) < total_Meal_lunch$date & total_Meal_lunch$date <= Today) %>%
  summarize(calorie_mean = mean(calorie_sum, na.rm = TRUE),carbo_mean = mean(carbo_sum, na.rm = TRUE), protein_mean = mean(protein_sum, na.rm = TRUE), fat_mean = mean(fat_sum, na.rm = TRUE), sugars_mean = mean(sugars_sum, na.rm = TRUE), cholesterol_mean = mean(cholesterol_sum, na.rm = TRUE), sat_fat_mean = mean(sat_fat_sum, na.rm = TRUE), trans_fat_mean = mean(trans_fat_sum, na.rm = TRUE), triglyceride_mean = mean(triglyceride_sum, na.rm = TRUE))
d30_mean_dinner <- subset(total_Meal_dinner, Today - days(30) < total_Meal_dinner$date & total_Meal_dinner$date <= Today) %>%
  summarize(calorie_mean = mean(calorie_sum, na.rm = TRUE),carbo_mean = mean(carbo_sum, na.rm = TRUE), protein_mean = mean(protein_sum, na.rm = TRUE), fat_mean = mean(fat_sum, na.rm = TRUE), sugars_mean = mean(sugars_sum, na.rm = TRUE), cholesterol_mean = mean(cholesterol_sum, na.rm = TRUE), sat_fat_mean = mean(sat_fat_sum, na.rm = TRUE), trans_fat_mean = mean(trans_fat_sum, na.rm = TRUE), triglyceride_mean = mean(triglyceride_sum, na.rm = TRUE))
d30_mean_snack <- subset(total_Meal_snack, Today - days(30) < total_Meal_snack$date & total_Meal_snack$date <= Today) %>%
  summarize(calorie_mean = mean(calorie_sum, na.rm = TRUE),carbo_mean = mean(carbo_sum, na.rm = TRUE), protein_mean = mean(protein_sum, na.rm = TRUE), fat_mean = mean(fat_sum, na.rm = TRUE), sugars_mean = mean(sugars_sum, na.rm = TRUE), cholesterol_mean = mean(cholesterol_sum, na.rm = TRUE), sat_fat_mean = mean(sat_fat_sum, na.rm = TRUE), trans_fat_mean = mean(trans_fat_sum, na.rm = TRUE), triglyceride_mean = mean(triglyceride_sum, na.rm = TRUE))

# 한달동안 일일 평균 영양분 섭취량
d30_mean_total <- rbind(d30_mean_breakfast, d30_mean_lunch, d30_mean_dinner, d30_mean_snack) %>%
  summarise(calorie_mean_total = sum(calorie_mean, na.rm = TRUE),carbo_mean_total = sum(carbo_mean, na.rm = TRUE), protein_mean_total = sum(protein_mean, na.rm = TRUE), fat_mean_total = sum(fat_mean, na.rm = TRUE), sugars_mean_total = sum(sugars_mean, na.rm = TRUE), cholesterol_mean_total = sum(cholesterol_mean, na.rm = TRUE), sat_fat_mean_total = sum(sat_fat_mean, na.rm = TRUE), trans_fat_mean_total = sum(trans_fat_mean, na.rm = TRUE), triglyceride_mean_total = sum(triglyceride_mean, na.rm = TRUE)) %>%
  mutate(calorie_ratio = round(calorie_mean_total/User_Data$BMR*100,1),   # 기초대사량 대비 섭취 칼로리 비율
         carbo_ratio = round(carbo_mean_total*4/(carbo_mean_total*4+protein_mean_total*4+fat_mean_total*9)*100,1),     # 탄수화물 섭취 비율
         protein_ratio = round(protein_mean_total*4/(carbo_mean_total*4+protein_mean_total*4+fat_mean_total*9)*100,1), # 단백질 섭취 비율
         fat_ratio = 100-carbo_ratio-protein_ratio,                       # 지방 섭취 비율
         sugars_ratio = round(sugars_mean_total/standard[4]*100,2),       # 권장 당 섭취량 대비 당 섭취 비율
         chol_ratio = round(cholesterol_mean_total/standard[3]*100,2),    # 권장 콜레스테롤 섭취량 대비 콜레스테롤 섭취 비율
         sat_fat_ratio = round(sat_fat_mean_total/standard[2]*100,2),     # 권장 포화지방 섭취량 대비 포화지방 섭취 비율
         trans_fat_ratio = round(trans_fat_mean_total/standard[1]*100,2) )# 권장 트랜스지방 섭취량 대비 트랜스지방 섭취 비율

