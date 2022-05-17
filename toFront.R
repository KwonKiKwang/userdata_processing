date <- Today

## 입력하지 않은 데이터의 결과값인 numeric(0) 처리
isEmpty <- function(x) {
  return(length(x)==0)
}

############ 운동 분석 ############
if(flag==2|flag=='2') {
  if(!isEmpty(Total[which(Total$date == date), ]$calorie_total)) {
    # 입력값이 있을 경우
    cat("###totalConsumeCal",Total[which(Total$date == date), ]$calorie_total,
        "###exerciseConsume",Total[which(Total$date == date), ]$Exercise_calories_consumed_sum,
        "###exerEfficiency",Total[which(Total$date == date), ]$Exercise_calories_consumed_sum/Total[which(Total$date == date), ]$calorie_total * 100,
        "###aeroRatio",total_exercise_Data$calories_total[which(total_exercise_Data$date == date & total_exercise_Data$types == "aero")] / Total[which(Total$date == date), ]$Exercise_calories_consumed_sum,
        "###unaeroRatio",total_exercise_Data$calories_total[which(total_exercise_Data$date == date & total_exercise_Data$types == "anaero")] / Total[which(Total$date == date), ]$Exercise_calories_consumed_sum,
        "###mixRatio",total_exercise_Data$calories_total[which(total_exercise_Data$date == date & total_exercise_Data$types == "mix")] / Total[which(Total$date == date), ]$Exercise_calories_consumed_sum) 
  } else {
    # 입력값이 없을 경우
    cat("###totalConsumeCal",0,
        "###exerciseConsume",0,
        "###exerEfficiency",0,
        "###aeroRatio",0.25,
        "###unaeroRatio",0.25,
        "###mixRatio",0.5) # total_exercise_Data[which(total_exercise_Data$date == date & total_exercise_Data$types == "mix"), ]$calories_total / Total[which(Total$date == date), ]$Exercise_calories_consumed_sum
  }
} else {
  cat("###totalConsumeCal",0,
      "###exerciseConsume",0,
      "###exerEfficiency",0,
      "###aeroRatio",0,
      "###unaeroRatio",0,
      "###mixRatio",0)
}


############ 식단 분석 ############
if(flag==1|flag=='1') {
  if(!isEmpty(d1_total$calorie_total)) {  # 입력값이 있을 경우-하루
    cat("###recommendCal",User_Data$BMR,
        "###d1ConsumedCal",d1_total$calorie_total,
        "###d1ComsumedCalRate",d1_total$calorie_total/User_Data$BMR * 100,
        "###carboRatio",d1_total$carbo_ratio,
        "###carboGram",d1_total$carbo_total,
        "###proRatio",d1_total$protein_ratio,
        "###proGram",d1_total$protein_total,
        "###fatRatio",d1_total$fat_ratio,
        "###fatGram",d1_total$fat_total,
        "###cholRatio",d1_total$chol_ratio,
        "###satFatRatio",d1_total$sat_fat_ratio,
        "###transRatio",d1_total$trans_fat_ratio,
        "###recommendSugar",standard[4],
        "###consumedSugar",d1_total$sugars_total)
    cat("###d1Break",paste0(d1_meal_breakfast$name,collapse = "|"),
        "###d1Lunch",paste0(d1_meal_lunch$name,collapse = "|"),
        "###d1Dinner",paste0(d1_meal_dinner$name,collapse = "|"),
        "###d1Snack",paste0(d1_meal_snack$name,collapse = "|") )
  } else {                                # 입력값이 없을 경우-하루
    cat("###recommendCal",User_Data$BMR,
        "###d1ConsumedCal",0,
        "###d1ComsumedCalRate",0,
        "###carboRatio",0,
        "###carboGram",0,
        "###proRatio",0,
        "###proGram",0,
        "###fatRatio",0,
        "###fatGram",0,
        "###cholRatio",0,
        "###satFatRatio",0,
        "###transRatio",0,
        "###recommendSugar",standard[4],
        "###consumedSugar",0)
    cat("###d1Break","empty",
        "###d1Lunch","empty",
        "###d1Dinner","empty",
        "###d1Snack","empty")
  }
  if(sum(d7_total$calorie_total == "0") != 7) { # 입력값이 있을 경우-일주일
    cat("###d7ConsumedCal",d7_total$calorie_total,
        "###d7Carbo",d7_total$carbo_ratio,
        "###d7Protein",d7_total$protein_ratio,
        "###d7Fat",d7_total$fat_ratio,
        "###d7Chol",d7_total$chol_ratio,
        "###d7Satfat",d7_total$sat_fat_ratio,
        "###d7TransFat",d7_total$trans_fat_ratio,
        "###d7TotalSugar",d7_total$sugars_ratio)
  } else {                                      # 입력값이 없을 경우-일주일
    cat("###d7ConsumedCal",c("0","0","0","0","0","0","0"),
        "###d7Carbo",c("0","0","0","0","0","0","0"),
        "###d7Protein",c("0","0","0","0","0","0","0"),
        "###d7Fat",c("0","0","0","0","0","0","0"),
        "###d7Chol",c("0","0","0","0","0","0","0"),
        "###d7Satfat",c("0","0","0","0","0","0","0"),
        "###d7TransFat",c("0","0","0","0","0","0","0"),
        "###d7TotalSugar",c("0","0","0","0","0","0","0"))    
  }
  if(d30_mean_total$calorie_mean_total[1] != 0) { # 입력값이 있을 경우-한달
    cat("###d30CalCarboProteinFatSugarCholSatfatTransfat",ifelse(d30_mean_total[ ,c("carbo_ratio","protein_ratio","fat_ratio","sugars_ratio","chol_ratio","sat_fat_ratio","trans_fat_ratio")]<80,"L",
                                                                 ifelse(d30_mean_total[ ,c("carbo_ratio","protein_ratio","fat_ratio","sugars_ratio","chol_ratio","sat_fat_ratio","trans_fat_ratio")]>120,"H","M") ) )  
  } else {                                        # 입력값이 없을 경우-한달
    cat("###d30CalCarboProteinFatSugarCholSatfatTransfat",c("L","L","L","L","L","L","L")  )
  }
} else {
  cat("###recommendCal",0,
      "###d1ConsumedCal",0,
      "###d1ComsumedCalRate",0,
      "###carboRatio",0,
      "###carboGram",0,
      "###proRatio",0,
      "###proGram",0,
      "###fatRatio",0,
      "###fatGram",0,
      "###cholRatio",0,
      "###satFatRatio",0,
      "###transRatio",0,
      "###recommendSugar",0,
      "###consumedSugar",0,
      "###d7ConsumedCal",0,
      "###d7Carbo",0,
      "###d7Protein",0,
      "###d7Fat",0,
      "###d7Chol",0,
      "###d7Satfat",0,
      "###d7TransFat",0,
      "###d7TotalSugar",0,
      "###d30CalCarboProteinFatSugarCholSatfatTransfat",c("M","M","M","M","M","M","M")
      )
}


############ 추천 ############
if(flag==3|flag=='3') {
  ## 추천식단
  cat("###d7BreakfastMenu",paste0(br_menu[1, ],collapse = "|"),
      paste0(br_menu[2, ],collapse = "|"),
      paste0(br_menu[3, ],collapse = "|"),
      paste0(br_menu[4, ],collapse = "|"),
      paste0(br_menu[5, ],collapse = "|"),
      paste0(br_menu[6, ],collapse = "|"),
      paste0(br_menu[7, ],collapse = "|"),
      "###d7BreakfastCategory",paste0(br_cat[1, ],collapse = "|"),
      paste0(br_cat[2, ],collapse = "|"),
      paste0(br_cat[3, ],collapse = "|"),
      paste0(br_cat[4, ],collapse = "|"),
      paste0(br_cat[5, ],collapse = "|"),
      paste0(br_cat[6, ],collapse = "|"),
      paste0(br_cat[7, ],collapse = "|"),
      "###d7BreakfastCal",br_nut$calorie)
  cat("###d7LunchMenu",paste0(meal$lu_menu[1, ],collapse = "|"),
      paste0(meal$lu_menu[2, ],collapse = "|"),
      paste0(meal$lu_menu[3, ],collapse = "|"),
      paste0(meal$lu_menu[4, ],collapse = "|"),
      paste0(meal$lu_menu[5, ],collapse = "|"),
      paste0(meal$lu_menu[6, ],collapse = "|"),
      paste0(meal$lu_menu[7, ],collapse = "|"),
      "###d7LunchCategory",paste0(meal$lu_cat[1, ],collapse = "|"),
      paste0(meal$lu_cat[2, ],collapse = "|"),
      paste0(meal$lu_cat[3, ],collapse = "|"),
      paste0(meal$lu_cat[4, ],collapse = "|"),
      paste0(meal$lu_cat[5, ],collapse = "|"),
      paste0(meal$lu_cat[6, ],collapse = "|"),
      paste0(meal$lu_cat[7, ],collapse = "|"),
      "###d7LunchCal",meal$lu_nut$calorie)
  cat("###d7DinnerMenu",paste0(meal$di_menu[1, ],collapse = "|"),
      paste0(meal$di_menu[2, ],collapse = "|"),
      paste0(meal$di_menu[3, ],collapse = "|"),
      paste0(meal$di_menu[4, ],collapse = "|"),
      paste0(meal$di_menu[5, ],collapse = "|"),
      paste0(meal$di_menu[6, ],collapse = "|"),
      paste0(meal$di_menu[7, ],collapse = "|"),
      "###d7DinnerCategory",paste0(meal$di_cat[1, ],collapse = "|"),
      paste0(meal$di_cat[2, ],collapse = "|"),
      paste0(meal$di_cat[3, ],collapse = "|"),
      paste0(meal$di_cat[4, ],collapse = "|"),
      paste0(meal$di_cat[5, ],collapse = "|"),
      paste0(meal$di_cat[6, ],collapse = "|"),
      paste0(meal$di_cat[7, ],collapse = "|"),
      "###d7DinnerCal",meal$di_nut$calorie)
  cat("###dailyNut",paste0(to_nut$calorie,"|",to_nut$carbo,"|",to_nut$protein,"|",to_nut$fat,"|",to_nut$sugars,"|",
                           to_nut$cholesterol,"|",to_nut$sat_fat,"|",to_nut$trans_fat,"|",to_nut$triglyceride))
  
  ## 추천운동
  cat("###exercise",paste0(exer$ex,collapse = "|"),
      "###exerciseTime",paste0(exer$RExTime,collapse = "|") )
  cat("###exerciseCal",paste0(exer$excal,collapse = "|"),
      "###exerciseMuscleInc",paste0(exer$mus,collapse = "|"),
      "###exerciseBodyfatDec",paste0(exer$bodyf,collapse = "|") )
} else {
  ## 추천식단
  cat("###d7BreakfastMenu",0,
      "###d7BreakfastCategory",0,
      "###d7BreakfastCal",0)
  cat("###d7LunchMenu",0,
      "###d7LunchCategory",0,
      "###d7LunchCal",0)
  cat("###d7DinnerMenu",0,
      "###d7DinnerCategory",0,
      "###d7DinnerCal",0)
  cat("###dailyNut",0)
  
  ## 추천운동
  cat("###exercise",0,
      "###exerciseTime",0)
  cat("###exerciseCal",0,
      "###exerciseMuscleInc",0,
      "###exerciseBodyfatDec",0)
}

