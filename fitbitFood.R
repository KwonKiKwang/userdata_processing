# food fx (https://api.fitbit.com/1/user/[user-id]/foods/log/date/[date].json)
getFood <- function(date = NULL){
  url_api  <- paste0("https://api.fitbit.com/1/user/",fitbitID,'/') # paste0(url_base, "user/-/")
  url_food <- paste0(url_api, "foods/log/")
  GET(url = paste0(url_food, sprintf("date/%s.json", date)),
      add_headers(Authorization = paste0("Bearer ", fitbitToken)), content_type_json())
}

foodDB <- read.csv("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\fitbit\\foods_210406.csv",fileEncoding = "CP949")
foodDB <- foodDB[,c("name", "category", "calorie", "protein_g", "fat_g", "carbo_g", "sugars_g", "cholesterol_mg", "saturatedFat_g", "transFat_g", "triglyceride_g")]
names(foodDB) <- c("name","category","calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")
foodDB$cholesterol <- foodDB$cholesterol/1000
# foodDB <- read.csv("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\fitbit\\Foods_all.csv")

##### use with mongo ver. #####
# from mongo(food DB)
# tmp <- mongo(collection = "food_tests", url = "mongodb+srv://testuser:testuser@hnbgenomics.hyvl1.gcp.mongodb.net/balanstate")
# foodDB <- tmp$find(query = '{}', fields = '{"name":1,"category":1,"calorie":1,"carbo_g":1,"protein_g":1,"fat_g":1, "sugars_g":1, "cholesterol_mg":1, "saturatedFat_g":1, "transFat_g":1, "triglyceride_g":1}')
# foodDB <- foodDB[,c("name", "category", "calorie", "carbo_g", "protein_g", "fat_g", "sugars_g", "cholesterol_mg", "saturatedFat_g", "transFat_g", "triglyceride_g")]
# names(foodDB) <- c("name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride")
#################################

colnames(Meal_breakfast)%in%colnames(fbBreak.tb)
# execution
p <- 6 # period
date <- as.character.Date(today() - days(p:0))

# 기존 테이블에 rbind할 임시 테이블
fbBreak.tb <- data.table()
fbLunch.tb <- data.table()
fbDinner.tb <- data.table()
fbSnack.tb <- data.table()
nomatch.tb <- data.table()

# p일전부터 오늘까지 불러오기
for(i in 1:(p+1) ) {
  logFood <- tidy_output(extract_content(check_response(getFood(date[i]))), simplify = FALSE)  # check - str(logFood)
  if(length(logFood$foods)==0) next
  tFood <- data.table(time=case_when(logFood$foods$loggedFood$mealTypeId==1 ~ "breakfast",
                                     logFood$foods$loggedFood$mealTypeId==3 ~ "lunch",
                                     logFood$foods$loggedFood$mealTypeId==5 ~ "dinner",
                                     T ~ "snack"),
                      name=logFood$foods$loggedFood$name,
                      calorie=logFood$foods$nutritionalValues$calories,
                      protein=logFood$foods$nutritionalValues$protein,
                      fat=logFood$foods$nutritionalValues$fat,
                      carbo=logFood$foods$nutritionalValues$carbs,
                      amount=logFood$foods$loggedFood$amount)
  # 음식 갯수 받아와서 range 만들기
  len_range <- 1:nrow(logFood$foods)[1]
  cat(len_range)
  for (k in len_range){
    # unit name에 인분이라는 텍스트가 포함되어 있지 않으면 1로 고정..!!
    # cat(i)
    cat(logFood$foods[i, ]$loggedFood$unit$name)
    if (
      ((grepl('인분', logFood$foods[k, ]$loggedFood$unit$name))==FALSE) &
      ((grepl('봉지', logFood$foods[k, ]$loggedFood$unit$name))==FALSE)
      ){
      tFood[k, ]$amount <- 1
    }
  }
  bFood <- tFood[tFood$time=="breakfast", ]
  lFood <- tFood[tFood$time=="lunch", ]
  dFood <- tFood[tFood$time=="dinner", ]
  sFood <- tFood[tFood$time=="snack", ]
  
  # 아침
  if(nrow(bFood)!=0) {
    for(j in 1:nrow(bFood) ) {
      if(nrow(foodDB[foodDB$name==bFood$name[j], ])==0) {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                        bFood$name[j],"-",bFood$calorie[j],bFood$carbo[j],bFood$protein[j],bFood$fat[j],0,0,0,0,0,bFood$amount[j])
        nomatch.tb <- rbind(nomatch.tb,t, use.names=F)
      } else {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                        foodDB[foodDB$name==bFood$name[j],c("name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride")],bFood$amount[j])
      }
      fbBreak.tb <- rbind(fbBreak.tb,t, use.names=FALSE)
    }
  }
  # 점심
  if(nrow(lFood)!=0) {
    for(j in 1:nrow(lFood) ) {
      if(nrow(foodDB[foodDB$name==lFood$name[j], ])==0) {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                        lFood$name[j],"-",lFood$calorie[j],lFood$protein[j],lFood$fat[j],lFood$carbo[j],0,0,0,0,0,lFood$amount[j])
        nomatch.tb <- rbind(nomatch.tb,t, use.names=F)
      } else {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                   foodDB[foodDB$name==lFood$name[j],c("name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride")],lFood$amount[j])
      }
      fbLunch.tb <- rbind(fbLunch.tb,t, use.names=FALSE)
    }
  }
  # 저녁
  if(nrow(dFood)!=0) {
    for(j in 1:nrow(dFood) ) {
      if(nrow(foodDB[foodDB$name==dFood$name[j], ])==0) {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                        dFood$name[j],"-",dFood$calorie[j],dFood$protein[j],dFood$fat[j],dFood$carbo[j],0,0,0,0,0,dFood$amount[j])
        nomatch.tb <- rbind(nomatch.tb,t, use.names=F)
      } else {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                   foodDB[foodDB$name==dFood$name[j],c("name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride")],dFood$amount[j])
      }
      fbDinner.tb <- rbind(fbDinner.tb,t, use.names=FALSE)
    }
  }
  # 간식
  if(nrow(sFood)!=0) {
    for(j in 1:nrow(sFood) ) {
      if(nrow(foodDB[foodDB$name==sFood$name[j], ])==0) {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                        sFood$name[j],"-",sFood$calorie[j],sFood$protein[j],sFood$fat[j],sFood$carbo[j],0,0,0,0,0,sFood$amount[j])
        nomatch.tb <- rbind(nomatch.tb,t, use.names=F)
      } else {
        t <- data.table(fitbitID,date[i],as.POSIXct(date[i]),
                   foodDB[foodDB$name==sFood$name[j],c("name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride")],sFood$amount[j])
      }
      fbSnack.tb <- rbind(fbSnack.tb,t, use.names=FALSE)
    }
  }
}


if(nrow(fbBreak.tb)!=0) {
  names(fbBreak.tb) <- c("user_ID","date","breakfast_time","name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride","amount")
  if(is.na(Meal_breakfast$name[1])) {
    Meal_breakfast <- fbBreak.tb
  } else {
    Meal_breakfast <- rbind(Meal_breakfast,fbBreak.tb)
  }
}
Meal_breakfast <- rbind(Meal_breakfast,fbBreak.tb)
if(nrow(fbLunch.tb)!=0) {
  names(fbLunch.tb) <- c("user_ID","date","lunch_time","name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride","amount")
  if(is.na(Meal_lunch$name[1])) {
    Meal_lunch <- fbLunch.tb
  } else {
    Meal_lunch <- rbind(Meal_lunch,fbLunch.tb)
  }
}
if(nrow(fbDinner.tb)!=0) {
  names(fbDinner.tb) <- c("user_ID","date","dinner_time","name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride","amount")
  if(is.na(Meal_dinner$name[1])) {
    Meal_dinner <- fbDinner.tb
  } else {
    Meal_dinner <- rbind(Meal_dinner,fbDinner.tb)
  }
}
if(nrow(fbSnack.tb)!=0) {
  names(fbSnack.tb) <- c("user_ID","date","snack_time","name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride","amount")
  if(is.na(Meal_snack$name[1])) {
    Meal_snack <- fbSnack.tb
  } else {
    Meal_snack <- rbind(Meal_snack,fbSnack.tb)
  }
}
if(nrow(nomatch.tb)!=0) {
  nomatch <- read.csv("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\fitbit\\nomatch.csv")
  nomatch$time <- as.POSIXct(nomatch$time)
  names(nomatch.tb) <- c("user_ID","date","time","name","category","calorie","carbo","protein","fat","sugars","cholesterol","sat_fat","trans_fat","triglyceride","amount")
  if(is.na(nomatch$name[1])) {
    nomatch <- nomatch.tb
  } else {
    nomatch <- rbind(nomatch,nomatch.tb)
  }
  # if(sum(duplicated(nomatch$name))>0) nomatch <- nomatch[-which(duplicated(nomatch$name,fromLast=T)), ]
  write.csv(nomatch,"C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\fitbit\\nomatch.csv", row.names=FALSE)
}
