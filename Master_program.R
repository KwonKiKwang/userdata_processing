############ from node ############
args <- commandArgs(TRUE)
user <- args[1]
flag <- args[2]
fitbitID <- args[3]
fitbitToken <- args[4]

############ for test ############
#user <- "jw.kang@hnbgenomics.co.kr"
user <- "h91oon@gmail.com"
flag <- "1"
fitbitID <- "99XPS2"
fitbitToken <- "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiIyMkM4UEYiLCJzdWIiOiI5OVhQUzIiLCJpc3MiOiJGaXRiaXQiLCJ0eXAiOiJhY2Nlc3NfdG9rZW4iLCJzY29wZXMiOiJ3aHIgd251dCB3cHJvIHdzbGUgd3dlaSB3c29jIHdhY3Qgd3NldCB3bG9jIiwiZXhwIjoxNjQ4MTg0OTExLCJpYXQiOjE2MTY2NDg5MTF9.Lkud9dwDCRHUDHtx-ZLzpHH0WLqS-uRzvevEA--Hrvk"

# user <- "jsuwan961205@gmail.com"
# flag <- "1"
# fitbitID <- "98CBNS"
# fitbitToken <- "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiIyMkM4UEYiLCJzdWIiOiI5OENCTlMiLCJpc3MiOiJGaXRiaXQiLCJ0eXAiOiJhY2Nlc3NfdG9rZW4iLCJzY29wZXMiOiJ3aHIgd251dCB3cHJvIHdzbGUgd3dlaSB3c29jIHdzZXQgd2FjdCB3bG9jIiwiZXhwIjoxNjQ2NDQxMDc2LCJpYXQiOjE2MTczNDM5NTV9.l2vkuHLGZz88GEIk2AzcMfL63_SVhRcrHDULvcBi2pU"

############ library call ############
library(dplyr, warn.conflicts = F)      # data processing
library(mongolite, warn.conflicts = F)  # data import from mongolite
library(lubridate, warn.conflicts = F)  # time control
library(data.table, warn.conflicts = F) # enable data table
library(rjson, warn.conflicts = F)
# library(jsonlite)   # fromJSON
if(flag==3|flag=="3"){
  library(cluster, warn.conflicts = F)    # call clara
  library(MASS, warn.conflicts = F)       # qda
  library(DMwR, warn.conflicts = F)       # knnimputation
}
if(!is.na(fitbitID) & !is.na(fitbitToken)) {
  library(httr, warn.conflicts = F)       # web api
  library(RCurl, warn.conflicts = F)      # web api
  library(stringr, warn.conflicts = F)    # string
  source("/home/hnbgenomics/Fitbit/fitbitCommonFx.R")   # use fitbit
}

############ user information ############
# from mongoDB(user info & blood sugar & blood pressure)
tmp <- mongo(collection = "users-permissions_user", url = "mongodb+srv://testuser:testuser@hnbgenomics.hyvl1.gcp.mongodb.net/balanstate")
# Users_Data <- tmp$find(query = '{}',fields = '{"_id":1,"username":1,"gender":1,"job":1,"email":1,"cellphone":1, "birthday":1, "height":1, "weight":1, "createdAt":1}')
tmp <- mongo(collection = "blood_data", url = "mongodb+srv://testuser:testuser@hnbgenomics.hyvl1.gcp.mongodb.net/balanstate")
Bloods_Data <- tmp$find(query = '{}')
Blood_pressures_Data <- Bloods_Data[ ,c("date","pressure","user")]
Blood_sugars_Data <- Bloods_Data[ ,c("date","sugar","user")]

# extract user info
# user_id <- as.character(subset(Users_Data, Users_Data$email==user)$`_id`) # 데이터 베이스 상 이메일로부터 User의 고유 id 가져와 user_id 변수에 저장
# User_Data <- subset(Users_Data,Users_Data$`_id`==user_id)                 # 데이터 베이스 상 User의 상세 개인 정보를 가져와 User_Data에 저장
tmp <- mongo(collection = "users-permissions_user", url = "mongodb+srv://testuser:testuser@hnbgenomics.hyvl1.gcp.mongodb.net/balanstate")
query = toJSON(list(email=user))
User_Data <- tmp$find(query=query, fields = '{"_id":1,"username":1,"gender":1,"job":1,"email":1,"cellphone":1, "birthday":1, "height":1, "weight":1, "createdAt":1, "meals": 1}')
user_id <- User_Data$'_id'
# When absence info
if(sum(is.na(User_Data))>0) source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\inputManage.R")
# extract user blood sugar&pressure
Blood_pressure_Data <- subset(Blood_pressures_Data,Blood_pressures_Data$user==user_id)
Blood_sugar_Data <- subset(Blood_sugars_Data,Blood_sugars_Data$user==user_id)

# processing user info
source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\추가정보.R")

# clear useless
rm(Blood_pressure_Data, Blood_pressures_Data, Blood_sugar_Data, Blood_sugars_Data, Bloods_Data)

############ 식단 분석 ############
if(flag==1 | flag=='1' | flag==2 | flag=='2' | flag==3 | flag=='3') {
  # from mongoDB(meals data)
  tmp <- mongo(collection = "meals", url = "mongodb+srv://testuser:testuser@hnbgenomics.hyvl1.gcp.mongodb.net/balanstate")
  # Meals_Data <- tmp$find(query = '{}')
  # # extract user meal
  # Meal_Data <- subset(Meals_Data,Meals_Data$user==user_id)  # 식단 데이터 베이스 상 User의 상세 식단 정보를 가져와 Meal_Data에 저장
  query <- toJSON(list("user" = list("$oid" = user_id)))
  Meal_Data <- tmp$find(query=query)
  Meal_Data <- Meal_Data[order(Meal_Data$date), ]           # 가장 최신 데이터? ,Meal_Data$updatedAt
  # 중복값 제거
  if(sum(duplicated(Meal_Data$date))>0) Meal_Data <- Meal_Data[-which(duplicated(Meal_Data$date,fromLast=T)), ]
  # clear useless
  # rm(tmp,Meals_Data)

  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\식단분석.R")
  if(flag==1 | flag=='1') source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\toFront.R")
}

############ 운동 분석 ############
if(flag==2 | flag=='2' | flag==3 | flag=='3') {
  # from mongoDB(exercises data)
  tmp <- mongo(collection = "exercises", url = "mongodb+srv://testuser:testuser@hnbgenomics.hyvl1.gcp.mongodb.net/balanstate")
  query <- toJSON(list("user" = list("$oid" = user_id)))
  Exercise_Data <- tmp$find(query = query)
  # extract user exercise
  # Exercise_Data <- subset(Exercises_Data, Exercises_Data$user==user_id) # 운동 데이터 베이스 상 User의 상세 운동 정보를 가져와 Exercises_Data에 저장
  Exercise_Data <- Exercise_Data[order(Exercise_Data$date), ]           # 가장 최신 데이터? ,Meal_Data$updatedAt
  # 중복값 제거
  if(sum(duplicated(Exercise_Data$date))>0) Exercise_Data <- Exercise_Data[-which(duplicated(Exercise_Data$date,fromLast=T)), ]
  # clear useless
  # rm(tmp,Exercises_Data)

  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\운동분석.R")
  if(flag==2 | flag=='2') source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\toFront.R")
}

############ 추천 ############
if(flag==3 | flag=='3') {
  # 건강검진 DB기반으로 cluster 생성
  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\clustering.R")
  # 건강검진서를 ocr로 업데이트한 파일 혹은 입력된 정보만을 가지고 사용자의 cluster 판별
  # dataframe o는 클러스터 반별된 사용자 데이터, imp 결측값 예상된 사용자 데이터
  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\qda.R")

  # from mongo(food DB)
  tmp <- mongo(collection = "foods", url = "mongodb+srv://testuser:testuser@hnbgenomics.hyvl1.gcp.mongodb.net/balanstate")
  Foods_Data <- tmp$find(query = '{}')
  # Foods data 에 대해 기본 reward 질병별 reward 적용
  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\Food_reward.R")
  # 사용자의 cluster 에 따른 Foods reward 적용 후 식단 추천
  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\mealsolution_d.R")

  # Exercises data 에 대해 기본 reward 질병별 reward 적용
  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\Exercise_reward.R")
  # 사용자의 cluster 에 따른 Exercises reward 적용 후 운동 추천
  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\exersolution_d.R")

  source("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\toFront.R")
}