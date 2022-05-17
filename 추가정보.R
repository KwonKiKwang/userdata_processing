# BMI 계산
User_Data$weight <- as.numeric(User_Data$weight)
User_Data$height <- as.numeric(User_Data$height)
User_Data <- User_Data %>% mutate (BMI = round(weight/(height/100)/(height/100),2))

# Age 계산
User_Data <- User_Data %>% mutate (age = year(today())-year(ymd(User_Data$birthday))+1)

# PA 값 할당
# PA : 기초대사량을 구하기 위해 사용하는 변수로 직업의 활동량에 따라 차등되어 부여되는 값
PA_F0 <- 1.0  # 비활동적 직업의 여성
PA_F1 <- 1.12 # 저활동적 직업의 여성
PA_F2 <- 1.27 # 활동적 직업의 여성
PA_F3 <- 1.45 # 매우 활동적인 직업의 여성

PA_M0 <- 1.0  # 비활동적 직업의 남성
PA_M1 <- 1.11 # 저활동적 직업의 남성
PA_M2 <- 1.25 # 활동적 직업의 남성
PA_M3 <- 1.48 # 매우 활동적인 직업의 남성

if (User_Data$gender == "female"){
  User_Data <- User_Data %>% mutate(PA = case_when(job=="professional" ~ PA_F0,    #전문직(의료/법조/재무/종교/디자인/소프트웨어)
                                                   job=="phy_edu" ~ PA_F2,         #체육지도자 및 강사
                                                   job=="sports" ~ PA_F3,          #운동선수 및 준비생
                                                   job=="public_official" ~ PA_F2, #특정직 공무원(경위 이하 경찰, 대위 이하 군인 등)
                                                   job=="teacher" ~ PA_F1,         #교직
                                                   job=="manager" ~ PA_F0,         #관리직
                                                   job=="office_worker" ~ PA_F0,   #사무직
                                                   job=="self_employed" ~ PA_F2,   #자영업
                                                   job=="sales" ~ PA_F2,           #판매직
                                                   job=="service" ~ PA_F2,         #서비스직
                                                   job=="production" ~ PA_F3,      #생산노무직
                                                   job=="technician" ~ PA_F2,      #기능직
                                                   job=="agriculture" ~ PA_F3,     #농축광수산업
                                                   job=="forestry" ~ PA_F3,        #임업
                                                   job=="student" ~ PA_F1,         #학생
                                                   job=="housewife_child" ~ PA_F2, #영유아가 있는 주부
                                                   job=="housewife" ~ PA_F1,       #영유아가 없는 주부
                                                   job=="retired" ~ PA_F0,         #퇴직연금생활자
                                                   job=="jobless" ~ PA_F1,         #무직
                                                   T ~ PA_M2                                ) )
} else{
    User_Data <- User_Data %>% mutate(PA = case_when(job=="professional" ~ PA_M0,
                                                     job=="phy_edu" ~ PA_M2,
                                                     job=="sports" ~ PA_M3,
                                                     job=="public_official" ~ PA_M2,
                                                     job=="teacher" ~ PA_M1,
                                                     job=="manager" ~ PA_M0,
                                                     job=="office_worker" ~ PA_M0,
                                                     job=="self_employed" ~ PA_M2,
                                                     job=="sales" ~ PA_M2,
                                                     job=="service" ~ PA_M2,
                                                     job=="production" ~ PA_M3,
                                                     job=="technician" ~ PA_M2,
                                                     job=="agriculture" ~ PA_M3,
                                                     job=="forestry" ~ PA_M3,
                                                     job=="student" ~ PA_M1,
                                                     job=="housewife_child" ~ PA_M2,
                                                     job=="housewife" ~ PA_M1,
                                                     job=="retired" ~ PA_M0,
                                                     job=="jobless" ~ PA_M1,
                                                     T ~ PA_M2                              ) )
}

# BMR 계산
User_Data <- User_Data %>% mutate(BMR = case_when(gender == "female" ~ round((354-6.91*age+PA*(9.36*weight+7.26*height)) ),
                                                  T ~ round((662-9.53*age+PA*(15.91*weight+5.396*height))) ) )

# 유저 혈압 데이터 추출
if(nrow(Blood_pressure_Data)==0) {
  User_blood_pressure_data = data.table(User_Data$`_id`,NA,NA,NA,NA,NA)
} else{
  dfdata = data.frame(Blood_pressure_Data)
  n <- nrow(dfdata)
  User_blood_pressure_data <- data.table()
  blood <- data.table()

  try(pressure <- dfdata$pressure[[1]] )
  try(for (i in 1:n){
    pressure <- data.table()
    # a = dfdata$user[i,] // dfdata$user에 1개의 항목밖에 없어서 [1, ] 을 못받아 오는거 같다??
    a = dfdata$user
    t = dfdata$date[i]
    pressure <- dfdata$pressure[[i]]
    k = nrow(dfdata$pressure[[i]])

    if(mode(k)!= 'NULL'){
      for(m in 1:k){
        blood <- data.table()
        pressure_time <- pressure[m,][1]
        medicine <- pressure[m,][2]
        systolic <- pressure[m,][3]
        diastolic <- pressure[m,][4]

        blood <- cbind(a, t, pressure_time, medicine, systolic, diastolic)
        User_blood_pressure_data <- rbind(User_blood_pressure_data, blood, fill=TRUE)
        #print(pressure_time)
      }
      rm(m,pressure_time,medicine,systolic,diastolic)
    }
  },
  rm(a,t,pressure,k,i) )
  rm(dfdata,n,blood)
}
names(User_blood_pressure_data) <- c("user_ID", "date", "medicine", "time", "systolic", "diastolic")

# 유저 혈당 데이터 추출
if (nrow(Blood_sugar_Data)==0){
  User_blood_sugar_data = data.table(User_Data$`_id`,NA,NA,NA,NA,NA)
}else{
  dfdata = data.frame(Blood_sugar_Data)
  n <- nrow(dfdata)
  User_blood_sugar_data <- data.table()
  blood <- data.table()

  try(sugar <- dfdata$sugar[[1]])
  try(for (i in 1:n){
    sugar <- data.table()
    # a = dfdata$user[i,] // dfdata$user에 1개의 항목밖에 없어서 [1, ] 을 못받아 오는거 같다??
    a = dfdata$user
    t = dfdata$date[i]
    sugars <- dfdata$sugar[[i]]
    k = nrow(dfdata$sugar[[i]])

    if(mode(k)!= 'NULL'){
      for(m in 1:k){
        blood <- data.table()
        time <- sugars[m,][1]
        insulin <- sugars[m,][2]
        meal <- sugars[m,][3]
        sugar <- sugars[m,][4]

        blood <- cbind(a, t, time, insulin, meal, sugar)
        User_blood_sugar_data <- rbind(User_blood_sugar_data, blood, fill=TRUE)
      }
      rm(m,time,insulin,meal)
    }
  },
  rm(sugar,a,t,sugars,k) )
  rm(dfdata,n,blood)
}
names(User_blood_sugar_data) <- c("user_ID", "date", "insulin", "time", "meal", "sugar")