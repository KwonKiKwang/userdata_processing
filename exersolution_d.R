# 생일을 기본 seed로 생각하고 몇주차인지를 더해서 주마다 고유성을 가지도록
set.seed(as.numeric(User_Data$birthday)+week(Sys.Date()))

# preprocessing
Exercise_reward_Data$활동명 <- as.character(Exercise_reward_Data$활동명)

if(nrow(Total)<4){
  calories.remain <- 200
} else{
  calories.remain <- mean(tail(Total$calories_remained,3))
}
# 최근 3일치의 평균 잉여칼로리 기반으로 운동 추천시간을 계산
extime <- function(dfC) {
  et <- data.table(20,20,20,20)
  ec <- data.table(10,10,10,10)
  for (i in 1:ncol(dfC)) {
    for (j in seq(from=20,to=60,by=5)) {
      if (dfC[i]*j > calories.remain) {
        et[1,i] <- j
        ec[1,i] <- round(dfC[i]*j)
        break
      }
      et[1,i] <- j
      ec[1,i] <- dfC[i]*j
    }
  }
  return(list(et=et,ec=ec) )
}

workout <- function(df) {
  main <- df[which(df$활동구분=="걷기/달리기" | df$활동구분=="생활체육"), ]
  sub <- df[which(df$활동구분=="댄스" | df$활동구분=="구기운동"), ]
  if(quarter(Sys.Date()) == 1|quarter(Sys.Date()) == 4)
    sp <- df[which(df$활동구분=="겨울운동"), ]
  else
    sp <- df[which(df$활동구분=="수중운동"), ]
  
  # # 얼마나 먹을 수 있는지
  # cat("제한 운동",nrow(df),"총 운동",nrow(Exercise_reward_Data), "\n")
  # cat("제한 메인",nrow(main),"총 메인",length(which(Exercise_reward_Data$활동구분=="걷기/달리기" | Exercise_reward_Data$활동구분=="생활체육") ), "\n")
  # cat("제한 보조",nrow(sub),"총 보조",length(which(Exercise_reward_Data$활동구분=="댄스" | Exercise_reward_Data$활동구분=="구기운동") ), "\n")
  # cat("제한 특별",nrow(sp),"총 특별",length(which(Exercise_reward_Data$활동구분=="수중운동" | Exercise_reward_Data$활동구분=="겨울운동") ), "\n")
  
  # main 은 2개씩 나머지 하나
  exma <- main[sample(nrow(main),2), ]
  exsu <- sub[sample(nrow(sub),1), ]
  exsp <- sp[sample(nrow(sp),1), ]
  
  # table
  ex <- data.table(exma$활동명[1],exma$활동명[2],exsu$활동명[1],exsp$활동명[1])
  ex <- gsub("[ ]","",ex)
  
  cpm <- data.frame("메인1" = exma$METs[1]*0.35*0.005*User_Data$weight,
                    "메인2" = exma$METs[2]*0.35*0.005*User_Data$weight,
                    "보조" = exsu$METs*0.35*0.005*User_Data$weight,
                    "특별" = exsp$METs*0.35*0.005*User_Data$weight)
  ETC <- extime(cpm)
  RExTime <- ETC$et # 시간
  excal <- ETC$ec   # 소모칼로리
  
  # 근력 향상
  mus <- data.frame("메인1" = ifelse(exma$category[1] != "aero", exma$reward[1], 0),
                    "메인2" = ifelse(exma$category[2] != "aero", exma$reward[2], 0),
                    "보조" = ifelse(exsu$category != "aero", exsu$reward, 0),
                    "특별" = ifelse(exsp$category != "aero", exsp$reward, 0))
  mus <- round(mus*50)
  # 체지방 감소
  bodyf <- data.frame("메인1" = ifelse(exma$category[1] != "unaero", exma$reward[1], 0),
                      "메인2" = ifelse(exma$category[2] != "unaero", exma$reward[2], 0),
                      "보조" = ifelse(exsu$category != "unaero", exsu$reward, 0),
                      "특별" = ifelse(exsp$category != "unaero", exsp$reward, 0))
  bodyf <- -round(bodyf*50)
  return(list(ex = ex,RExTime = RExTime, excal = excal, mus = mus, bodyf = bodyf) )
}

# 구분은 manual, column에서 median값이 가장 큰 2가지씩 선정
# 남성
# 1 비만 당뇨
# 2 비만 (고혈압) 고지혈증
# 3 정상 (제한)
# 4 정상 (고혈압)
# 5 정상
# 6 당뇨 고지혈

# 여성
# 1 비만 (고혈압) 당뇨
# 2 비만 당뇨 고지혈증
# 3 고지혈증 (고혈압)
# 4 정상 (제한)
# 5 정상 (제한)
# 6 정상

# 비만 격한운동 안하도록
# 당뇨 무산소운동(근력운동) 추천
# 고지혈증 유산소운동 추천
if(o$gender==1){
  if(o$cluster == 1) 
    rE <- Exercise_reward_Data[which(Exercise_reward_Data$METs<90 & Exercise_reward_Data$category!="aero"), ]
  else if(o$cluster == 2) 
    rE <- Exercise_reward_Data[which(Exercise_reward_Data$METs<90 & Exercise_reward_Data$category!="unaero"), ]
  else if(o$cluster == 3) 
    rE <- Exercise_reward_Data
  else if(o$cluster == 4) 
    rE <- Exercise_reward_Data
  else if(o$cluster == 5) 
    rE <- Exercise_reward_Data
  else 
    rE <- Exercise_reward_Data[which(Exercise_reward_Data$category=="mix"), ]
} else {
  if(o$cluster == 1) 
    rE <- Exercise_reward_Data[which(Exercise_reward_Data$METs<90 & Exercise_reward_Data$category!="aero"), ]
  else if(o$cluster == 2) 
    rE <- Exercise_reward_Data[which(Exercise_reward_Data$METs<90), ]
  else if(o$cluster == 3) 
    rE <- Exercise_reward_Data[which(Exercise_reward_Data$category!="unaero"), ]
  else if(o$cluster == 4) 
    rE <- Exercise_reward_Data
  else if(o$cluster == 5) 
    rE <- Exercise_reward_Data
  else 
    rE <- Exercise_reward_Data
}
exer <- workout(rE)

rm(rE,extime,workout)