# 생일을 기본 seed로 생각하고 몇주차인지를 더해서 주마다 고유성을 가지도록
set.seed(as.numeric(User_Data$birthday)+week(Sys.Date()))

# preprocessing
# making sub category function
subcat <- function(df) {
  df <- df %>% mutate(sub_category = case_when(category=="밥류"|category=="죽류"|category=="곡류 및 그 제품"|category=="떡류" ~ "rice",
                                               category=="국류"|category=="탕류"|category=="찌개류"|category=="조림류" ~ "soup",
                                               category=="구이류"|category=="육류"|category=="적류"|category=="포류" ~ "meat",
                                               category=="어패류 및 기타 수산물" ~ "fish",
                                               category=="채소류"|category=="과실류" ~ "salad",
                                               category=="우유 및 유제품류"|category=="음료류"|category=="주류"|category=="차류" ~ "drink",
                                               category=="한과류"|category=="당류"|category=="아이스크림류" ~ "sweet",
                                               category=="면류" ~ "noodle",
                                               category=="튀김류"|category=="전류" ~ "fried",
                                               category=="찜류"|category=="만두류" ~ "boiled",
                                               category=="빵류" ~ "bread",
                                               category=="감자 및 전분류" ~ "potato",
                                               category=="조리가공품류" ~ "instant",
                                               category=="견과류 및 종실류" ~ "nuts",
                                               category=="나물류"|category=="김치류"|category=="무침류"|category=="볶음류"|category=="장아찌류"|category=="젓갈류" ~ "side",
                                               category=="난류" ~ "egg",
                                               category=="두류" ~ "bean",
                                               category=="버섯류" ~ "mushroom"            ) )
  return(df)
}
Foods_Data <- subcat(Foods_Data)

# 식단 추천
# 아침
# 아침은 공평하게
bF <- read.csv("/home/hnbgenomics/Balanstate/아침식단_1435.csv")
bF <- bF[which(bF$calorie<400), ]
bF <- subcat(bF)
br_ma <- bF[sample(nrow(bF),7), ]

bFV <- Foods_Data[which(Foods_Data$category=="과실류"|Foods_Data$category=="채소류"), ]
bFV <- bFV[!grepl("잼", bFV$name), ]
bFV <- bFV[!grepl("고추", bFV$name), ]
bFV <- bFV[!grepl("쌈무", bFV$name), ]
bFV <- bFV[!grepl("마늘", bFV$name), ]
bFV <- bFV[!grepl("락교", bFV$name), ]
bFV <- bFV[!grepl("절임", bFV$name), ]
br_fv <- bFV[sample(nrow(bFV),7), ]

bDR <- Foods_Data[which(Foods_Data$main_category=="음료" & Foods_Data$category!="주류"), ]
br_dr <- bDR[sample(nrow(bDR),7), ]

br_menu <- cbind(as.character(br_ma$name),as.character(br_fv$name),as.character(br_dr$name) )
br_menu <- gsub("[ ]","",br_menu) # 공백 제거
br_cat <- cbind(br_ma$sub_category,br_fv$sub_category,br_dr$sub_category)
br_nut <- br_ma[ ,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
  br_fv[ ,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
  br_dr[ ,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]


# 점심과 저녁
# main soup sub drink desert fruit&vegetable
ludi <- function(df) {
  main <- df[which(df$main_category=="메인"), ]
  soup <- df[which(df$main_category=="국물류"), ]
  sub  <- df[which(df$main_category=="반찬류"), ]
  drink <- df[which(df$main_category=="음료"), ]
  dessert <- df[which(df$main_category=="디저트"), ]
  frve <- df[which(df$category=="과실류"|df$category=="채소류"), ]
  frve <- frve[!grepl("잼", frve$name), ]
  frve <- frve[!grepl("고추", frve$name), ]
  frve <- frve[!grepl("쌈무", frve$name), ]
  frve <- frve[!grepl("마늘", frve$name), ]
  frve <- frve[!grepl("락교", frve$name), ]
  frve <- frve[!grepl("절임", frve$name), ]
  
  # 얼마나 먹을 수 있는지 check 용도
  # cat("제한 음식",nrow(df),"총 음식",nrow(Food_reward_data), "\n")
  # cat("제한 메인",nrow(main),"총 메인",length(which(Food_reward_data$main_category=="메인") ), "\n")
  # cat("제한 국",nrow(soup),"총 국",length(which(Food_reward_data$main_category=="국물류") ), "\n")
  # cat("제한 반찬",nrow(sub),"총 반찬",length(which(Food_reward_data$main_category=="반찬류") ), "\n")
  # cat("제한 음료",nrow(drink),"총 음료",length(which(Food_reward_data$main_category=="음료") ), "\n")
  # cat("제한 후식",nrow(dessert),"총 후식",length(which(Food_reward_data$main_category=="디저트") ), "\n")
  
  # 점심 저녁 합쳐서 랜덤 14개, 그후 7개씩 나눠서
  ludi_ma <- main[sample(nrow(main),14, replace = T), ]
  ludi_so <- soup[sample(nrow(soup),14), ]
  ludi_su <- sub[sample(nrow(sub),14), ]
  ludi_dr <- drink[sample(nrow(drink),14), ]
  ludi_de <- dessert[sample(nrow(dessert),14), ]
  ludi_fv <- frve[sample(nrow(frve),14), ]
  
  # 음식 조합 생각
  ludi_so[which(ludi_ma$category == "빵류" | ludi_ma$category == "조리가공품류" | ludi_ma$category == "면류"),
          "name"] <- "NULL"
  ludi_so[which(ludi_ma$category == "빵류" | ludi_ma$category == "조리가공품류" | ludi_ma$category == "면류"),
          c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")] <- 0
  
  ludi_su[which(ludi_ma$category == "빵류"),
          "name"] <- "NULL"
  ludi_su[which(ludi_ma$category == "빵류"),
          c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")] <- 0
  
  # 식단표
  lu_menu <- cbind(ludi_ma$name[1:7],ludi_so$name[1:7],ludi_su$name[1:7],ludi_dr$name[1:7],ludi_de$name[1:7],ludi_fv$name[1:7])
  lu_menu <- gsub("[ ]","",lu_menu)
  di_menu <- cbind(ludi_ma$name[8:14],ludi_so$name[8:14],ludi_su$name[8:14],ludi_dr$name[8:14],ludi_de$name[8:14],ludi_fv$name[8:14])
  di_menu <- gsub("[ ]","",di_menu)
  # colnames(lu_menu) <- c("메인","국","반찬","음료","디저트")
  # colnames(di_menu) <- c("메인","국","반찬","음료","디저트")
  
  # 카테고리
  lu_cat <- cbind(ludi_ma$sub_category[1:7],ludi_so$sub_category[1:7],ludi_su$sub_category[1:7],ludi_dr$sub_category[1:7],ludi_de$sub_category[1:7],ludi_fv$sub_category[1:7])
  di_cat <- cbind(ludi_ma$sub_category[8:14],ludi_so$sub_category[8:14],ludi_su$sub_category[8:14],ludi_dr$sub_category[8:14],ludi_de$sub_category[8:14],ludi_fv$sub_category[8:14])
  
  # 영양소
  lu_nut <- ludi_ma[1:7,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_so[1:7,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_su[1:7,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_dr[1:7,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_de[1:7,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_fv[1:7,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]
  di_nut <- ludi_ma[8:14,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_so[8:14,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_su[8:14,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_dr[8:14,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_de[8:14,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]+
    ludi_fv[8:14,c("calorie","protein","fat","carbo","sugars","cholesterol","sat_fat","trans_fat","triglyceride")]
  return(list(lu_menu = as.data.table(lu_menu), di_menu = as.data.table(di_menu), lu_cat = as.data.table(lu_cat), di_cat = as.data.table(di_cat), lu_nut = lu_nut, di_nut = di_nut) )
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

# 정상 Foods_Data$Final_reward
# 비만 Foods_Data$BMI_Final_reward
# 당뇨 Foods_Data$diabetes_Final_reward
# 고지 Foods_Data$Hyperlipidemia_Final_reward

if(o$gender==1){
  # bmi 비만 bs 당뇨 cholesterol 고지혈증 
  if(o$cluster == 1) {
    rF <- Foods_Data[which(Foods_Data$BMI_Final_reward>0 & Foods_Data$diabetes_Final_reward>0), ]
  } else if(o$cluster == 2) {
    rF <- Foods_Data[which(Foods_Data$BMI_Final_reward>0 & Foods_Data$Hyperlipidemia_Final_reward>0), ]
  } else if(o$cluster == 3) {
    rF <- Foods_Data[which(Foods_Data$Final_reward>0), ]
  } else if(o$cluster == 4) {
    rF <- Foods_Data[which(Foods_Data$Final_reward>0), ]
  } else if(o$cluster == 5) {
    rF <- Foods_Data
  } else {
    rF <- Foods_Data[which(Foods_Data$diabetes_Final_reward>0 & Foods_Data$Hyperlipidemia_Final_reward>0), ]
  }
} else{
  if(o$cluster == 1) {
    rF <- Foods_Data[which(Foods_Data$BMI_Final_reward>0 & Foods_Data$diabetes_Final_reward>0), ]
  } else if(o$cluster == 2) {
    rF <- Foods_Data[which(Foods_Data$BMI_Final_reward>0 & Foods_Data$diabetes_Final_reward>0 & Foods_Data$Hyperlipidemia_Final_reward>0), ]
  } else if(o$cluster == 3) {
    rF <- Foods_Data[which(Foods_Data$Hyperlipidemia_Final_reward>0), ]
  } else if(o$cluster == 4) {
    rF <- Foods_Data[which(Foods_Data$Final_reward>0), ]
  } else if(o$cluster == 5) {
    rF <- Foods_Data[which(Foods_Data$Final_reward>0), ]
  } else {
    rF <- Foods_Data
  }
}
meal <- ludi(rF)

to_nut <- br_nut + meal$lu_nut + meal$di_nut

#clear useless
rm(bF,bDR,bFV,br_dr,br_fv,br_ma, ludi, rF)