setwd("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server")

install.packages("gam")
install.packages("scales")
library(gam)
library(scales)

####여러 요소의 rescale 범위 설정#####

Age_rescale_range = c(-5,5)
BMI_rescale_range = c(-10,5)



####기본 점수 산출#####

Body_score <- (50/(ncol(Gene_Data$body[1,])*2))*sum(Gene_Data$body)+30
Fat_score <- (50/(ncol(Gene_Data$fat[1,])*2))*sum(Gene_Data$fat)+30
Blood_score <- (50/(ncol(Gene_Data$blood[1,])*2))*sum(Gene_Data$blood)+30


####기본 점수에서 여러 요소에 따라 가감#######

BMI_r <- read.csv("BMI.csv")
Age_r <- read.csv("Age.csv")

############ 영양 섭취 기준 값 설정 ############ 

# 기준 값 

trans_fat_standard <-2.2  # 권장 섭취량 2000kcal인 사람 기준 트랜스지방 권장섭취량 2.2g 
sat_fat_standard <-17.6   # 권장 섭취량 2000kcal인 사람 기준 포화지방 권장섭취량 17.6g 
chol_standard <-0.3       # 권장 콜레스테롤 섭취량 300mg 
sugar_standard <- 50      # 권장 섭취량 2000kcal인 사람 기준 당 권장 섭취량 50g 
triglyceride_standard <- 25
# user 기초 대사량 기반 기준 값 

User_trans_fat_standard <- (trans_fat_standard*User_Data$BMR/2000)  
User_sat_fat_standard <- (sat_fat_standard*User_Data$BMR/2000)  
User_sugar_standard <- (sugar_standard*User_Data$BMR/2000)
User_triglyceride_standard <- (triglyceride_standard*User_Data$BMR/2000)
standard <- c(User_trans_fat_standard, User_sat_fat_standard, chol_standard, User_sugar_standard, triglyceride_standard)

# 섭취 비율에 따른 gene score 차별화 (논문에 근거하여 로직 세밀화 필요)

Total <- Total %>% mutate(calorie_gene_score = case_when(calorie_ratio >120 ~ -2, 
                                                         calorie_ratio > 110 ~ -1, 
                                                         calorie_ratio > 100 ~ 0,
                                                         calorie_ratio > 90 ~ 1, 
                                                         calorie_ratio > 70 ~ 2, 
                                                         calorie_ratio > 40 ~ -1, 
                                                         T ~-2))

Total <- Total %>% mutate(chol_gene_score = case_when(chol_ratio >120 ~ -2,
                                                      chol_ratio > 110 ~ -1,
                                                      chol_ratio > 90 ~ 0,
                                                      chol_ratio > 70 ~ 1,
                                                      T ~ 2))

Total <- Total %>% mutate(transfat_gene_score = case_when(trans_fat_ratio >120 ~ -2,
                                                          trans_fat_ratio > 110 ~ -1,
                                                          trans_fat_ratio > 90 ~ 0,
                                                          trans_fat_ratio > 70 ~ 1,
                                                          T ~ 2))

Total <- Total %>% mutate(satfat_gene_score = case_when(sat_fat_ratio >120 ~ -2,
                                                        sat_fat_ratio > 110 ~ -1,
                                                        sat_fat_ratio > 90 ~ 0,
                                                        sat_fat_ratio > 70 ~ 1,
                                                        T ~ 2))

Total <- Total %>% mutate(triglyceride_gene_score = case_when(triglyceride_ratio >120 ~ -2,
                                                              triglyceride_ratio > 110 ~ -1,
                                                              triglyceride_ratio > 90 ~ 0,
                                                              triglyceride_ratio > 70 ~ 1,
                                                        T ~ 2))

Total <- Total %>% mutate(carbo_type = abs(carbo_ratio-50))
Total <- Total %>% mutate(protein_type = case_when (carbo_ratio<30 ~ abs(carbo_ratio-30),
                          T~0))
Total <- Total %>% mutate(fat_type = case_when (fat_ratio>20 ~ abs(fat_ratio-20),
                          T~0))
Total <- Total %>% mutate(typs = carbo_type+protein_type+fat_type)

# BMI 당 사망률에 따른 BMI gene score 계산

BMI_r$X1.Danger <- rescale(BMI_r$X1.Danger , to=BMI_rescale_range)
index <- which(BMI_r$BMI_min<User_Data$BMI&BMI_r$BMI_max>User_Data$BMI)

if (BMI_r$BMI_median[index]<User_Data$BMI){ 
  a<-(BMI_r$X1.Danger[index+1]-BMI_r$X1.Danger[index])/(BMI_r$BMI_median[index+1]-BMI_r$BMI_median[index])
  b<-(BMI_r$X1.Danger[index]-a*BMI_r$BMI_median[index])
  BMI_gene_score <- a*User_Data$BMI + b
}else{
  a<-(BMI_r$X1.Danger[index]-BMI_r$X1.Danger[index-1])/(BMI_r$BMI_median[index]-BMI_r$BMI_median[index-1])
  b<-(BMI_r$X1.Danger[index]-a*BMI_r$BMI_median[index])
  BMI_gene_score <- a*User_Data$BMI + b 
}


# 나이 당 사망률에 따른 Age gene score 계산
if (User_Data$gender == "male"){
  Age_r$Danger_M<-rescale(Age_r$Danger_M, to=Age_rescale_range)
  Age_gene_score<- Age_r$Danger_M[Age_r$age_min<User_Data$age&Age_r$age_max>User_Data$age]
  
} else{
  
  Age_r$Danger_F<-rescale(Age_r$Danger_F, to=Age_rescale_range)
  Age_gene_score <- Age_r$Danger_F[Age_r$age_min<User_Data$age&Age_r$age_max>User_Data$age]
}

###Body_score 조정 #####

Body_score<-Body_score+BMI_gene_score+Age_gene_score

Body_score<- round(Body_score-case_when(sum(Total$calorie_gene_score)>60 ~ 5,
          sum(Total$calorie_gene_score)>40 ~ 3,
          sum(Total$calorie_gene_score)>20 ~ 1,
          sum(Total$calorie_gene_score)>0 ~ 0,
          sum(Total$calorie_gene_score)>-20 ~ -1,
          sum(Total$calorie_gene_score)> -40 ~ -3,
          T ~ -5),0)


###Fat_score 조정 #####

Fat_score<- Fat_score + BMI_gene_score+Age_gene_score

Fat_score<- round(Fat_score-case_when(sum(Total$calorie_gene_score)>60 ~ 5,
                                        sum(Total$calorie_gene_score)>40 ~ 3,
                                        sum(Total$calorie_gene_score)>20 ~ 1,
                                        sum(Total$calorie_gene_score)>0 ~ 0,
                                        sum(Total$calorie_gene_score)>-20 ~ -1,
                                        sum(Total$calorie_gene_score)> -40 ~ -3,
                                        T ~ -5),0)


###Blood_score 조정 #####

Blood_score<-Blood_score + BMI_gene_score + Age_gene_score

Blood_score <- round()


case_when(sum(Total$triglyceride_gene_score)>60 ~ 3,
          sum(Total$triglyceride_gene_score)>40 ~ 2,
          sum(Total$triglyceride_gene_score)>20 ~ 1,
          sum(Total$triglyceride_gene_score)>0 ~ 0,
          sum(Total$triglyceride_gene_score)>-20 ~ -1,
          sum(Total$triglyceride_gene_score)> -40 ~ -2,
          T ~ -3)

case_when(sum(Total$satfat_gene_score)>60 ~ 3,
          sum(Total$satfat_gene_score)>40 ~ 2,
          sum(Total$satfat_gene_score)>20 ~ 1,
          sum(Total$satfat_gene_score)>0 ~ 0,
          sum(Total$satfat_gene_score)>-20 ~ -1,
          sum(Total$satfat_gene_score)> -40 ~ -2,
          T ~ -3)

case_when(sum(Total$calorie_gene_score)>60 ~ 5,
          sum(Total$calorie_gene_score)>40 ~ 3,
          sum(Total$calorie_gene_score)>20 ~ 1,
          sum(Total$calorie_gene_score)>0 ~ 0,
          sum(Total$calorie_gene_score)>-20 ~ -1,
          sum(Total$calorie_gene_score)> -40 ~ -3,
          T ~ -5)

case_when(sum(Total$transfat_gene_score)>60 ~ 3,
          sum(Total$transfat_gene_score)>40 ~ 2,
          sum(Total$transfat_gene_score)>20 ~ 1,
          sum(Total$transfat_gene_score)>0 ~ 0,
          sum(Total$transfat_gene_score)>-20 ~ -1,
          sum(Total$transfat_gene_score)> -40 ~ -2,
          T ~ -3)

case_when(sum(Total$chol_gene_score)>60 ~ 3,
          sum(Total$chol_gene_score)>40 ~ 2,
          sum(Total$chol_gene_score)>20 ~ 1,
          sum(Total$chol_gene_score)>0 ~ 0,
          sum(Total$chol_gene_score)>-20 ~ -1,
          sum(Total$chol_gene_score)> -40 ~ -2,
          T ~ -3)

