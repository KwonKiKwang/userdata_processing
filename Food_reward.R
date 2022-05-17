Foods_Data$fat = case_when(Foods_Data$fat=="0" ~ 0.001,
                           T ~ Foods_Data$fat)
Foods_Data$protein = case_when(Foods_Data$protein=="0" ~ 0.001,
                               T ~ Foods_Data$protein)
Foods_Data <- mutate(Foods_Data, Score = protein/fat, S_Score = (protein/fat)*sugars)

#Normal
Foods_Data<-Foods_Data %>% mutate(reward1 = case_when(Score <1 ~ "-10",
                                                      Score <=2 ~ "-5",
                                                      Score <=3 ~ "0",
                                                      Score <=5 ~ "5",
                                                      Score <=7  ~"10",
                                                      T ~ "15"))
Foods_Data<-Foods_Data %>% mutate(reward2 = case_when(S_Score <1 ~ "15",
                                                      S_Score <=2 ~ "10",
                                                      S_Score <=3 ~ "5",
                                                      S_Score <=5 ~ "0",
                                                      S_Score <=7  ~"-5",
                                                      T ~ "-10"))
Foods_Data<-Foods_Data %>% mutate(reward = as.numeric(reward1) + as.numeric(reward2))
Foods_Data<-Foods_Data %>% mutate(main_category = case_when(category=="구이류"|category=="밥류"|category=="죽류"|category=="튀김류"|category=="찜류"|category=="전류"|category=="만두류"|category== "빵류"|category=="감자 및 전분류"|category=="육류"|category=="조리가공품류" | category=="견과류 및 종실류" | category=="곡류 및 그 제품"|category=="면류" ~ "메인",
                                                            category=="탕류"|category=="찌개류"|category=="국류" ~ "국물류",
                                                            category=="김치류"|category=="무침류"|category=="나물류"|category=="볶음류"|category=="조림류"|category=="난류"|category=="두류"|category=="버섯류"|category=="어패류 및 기타 수산물"|category=="장아찌류"|category=="적류"|category=="젓갈류"|category=="채소류"|category=="포류" ~ "반찬류",
                                                            category=="떡류"|category=="한과류"|category=="유지류"|category=="과실류"|category=="당류"|category=="아이스크림류"|category=="우유 및 유제품류" ~ "디저트",
                                                            category=="음료류"|category=="주류"|category=="차류" ~ "음료",
                                                            category=="기타"|category=="해조류"|category=="조미료류" ~ "기타"))
Foods_Data<-Foods_Data %>% mutate(Final_reward = case_when(main_category=="메인" ~ reward*1,
                                                           main_category=="국물류" ~ reward*0.7,
                                                           main_category=="반찬류" ~ reward*0.6,
                                                           main_category=="디저트" ~ reward*0.4,
                                                           T ~ reward*0.2))
Foods_Data$Final_reward = case_when(Foods_Data$category =="주류" ~ Foods_Data$Final_reward - 17,
                                    T ~ Foods_Data$Final_reward)
Foods_Data$Final_reward = case_when(Foods_Data$cholesterol<=0.05 ~ Foods_Data$Final_reward +1,
                                    Foods_Data$cholesterol <=0.1 ~ Foods_Data$Final_reward,
                                    Foods_Data$cholesterol <= 0.2 ~ Foods_Data$Final_reward -1,
                                    Foods_Data$cholesterol <=0.4 ~ Foods_Data$Final_reward -2,
                                    T ~ Foods_Data$Final_reward-3)
Foods_Data$Final_reward = case_when(Foods_Data$trans_fat <=User_trans_fat_standard/3 ~ Foods_Data$Final_reward,
                                    Foods_Data$trans_fat <= User_trans_fat_standard/3*2 ~ Foods_Data$Final_reward -1,
                                    Foods_Data$trans_fat <=User_trans_fat_standard ~ Foods_Data$Final_reward -2,
                                    T ~ Foods_Data$Final_reward-3)
Foods_Data$Final_reward = case_when(Foods_Data$sat_fat <=User_sat_fat_standard/3 ~ Foods_Data$Final_reward,
                                    Foods_Data$sat_fat <=User_sat_fat_standard/3*2 ~ Foods_Data$Final_reward-1,
                                    Foods_Data$sat_fat <=User_sat_fat_standard ~ Foods_Data$Final_reward-2 ,
                                    T ~ Foods_Data$Final_reward -3)

#BMI reward
Foods_Data<-Foods_Data %>% mutate(reward_BMI1 = case_when(Score <1 ~ "-10",
                                                          Score <=2 ~ "-5",
                                                          Score <=3 ~ "0",
                                                          Score <=5 ~ "5",
                                                          Score <=7  ~"10",
                                                          T ~ "15"))
Foods_Data<-Foods_Data %>% mutate(reward_BMI2 = case_when(S_Score <1 ~ "13",
                                                          S_Score <=2 ~ "9",
                                                          S_Score <=3 ~ "4",
                                                          S_Score <=5 ~ "0",
                                                          S_Score <=7  ~"-9",
                                                          T ~ "-14"))
Foods_Data<-Foods_Data %>% mutate(BMI_reward = as.numeric(reward_BMI1) + as.numeric(reward_BMI2))
Foods_Data<-Foods_Data %>% mutate(BMI_Final_reward = case_when(main_category=="메인" ~ BMI_reward*1,
                                                               main_category=="국물류" ~ BMI_reward*0.7,
                                                               main_category=="반찬류" ~ BMI_reward*0.6,
                                                               main_category=="디저트" ~ BMI_reward*0.4,
                                                               T ~ BMI_reward*0.2))
Foods_Data$BMI_Final_reward = case_when(Foods_Data$category =="주류" ~ Foods_Data$BMI_Final_reward - 17,
                                        T ~ Foods_Data$BMI_Final_reward)
Foods_Data$BMI_Final_reward = case_when(Foods_Data$cholesterol <=0.1 ~ Foods_Data$BMI_Final_reward,
                                        Foods_Data$cholesterol <= 0.2 ~ Foods_Data$BMI_Final_reward -2,
                                        Foods_Data$cholesterol <=0.4 ~ Foods_Data$BMI_Final_reward -3,
                                        T ~ Foods_Data$BMI_Final_reward-5)
Foods_Data$BMI_Final_reward = case_when(Foods_Data$trans_fat <=User_trans_fat_standard/3 ~ Foods_Data$BMI_Final_reward,
                                        Foods_Data$trans_fat <= User_trans_fat_standard/3*2 ~ Foods_Data$BMI_Final_reward -2,
                                        Foods_Data$trans_fat <=User_trans_fat_standard ~ Foods_Data$BMI_Final_reward -3,
                                        T ~ Foods_Data$BMI_Final_reward-5)
Foods_Data$BMI_Final_reward = case_when(Foods_Data$sat_fat <=User_sat_fat_standard/3 ~ Foods_Data$BMI_Final_reward,
                                        Foods_Data$sat_fat <=User_sat_fat_standard/3*2 ~ Foods_Data$BMI_Final_reward,
                                        Foods_Data$sat_fat <=User_sat_fat_standard ~ Foods_Data$BMI_Final_reward-2 ,
                                        T ~ Foods_Data$BMI_Final_reward -3)

#diabetes reward
Foods_Data<-Foods_Data %>% mutate(reward_diabetes1 = case_when(Score <1 ~ "-10",
                                                               Score <=2 ~ "-5",
                                                               Score <=3 ~ "0",
                                                               Score <=5 ~ "5",
                                                               Score <=7  ~"10",
                                                               T ~ "15"))
Foods_Data<-Foods_Data %>% mutate(reward_diabetes2 = case_when(S_Score <1 ~ "9",
                                                               S_Score <=2 ~ "6",
                                                               S_Score <=3 ~ "0",
                                                               S_Score <=5 ~ "-6",
                                                               S_Score <=7  ~"-13",
                                                               T ~ "-23"))
Foods_Data<-Foods_Data %>% mutate(diabetes_reward = as.numeric(reward_diabetes1) + as.numeric(reward_diabetes2))
Foods_Data<-Foods_Data %>% mutate(diabetes_Final_reward = case_when(main_category=="메인" ~ diabetes_reward*1,
                                                                    main_category=="국물류" ~ diabetes_reward*0.7,
                                                                    main_category=="반찬류" ~ diabetes_reward*0.6,
                                                                    main_category=="디저트" ~ diabetes_reward*0.4,
                                                                    T ~ diabetes_reward*0.2))
Foods_Data$diabetes_Final_reward = case_when(Foods_Data$category =="주류" ~ Foods_Data$diabetes_Final_reward - 17,
                                             T ~ Foods_Data$diabetes_Final_reward)
Foods_Data$diabetes_Final_reward = case_when(Foods_Data$cholesterol <=0.1 ~ Foods_Data$diabetes_Final_reward,
                                             Foods_Data$cholesterol <= 0.2 ~ Foods_Data$diabetes_Final_reward -1,
                                             Foods_Data$cholesterol <=0.4 ~ Foods_Data$diabetes_Final_reward -2,
                                             T ~ Foods_Data$diabetes_Final_reward-3)
Foods_Data$diabetes_Final_reward = case_when(Foods_Data$trans_fat <=User_trans_fat_standard/3 ~ Foods_Data$diabetes_Final_reward,
                                             Foods_Data$trans_fat <= User_trans_fat_standard/3*2 ~ Foods_Data$diabetes_Final_reward -1,
                                             Foods_Data$trans_fat <=User_trans_fat_standard ~ Foods_Data$diabetes_Final_reward -2,
                                             T ~ Foods_Data$diabetes_Final_reward-3)
Foods_Data$diabetes_Final_reward = case_when(Foods_Data$sat_fat <=User_sat_fat_standard/3 ~ Foods_Data$diabetes_Final_reward,
                                             Foods_Data$sat_fat <=User_sat_fat_standard/3*2 ~ Foods_Data$diabetes_Final_reward,
                                             Foods_Data$sat_fat <=User_sat_fat_standard ~ Foods_Data$diabetes_Final_reward-1 ,
                                             T ~ Foods_Data$diabetes_Final_reward -2)
Foods_Data$diabetes_Final_reward = case_when(Foods_Data$carbo >=80 ~ Foods_Data$diabetes_Final_reward -2,
                                             T~ Foods_Data$diabetes_Final_reward)

#Hyperlipidemia reward
Foods_Data<-Foods_Data %>% mutate(reward_Hyperlipidemia1 = case_when(Score <1 ~ "-10",
                                                                     Score <=2 ~ "-5",
                                                                     Score <=3 ~ "0",
                                                                     Score <=5 ~ "5",
                                                                     Score <=7  ~"10",
                                                                     T ~ "15"))
Foods_Data<-Foods_Data %>% mutate(reward_Hyperlipidemia2 = case_when(S_Score <1 ~ "13",
                                                                     S_Score <=2 ~ "8",
                                                                     S_Score <=3 ~ "3",
                                                                     S_Score <=5 ~ "0",
                                                                     S_Score <=7  ~"-7",
                                                                     T ~ "-14"))
Foods_Data<-Foods_Data %>% mutate(Hyperlipidemia_reward = as.numeric(reward_Hyperlipidemia1) + as.numeric(reward_Hyperlipidemia2))
Foods_Data<-Foods_Data %>% mutate(Hyperlipidemia_Final_reward = case_when(main_category=="메인" ~ Hyperlipidemia_reward*1,
                                                                          main_category=="국물류" ~ Hyperlipidemia_reward*0.7,
                                                                          main_category=="반찬류" ~ Hyperlipidemia_reward*0.6,
                                                                          main_category=="디저트" ~ Hyperlipidemia_reward*0.4,
                                                                          T ~ Hyperlipidemia_reward*0.2))
Foods_Data$Hyperlipidemia_Final_reward = case_when(Foods_Data$category =="주류" ~ Foods_Data$Hyperlipidemia_Final_reward - 17,
                                                   T ~ Foods_Data$Hyperlipidemia_Final_reward)
Foods_Data$Hyperlipidemia_Final_reward = case_when(Foods_Data$cholesterol <=0.1 ~ Foods_Data$Hyperlipidemia_Final_reward,
                                                   Foods_Data$cholesterol <= 0.2 ~ Foods_Data$Hyperlipidemia_Final_reward -2,
                                                   Foods_Data$cholesterol <=0.4 ~ Foods_Data$Hyperlipidemia_Final_reward -5,
                                                   T ~ Foods_Data$Hyperlipidemia_Final_reward-8)
Foods_Data$Hyperlipidemia_Final_reward = case_when(Foods_Data$trans_fat <=User_trans_fat_standard/3 ~ Foods_Data$Hyperlipidemia_Final_reward,
                                                   Foods_Data$trans_fat <= User_trans_fat_standard/3*2 ~ Foods_Data$Hyperlipidemia_Final_reward -2,
                                                   Foods_Data$trans_fat <=User_trans_fat_standard ~ Foods_Data$Hyperlipidemia_Final_reward -5,
                                                   T ~ Foods_Data$Hyperlipidemia_Final_reward-8)
Foods_Data$Hyperlipidemia_Final_reward = case_when(Foods_Data$sat_fat <=User_sat_fat_standard/3 ~ Foods_Data$Hyperlipidemia_Final_reward,
                                                   Foods_Data$sat_fat <=User_sat_fat_standard/3*2 ~ Foods_Data$Hyperlipidemia_Final_reward,
                                                   Foods_Data$sat_fat <=User_sat_fat_standard ~ Foods_Data$Hyperlipidemia_Final_reward-3 ,
                                                   T ~ Foods_Data$Hyperlipidemia_Final_reward -6)
