Exercise_reward_Data <- read.csv("/home/hnbgenomics/Balanstate/Exercise_list.csv")

# Age_Weight
age_unaero_w_refer <- data.frame(c(0,13,25,40,70),c(0.6,0.8,1,1,0.6))
age_aero_w_refer <- data.frame(c(0,13,25,40,70),c(1,1,1,0.8,0.8))
age_mix_w_refer <- data.frame(c(0,13,25,40,70),c(0.9,0.9,1,0.9,0.7))

age_unaero_weight = spline(age_unaero_w_refer,xout = User_Data$age)$y
age_aero_weight = spline(age_aero_w_refer,xout = User_Data$age)$y
age_mix_weight = spline(age_mix_w_refer,xout = User_Data$age)$y

#BMI_weight
BMI_unaero_w_refer <- data.frame(c(10,18.5,23,25,30,35), c(1.1,1,1,0.9,0.8,0.7))
BMI_aero_w_refer <- data.frame(c(10,18.5,23,25,30,35), c(0.7,1,1.05,1.1,1.2,1.3))
BMI_mix_w_refer <- data.frame(c(10,18.5,23,25,30,35), c(0.9,1,1,0.95,0.9,0.8))

BMI_unaero_weight = spline(BMI_unaero_w_refer,xout = User_Data$BMI)$y
BMI_aero_weight = spline(BMI_aero_w_refer,xout = User_Data$BMI)$y
BMI_mix_weight = spline(BMI_mix_w_refer,xout = User_Data$BMI)$y

#job_weight
#amount of job's activity adjust
Active_0 <- "1.1,1.2,1.1"
Active_1 <- "1.05,1.1,1.05"
Active_2 <- "1.0,1.0,1.0"
Active_3 <- "0.9,1.0,1.0"

job_weight= case_when(User_Data$job=="professional" ~ Active_0 ,
                      User_Data$job=="phy_edu" ~ Active_2,
                      User_Data$job=="sports" ~ Active_3,
                      User_Data$job=="public_official" ~ Active_2,
                      User_Data$job=="teacher" ~ Active_1,
                      User_Data$job=="manager" ~ Active_0,
                      User_Data$job=="office_worker" ~ Active_0,
                      User_Data$job=="self_employed" ~ Active_2,
                      User_Data$job=="sales" ~ Active_2,
                      User_Data$job=="service" ~ Active_2,
                      User_Data$job=="production" ~ Active_3,
                      User_Data$job=="technician" ~ Active_2,
                      User_Data$job=="agriculture" ~ Active_3,
                      User_Data$job=="forestry" ~ Active_3,
                      User_Data$job=="student" ~ Active_1,
                      User_Data$job=="housewife_child" ~ Active_2,
                      User_Data$job=="housewife" ~ Active_1,
                      User_Data$job=="retired" ~ Active_0,
                      User_Data$job=="jobless" ~ Active_1,
                      T ~ Active_2
)

job_weight <- strsplit(job_weight,split= ",")

job_unaero_weight = as.numeric (job_weight[[1]][1])
job_aero_weight = as.numeric (job_weight[[1]][2])
job_mix_weight = as.numeric (job_weight[[1]][3])

Exercise_reward_Data <- Exercise_reward_Data %>% mutate(reward = case_when(category == "unaero" ~ round(METs/120,2)*age_unaero_weight*BMI_unaero_weight*job_unaero_weight,
                                                                           category == "aero" ~ round(METs/120,2)*age_aero_weight*BMI_aero_weight*job_aero_weight,
                                                                           category == "mix" ~ round(METs/120,2)*age_mix_weight*BMI_mix_weight*job_mix_weight
                                                                           ) )
