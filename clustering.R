# "NHIS_OPEN_GJ_2017.csv" 파일 사이즈가 커서 이미 샘플링된 data로 대체합니다.
# 사용 시 아래 코드들의 주석처리를 해제하고 임시 코드를 제거(주석처리) 해주세요.(ctrl + shift + c) - 진우

# data <- read.csv("NHIS_OPEN_GJ_2017.csv")
# 
# # a jitter
# # for anonymity
# 
# set.seed(1234)
# data$age <- floor(jitter(5*(data$age-1)) + 2.5)
# data$height <- floor(jitter(data$height) + 2.5)
# data$weight <- floor(jitter(data$weight) + 2.5)
# 
# # b preparation
# 
# names(data)[names(data) == "SBP"] <- "bp_H" ;  names(data)[names(data) == "DBP"] <- "bp_L"
# data2 <- data[ ,c("gender","age","height","weight","waist","bp_H","bp_L","bs","tc","t","hdl","ldl","hemo","urine","crea","AST","ALT","GTP","smoke")]
# data3 <- na.omit(data2)
# data4 <- data3[which(data3$waist>10 & data3$waist<200), ]
# 
# # c clustering - male
# mdata <- data4[which(data4$gender==1 & data4$age<65), ]
# 
# set.seed(1234)
# row = sample(dim(mdata)[1], 60000)
# mdata2 <- mdata[row, ]
# 
# mdata3 <- mdata2 %>% 
#   mutate(bmi = weight/(height*0.01)^2, pp = bp_H-bp_L, cholratio= tc/hdl)
# 
# mdata4 <- mdata3[ ,c("age","bmi","waist","pp","bp_H","bs","cholratio")]
# mclara.out <- clara(mdata4, k=6)
# 
# mdata5 <- cbind(mdata3, cluster = mclara.out$clustering)
# 
# # c clustering - female
# fdata <- data4[which(data4$gender==2 & data4$age<65), ]
# 
# set.seed(1234)
# row = sample(dim(fdata)[1], 60000)
# fdata2 <- fdata[row, ]
# 
# fdata3 <- fdata2 %>% 
#   mutate(bmi = weight/(height*0.01)^2, pp = bp_H-bp_L, cholratio= tc/hdl)
# 
# fdata4 <- fdata3[ ,c("age","bmi","waist","pp","bp_H","bs","cholratio")]
# fclara.out <- clara(fdata4, k=6)
# 
# fdata5 <- cbind(fdata3, cluster = fclara.out$clustering)
# 
# # clear useless
# rm(data,data2,data3,data4,row)
# rm(mdata,mdata2,mdata3,mdata4,mclara.out)
# rm(fdata,fdata2,fdata3,fdata4,fclara.out)
# 
# # output mdata5 fdata5 - append column(bmi,pp,cholratio,cluster) at NHIS_OPEN_GJ_2017.csv data

data <- read.csv("C:\\Users\\kwonn\\OneDrive\\1411~1-MSI\\R server\\NHIS_2017_sam.csv")
mdata3 <- data[which(data$gender==1),-1]
fdata3 <- data[which(data$gender==2),-1]

mdata4 <- mdata3[ ,c("age","bmi","waist","pp","bp_H","bs","cholratio")]
mclara.out <- clara(mdata4, k=6)
mdata5 <- cbind(mdata3, cluster = mclara.out$clustering)

fdata4 <- fdata3[ ,c("age","bmi","waist","pp","bp_H","bs","cholratio")]
fclara.out <- clara(fdata4, k=6)
fdata5 <- cbind(fdata3, cluster = fclara.out$clustering)

rm(data, mdata3,mdata4,mclara.out, fdata3,fdata4,fclara.out)
