# qda
mdata5$cluster <- as.factor(mdata5$cluster)
mqda <- qda(cluster ~ age + bmi + waist + pp + bp_H + bs + cholratio, data = mdata5)

fdata5$cluster <- as.factor(fdata5$cluster)
fqda <- qda(cluster ~ age + bmi + waist + pp + bp_H + bs + cholratio, data = fdata5)

############ apply ############

# individual
# ocr 있고 없고
if(F) {
  # ocr data 불러오기
  n <- sample(1:nrow(m_tst),1)
  o <- m_tst[n, ]
  o <- subset(o, select = -cluster)
} else {
  o <- subset(mdata5[1, ], select = -c(tc,t,hdl,ldl,hemo,urine,crea,AST,ALT,GTP,smoke,cluster))
  o[1, ] <- NA
  o$gender = ifelse(User_Data$gender=="male",1,2)
  o$age = User_Data$age
  o$height = User_Data$height
  o$weight = User_Data$weight
  o$bmi = User_Data$BMI
  o$waist = NA
  # 최신 데이터를 사용하기 위해 tail 사용
  o$pp = tail(User_blood_pressure_data,1)$systolic - tail(User_blood_pressure_data,1)$diastolic
  o$bp_H = tail(User_blood_pressure_data,1)$systolic
  o$bp_L = tail(User_blood_pressure_data,1)$diastolic
  o$bs = tail(User_blood_sugar_data,1)$sugar
  o$cholratio = NA
}

if(sum(is.na(o[ ,c("age","bmi","waist","pp","bp_H","bs","cholratio")]) )==0) {
  # with no NA
  if(o$gender==1)
    o <- cbind(o,cluster = predict(mqda, o)$class)
  else {
    o <- cbind(o,cluster = predict(fqda, o)$class)
  }
} else {
  # with NA
  if(o$gender==1){
    imp <- knnImputation(o[ ,-1], k=17, meth = "median", distData = subset(mdata5, select = -c(gender,tc,t,hdl,ldl,hemo,urine,crea,AST,ALT,GTP,smoke,cluster) ) )
    o <- cbind(o,cluster = predict(mqda, imp)$class)
  } else {
    imp <- knnImputation(o[ ,-1], k=17, meth = "median", distData = subset(fdata5, select = -c(gender,tc,t,hdl,ldl,hemo,urine,crea,AST,ALT,GTP,smoke,cluster) ) )
    o <- cbind(o,cluster = predict(fqda, imp)$class)
  }
  # clear useless
  # rm(imp)
}

rm(fdata5,fqda, mdata5, mqda)