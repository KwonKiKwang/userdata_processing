if(is.na(User_Data$gender)) {
  User_Data$gender = "male"
}

if(is.na(User_Data$job)) {
  User_Data$job = "not_avail"
}

if(is.na(User_Data$username)) {
  User_Data$username = "홍길동"
}

if(User_Data$cellphone=="" | is.na(User_Data$cellphone)) {
  User_Data$cellphone = "01098761234"
}

if(User_Data$birthday=="" | is.na(User_Data$birthday)) {
  User_Data$birthday = "19910917"
}

if(User_Data$height==""| is.na(User_Data$height)) {
  User_Data$height = "170"
}

if(User_Data$weight==""|is.na(User_Data$weight)) {
  User_Data$weight = "60"
}

# error control
# stop("~") 함수 종료
# warning("~") 종료시키진 않지만 경고 메세지 출력