library(HMM)
library(dplyr)

# 1 2 3 4 5 6
# 환자      건강

# state & observation label
S <- as.factor(c(1,2,3,4,5,6) )
O <- as.factor(c("VL","L","M","H","VH") )

# user's cluster means initial probability 
Ini_P <- rbind(c(0,0,0,0,0,0) )
Ini_P[as.integer(o$cluster)] <- 1

# observation 값 구분
# 일단 cluster 3 4 에서 왔다갔다
set.seed(1)
cluster <- as.factor(sample(c(3,4),100,replace=T,prob=c(0.4,0.6) ) )
set.seed(2)
calorie_ratio <- sample(70:130,100,replace=T)/100
set.seed(3)
ex_amount <- sample(70:130,100,replace=T)
set.seed(4)
sugars_total <- sample(30:70,100,replace=T)

user <- data.frame(cluster,calorie_ratio,ex_amount,sugars_total)
user <- user %>% mutate(
  observation = ifelse(user$calorie_ratio<1,
                       ifelse(user$ex_amount<100,"VL",
                                                 "L"),
                       ifelse(user$ex_amount>100,
                              ifelse(user$sugars_total<50,"VH",
                                                          "H"),
                              "M") 
  ) )
user$observation <- as.factor(user$observation)
cat("VL",mean(user$observation=="VL"),"\n")
cat("L",mean(user$observation=="L"),"\n")
cat("M",mean(user$observation=="M"),"\n")
cat("H",mean(user$observation=="H"),"\n")
cat("VH",mean(user$observation=="VH"),"\n")

# transition
# 자신의 클러스터에 속할 확률이 높도록
r1 <- c(10,5,4,3,2,1)
r2 <- c(4,10,4,3,2,1)
r3 <- c(2,3,10,3,2,1)
r4 <- c(1,2,3,10,3,2)
r5 <- c(1,2,3,4,10,4)
r6 <- c(1,2,3,4,5,10)
A <- rbind(r1,r2,r3,r4,r5,r6)
A = A/rowSums(A)
A

# emission
# 건강한 cluster 건강 행동을 많이 방출함
r1 <- c(10,4,3,2,1)
r2 <- c(3,10,3,2,1)
r3 <- c(1,2,10,2,1)
r4 <- c(1,2,10,2,1)
r5 <- c(1,2,3,10,3)
r6 <- c(1,2,3,4,10)
B <- rbind(r1,r2,r3,r4,r5,r6)
B = B/rowSums(B)
B

hmm <- initHMM(S, O, startProbs = Ini_P, transProbs = A, emissionProbs = B)
print(hmm)

HMM_r1 <- viterbi(hmm, user$observation)
mean(HMM_r1 == user$cluster)

bw <- baumWelch(hmm,user$observation,2)
print(bw$hmm)

HMM_r2 <- viterbi(bw$hmm,user$observation)
cbind(user,HMM_r2)
mean(HMM_r2 == user$cluster)