#Practice

#Loading Data
HeartAttack = read.csv("C:/Users/Usama/Desktop/HeartAttack.csv")
LifeExpectancy = read.csv("C:/Users/Usama/Desktop/LifeExpectancy.csv")

#Question 1.a
Alco = LifeExpectancy$Alcohol
LifeEx = LifeExpectancy$LifeExpectancy
cor(Alco,LifeEx)

#Question 1.b
School = LifeExpectancy$Schooling
AdultM = LifeExpectancy$AdultMortality

modl = lm(LifeEx~School+AdultM)
summary(modl)

#Question 2.a
LifeExpectancy_M = LifeExpectancy$LifeExpectancy
DataFrame = LifeExpectancy[,c(3:20)]
lm_model = lm(LifeExpectancy_M~., data = DataFrame)
summary(lm_model)

#Question 2.b
Schooling=LifeExpectancy$Schooling

AdultMortality=LifeExpectancy$AdultMortality

HIVAIDS=LifeExpectancy$HIV.AIDS

ICoResources=LifeExpectancy$ICoResources

backward_elm=lm(LifeExpectancy_M~Schooling+AdultMortality+HIVAIDS+ICoResources)
summary(backward_elm)

#Question 2.c
Ireland = data.frame(Schooling = 18.2, AdultMortality = 68, HIVAIDS = 0.1, ICoResources = 0.907)
pred = predict(backward_elm, Ireland)
pred

confint(backward_elm,level = 0.95)

#Question 3.a
tw_anova = aov(cigsPerDay~male*currentSmoker, data = HeartAttack)
summary(tw_anova)

#Question 3.b
dataMale = HeartAttack[HeartAttack$male==1,]
dataFemal = HeartAttack[HeartAttack$male==0,]
cigMen = subset(dataMale, select = c("cigsPerDay"))
Menmean = sapply(cigMen,mean)
Menmean

#Question 4.a
HeartAttack_M = HeartAttack$TenYearCHD
Data = HeartAttack[,c(1:15)]
mdl = glm(HeartAttack_M~.,data = Data)
summary(mdl)

#Question 4.b
Male = HeartAttack$male

Age = HeartAttack$age

cigsPerDay = HeartAttack$cigsPerDay

sysBP = HeartAttack$sysBP

glucose = HeartAttack$glucose

back_elm = glm(HeartAttack_M ~ Male + Age + cigsPerDay + sysBP + glucose)
summary(back_elm)

#Question 4.c
exp(back_elm$coefficients[-1])

exp(back_elm$coefficients[-1]-1)*100

#Question 4.d.1
data = data.frame(Data)
p = predict(mdl, data)
p

#Question 4.d.2
yhat = ifelse(p>0.5,1,0)
yhat

#Question 4.d.3
TenYearCHD = HeartAttack_M
score = 0
for (i in 1:length(TenYearCHD)) {
  
  if(TenYearCHD[i]==yhat[i]){
    score = score+1
    
  }
}
len = length(TenYearCHD)
percentage = (score/len)*100
percentage
