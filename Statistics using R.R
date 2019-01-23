rm(list =ls())

#Q1.
 #a).
 rec <- read.csv("file:///C:/Users/TARUN AERVA TEJA/Desktop/statistics/recruitment_data.csv",stringsAsFactors = FALSE)
 View(rec)
 summary(rec)
 rec <- na.omit(rec)
 summary(rec)
 dim_rec <- dim(rec)
 #b).
 h <- hist(rec$sales_quota_pct)
 boxplot(rec$sales_quota_pct)
 summary(rec$sales_quota_pct)
 qqnorm(rec$sales_quota_pct)
 qqline(rec$sales_quota_pct)
 library(agricolae)
 ogive.freq(h)
  #the sales_quota_pct is very much close to normal distribution since the values are arranged on a line 
  #and also the mean and median values are very close
  #there is very little skewness in the data and all the 
 #c).
  #Ho: mu(sales,applied online) =  mu(sales,referral)
 table(rec$sales_quota_pct, rec$recruiting_source)
 g1 <- rec$sales_quota_pct[rec$recruiting_source == "Applied Online"]
 g2 <- rec$sales_quota_pct[rec$recruiting_source == "Referral"]
 t.test(g1,g2,alternative = "t",paired = F,conf.level = 0.95)
  #no they dont vary significantlly and we fail to reject the null hypothesis
  #the mean of those who applied online is same as those who can with the referral
 #d).
  #Ho: mu(attrition,applied online) = mu(attrition,referral) = mu(attrition,campus) = mu(attrition,firm)
 z_dat <- table(rec$attrition,rec$recruiting_source)
 chisq.test(z_dat)
  # there is a dependency and yes they vary significantly, we reject null hypothesis
  " __________________________________________________________________ "
 
#Q2.
 #a). 
 pri <- read.csv("file:///C:/Users/TARUN AERVA TEJA/Desktop/statistics/prima-indians-diabetes (1).csv",header = FALSE,stringsAsFactors = FALSE)
 View(pri)
 colnames(pri) <- c("preg", "gluc", "BP", "Skin thick", "Insulin", "BMI", "DPF", "Age", "Class")
 
 #b). Ho: mu(0,bmi) = mu(1,bmi)
 g3 <- pri$BMI[pri$Class == "0"]
 g4 <- pri$BMI[pri$Class == "1"]
 t.test(g3,g4,alternative = "t",paired = F,conf.level = 0.95)
  #we reject the null hypothesis. this means bmi is having a effect on the diabities
 
 #c).
  #Ho: mu(age,0) = mu(age,1)
 g5 <- pri$Age[pri$Class == "0"]
 g6 <- pri$Age[pri$Class == "1"]
 t.test(g5,g6,alternative = "t",paired = F,conf.level = 0.95)
  #we reject null hypothesis. this means age is a factor for diabities and irrespective of the age, people are affected with the diabities
  " ___________________________________________________________________ " 

#Q3.
 library(MASS)
 sur <- survey
 View(sur)
 summary(sur)
 f <- sur$Smoke
 f["NA"] <- names(f[f == max(f)])
 table(sur$Smoke)
 
 " _____________________________________________________________________ "
 
#Q4.
 library(MASS)
 car<- Cars93
 View(car)
 table(car$AirBags)
 table(car$Price)
 #a).
  #Ho: mu(price, driver&pass) = mu(price,driver) = mu(price,none)
 a2_test <- aov(car$Price ~ car$AirBags)
 summary(a2_test)
 #we reject Ho and there is a significant diff between mu1,mu2,mu3
 # we can say that price varies according t the air bags present in the car
 
 #b).
  #Ho: mu()
 a3_test <- aov(car$MPG.city ~ car$DriveTrain)
 summary(a3_test)
  #we reject null hypothesis and there is a significant difference 
  #we can say that depending on the drive train the mileages vary accordingly
 
 " _______________________________________________________________________ "
 
#Q5. 
 sample1 <- c(34,67,40,72,37,33,42,62,49,32,52,40,31,19,68,55,57,54,37,32,54,38,20,50,56,48,35,52,29,56,68,65,45,44,54,39,29,56,43,42)
 result <- summary(sample1)
 first_Q1 <- result[2]
 third_Q3 <- result[5]
 IQR <- result[5] - result[2]
 inner_fence <- result[2] - (1.5*(IQR))
 outer_fence <- result[5] + (1.5*(IQR)) 
 
 "__________________________________________________________________________ "
 
#Q6.Binomial Distribution
 x <- c(0:20)
 p <- c()
 for(i in 1:length(x)){
   p[i] <- dbinom(x[i],prob = 0.95,size = 20)
 }
 print(p)
 max_p <- C()
 for(i in 1:length(p)){
   if(p[i] == max(p)){
     max_p <- x[i]

   }
 }
 print(max_p)#since this is a binomial distribution, the max prob value always lie at 95% of the size(x). so here we are getting at x = 19 and till 19 the probability is increasing and then decreases
 # so here we can infer that out of 20 samples we have a max probability of getting 19 defect free valves is more
 
  " __________________________________________________________________________ " 
 
#Q7.
  y_bar <- 235
  stand_dev <- 5
  
  #a). P(y<230)
  y1 <- 230
  z_data1 <- ((y1-y_bar))/stand_dev
  prob1 <- pnorm(z_data1)
   #the probability that randomly picked bottle is less than 230 is 0.1586553
  
  #b).
  y2 <- 235
  z_data2 <- ((y2-y_bar))/stand_dev
  prob2 <- pnorm(z_data2)
  #the probability that randomly picked bottle is less than 235 is 0.5
  
  #c).
  #Ans: in a, we have less prob of getting
  
  " _________________________________________________________________________ "

#8.
  
  " ___________________________________________________________________________ "
#10.
 #a).
 HR <- read.table("file:///C:/Users/TARUN AERVA TEJA/Desktop/statistics/HR.txt",header = TRUE,sep = "\t")
 View(HR)
 #gender: 1 is male, 2 is female
 #a). Ho : mu(yes,male) = mu(yes,female)
    # Ha : mu(yes,male) not equal to mu(yes,female)
 CT <- table(HR$Attrition,HR$Gender)
 colnames(CT) <- c("Male","Female")
 x1 <- CT[2,1]#no of males who left
 x2 <- CT[2,2]#no of females who left
 n1 <- sum(CT[ ,1])#total no of males
 n2 <- sum(CT[ ,2])#total no of females
 p1 <- x1/n1#proportions
 p2<- x2/n2
 ppooled <- (x1+x2)/(n1+n2)#this tells,for complete organisation, what is the attrition. 
 zdata <-(p1-p2)/sqrt((ppooled)*(1-ppooled)*((1/n1)+(1/n2)))
 p_val <- 2*pnorm(abs(zdata),lower.tail = FALSE) #since this is two talied so we do 2*pnorm() and lower.tail.
 zdata;p_val
  #with these values of p and z, we fail to reject the null hypothesis(Ho)
  #this means that gender is not dependent on attrition i.e., gender does not effect leaving of the organisation
 
 #b).
 # Ho: mu(yes,1) = mu(yes,2) = mu(yes,3)
 CT2<- table(HR$Attrition,HR$Department)
 chisq.test(CT2) #
  #since p value is less than 0.05, so we reject the null hypothesis. so there is a dependency i.e., the department is effecting the attrition rate(leaving the organisation)
 
 #c).
 g7 <- HR$MonthlyIncome[HR$Gender == "1"]
 g8 <- HR$MonthlyIncome[HR$Gender == "2"]
 t.test(g7,g8,paired = F,alternative = "t",conf.level = 0.95)
  #we reject null hypothesis and the monthly income is dependant on the gender.
 
 #d).
 g9 <- HR$MonthlyIncome[HR$Department == "1"]
 g10 <- HR$MonthlyIncome[HR$Department == "2"]
 g11 <- HR$MonthlyIncome[HR$Department == "3"]
 a4_test <- aov(HR$MonthlyIncome ~ HR$Department)
 summary(a4_test)
  #we reject null hypothesis since for 95% confidence interval itself we are getting <0.04 and there is a dependancy of the income on the department
 
  " ____________________________________________________________________ "
 
 
 
 
 
 
 