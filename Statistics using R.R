rm(list = ls())
x <- read.csv("file:///F:/DSE/R lang/quiz and exam/KSEA.csv",stringsAsFactors = FALSE)
x
View(x)
x <- na.omit(x)
summary(x)
#The plot ofr each attribute
 hist(x$actual_mean_temp)
 boxplot(x$actual_mean_temp)
 hist(x$actual_min_temp)
 boxplot(x$actual_min_temp)
 hist(x$actual_max_temp)
 boxplot(x$actual_max_temp)
 hist(x$record_min_temp)
 boxplot(x$record_min_temp)
 hist(x$record_max_temp)
 boxplot(x$record_max_temp)
 hist(x$actual_precipitation)
 boxplot(x$actual_precipitation)
  #checking for outliers: 
  sd(x$actual_precipitation)
  q <- 3*sd(x$actual_precipitation)
  temp1 <- c(x$actual_precipitation)
  out4 <- c()
   for(i in 1:length(temp1)){
    if((abs(temp1[i]-mean(temp1))>q)){
      out4 <- temp1[i]
      
    }
  }
  print(out4)# there is one outlayer(2.2) in actual_precipitation attribute
 hist(x$record_precipitation)
 boxplot(x$record_precipitation)

#QUESTION 1).
 sd(x$record_max_temp)
 3*sd(x$record_max_temp)
 temp <- c(x$record_max_temp)
 out1 <- c()
 f <-  3*sd(x$record_max_temp)
 for(i in 1:length(temp)){
   if((abs(temp[i]-mean(temp))>f)){
     out1 <- temp[i]
     
   }
 }
 print(out1)# no outliers
  #since there is no sample which is greater than 3*sd(record_max_temp), so we dont have any outliers for record max temp

 max_temp <- c(x$record_max_temp)
 date_max1 <- c()
 for(i in 1:length(max_temp)){
  if(max_temp[i]==max(max_temp)){
     date_max1 <- x$date[i]
   }
 }
 date_max1
 
 " ___________________________________________________________________ "
 
#QUESTION 2).
 #Checking for Outliers in record_precipitation
 prec <- c(x$record_precipitation)
 c <- 3 * sd(prec)
 out2 <-c()
 k = 1
 for(i in 1:length(prec)){
   if((abs(prec[i]-mean(prec))>c)){
     out2[k] <- prec[i]
     k = k + 1
    
   }
 }
 print(out2)# there are 5 outliers.
  # there are elements(outliers) which are greater than 3*sd(x$record_precipitation)
 View(arrange(x, desc(x$record_precipitation)))
 prec <- c(x$record_precipitation)
 date_max2 <- c()
 for(i in 1:length(prec)){
   if(prec[i]==max(prec)){
     date_max2 <- x$date[i]
   }
 }
 date_max2
 
 " ________________________________________________________________________ "

#QUESTION 3).
 #checking for the outliers
 summary(x$actual_mean_temp)
 sd(x$actual_mean_temp)
 mean_temp <- c(x$actual_mean_temp)
 u <- 3 * sd(mean_temp)
 out3 <-c()
 k = 1
 for(i in 1:length(mean_temp)){
   if((abs(mean_temp[i]-mean(mean_temp))>u)){
     out3[k] <- mean_temp[i]
     k = k + 1
     
   }
 }
 print(out3)
 #there are no outliers.
 library(dplyr)
 x <- mutate(x,intensity = case_when(actual_mean_temp>=30 & actual_mean_temp<=42~("Low"),
                                actual_mean_temp>=43 & actual_mean_temp <= 60~("Medium"),
                                actual_mean_temp > 60 ~("High"))
 )
 g1 <- x$date[x$intensity == "Low"]
 g2 <- x$date[x$intensity == "Medium"]
 g3 <- x$date[x$intensity == "High"]
 print(g1)
 h <- table(x$intensity)
 print(h)
 barplot(h,col = rainbow(5),xlab = "intensities",ylab = "frequencies")
 #Ans: we can say from the plots that throughout all the days the percentage of medium temperatures is more and there were only few days which had low temperatures
 
  " _________________________________________________________________________ "
  
#QUESTION 4).
 y<- c(x[185:365, ])
 rainfall_for_past_6months <- sum(y$record_precipitation)
 hottest_temp_rec <- max(y$record_max_temp)
 
 " __________________________________________________________________________ " 
 
#QUESTION 5).
 t <- c(x$actual_mean_temp)
 sd(t)
 print(sd(t)/mean(t))
 #Ans: Since, the coefficient of variance(consistency coeff) is very small(0.1845) we can say that the consistency is very high. this means the temperature is consistent during all the periods of time














