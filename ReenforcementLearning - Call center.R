options(scipen=999)
##############################################
#CATI Survey Response Rate simulation        # 
##############################################

#Define call probabilities
#7 days a week and 2 times per day 14 options
#3 call back scenario
#call probs attempt 1
# true prob, starting prob, complete tracker, attempt tracker
c1<-NULL
c1<-rbind(c1,c(0.14,0.5,0,0))
c1<-rbind(c1,c(0.08,0.5,0,0))
c1<-rbind(c1,c(0.07,0.5,0,0))
c1<-rbind(c1,c(0.05,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c1<-rbind(c1,c(0.02,0.5,0,0))
c1<-rbind(c1,c(0.04,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c1<-rbind(c1,c(0.08,0.5,0,0))
c1<-rbind(c1,c(0.01,0.5,0,0))
c1<-rbind(c1,c(0.00,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c1<-rbind(c1,c(0.04,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c2<-as.data.frame(c1)
names(c2)<-c("act","pred","complete",'n')





c2<-as.data.frame(c1)
names(c2)<-c("act","pred","complete",'n')

tracker<-NULL
tracker2<-NULL

for(p in 1:2000){
  #intialize dataset for clean run
  c2<-as.data.frame(c1)
  names(c2)<-c("act","pred","complete",'n')
  tracker<-NULL
  #hyper parameters
  
  #Simple hyper parameter test
    #greed<- runif(1)
    #learnrate<-runif(1)
  #advanced Test: contact greed/learnrate
  
    greed_decay<-sample(1:500,1)
    learn_decay<-sample(1:500,1)

  
for(i in 1:1000){
    greed<-exp(-1/greed_decay*i)*1
    learnrate<-exp(-1/learn_decay*c2[cond,4])
  
    #select day for calling
    #x<-sample(1:14,1,replace=T) 
    #cond<-sample(1:14,1,replace=T)
  
    #calculate the best condition based on current predicted RR
    bestcond<-as.numeric(
      sample(
        rownames(
          c2[c2$pred==max(c2$pred),]
        )
        ,1)
    )    
    #Continue to explore the conditions, ensure no local minimal trap
    #20% of calls will randomly select a condition to call.
    #cond<-ifelse(runif(1)>0.2,bestcond,sample(rownames(c2),1))
    cond<-ifelse(runif(1)>(greed),bestcond,sample(rownames(c2),1))
    #if greed==0 then best always
    #if greed==1 then random always
    #test if call is completed complete based on the condition
    y<-ifelse(runif(1)<c2[cond,1],1,0)
    
    c2[cond,3]<-y+c2[cond,3] # record # of compeletes
    c2[cond,4]<-1+c2[cond,4] # recod total attempts
        #Gradient Descent
        #calculate error (Pred Prob -  Outcome)
        
        yhat<-c2[cond,2]
        error<-(yhat - y)
        #Learning rate adjustment, exponential
          #learnrate<-ifelse(i<250,0.01,0.005)
        
        #exponetial decay function for learning rate.
        #starts 0.25+0.005(minimum learning rate)
        #shring learning rate as you learn more (exp(-%*n))
        #n - number of attempts
        #% - decay rate, smaller number = less decay
        #learnrate<-0.25*exp(-1/100*c2[cond,4])+.005
        
        #store new value
        c2[cond,2]<-yhat-error*learnrate
        
        cond_err<-sum(abs(rank(c2$pred)-rank(c2$act)))
        tracker<-rbind(tracker,cond_err)
    #print(paste(c2[cond,1],round(c2[cond,2],3),round(learnrate,3)))
    #tracker<-rbind(tracker,yhat)
    #plot(tracker, xlim=c(1, 1000),ylim=c(0, 1),col=sample(rainbow(10)))
    
}
#plot(tracker, xlim=c(1, 1000),ylim=c(1, 100),col=sample(rainbow(10)))


#store output fron each run, for future comparisons;
#final response rate vs. null response rate
#hyper paramters
#
  final_rr<-sum(c2$complete)/sum(c2$n)
  null_rr<-sum(c2$act*sum(c2$n)/14)/sum(c2$n)
  paste(final_rr,round(null_rr,3))
  #Rank Error
  rankerr<-sum(abs(rank(c2$pred)-rank(c2$act)))
  
  
  tracker2<-rbind(tracker2,c(greed_decay,greed,learn_decay,learnrate,final_rr,null_rr,rankerr))

}
out1<-as.data.frame(tracker2)





names(out1)<-c("greed_decay","greed","learn_decay","learnrate","final_rr","null_rr","rankerr")
out1$gain<-out1$final_rr-out1$null_rr
hist(out1$gain,xlim=c(-.03,.15))
summary(out1$gain)
range(out1$final_rr)
head(out1)


outx<-out1[out1$final_rr>.13,]
plot(outx$greed_decay,outx$learn_decay)
mean(outx$greed_decay)
mean(outx$learn_decay)
summary(outx$gain)


#save output... Pick only one
write.csv(out1,"simple hyper parameters")
write.csv(out1,"complex hyper parameters 1-500.csv")


#setwd("D:/work/R workspace/bandit")
out1<-read.csv("simple hyper parameters")
out2<-read.csv("complex hyper parameters")

hist(out1$gain,xlim=c(-.05,.1))
hist(out2$gain,xlim=c(-.05,.1))

summary(out1$gain)
summary(out2$gain)

plot(out2$greed_decay, out2$gain)
plot(out2$learn_decay, out2$gain)

plot(out1$greed, out1$gain)
plot(out1$learnrate, out1$gain)

plot(out2$greed, out2$gain)
plot(out2$learnrate, out2$gain)

#install.packages("scatterplot3d")
#install.packages("rgl")
library(rgl)
library(car)
library(scatterplot3d)
#install.packages("mime")


scatter3d(x=out1$learn_decay, z=out1$greed_decay, y=out1$gain,surface=T, point.col = "blue")
scatter3d(x=out1$learnrate, z=out1$greed, y=out1$gain,surface=T, point.col = "blue")


scatter3d(x=out2$learn_decay, z=out2$greed_decay, y=out2$gain,surface=FALSE, point.col = "blue")
scatter3d(x=out2$learnrate, z=out2$greed, y=out2$gain,surface=FALSE, point.col = "blue")



scatter3d(x=out1$learnrate, z=out1$greed, y=out1$gain,surface=FALSE, point.col = "blue")

opmodel<-lm(gain~greed_decay+learn_decay,out2)
summary(opmodel)


?rgl()










hist(tracker2)

hist(tracker2-0.05857, main="Percentage Point Improvement")

#0.05857
sum(c2$act*1000/14)/sum(c2$n)
#Gradient Descent
#calculate error (Pred Prob -  Outcome)
yhat<-day1[[1]][2]
error<-(yhat - y)
learnrate<-0.01
day1[[1]][2]<-yhat-error*learnrate

plot()

?as.data.frame()
#call probs attempt 1
# true prob, starting prob, complete tracker, attempt tracker
c1<-NULL
c1<-rbind(c1,c(0.15,0.5,0,0))
c1<-rbind(c1,c(0.08,0.5,0,0))
c1<-rbind(c1,c(0.07,0.5,0,0))
c1<-rbind(c1,c(0.05,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c1<-rbind(c1,c(0.02,0.5,0,0))
c1<-rbind(c1,c(0.04,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c1<-rbind(c1,c(0.08,0.5,0,0))
c1<-rbind(c1,c(0.01,0.5,0,0))
c1<-rbind(c1,c(0.02,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c1<-rbind(c1,c(0.04,0.5,0,0))
c1<-rbind(c1,c(0.06,0.5,0,0))
c2<-as.data.frame(c1)
names(c2)<-c("act","pred","complete",'n')


day1<-list(c1_1,c1_2 ,c1_3 ,c1_4 ,c1_5 ,c1_6 ,c1_7 ,c1_8 ,c1_9 ,c1_10,c1_11,c1_12,c1_13,c1_14)

c2_1  <-0.15
c2_2  <-0.10
c2_3  <-0.07
c2_4  <-0.05
c2_5  <-0.06
c2_6  <-0.02
c2_7  <-0.04
c2_8  <-0.06
c2_9  <-0.08
c2_10 <-0.01
c2_11 <-0.02
c2_12 <-0.06
c2_13 <-0.04
c2_14 <-0.06
day2<-c(c2_1,c2_2 ,c2_3 ,c2_4 ,c2_5 ,c2_6 ,c2_7 ,c2_8 ,c2_9 ,c2_10,c2_11,c2_12,c2_13,c2_14)

c3_1  <-0.15
c3_2  <-0.10
c3_3  <-0.07
c3_4  <-0.05
c3_5  <-0.06
c3_6  <-0.02
c3_7  <-0.04
c3_8  <-0.06
c3_9  <-0.08
c3_10 <-0.01
c3_11 <-0.02
c3_12 <-0.06
c3_13 <-0.04
c3_14 <-0.06
day3<-c(c3_1,c3_2 ,c3_3 ,c3_4 ,c3_5 ,c3_6 ,c3_7 ,c3_8 ,c3_9 ,c3_10,c3_11,c3_12,c3_13,c3_14)


