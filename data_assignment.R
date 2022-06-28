
#q1 (a)
main <- do.call(data.frame,lapply(main,function(x) replace(x, is.infinite(x), NA)))

kharif_data <- subset(main,main$season=="Kharif")

kharif1 <- do.call(data.frame,lapply(kharif_data,function(x) replace(x, is.infinite(x), NA)))

rabi_data <- subset(main,main$season=="Rabi")

main$lgdp <- log(main$gdp)

main$lv34 <- log(main$v34)

main$lbeds <- log(main$beds)

model1 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34,data = kharif_data)
summary(model1)

model2 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34,data = rabi_data)
summary(model2)

# q3 (a)
main$nz <- c(0)
main$ez <- c(0)
main$wz <- c(0)
main$sz <- c(0)
main$cz <- c(0)
main$nez <- c(0)

for(i in 2:70572){
  s = main[i,4]
  if(s=="Himachal Pradesh" || s=="Punjab" || s=="Uttarakhand" || s=="Uttar Pradesh" || s=="Haryana") {
    main[i,98] = 1
  }
  if(s=="Bihar" || s=="Orissa" || s=="Jharkhand" || s=="West Bengal") {
    main[i,99] = 1
  }
  if(s=="Rajasthan" || s=="Gujarat" || s=="Goa" || s=="Maharashtra") {
    main[i,100] = 1
  }
  if(s=="Andhra Pradesh" || s=="Telangana" || s=="Karnataka" || s=="Kerala" || s=="Tamil Nadu") {
    main[i,101] = 1
  }
  if(s=="Madhya Pradesh" || s=="Chhattisgarh") {
    main[i,102] = 1
  }
  if(s=="Assam" || s=="Sikkim" || s=="Nagaland" || s=="Meghalaya" || s=="Manipur" || s=="Mizoram" || s=="Tripura" || s=="Arunachal Pradesh") {
    main[i,103] = 1
  }
}

model3 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + nz,data = kharif_data)
summary(model3)

model4 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + ez,data = kharif_data)
summary(model4)

model5 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + wz,data = kharif_data)
summary(model5)

model6 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + sz,data = kharif_data)
summary(model6)

model7 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + cz,data = kharif_data)
summary(model7)

model8 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + nez,data = kharif_data)
summary(model8)

# rabi_data
model3 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + nz,data = rabi_data)
summary(model3)

model4 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + ez,data = rabi_data)
summary(model4)

model5 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + wz,data = rabi_data)
summary(model5)

model6 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + sz,data = rabi_data)
summary(model6)

model7 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + cz,data = rabi_data)
summary(model7)

model8 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + nez,data = rabi_data)
summary(model8)







#Ques no 1 b)

kharif2<-subset(kharif_data,v36!="NA")
kharif2<-subset(kharif2,v37!="NA")
kharif2<-subset(kharif2,v16!="NA")
kharif2<-subset(kharif2,v21!="NA")
kharif2<-subset(kharif2,v46!="NA")
kharif2<-subset(kharif2,v45!="NA")
kharif2<-subset(kharif2,tap!="NA")
kharif2<-subset(kharif2,beds!="NA")
kharif2<-subset(kharif2,v34!="NA")
kharif2<-subset(kharif2,gdp!="NA")
kharif2<-subset(kharif2,v28!="NA")


rabi2<-subset(rabi_data,v36!="NA")
rabi2<-subset(rabi2,v37!="NA")
rabi2<-subset(rabi2,v16!="NA")
rabi2<-subset(rabi2,v21!="NA")
rabi2<-subset(rabi2,v46!="NA")
rabi2<-subset(rabi2,v45!="NA")
rabi2<-subset(rabi2,tap!="NA")
rabi2<-subset(rabi2,beds!="NA")
rabi2<-subset(rabi2,v34!="NA")
rabi2<-subset(rabi2,gdp!="NA")
rabi2<-subset(rabi2,v28!="NA")


plot(kharif2$index,  kharif2$v36, ylab="v36", xlab="yield index", main="Kharif")

plot(rabi2$index,  rabi2$v36, ylab="v36", xlab="yield index", main=" Rabi")

# calculating the residual value for kharif
res1 = resid(model1)
# ?i,t on y-axis and yield index on x-axis for kharif.
plot(kharif2$index,res1, ylab="Residual error", xlab="Yield index", main="Kharif") 


# calculating the residual value for Rabi
res2 = resid(model2)
# ?i,t on y-axis and yield index on x-axis for Rabi.
plot(rabi2$index,res2, ylab="Residual error", xlab="Yield index", main="Rabi") 


#predicted values of the health indicator on y-axis and true values of the health indictor on x-axis Kharif
plot(kharif2$v36,predict(model1), ylab="Predicted", xlab="True", main="Kharif") 

#predicted values of the health indicator on y-axis and true values of the health indictor on x-axis Rabi
plot(rabi2$v36,predict(model2), ylab="Predicted", xlab="True", main="Rabi") 


#Plot a histogram of ?i,t and verify that ???i,t ?i,t = 0
# for kharif

x<- res1
temp<- res1[x>- (mean(res1)+ 3*sd(res1)) & x< mean(res1)+ 3*sd(res1)]

hist(temp, col='red', main='kharif')


sum(resid(model1))

#Plot a histogram of ?i,t and verify that ???i,t ?i,t = 0
# for rabi

x<- res2
temp<- res2[x>- (mean(res1)+ 3*sd(res2)) & x< mean(res2)+ 3*sd(res2)]

hist(temp, col='blue', main='Rabi')

sum(resid(model2))


# q1 (d)

res1 <- residuals(model1)
k1<-kharif2$v37
sum(temp<- res1*k1)

#View(kharif2)


sum(temp<-res1*kharif2$v37)

temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]

hist(temp, xlab = "Ui,t*v37")


sum(temp<-res1*log(kharif2$gdp))

temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]

hist(temp, xlab = "Ui,t*log(gdp)")



sum(temp<-res2*rabi2$v37)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]

hist(temp, xlab = "Ui,t*v37")

sum(temp<-res1*log(rabi2$gdp))
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*log(gdp)")

sum(temp<-res1*kharif2$tap)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*tap")

sum(temp<-res2*rabi2$tap)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*tap")

sum(temp<-res1*log(kharif2$beds))
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*log(beds)")

sum(temp<-res2*log(rabi2$beds))
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*log(beds)")

sum(temp<-res1*kharif2$v16)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v16")

sum(temp<-res2*rabi2$v16)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v16")

sum(temp<-res1*kharif2$v21)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v21")

sum(temp<-res2*rabi2$v21)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v21")

sum(temp<-res1*kharif2$v28)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v28")

sum(temp<-res2*rabi2$v28)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v28")

sum(temp<-res1*kharif2$v46)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v46")

sum(temp<-res2*rabi2$v46)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v46")

sum(temp<-res1*kharif2$v45)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v45")

sum(temp<-res2*rabi2$v45)
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*v45")

sum(temp<-res1*kharif2$log(v34))
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*log(v34)")

sum(temp<-res2*rabi2$log(v34))
temp<- temp[temp>-(mean(temp)+3*sd(temp)) & temp<(mean(temp)+3*sd(temp)) ]
hist(temp, xlab = "Ui,t*log(v34)")


sum(temp<-res1*kharif2$v37)
sum(temp<-res1*log(kharif2$gdp))
sum(temp<-res1*kharif2$tap)
sum(temp<-res1*log(kharif2$beds))
sum(temp<-res1*kharif2$v16)
sum(temp<-res1*kharif2$v21)
sum(temp<-res1*kharif2$v28)
sum(temp<-res1*kharif2$v46)
sum(temp<-res1*kharif2$v45)
sum(temp<-res1*log(kharif2$v34))

sum(temp<-res2*rabi2$v37)
sum(temp<-res2*log(rabi2$gdp))
sum(temp<-res2*rabi2$tap)
sum(temp<-res2*log(rabi2$beds))
sum(temp<-res2*rabi2$v16)
sum(temp<-res2*rabi2$v21)
sum(temp<-res2*rabi2$v28)
sum(temp<-res2*rabi2$v46)
sum(temp<-res2*rabi2$v45)
sum(temp<-res2*log(rabi2$v34))

sum(res1)
#res1 <- residuals(model)
k1<-kharif2$tap
temp<- res1*k1
sum((temp))


model12 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + sz,data = kharif2)
summary(model12)
library(car)
nullhyp <- c("sz")
linearHypothesis(model12,nullhyp)


model11 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34 + sz,data = rabi2)
summary(model11)
library(car)
nullhyp <- c("sz")
linearHypothesis(model11,nullhyp)



model <- lm(v36 ~ gdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + v45 + lv34,data = kharif_data)
summary(model)


hist(kharif2$gdp, col='blue', main='Rabi')
cor(kharif2$tap,kharif2$v37)
                                   
# project code

add <- read.csv("C:/Users/dahuj/Downloads/add.csv")
data <- read.csv("C:/Users/dahuj/Downloads/final1.csv")

write.csv(data,"C:/Users/dahuj/Downloads/project.csv")

View(add)
View(data)

data$toilet = NA
data$ntoilet = NA

for(i in 1:36) {
  for(j in 1:70572) {
    if(!(is.na(add[i,2]))) {
      if(!(is.na(data[j,4]))) {
        if(add[i,2]==data[j,4]) {
          data[j,74]=add[i,4]
          data[j,79]=add[i,3]
        }
      }
    }
  }
}

for(i in 1:70572) {
  if(data[i,4]=="Andaman And Nicobar Islands") {
    data[i,74]=100
    data[i,79]=37359
  }
  if(data[i,4]=="Delhi") {
    data[i,74]=82.3
    data[i,79]=8231129
  }
  if(data[i,4]=="Jammu And Kashmir") {
    data[i,74]=100
    data[i,79]=1448584
  }
  if(data[i,4]=="Ladakh") {
    data[i,74]=100
    data[i,79]=21909
  }
  if(data[i,4]=="The Dadra And Nagar Haveli And Daman And Diu") {
    data[i,74]=100
    data[i,79]=35049
  }
}

for(i in 1:70572) {
  if(length(data[i,73])==0) {data[i,73]=1}
}

cor(data$ntoilet,data$v36, use="pairwise.complete.obs")

data$lr <- log(as.numeric(data$rain))

data$lgdp <- log(data$gdp)

data$lv34 <- log(data$v34)

data$lbeds <- log(data$beds)

kharif_data <- subset(data,data$season=="Kharif")
rabi_data <- subset(data,data$season=="Rabi")

View(kharif_data)

model12 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + lv34 + toilet + lr,data = data)
summary(model12)

model12 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + lv34,data = data)
summary(model12)

cor(data$v36,data$toilet, use = "pairwise.complete.obs")
cor(data$v36,data$lr, use = "pairwise.complete.obs")

View(rabi_data)
model13 <- lm(v36 ~ lgdp + tap + lbeds + v16 + v21 + v28 + v46 + v45 + lv34 + toilet + lr,data = rabi_data)
summary(model13)

cor(data$v36,data$lv34,use = "pairwise.complete.obs")

mean((data$lgdp),na.rm=TRUE)
median((data$lgdp),na.rm=TRUE)
sd((data$lgdp),na.rm=TRUE)
max((main$lgdp),na.rm=TRUE)
min((main$lgdp),na.rm=TRUE)

mean((data$tap),na.rm=TRUE)
median((data$tap),na.rm=TRUE)
sd((data$tap),na.rm=TRUE)
max((main$tap),na.rm=TRUE)
min((main$tap),na.rm=TRUE)

mean((data$beds),na.rm=TRUE)
median((data$beds),na.rm=TRUE)
sd((data$beds),na.rm=TRUE)
max((main$beds),na.rm=TRUE)
min((main$beds),na.rm=TRUE)

mean((data$v37),na.rm=TRUE)
median((data$v37),na.rm=TRUE)
sd((data$v37),na.rm=TRUE)
max((main$v37),na.rm=TRUE)
min((main$v37),na.rm=TRUE)

mean((data$v16),na.rm=TRUE)
median((data$v16),na.rm=TRUE)
sd((data$v16),na.rm=TRUE)
max((main$v16),na.rm=TRUE)
min((main$v16),na.rm=TRUE)


mean((data$v21),na.rm=TRUE)
median((data$v21),na.rm=TRUE)
sd((data$v21),na.rm=TRUE)
max((main$v21),na.rm=TRUE)
min((main$v21),na.rm=TRUE)

mean((data$v28),na.rm=TRUE)
median((data$v28),na.rm=TRUE)
sd((data$v28),na.rm=TRUE)
max((main$v28),na.rm=TRUE)
min((main$v28),na.rm=TRUE)

mean((data$v46),na.rm=TRUE)
median((data$v46),na.rm=TRUE)
sd((data$v46),na.rm=TRUE)
max((main$v46),na.rm=TRUE)
min((main$v46),na.rm=TRUE)

mean((data$v34),na.rm=TRUE)
median((data$v34),na.rm=TRUE)
sd((data$v34),na.rm=TRUE)
max((main$v34),na.rm=TRUE)
min((main$v34),na.rm=TRUE)

mean((data$toilet),na.rm=TRUE)
median((data$toilet),na.rm=TRUE)
sd((data$toilet),na.rm=TRUE)
max((main$toilet),na.rm=TRUE)
min((main$toilet),na.rm=TRUE)

mean((data$rain),na.rm=TRUE)
median((data$rain),na.rm=TRUE)
sd((data$rain),na.rm=TRUE)
max((main$rain),na.rm=TRUE)
min((main$rain),na.rm=TRUE)

library(car)
nullhyp <- c("toilet")
linearHypothesis(model12,nullhyp)

library(car)
nullhyp <- c("lr")
linearHypothesis(model12,nullhyp)

model1 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + lv34 + toilet + lr,data = data, na.action = na.exclude)
summary(model1)

na.omit(model2)

model2 <- lm(v36 ~ lgdp + tap + lbeds + v37 + v16 + v21 + v28 + v46 + lv34 ,data = data, na.action = na.exclude)
summary(model2)

na.omit(model1)

anova(model1,model2)

