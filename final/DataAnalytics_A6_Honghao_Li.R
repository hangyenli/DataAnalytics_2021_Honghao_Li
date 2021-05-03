#Clear working space
rm(list = ls())

library(kknn)
library(rpart)
library(ggplot2)
library(tidyr)
library(resample)
library(readxl)
library(dplyr)
library(randomForest)
library(MASS)
library(party)
#Import dataset

# W_PER_CAPITA
egy_rnew <- read_excel("EG_EGY_RNEW.xlsx", sheet = 2)
summary(egy_rnew)


# % access to electricity
acs_elec <- read_excel("EG_ACS_ELEC.xlsx", sheet = 2)
summary(acs_elec)

# % rely on renewable energy
fec_rnew <- read_excel("EG_FEC_RNEW.xlsx", sheet = 2)
summary(acs_elec)

# % CON_USD_M dollars international investment
iff_randn <- read_excel("EG_IFF_RANDN.xlsx", sheet = 2)
summary(iff_randn)

#Clean/selecting data
#Find NAs
egy_rnew[egy_rnew=="N"] <- NA
acs_elec[acs_elec=="N"] <- NA
fec_rnew[fec_rnew=="N"] <-NA
iff_randn[iff_randn=="N"] <-NA

#Select data from the same year
egy_rnew_f <- egy_rnew[egy_rnew$TimePeriod==2017 & egy_rnew$Goal==7,]
acs_elec_f <- acs_elec[acs_elec$TimePeriod==2017 & acs_elec$Location=="ALLAREA",]
fec_rnew_f <- fec_rnew[fec_rnew$TimePeriod==2017,]
iff_randn_f <- iff_randn[iff_randn$TimePeriod==2017,]

data_merge <- merge(egy_rnew_f,acs_elec_f, by.x="GeoAreaName", by.y="GeoAreaName", incomparables=NA)
data <- data_merge[, c("GeoAreaName", "Value.x", "Value.y")]
data <- rename(data,"Value_rnew"="Value.x", "Value_acs"="Value.y")
data_merge <- merge(data,fec_rnew_f, by.x="GeoAreaName", by.y="GeoAreaName", incomparables=NA)
data <- data_merge[, c("GeoAreaName", "Value_rnew", "Value_acs", "Value")]
data <- rename(data,"Value_fec"="Value")
data_merge <- merge(data,iff_randn_f, by.x="GeoAreaName", by.y="GeoAreaName", incomparables=NA)
data <- data_merge[, c("GeoAreaName", "Value_rnew", "Value_acs", "Value_fec", "Value")]
data <- rename(data,"Value_iff"="Value")

#ignore NA values
data <- na.omit(data)
data$Value_rnew <- as.numeric(data$Value_rnew)
data$Value_acs <- as.numeric(data$Value_acs)
data$Value_fec <- as.numeric(data$Value_fec)
data$Value_iff <- as.numeric(data$Value_iff)
data.values <- data[2:5]

#check for na
any(is.na(data))

# make default
attach(data.values)


#filter acc != 100
d <- filter(data.values, data.values$Value_acs!=100)
d <- filter(data.values, data.values$Value_iff!=0)

ggplot(gather(d, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 10)+
  facet_wrap(~cols, scales = 'free_x')

ggplot(gather(d, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 10)+
  facet_wrap(~cols, scales = 'free_x')

ggplot(gather(d, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 20000)+
  facet_wrap(~cols, scales = 'free_x')

qqnorm(data$Value_acs)

hist(d$Value_rnew)
hist(d$Value_acs)
hist(d$Value_fec)
hist(d$Value_iff)

boxplot(d$Value_rnew)
boxplot(d$Value_acs)
boxplot(d$Value_fec)
boxplot(d$Value_iff)

summary(data)

########################################################
#Modeling


#regression

plot(d$Value_acs,d$Value_fec)
lm2 <- lm(d$Value_acs ~  d$Value_fec, data = d)
summary(lm2)
ggplot(d, aes(y=Value_fec, x=Value_acs)) + 
  geom_point(alpha = .3) + 
  stat_smooth(method = "lm", formula = y ~ x, color="hotpink", fill="#76D7C4", alpha = 0.4)+
  theme_classic()

plot(d$Value_acs,log(d$Value_iff))
lm3 <- lm(log(d$Value_iff) ~  d$Value_acs)
abline(lm3)
summary(lm3)
ggplot(d, aes(y=log(Value_iff), x=Value_acs)) + 
  geom_point(alpha = .3) + 
  stat_smooth(method = "lm", formula = y ~ x, color="hotpink", fill="#76D7C4", alpha = 0.4)+
  theme_classic()


#knn
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

data.values$Value_rnew<-normalize(data.values$Value_rnew)
data.values$Value_acs<-normalize(data.values$Value_acs)
data.values$Value_iff<-normalize(data.values$Value_iff)
data.values$Value_fec<-normalize(data.values$Value_fec)

m <- dim(data.values)[1]

val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 

#corss validation
data.learn <- data.values[-val,]
data.valid <- data.values[val,]


kknnm <- kknn(Value_iff ~., data.learn, data.valid, distance = 1,
              kernel = "triangular", label)
summary(kknnm)
fit <- round(as.integer(fitted(kknnm)), -2)
table(round(as.integer(data.valid$Value_iff, data.valid$Value_rnew),-2), fit)


# random forest

# bagging classifier
set.seed(1)
train=sample(1:nrow(d),nrow(d)/2)
bag.data <- randomForest(Value_fec ~ ., data = d, subset = train,
                         mtry=3,ntrees=500, importance=T)

print(bag.data) 	# view results
importance(bag.data) # importance of each predictor
varImpPlot(bag.data)

yhat.bag=predict(bag.data, newdata = d[-train,])
d.test=d[-train,"Value_fec"]
mean((yhat.bag-d.test)^2)

#random classifier
rf.d<-randomForest(Value_fec ~ ., data=d, subset = train, mtry=2, ntrees=500,
                  importance=T)

print(rf.d) 	# view results
importance(rf.d) # importance of each predictor
varImpPlot(rf.d)
yhat.rf<-predict(rf.d, newdata=d[-train,])
d.test2=d[-train,"Value_fec"]
mean((yhat.rf-d.test2)^2)



#tree
help("ctree")
tr<-ctree(d$Value_fec ~ . , data=d, subset = train)
plot(tr)


#detach(data.values)

