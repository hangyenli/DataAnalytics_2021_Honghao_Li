#lab1_data.r
EPI_data <- read.csv('/Users/hong/Dropbox/10th/data analytics/lab1/2010EPI_data.csv')
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI_data$EPI

tf<- is.na(EPI_data)
E <- EPI_data[!tf]

#exercise 1

summary(EPI)

fivenum(EPI)
stem(EPI)
hist(EPI)

# there is an issue with my R version, environment and everything is not working well on my mac.
# I will do the the lab exercise on my PC later.
