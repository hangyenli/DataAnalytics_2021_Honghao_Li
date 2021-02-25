#lab2 Part 1
#read in data
EPI_data <- read.csv("EPI_data.csv")

#set as default
attach(EPI_data)

#set to T/F if N/A
tf<-is.na(EPI)
#filter out F
E<-EPI[!tf]

summary(EPI)
fivenum(EPI, na.rm = TRUE)

#generate histogram of EPI
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw="SJ"))
rug(EPI)


plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(EPI)
qqline(EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

boxplot(EPI,DALY)
qqplot(EPI,DALY)




