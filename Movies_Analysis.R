#Setting directory
getwd()
path <- "/Users/IBM_ADMIN/Desktop"
setwd(path)

#import data from excel
library(readxl)
movies <- read_excel("Movies.xlsx")
head(movies)

#getting an idea of the data
str(movies)
movies$Verdict <- as.factor(movies$Verdict)   #typecasting
plot(movies$Verdict, xlab = "Movie Performance", ylab = "Frequency", col = "lightblue")
pie(movies$`Boxoffice collection`, labels = movies$Movie, cex = 0.7)

Hit <- subset(movies,subset = (Verdict == "Blockbuster"| 
                                  Verdict == "Super Hit"))

per <- round(Hit$`Boxoffice collection`/sum(movies$`Boxoffice collection`)*100)
lbls <- paste(Hit$Movie, per) # add percents to labels
lbls <- paste(lbls,"%",sep="")
pie(Hit$`Boxoffice collection`, labels= lbls, cex = 0.7)


#outlier values
summary(movies)
boxplot(movies$`Boxoffice collection`, col = "blue", ylab = "Box office collection in Cr")
#install.packages("car")
library(car)
Boxplot(movies$`Boxoffice collection`, labels = movies$Movie , col = "Blue")
outliers <- Boxplot(movies$`Boxoffice collection`)
outliers
mean(movies$`Boxoffice collection`)
median(movies$`Boxoffice collection`)
var(movies$`Boxoffice collection`)
sd(movies$`Boxoffice collection`)

#removing the outliers and checking the analysis
without_outliers <- movies$`Boxoffice collection`[-outliers]

mean(without_outliers)
median(without_outliers)
var(without_outliers)
sd(without_outliers)



#Regression Analysis
plot(movies$`Budget(cost+P&A)`,movies$`Boxoffice collection`)
cor(movies$`Budget(cost+P&A)`,movies$`Boxoffice collection`) #pearson's coeff
abline(lm(movies$`Boxoffice collection`~ movies$`Budget(cost+P&A)`), col="red") # regression line (y~x)

