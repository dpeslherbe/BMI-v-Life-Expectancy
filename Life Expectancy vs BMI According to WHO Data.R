##Open data in RStudio
Life_Expectancy_Data <- read.csv("~/Life Expectancy Data.csv", header=TRUE)
View(Life_Expectancy_Data)

## Setup overall function to automate for the year
Lregryear <- function(m){

##Subset for chosen year
chooseyear <- function(n){
  chosendata <- subset.data.frame(Life_Expectancy_Data, Life_Expectancy_Data$Year == n)
chosendata
}

##Say we want to model data on 2015 only
data <- chooseyear(m)
data

##Subset for developing(0) or developed(1) countries
choosedevstatus <- function(a){
  if (a == 0){
    chosendata <- subset.data.frame(data, data$Status == "Developing")
  }
  else {
    chosendata <- subset.data.frame(data, data$Status == "Developed")
  }
  chosendata
}

##Split into developing & developed status for year chosen
datadeveloping <- choosedevstatus(0)
datadeveloping
datadeveloped <- choosedevstatus(1)
datadeveloped

##Let us compute the linear model
fitdeveloping <<- lm(datadeveloping$Life.expectancy ~ datadeveloping$BMI,
                     data = datadeveloping)
fitdeveloped <<- lm(datadeveloped$Life.expectancy ~ datadeveloped$BMI,
                    data = datadeveloped)
anovadeveloping <<- anova(lm(datadeveloping$Life.expectancy ~ datadeveloping$BMI,
                             data = datadeveloping))
anovadeveloped <<- anova(lm(datadeveloped$Life.expectancy ~ datadeveloped$BMI,
                            data = datadeveloped))
summdeveloping <<- summary(lm(datadeveloping$Life.expectancy ~ datadeveloping$BMI,
                              data = datadeveloping))
summdeveloped <<- summary(lm(datadeveloped$Life.expectancy ~ datadeveloped$BMI,
                             data = datadeveloped))

##Let us create a plot to visualize
{dataplot <- plot(data$BMI, data$Life.expectancy, main = "Life Expectancy vs BMI in Developed (Blue)
                  vs Developing Countries (Red) According to WHO Data", sub = m, xlab= "BMI", ylab = "Life Expectancy")
abline(fitdeveloped, col="blue")
abline(fitdeveloping, col="red")
}
return(c(fitdeveloping$coefficients, fitdeveloped$coefficients, dataplot))
}

##Let us create a GIF to show the plots progressing through the years
##First we need to install animation package through install.packages("animation")
library(animation)
saveGIF({for (j in 2000:2015){Lregryear(j)}}, movie.name = "Lreggif.gif")

##Let us create  the summary function taking the year, & 0, 1 for developing, developed
##status respectively
summfunc <- function(o, p){
if (p ==0){
  datayear <- Lregryear(o)
summary(fitdeveloping)
}
else{
  datayear <- Lregryear(o)
  summary(fitdeveloped)
}
}

##Let us set up a function loop to calculate lm summary for developing=0, then developed=1
returnsumm <- function(r){
q <- 2000
while (q <= 2015){
  summtable <- summfunc(q,r)
  print(summtable)
  q <- q+1
}
}
returnsumm(0)
returnsumm(1)

##Conclusions:
##There seems to be a difference in the relation between BMI and Life Expectancy
##depending on whether the WHO classifies the country as Developed or Developing.
##Note that for Developed countries, BMI is not appropriate to use for regression
##estimation as the significance from the linear regression summary is not an
##acceptable level of significance.
##However, for Developing countries, BMI does seem to be an appropriate predictor
##for Life Expectancy from the linear regression summary with the highest level
##of significance.
##Finally, we must also note that while BMI is a good predictor for Life
##Expectancy in Developing countries, having an intercept and BMI alone does not
##suffice to create sufficient linear regression model, since the R-squared
##values (non-adjusted & adjusted) are under 0.5 for every year; this means that
##there is probably another predictor or set of predictors that we need for the
##linear regression model to be more precise.
