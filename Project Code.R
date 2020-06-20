library(knitr)
library(dplyr)
library(ggplot2)
library(VGAM)
library(readr)
library(lubridate)
library(caTools)
library(imputeMissings)
library(leaps)
library(bestglm)
library(randomForest)
library(Hmisc)
library(caret)
library(mctest)

#Read in the original dataset
accident <- read_csv("C:/James Laptop/M.S. Applied Statistics/MAT 8414 Categorical Data Analysis - Dr. Bernhardt/Final Project/US_Accidents_Dec19.csv")
#Remove some variables that are not useful
accident2 <- accident %>% select(-c("End_Lat", "End_Lng", "Description", "ID", "Number", "Country", "Airport_Code", "Weather_Timestamp", "Source", "Timezone", "County", "City"))
#Break time variables into separate smaller time variables
accident2 <- accident2 %>% 
  mutate(Year=year(Start_Time),
         Month=month(Start_Time, label=T, abbr=T),
         Day=day(Start_Time),
         Weekday=wday(Start_Time, label=T, abbr=T),
         Start_Time_Hour_th=hour(Start_Time)+1) %>%
  select(-Start_Time, -End_Time)
accident2$Month <- as.factor(accident2$Month)
accident2$Weekday <- as.factor(accident2$Weekday)
accident2$Day <- as.factor(accident2$Day)
accident2$Start_Time_Hour_th <- as.factor(accident2$Start_Time_Hour_th)
#Filter out only PA 2019 data to narrow down our analysis
accident2 %>% filter(Year==2019, State=="PA") %>% 
  select(-Year, -State) -> accident2
#Reformat the Wind_Direction variable to avoid duplicated levels 
accident2$Wind_Direction <- accident2$Wind_Direction %>% 
  gsub(pattern="CALM", replacement="Calm", fixed=T) %>% 
  gsub(pattern="East", replacement="E", fixed=T) %>% 
  gsub(pattern="North", replacement="N", fixed=T) %>% 
  gsub(pattern="South", replacement="S", fixed=T) %>% 
  gsub(pattern="West", replacement="W", fixed=T) %>% 
  gsub(pattern="VAR", replacement="Variable", fixed=T)
#Transform Street variable into Street_Type for better grouping and interpretation
accident2$Street <- ifelse(substr(accident2$Street, start=1, stop=2)=="I-", "Highway",
                           ifelse(substr(accident2$Street, start=nchar(accident2$Street)-2+1, stop=nchar(accident2$Street))=="Rd", "Road",
                                  ifelse(substr(accident2$Street, start=nchar(accident2$Street)-3+1, stop=nchar(accident2$Street))=="Ave", "Avenue",
                                         ifelse(substr(accident2$Street, start=nchar(accident2$Street)-2+1, stop=nchar(accident2$Street))=="Ln", "Lane",
                                                ifelse(substr(accident2$Street, start=nchar(accident2$Street)-4+1, stop=nchar(accident2$Street))=="Blvd", "Boulevard",
                                                       ifelse(substr(accident2$Street, start=nchar(accident2$Street)-2+1, stop=nchar(accident2$Street))=="Rd", "Road",
                                                              ifelse(substr(accident2$Street, start=nchar(accident2$Street)-2+1, stop=nchar(accident2$Street))=="Dr", "Lane",
                                                                     ifelse(substr(accident2$Street, start=nchar(accident2$Street)-2+1, stop=nchar(accident2$Street))=="St", "Street", "Road"))))))))
accident2 %>% mutate(Street_Type=Street) %>% select(-Street) -> accident2
accident2$Street_Type <- as.factor(accident2$Street_Type)
#Change the name of variables to drop the unit in parentheses
accident2 %>% rename(
  Distance = `Distance(mi)`,
  Temperature = `Temperature(F)`,
  Wind_Chill = `Wind_Chill(F)`,
  Humidity = `Humidity(%)`,
  Pressure = `Pressure(in)`,
  Visibility = `Visibility(mi)`,
  Wind_Speed = `Wind_Speed(mph)`,
  Precipitation = `Precipitation(in)`) -> accident2
#Set some variables into categorical format
accident2 %>% select(
  c(TMC, Zipcode, Severity, 
    names(accident2[sapply(accident2, is.character)]))) -> reformat
reformat <- sapply(reformat, as.factor)
accident2 %>% select(
  -c(TMC, Zipcode, Severity, 
     names(accident2[sapply(accident2, is.character)]))) -> noreformat
accident2 <- cbind(reformat, noreformat)
#Check amount of missing values
for (i in 1:length(names(accident2))){
  print(paste(names(accident2)[i], " ", sum(is.na(accident2[,i])), " ", round(sum(is.na(accident2[,i]))/20099*100, digits=4), "%"))
}
#Check histogram of "Distance" variable
hist(accident2$Distance) #Looks skewed, may need log-transformation
quantile(accident2$Distance, probs=.99) #Unusual/unlikely value
#Cut off the unusually high values of "Distance" by using 99th percentile
accident2$Distance <- ifelse(
  accident2$Distance>quantile(accident2$Distance, probs=.99), 
  quantile(accident2$Distance, probs=.99),
  accident2$Distance)
#Add some tiny values to the distance variable to avoid excessive amount of 0 values, which does not really make sense for car accidents
#Log-transform distance to make it less skewed and improve the variable
#Then drop original distance variable
accident2 %>% 
  mutate(Log_Distance = log(accident2$Distance+runif(length(accident2$Distance), min=0, max=0.01))) %>%
  select(-Distance) -> accident2
#Check histogram of "Distance" variable again
hist(accident2$Log_Distance)
#Check histogram of "Temperature" to detect skewness/unusual values
hist(accident2$Temperature)
#Check histogram of "Wind_Chill" to detect skewness/unusual values
hist(accident2$Wind_Chill)
#Check histogram of "Wind_Speed"
hist(accident2$Wind_Speed)
#Check histogram/quantiles of "Visibility"
hist(accident2$Visibility)
quantile(accident2$Visibility, na.rm=T)
#Cut off the unusual values of "Visibility" on both ends by using 99th and 1st percentiles
accident2$Visibility <- ifelse(
  accident2$Visibility>quantile(accident2$Visibility, na.rm=T, probs=.99),
  quantile(accident2$Visibility, na.rm=T, probs=.99),
  ifelse(
    accident2$Visibility<quantile(accident2$Visibility, na.rm=T, probs=.01),
    quantile(accident2$Visibility, na.rm=T, probs=.01),
    accident2$Visibility))
#Check histogram/quantiles of "Visibility" again
hist(accident2$Visibility)
quantile(accident2$Visibility, na.rm=T)
#Combine levels
accident2 %>% mutate(
  Zipcode.comb = combine.levels(accident2$Zipcode, minlev=.003),
  Weather.comb = combine.levels(accident2$Weather_Condition, minlev=.005)) %>% 
  select(-Zipcode, -Weather_Condition) -> accident2
accident.imp <- accident2
num.var <- sapply(accident.imp, is.numeric)
#Set seed to split the dataset into training and validation sets
#Training 80% and validation 20%
#Split dataset based on response variable: Severity
set.seed(267)
split = sample.split(accident.imp$Severity, SplitRatio = 0.8)
#Imputation
impu <- imputeMissings::compute(accident.imp[split,], method="median/mode")
accident.imp <- imputeMissings::impute(accident.imp, object=impu)
#Random Forest model
accident.md.train <- as.data.frame(accident.imp[split,])
accident.md.valid <- as.data.frame(accident.imp[!split,])
RF <- randomForest(Severity~., data=accident.md.train,
                   ntree=501, 
                   importance=TRUE,
                   mtry=10)
predictionRF <- predict(RF, newdata=accident.md.valid, type="response")
confusionMatrix(predictionRF, accident.md.valid$Severity)
#Check variable importance plot from Random Forest model
varImpPlot(RF)
#Pick out the top 30 variables based on MeanDecreaseAccuracy for cumulative logit model
logit.df <- as.data.frame(cbind(rownames(importance(RF)[,5:6]), importance(RF)[,5:6]), stringsAsFactors=F)
logit.df %>% rename(Variable=V1) %>% 
  mutate(MeanDecreaseAccuracy=as.numeric(MeanDecreaseAccuracy)) %>%
  select(-MeanDecreaseGini) %>% 
  arrange(desc(MeanDecreaseAccuracy)) -> logit.df
accident.cumu.train <- accident.md.train %>% 
  select(Severity, head(logit.df,30)[,1])
accident.cumu.valid <- accident.md.valid %>% 
  select(Severity, head(logit.df,30)[,1])
#Recode the response variable to ensure the model utilizes its ordinality
accident.cumu.train$Severity <- factor(accident.cumu.train$Severity, levels=c(1,2,3,4), ordered=T)
accident.cumu.valid$Severity <- factor(accident.cumu.valid$Severity, levels=c(1,2,3,4), ordered=T)
#Cumulative model with proportional odds
fitcumu <- vglm(Severity~.,
                family=cumulative(parallel=T), data=accident.cumu.train)
predcumu <- predict(fitcumu, newdata=accident.cumu.valid, type="response")
predcumu <- as.data.frame(predcumu)
predcumu.class <- base::apply(predcumu, 1, function(x) which.max(x))
predcumu.class <- factor(predcumu.class, levels=c(1,2,3,4), ordered=T)
confusionMatrix(predcumu.class, accident.cumu.valid$Severity)
#Cumulative model WITHOUT proportional odds
fitcumu.nopo <- vglm(Severity~.,
                     family=cumulative(parallel=F), data=accident.cumu.train)
predcumu.nopo <- predict(fitcumu.nopo, newdata=accident.cumu.valid, type="response")
predcumu.nopo <- as.data.frame(predcumu.nopo)
predcumu.classnopo <- base::apply(predcumu.nopo, 1, function(x) which.max(x))
predcumu.classnopo <- factor(predcumu.classnopo, levels=c(1,2,3,4), ordered=T)
confusionMatrix(predcumu.classnopo, accident.cumu.valid$Severity)
#Log-likelihood
logLik.vlm(fitcumu)
df.residual_vlm(fitcumu)
#Coefficients and estimated odds ratio
fitcumu.coef <- Coef(fitcumu)
head(sort(fitcumu.coef), 15)
tail(sort(fitcumu.coef), 15)
#Checking coefficients related to hours of a day
accident.imp$Start_Time_Hour_th <- factor(accident.imp$Start_Time_Hour_th, levels=c(1:24), ordered=T)
plot(accident.imp$Start_Time_Hour_th, xlab="Hour periods", ylab="Freq")
sort(fitcumu.coef[startsWith(names(fitcumu.coef),"Start_Time")])
exp(unname(fitcumu.coef["Start_Time_Hour_th9"]-fitcumu.coef["Start_Time_Hour_th21"]))
exp(unname(fitcumu.coef["Start_Time_Hour_th8"]-fitcumu.coef["Start_Time_Hour_th20"]))
#Checking coefficients related to street types
sort(fitcumu.coef[startsWith(names(fitcumu.coef),"Street_Type")])
exp(unname(fitcumu.coef["Street_TypeHighway"]-fitcumu.coef["Street_TypeRoad"]))
exp(unname(fitcumu.coef["Street_TypeHighway"]-fitcumu.coef["Street_TypeStreet"]))
#Some other estimated odds ratios
exp(fitcumu.coef["Temperature"])
exp(fitcumu.coef["Wind_Speed"])
exp(fitcumu.coef["Traffic_SignalTRUE"])
exp(fitcumu.coef["StopTRUE"])
exp(fitcumu.coef["CrossingTRUE"])
#Numeric variable coefficients
fitcumu.coef[names(accident.imp)[sapply(accident.imp, is.numeric)]]
