rm(list = ls())
setwd("E:/Study/edWisor Stuff/Project2")
getwd()

data = read.csv("day.csv", header = TRUE)
str(data)

sapply(data, function(x) sum(is.na(x))) #no missing values

data$season= factor(data$season)
data$yr = factor(data$yr)
data$mnth = factor(data$mnth)
data$holiday = factor(data$holiday)
data$weekday = factor(data$weekday)
data$workingday = factor(data$workingday)
data$weathersit = factor(data$weathersit)

#----------------------------------------------VISUALIZATION-----------------------------------------
plot(cnt ~ season , data = data) #least rentals are in season 1 and maximum are in season 3

plot(cnt ~ weathersit , data = data) #weather 3 have least of rentals

plot(cnt ~ weekday , data = data) 

install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)
#library(psych)

ggplot(data , aes_string(x=data$workingday)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
 xlab("Working day") + ylab("Count") + ggtitle("Working day distribution") + theme(text = element_text(size = 15))
#bikes are rented more on working days

reg1 = lm(cnt ~ temp, data = data)
summary(reg1)
with(data ,plot(temp, cnt))
abline(reg1) #rental counts increase with increase in temperature

reg2 = lm(cnt ~ hum, data = data)
summary(reg2)
with(data ,plot(hum, cnt))
abline(reg2) #rental count decrease with increase in humidity

#-----------------------------------------------OUTLIER ANALYSIS---------------------------------------

#Boxplotting
numeric_index = sapply(data, is.numeric)
numeric_data = data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames)){
  assign(paste0("gn", i), ggplot(aes_string(y = cnames[i], x = "cnt"), data = subset(data)) +
           stat_boxplot(geom = "errorbar", width = 0.5) + 
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.shape = 18, outlier.size = 1, notch = FALSE) + 
           theme(legend.position = "bottom") + labs(y = cnames[i], x="count") + ggtitle(paste("Boxplot of count for", cnames[i])))
}


#plotting plots together
gridExtra::grid.arrange(gn2, gn3, gn4, ncol = 3)
gridExtra::grid.arrange(gn5, gn6, gn7, ncol = 3)

#replace outliers with NA and impute
for(i in cnames) {
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i]) $out]
  
  print(length(val))
  data[,i][data[,i] %in% val] = NA
}

#imputing NA values
data$casual[is.na(data$casual)] = data$cnt - data$registered

data$hum[is.na(data$hum)] = mean(data$hum,na.rm = T)

data$windspeed[is.na(data$windspeed)] = mean(data$windspeed, na.rm = T)

# creating copy
copy1 = data

#----------------------------------------FEATURE SELECTION------------------------------------------

install.packages("corrgram")
library(corrgram)
round(cor(numeric_data),2)

corrgram(data[, numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot")
#temp and atemp are strongle correlated

data = subset(data, select=-c(instant, atemp, casual, registered, dteday)) 

#anova test

AnovaModel_season =(lm(cnt ~ season, data = data))
summary(AnovaModel_season) #keep

AnovaModel_year =(lm(cnt ~ yr, data = data))
summary(AnovaModel_year) #keep

AnovaModel_month =(lm(cnt ~ mnth, data = data))
summary(AnovaModel_month) #keep

AnovaModel_holiday =(lm(cnt ~ holiday, data = data))
summary(AnovaModel_holiday) #remove

AnovaModel_weekday =(lm(cnt ~ weekday, data = data))
summary(AnovaModel_weekday) # keep

AnovaModel_workingday =(lm(cnt ~ workingday, data = data))
summary(AnovaModel_workingday) #remove

AnovaModel_weathersit =(lm(cnt ~ weathersit, data = data))
summary(AnovaModel_weathersit) #keep

data = subset(data, select=-c(holiday, workingday))


#Multicollinearity test
install.packages("usdm")
library(usdm)

vifcor(data[,c(6,7,8)])

#--------------------------------------------FEATURE SCALING-----------------------------------------
hist(data$temp)
hist(data$hum)
hist(data$windspeed)
hist(data$cnt)

#data['cnt'] = (data$cnt-min(data$cnt))/(max(data$cnt)-min(data$cnt))

#------------------------------------------------MODELING----------------------------------------------

#sampling
set.seed(101)
train_index = sample(1:nrow(data), 0.8*nrow(data))
data_train = data[train_index,] 
data_test = data[-train_index,]

#mape
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100
}


##Random forest
install.packages("randomForest")
library(randomForest)
library(inTrees)

#model
set.seed(101)
model_RF = randomForest(cnt ~. , data_train, importance = TRUE, ntree = 500)
model_RF
#error plotting
plot(model_RF)

#predict test data using RF model
RF_predict = predict(model_RF, data_test[,-9])
plot(data_test$cnt, RF_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'RF model')


install.packages("caret")
library(caret)
postResample(RF_predict, data_test$cnt)#R-sq = 0.88
mape(data_test$cnt, RF_predict)

varImpPlot(model_RF)


##Linear regression

install.packages("dummies")
library(dummies)

#?dummy.data.frame()
data_new = dummy.data.frame(data, sep = '_')

set.seed(101)
train_index1 = sample(1:nrow(data_new), 0.8*nrow(data_new))
data_train_new = data_new[train_index1,] 
data_test_new = data_new[-train_index1,]

#model
set.seed(101)
model_LR = lm(cnt ~. , data = data_train_new)
summary(model_LR)

#predictions
LR_predict = predict(model_LR, data_test_new[,-32])
plot(data_test_new$cnt, LR_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'LR model') 

#evaluation statistics
postResample(LR_predict, data_test_new$cnt)#R-sq = 0.83
mape(data_test_new$cnt, LR_predict)


##Decison tree

install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#model
set.seed(101)
model_DT = rpart(cnt~. , data = data_train, method = "anova")
summary(model_DT)
plt = rpart.plot(model_DT, type = 5, digits = 2, fallen.leaves = TRUE)
#?rpart.plot

#predictions
DT_Predict = predict(model_DT, data_test[,-9])
plot(data_test$cnt, DT_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'DT model')

#evaluation statistics
postResample(DT_Predict, data_test$cnt)#R-sq = 0.76
mape(data_test$cnt, DT_Predict)


##SVM
install.packages("e1071")
library(e1071)

#model
set.seed(101)
SVM_model = svm(cnt ~., data = data_train)

#predictions
SVM_predict = predict(SVM_model, data_test[,-9])
plot(data_test$cnt, SVM_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'SVM model')

#evaluation statistics
postResample(SVM_predict, data_test$cnt)
#RMSE = 0.08, R-sq = 0.88, MAE = 0.05

#----------------------------------------------------------------------------------
#CROSS VALIDATION RESULTS
set.seed(101)
train_control = trainControl(method="cv", number=10)
model1 = train(cnt~., data=data_train, trControl=train_control, method="rf")
print(model1) 
pred1 = predict(model1, data_test[,-9])
postResample(pred1, data_test$cnt) #Rsq = 0.84
mape(data_test$cnt, pred1)
plot(data_test$cnt, pred1)


#model2 = train(cnt~., data=data_train, trControl=train_control, method="rpart")
#print(model2)
#pred2 = predict(model2, data_test[,-9])
#postResample(pred2, data_test$cnt) #Rsq = 0.62
#mape(data_test$cnt, pred2)

#model3 = train(cnt~., data=data_train_new, trControl=train_control, method="lm")
#print(model3)
#pred3 = predict(model3, data_test_new[,-32])
#postResample(pred3, data_test_new$cnt) #Rsq = 0.85
#mape(data_test_new$cnt, pred3)

#model4 = train(cnt~., data=data_train, trControl=train_control, method="svmPoly")
#print(model4)
#pred4 = predict(model4, data_test[,-9])
#postResample(pred4, data_test$cnt) #Rsq = 0.87
#mape(data_test$cnt, pred4)

