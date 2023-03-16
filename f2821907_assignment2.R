#Erasmia Kornelatou, f2821907

library(readxl)


#reading the project 2 2019-2020 FT.xls file
callsx <- read_excel("D:\\Users\\astar\\Desktop\\Statistics\\project 2 2019-2020 FT\\project 2 2019-2020 FT.xls")

#write data frame into a csv
write.csv(callsx,"D:\\Users\\astar\\Desktop\\Statistics\\project 2 2019-2020 FT\\calls.csv", row.names = FALSE)


calls <- read.csv("D:\\Users\\astar\\Desktop\\Statistics\\project 2 2019-2020 FT\\calls.csv",sep=",",header = TRUE)




#imbalanced dataset
barplot(table(calls$SUBSCRIBED),col=c("blue","orange"),legend = rownames(table(calls$SUBSCRIBED)), main = "Term Deposit")
sum(calls$SUBSCRIBED == 'yes') #3987 rows
sum(calls$SUBSCRIBED == 'no') #35896 rows

#counting the rows of the dataset
nrow(calls) #39883
#remove duplicate lines 
calls <- calls[!duplicated(calls), ]
#counting the rows of the dataset after deleting duplicates
nrow(calls) #39871, there where 12 duplicate lines

#having a look at the structure of the data
str(calls)
#define factor columns 11 variables
cols_f <- c("job", "marital", "education", "default", "housing",
            "loan", "contact","month","day_of_week","poutcome","SUBSCRIBED")
#converting variables into factor variables
calls[cols_f] <- lapply(calls[cols_f], as.factor) 
#show variable types after type modifications
str(calls)
sapply(calls, class)
str(calls)

#defind numeric columns 10 variables
cols_n <- c("age","duration","campaign","pdays","previous","emp.var.rate",
	"cons.price.idx","cons.conf.idx","euribor3m","nr.employed")
#converting variables into numeric variables
calls[cols_n] <- lapply(calls[cols_n], as.numeric)  
sapply(calls, class)


#getting the numeric variables
require(psych)
index <- sapply(calls, class) == "numeric"
num <- calls[,index]



#Visual Analysis for numerical variables
#Histograms
par(mfrow=c(1,2))
for( i in 1:10)
{
      hist(num[,i], main=names(num)[i],xlab = names(num[i]), col = c("blue"))
}

#boxplots
par(mfrow=c(1,2))
for( i in 1:10)
{
      boxplot(num[,i] ~ calls$SUBSCRIBED,data = calls, xlab = "SUBSCRIBED", ylab = names(num[i]), col = c("blue","orange") )
}


par(mfrow=c(1,2))
barplot(table(calls$SUBSCRIBED,calls$job),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Job",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$job)))
barplot(table(calls$SUBSCRIBED,calls$marital),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Marital",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$marital)))

barplot(table(calls$SUBSCRIBED,calls$education),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Education",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$education)))
barplot(table(calls$SUBSCRIBED,calls$default),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Default",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$default)))

barplot(table(calls$SUBSCRIBED,calls$housing),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Housing",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$housing)))
barplot(table(calls$SUBSCRIBED,calls$loan),col= c("blue","orange"),ylab="Number of Clients",las=2,main="loan",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$loan)))

barplot(table(calls$SUBSCRIBED,calls$contact),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Contact",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$contact)))
barplot(table(calls$SUBSCRIBED,calls$month),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Month",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$month)))

barplot(table(calls$SUBSCRIBED,calls$day_of_week),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Day_of_week",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$day_of_week)))
barplot(table(calls$SUBSCRIBED,calls$poutcome),col= c("blue","orange"),ylab="Number of Clients",las=2,main="Poutcome",cex.names = 0.8,cex.axis = 0.8, legend=rownames(table(calls$SUBSCRIBED,calls$poutcome)))


#finding out correlations in graph
require(corrplot)
par(mfrow=c(1,1))
corrplot(cor(num))
corrplot(cor(num), method = "number") 

#remove duration,cons.price.idx,euribor3m,nr.employed
calls$duration <- NULL
calls$cons.price.idx <- NULL
calls$euribor3m <- NULL
calls$nr.employed <- NULL
num$duration <- NULL
num$cons.price.idx <- NULL
num$euribor3m <- NULL
num$nr.employed <- NULL

#finding out correlations in graph after removing variables
par(mfrow=c(1,1))
require(corrplot)
corrplot(cor(num))
corrplot(cor(num), method = "number") 



#standardizing our numerical variables for better predictive results
for (colnames in names(calls))
{
  if (is.numeric(calls[[colnames]]))
{
    calls[[colnames]]<-scale(calls[[colnames]],scale = TRUE,center = TRUE)
  }
}



install.packages("caret")
library(caret)
install.packages("randomForest")
install.packages("psych")
library(randomForest)


forest_auc <- 0
for(i in 1:10){
	#Spliting our data into train and test
	train_index <- createDataPartition(y=calls$SUBSCRIBED,p=0.70)
	#train 
	for_train <- calls[train_index$Resample1,]
	for_test <- calls[-train_index$Resample1,]


	#Random Forest
	for_model <- randomForest(SUBSCRIBED~.,data=for_train, ntree = 50 , mtry = 5, na.action=na.roughfix)
	print(for_model$confusion)
	#test
	prediction<-predict(for_model,for_test)
	#confusion matrix
	print(table(for_test$SUBSCRIBED,prediction))
	confusionMatrix(prediction, for_test$SUBSCRIBED)

	#ROC curve because we see that there's small probability of determining accurate results for subscriptions
	#low true positive rate
	install.packages("ROCR")
	library(ROCR)
	prob <- predict(for_model,for_test,type="response")
	bank_add_logreg_roc <- prediction(as.numeric(prob), as.numeric(for_test$SUBSCRIBED))
	plot(performance(bank_add_logreg_roc, "tpr", "fpr"))
	bank_add_logreg_auc <- performance(bank_add_logreg_roc, "auc")
	forest_auc <- forest_auc + bank_add_logreg_auc@y.values[[1]]
}
forest_auc
forest_auc <- forest_auc/10
forest_auc




#KNN
install.packages("FNN")
library(FNN)
calls.knn <- train(SUBSCRIBED ~ ., data = for_train, method = "knn", 
                  maximize = TRUE,
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess=c("center", "scale"))
predictKNN <- predict(calls.knn , newdata = for_test)
#confusion MATRIX
confusionMatrix(predictKNN, for_test$SUBSCRIBED)



# CART TREE

library(caret) 


calls.cart<-rpart(SUBSCRIBED ~ ., for_train , method = 'class')


#predict
cart_pred <- predict( calls.cart , for_test , type = "class")
cart_prob <- predict( calls.cart , for_test , type = "prob")

# CONFUSION MATRIX
confusionMatrix(cart_pred , for_test$SUBSCRIBED)

