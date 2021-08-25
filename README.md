# Chrun
Predict what kind of customers will stay and who will Churn


#import library
library(dplyr)
library(ggplot2)
library(caret)
library(MASS)
library(tidyr)
library(rsample)
library(rpart)
library(rpart.plot)
install.packages('corrplot')
library(corrplot)


#Check the data
glimpse(telecom_users)

#prepare the data

#remove customerID column
df <- subset(telecom_users, select = -c(X,customerID))
df

#check the missing value
sum(is.na(df))
na.omit(df)

#Check unique value
df %>% summarize_all(n_distinct)

#remove "no phone service" and "no internet service" to No
df$MultipleLines[df$MultipleLines == 'No phone service'] <- 'No'
df$OnlineSecurity[df$OnlineSecurity == 'No internet service'] <- 'No'
df$OnlineBackup[df$OnlineBackup == 'No internet service'] <- 'No'
df$DeviceProtection[df$DeviceProtection == 'No internet service'] <- 'No'
df$StreamingTV[df$StreamingTV == 'No internet service'] <- 'No'
df$TechSupport[df$TechSupport == 'No internet service'] <- 'No'
df$StreamingMovies[df$StreamingMovies == 'No internet service'] <- 'No'
df
df %>% summarize_all(n_distinct)


#Check tenure
min(df$tenure)
max(df$tenure)

#Group tenure
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-1 year')
  }else if(tenure > 12 & tenure <= 24){
    return('1-2 year')
  }else if (tenure > 24 & tenure <= 48){
    return('2-3 year')
  }else if (tenure > 48 & tenure <=60){
    return('3-4 year')
  }else if (tenure > 60){
    return('> 4 year')
  }
}
df$tenure_group <- sapply(df$tenure,group_tenure)
df$tenure_group <- as.factor(df$tenure_group)
df$tenure <- NULL
df

#Change binomial value in senior citizen to yes or no
df$SeniorCitizen[df$SeniorCitizen == 1] <- 'Yes'
df$SeniorCitizen[df$SeniorCitizen == 0] <- 'No'
df

#Change char to fct
df2 <- df
df2[sapply(df2, is.character)] <- lapply(df2[sapply(df2, is.character)], as.factor)
glimpse(df2)

#Check Collinearity among numeric variables
num_var <- data.frame(df2$MonthlyCharges, df2$TotalCharges)
M <- cor(num_var)
corrplot(M, method = "circle")

#remove monthly charge
df2$MonthlyCharges <- NULL

#machine learning

#Split the data
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(df2), replace = T, prob =                   c(0.8,0.2))
train <- df2[sample, ]
test <- df2[!sample, ]
dim(train)
dim(test)

#Logistic regression
model1 <- glm(Churn ~., family = binomial (link = logit), data = train)
summary(model1)

#Check model accuracy with confussion matrix
test$logit.churn.pred <- predict(model1, newdata = test, type = 'response')
test$churn.pred <- ifelse(test$logit.churn.pred > 0.5, 1, 0)
with(test, table(Churn, churn.pred))
print(paste('recall = ',790/(790+148) ))
print(paste('precision = ', 790/(790+86) ))
print(paste('accuracy = ', (790+165)/(790+86+148+165)))

#important variable
caret::varImp(model1)


#Decision Tree
trees <- rpart(
  formula = Churn ~ .,
  data    = train,
)

rpart.plot(trees)

#Check model accuracy 
pred <- predict(trees, test, type="class")
confusionMatrix(pred, test$Churn)
print(paste('recall = ', 825/(825+53)))
print(paste('precision = ', 825/(825+199)))
print(paste('accuracy = ', (825+114)/(825+199+53+114)))


