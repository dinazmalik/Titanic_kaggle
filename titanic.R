library(mice)
library(e1071)
library(caret)

tr<-read.csv("C:/Users/User/Desktop/Spring2018/Titanic/train.csv")
test<-read.csv("C:/Users/User/Desktop/Spring2018/Titanic/test.csv")

# No iteration. But I want to get Predictor-Matrix
init = mice(tr, maxit=0) 
predM = init$predictorMatrix
# Do not use following columns to impute values in 'Age'. Use the rest.
predM[, c("PassengerId", "Name","Ticket","Cabin")]=0    
imp<-mice(tr, m=5, predictorMatrix = predM)
# Get the final data-frame with imputed values filled in 'Age'
tr <- complete(imp)
View(tr)

# No iteration. But I want to get Predictor-Matrix
init = mice(test, maxit=0) 
predM = init$predictorMatrix
# Do not use following columns to impute values in 'Age'. Use the rest.
predM[, c("PassengerId", "Name","Ticket","Cabin")]=0    
imp<-mice(test, m=5, predictorMatrix = predM)
# Get the final data-frame with imputed values filled in 'Age'
test <- complete(imp)
View(test)





tr <- subset(tr,select= -c(PassengerId, Name,Ticket,Cabin))
mod_tr <- subset(tr, select = -c(Survived))
Dep_var<- tr[,"Survived"]

  for (i in colnames(mod_tr[, sapply(mod_tr, is.factor)])){
  for (level in unique(mod_tr[, i])){
    mod_tr[paste(i, level, sep = "_")] = 
      as.integer(ifelse(mod_tr[, i] == level, 1, -1))
  }
}


mod_tr <- subset(mod_tr, select = -c(Sex,Embarked))


model <- svm(mod_tr,Dep_var)
pred <- predict(model, mod_tr)
pred<-round(pred)

confusionMatrix(pred,Dep_var)

test <- subset(test,select= -c(PassengerId, Name,Ticket,Cabin))


for (i in colnames(test[, sapply(test, is.factor)])){
  for (level in unique(test[, i])){
    test[paste(i, level, sep = "_")] = 
      as.integer(ifelse(test[, i] == level, 1, -1))
  }
}

test <- subset(test, select = -c(Sex,Embarked))
write.csv(test, file = "C:/Users/User/Desktop/Spring2018/Titanic/test_complete.csv", row.names = FALSE)

test_df <- read.csv("C:/Users/User/Desktop/Spring2018/Titanic/test_complete.csv")


test_mod <- predict(model, test_df)
test_mod<-round(test_mod)

write.csv(test_mod, file = "C:/Users/User/Desktop/Spring2018/Titanic/test_pred.csv", row.names = FALSE)





