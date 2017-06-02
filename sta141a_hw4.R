########### sta 144 hw4 ################
########### Kota Natsume 914530226
########### R code 
#"The codes and results derived by using these codes constitute my own work.
#I have consulted the following resources regarding this assignment:
#https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/lda.html" 
#

setwd("C:/Users/toshiya/Desktop/sta141A")

############## 1 #########################


# extract versicolor and virginica data from iris
data_versi <- subset(iris,iris$Species == 'versicolor')
data_virgi <- subset(iris,iris$Species == 'virginica')


# make test and training data
data_test  <- rbind(head(data_versi,10),head(data_virgi,10))
data_train <- rbind(tail(data_versi,40),tail(data_virgi,40))

# drop level setosa
data_test$Species <- factor(data_test$Species)
data_train$Species <- factor(data_train$Species)

################ 2 ##############
library(MASS)

iris_lda <- lda(Species ~ Sepal.Length + Sepal.Width, data_train)
iris_lda$means
iris_pred_lda  <- predict(iris_lda, data_test, type = "response")
iris_confusion <- table(true = data_test$Species, predicted = iris_pred_lda$class)

error_rate <- 1 - (iris_confusion[1,1] + iris_confusion[2,2])/sum(iris_confusion)



############### 3 ####################
## logistic reg
log_model = glm(Species ~ Sepal.Length + Sepal.Width, data_train, family = binomial)

# get stanndard error # p-value for width is 0.42
summary(log_model)

log_pred  = predict(log_model, data_test, type = "response")
head(log_pred)
log_pred = (log_pred > 0.5) + 1
log_pred = c('versicolor','virginica')[log_pred]
log_con  = table(true = data_test$Species, model = log_pred)
error_rate <- 1 - (log_con[1,1] + log_con[2,2])/sum(log_con)



## logistic reg only with length

log_model = glm(Species ~ Sepal.Length, data_train, family = binomial)

# get stanndard error
summary(log_model)

log_pred  = predict(log_model, data_test, type = "response")
head(log_pred)
log_pred = (log_pred > 0.5) + 1
log_pred = c('versicolor','virginica')[log_pred]
log_con  = table(true = data_test$Species, model = log_pred)
error_rate <- 1 - (log_con[1,1] + log_con[2,2])/sum(log_con)


## knn

library(class)

# Fit knn  k = 1 
knn_pred = knn(train = data_train[c('Sepal.Length')] ,test  = data_test[c('Sepal.Length')],cl = data_train$Species,k= 1)

knn_con = table(true = data_test$Species, model = knn_pred)

#Misclassification error rate
error_rate = 1 - (knn_con[1,1] + knn_con[2,2])/sum(knn_con)


# Fit knn  k = 5 
knn_pred = knn(train = data_train[c('Sepal.Length')] ,test  = data_test[c('Sepal.Length')],cl = data_train$Species,k= 5)

knn_con = table(true = data_test$Species, model = knn_pred)

#Misclassification error rate
error_rate = 1 - (knn_con[1,1] + knn_con[2,2])/sum(knn_con)

