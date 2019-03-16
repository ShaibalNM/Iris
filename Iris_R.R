
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2", dependencies = TRUE)
install.packages("ggvis", dependencies = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
 
iris_db = iris
colnames(iris_db) = c("Sepal_Length", "Sepal_Width","Petal_Length","Petal_Width","Species")

head(iris_db)
unique(iris_db$Species)
str(iris_db)
summary(iris_db)

#Data Visualization

#Checking Sepal Length and Width ratio for each Species
ggplot(iris_db, 
       aes(x = Sepal_Length, y = Sepal_Width, color = Species)) + 
  geom_point() + 
  geom_rug()
  
#High correlation between Sepal length and width for Setosa

ggplot(iris_db, 
       aes(x = Petal_Length, y = Petal_Width, color = Species)) + 
  geom_point() + 
  geom_rug()

#High correlation between Petal length and width for Setosa and Versicolor

ggplot(iris_db, 
       aes(x = Sepal_Length, y = Petal_Width, color = Species)) + 
  geom_point() + 
  geom_rug()
#Low correlation between Sepal Length and Petal Width

ggplot(iris_db, 
       aes(x = Petal_Length, y = Sepal_Width, color = Species)) + 
  geom_point() + 
  geom_rug()
#Low correlation between Petal Length and Sepal Width


#Checking the correlation between Sepal and Petal dimensions 
x = levels(iris_db$Species)

cor(iris_db$Sepal_Length, iris_db$Sepal_Width)
cor(iris_db$Petal_Length, iris_db$Petal_Width)

cor(iris_db[iris_db$Species == x[1], 1:4])
cor(iris_db[iris_db$Species == x[2], 1:4])
cor(iris_db[iris_db$Species == x[3], 1:4])

#High Correlations
#Setosa: Sepal Length and Sepal Width
#Versicolor: Sepal Length and Petal Length | Petal Width and Petal Length
#Virginica: Sepal Length and Petal Length

#Distribution of Species
round(prop.table(table(iris_db$Species))*100, digits = 1)
#Equal distribution of each Species

#Start KNN Algo
install.packages("class")
library(class)

#Normalize data - Helps KNN learn faster

normalize = function(x){
  return((x - min(x))/(max(x) - min(x)))
}

iris_db_n = as.data.frame(lapply(iris_db[1:4], normalize))
summary(iris_db_n)
iris_db_n = cbind(iris_db_n, iris_db$Species)

set.seed(123); 
train_index = sample(1:nrow(iris_db_n),0.6*nrow(iris_db_n))
test_index = setdiff(1:nrow(iris_db_n), train_index)

iris_train = iris_db_n[train_index, -5]
iris_test = iris_db_n[test_index, -5]

iris_train_label = iris_db_n[train_index,5]
iris_test_label = iris_db_n[test_index,5]

length(iris_test_label)
length(iris_train_label)

iris_pred = knn(train = iris_train, test = iris_test, cl = iris_train_label, k = 3)
iris_test_label = data.frame(iris_test_label)
iris_pred = data.frame(iris_pred)
merge = data.frame(iris_test_label, iris_pred)
colnames(merge) = c("Test Label", "Predicted Label")

any(grepl("gmodels", installed.packages()))
install.packages("gmodels")
library(gmodels)

?CrossTable

CrossTable(x = unlist(iris_test_label), y = unlist(iris_pred), prop.c = FALSE)

############Using Caret

install.packages("caret")
library(caret)

index = createDataPartition(iris$Species, p = 0.75, list = FALSE)
length(index)

iris_train = iris[index,]
iris_test = iris[-index,]
nrow(iris_train) + nrow(iris_test)

names(getModelInfo())
model_knn = train(iris_train[,1:4], iris_train[,5], method = "knn")

?predict
prediction = predict(object = model_knn, iris_test[,1:4])
table(prediction)
confusionMatrix(prediction, iris_test[,5])
