#Install the required package
install.packages("e1071")
install.packages('caret')

#Load the required library
library("e1071")
library("caret")
#Load the Iris data set
data(iris)
#Explore the Iris data set
head(iris)
summary(iris)
str(iris)
set.seed(123)
trainIndex <- sample(1:nrow(iris), 0.7 * nrow(iris))
iris_train <- iris[trainIndex, ]
iris_test <- iris[-trainIndex, ]
#Visualize the Iris data set
#Plot sepal length vs. sepal width with species as color and point type
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, pch = 19, main = "Sepal Length vs Sepal Width", xlab = "Sepal Length", ylab = "Sepal Width")
#Train the SVM model classifier
model <- svm(Species ~ ., data = iris_train, kernel = "linear", cost = 0.1, scale = TRUE)
#Tune the SVM model's parameters
set.seed(123)
tuned_model <- tune(svm, Species ~ ., data = iris, ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(2:7)))
plot(tuned_model)
#Evaluate the SVM prediction model
predicted <- predict(model, iris_test[, -5])
confusionMatrix(table(predicted, iris_test$Species))

#Add legend
legend("topright", legend = levels(iris$Species), col = unique(iris$Species), pch = 19, title = "Species")