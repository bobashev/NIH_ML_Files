install.packages("mlbench")
library(mlbench)
data(package="mlbench")
data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
data(iris)
head(iris)
summary(iris)
pairs(iris)

# Install caret
install.packages("caret", dependencies = TRUE)
# load caret package
library(caret)
# load the dataset
data(iris)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(iris[,1:4], method=c("range"))
# transform the dataset using the pre-processing parameters
transformed <- predict(preprocessParams, iris[,1:4])
# summarize the trans
summary(transformed)

trainControl <- trainControl(method="cv", number=10)
# estimate the accuracy of Naive Bayes on the dataset
fit <- train(Species~., data=iris, trControl=trainControl, method="nb")
# summarize the estimated accuracy
print(fit)

# prepare 5-fold cross validation and keep the class probabilities
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
# estimate accuracy using LogLoss of the CART algorithm
fit <- train(Species~., data=iris, method="rpart", metric="logLoss", trControl=control)
# display results
print(fit)

data(PimaIndiansDiabetes)
 # prepare 10-fold cross validation
 trainControl <- trainControl(method="cv", number=10)
 # estimate accuracy of logistic regression
 set.seed(7)
 fit.lr <- train(diabetes~., data=PimaIndiansDiabetes, method="glm", trControl=trainControl)
 # estimate accuracy of linear discriminate analysis
 set.seed(7)
 fit.lda <- train(diabetes~., data=PimaIndiansDiabetes, method="lda", trControl=trainControl)
 # collect resampling statistics
 results <- resamples(list(LR=fit.lr, LDA=fit.lda))
 # summarize results
 summary(results)
 #Summary of models could be found here
 #http://topepo.github.io/caret/bytag.html.
 
 dotplot(results)
 bwplot(results) 

 data(iris)
  # define training control
trainControl <- trainControl(method="cv", number=10)
# define a grid of parameters to search for random forest
  grid <- expand.grid(.mtry=c(1,2,3,4,5,6,7,8,10))
 # estimate the accuracy of Random Forest on the dataset
  fit <- train(Species~., data=iris, trControl=trainControl, tuneGrid=grid, method="rf")
  # summarize the estimated accuracy
  print(fit)

  install.packages("caretEnsemble")
  library(caretEnsemble)
   # load the Pima Indians Diabetes dataset
   data(PimaIndiansDiabetes)
   # create sub-models
   trainControl <- trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE)
   algorithmList <- c('knn', 'glm')
   set.seed(7)
   models <- caretList(diabetes~., data=PimaIndiansDiabetes, trControl=trainControl,
                         methodList=algorithmList)
   print(models)
   # learn how to best combine the predictions
   stackControl <- trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE)
   set.seed(7)
   stack.glm <- caretStack(models, method="glm", trControl=stackControl)
   print(stack.glm)

   finalModel <- randomForest(Species~., iris, mtry=2, ntree=2000)
   # display the details of the final model
   print(finalModel)



