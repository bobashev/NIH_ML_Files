
library('ISLR')
library('MASS')

## Bias Varinace Trade-off.

## Assume true model is f(x) = (x-2)^3 + (x-2)^2 + 1 for 1 <=x <= 3
## We simulate data drom this model.
f = function(x){
  y = (x-2)^3 + (x-2)^2 + 1
  y
}

x = seq(1,3,by = 0.1)
n = length(x)
y = f(x) + rnorm(n, 0, 0.1)

xlim = c(min(x), max(x))
ylim = c(min(y), max(y))
plot(f(x)~x, type = 'l', xlab = 'x', ylab = 'y', xlim = xlim, ylim = ylim, lwd = '2', col = 'red')
points(y~x, pch = 21, col = 'blue')


## Training data
train.data = data.frame(y = y, x = x)

## First fit a linear model
lin.fit = lm(y~x,data = train.data)
abline(lin.fit, col = 'orange')
summary(lin.fit)


## Fit a cubic model
cub.fit = lm(y~poly(x,3),data = train.data)
summary(cub.fit)
points(x, fitted(cub.fit), col = 'black', pch=20, type = 'b')

## Fit a model of degree 5
five.fit = lm(y~poly(x,5),data = train.data)
summary(five.fit)
points(x, fitted(five.fit), col = 'yellow', type = 'b', pch = 20)

## Fit a model of degree 10
ten.fit = lm(y~poly(x,10),data = train.data)
summary(ten.fit)
points(x, fitted(ten.fit), col = 'cyan', type = 'b', pch = 20)

## Fit a model of degree 15
fifteen.fit = lm(y~poly(x,15),data = train.data)
summary(fifteen.fit)
points(x, fitted(fifteen.fit), col = 'green', type = 'b', pch = 20)

## Estimate the training MSE for linear model
lin.MSE.train = (1/n) * sum((y - lin.fit$fitted.values)^2)
## Estimate the training MSE for cubic model
cub.MSE.train = (1/n) * sum((y - cub.fit$fitted.values)^2)
## Estimate the training MSE for degree 5 model
five.MSE.train = (1/n) * sum((y - five.fit$fitted.values)^2)
## Estimate the training MSE for degree 10 model
ten.MSE.train = (1/n) * sum((y - ten.fit$fitted.values)^2)
## Estimate the training MSE for degree 15 model
fifteen.MSE.train = (1/n) * sum((y - fifteen.fit$fitted.values)^2)

## Test data
x.test = seq(0.97,3.04,by = 0.1)
n.test = length(x.test)
y.test = f(x.test) + rnorm(n.test, 0, 0.1)
test.data = data.frame(y = y.test, x = x.test)


## Perform predictions on all models
lin.y.pred = predict(lin.fit, newdata=data.frame(x = x.test))
cub.y.pred = predict(cub.fit, newdata=data.frame(x = x.test))
five.y.pred = predict(five.fit, newdata=data.frame(x = x.test))
ten.y.pred = predict(ten.fit, newdata=data.frame(x = x.test))
fifteen.y.pred = predict(fifteen.fit, newdata=data.frame(x = x.test))

## Compute test MSE
lin.MSE.test = (1/n.test) * sum((lin.y.pred - y.test)^2)
cub.MSE.test = (1/n.test) * sum((cub.y.pred - y.test)^2)
five.MSE.test = (1/n.test) * sum((five.y.pred - y.test)^2)
ten.MSE.test = (1/n.test) * sum((ten.y.pred - y.test)^2)
fifteen.MSE.est = (1/n.test) * sum((fifteen.y.pred - y.test)^2)

## Plot MSE results
train.MSE = c(lin.MSE.train, cub.MSE.train, five.MSE.train, ten.MSE.train, fifteen.MSE.train)
test.MSE = c(lin.MSE.test, cub.MSE.test, five.MSE.test, ten.MSE.test, fifteen.MSE.est)
plot(c(1,2,3,4,5), train.MSE, pch = 1:5, col = 'blue', type = 'b', lwd = 2, xlab = 'degrees of freedom', ylab = 'MSE')
points(c(1,2,3,4,5), test.MSE, pch = 1:5, col = 'green', type = 'b', lwd = 2)


## Validation set approach
plot(Auto$mpg~Auto$horsepower, pch = 20, col = 'blue', xlab = 'hp', ylab = 'mpg')

## Dividing the data set into training and validation sets
training.ind = sample(1:nrow(Auto), (floor(nrow(Auto) / 2)))
training.n = length(training.ind)
validation.ind = c(1:nrow(Auto))[-training.ind]
validation.n = length(validation.ind)

training.data = data.frame(mpg = Auto$mpg[training.ind], hp = Auto$horsepower[training.ind])
validation.data = data.frame(mpg = Auto$mpg[validation.ind], hp = Auto$horsepower[validation.ind])

MSE.training = {}
MSE.validation = {}
for(i in 1:10){
  fit = lm(mpg~poly(hp,i), data = training.data)
  MSE = (1/training.n) * sum((training.data$mpg - fitted(fit))^2)
  MSE.training = c(MSE.training, MSE)
  y = predict(fit, newdata = validation.data)
  MSE = (1/validation.n) * sum((validation.data$mpg - y)^2)
  MSE.validation = c(MSE.validation, MSE)
}

xlim = c(1, 10)
ylim = c(min(c(MSE.training, MSE.validation)), max(c(MSE.training, MSE.validation)))
plot(1:10, MSE.training, type = 'b', pch = 20, lwd = 2, col = 'red', xlab = 'degrees of freedom', ylab = 'MSE',
     xlim = xlim, ylim = ylim)
points(1:10, MSE.validation, type = 'b', pch = 20, lwd = 2, col = 'blue')

## Effects of different subsets
MSE.validation = matrix(0, nrow = 10, ncol = 10)
for(j in 1:10){
  training.ind = sample(1:nrow(Auto), (floor(nrow(Auto) / 2)))
  training.n = length(training.ind)
  validation.ind = c(1:nrow(Auto))[-training.ind]
  validation.n = length(validation.ind)
  
  training.data = data.frame(mpg = Auto$mpg[training.ind], hp = Auto$horsepower[training.ind])
  validation.data = data.frame(mpg = Auto$mpg[validation.ind], hp = Auto$horsepower[validation.ind])
  for(i in 1:10){
    fit = lm(mpg~poly(hp,i), data = training.data)
    y = predict(fit, newdata = validation.data)
    MSE.validation[i,j] = (1/validation.n) * sum((validation.data$mpg - y)^2)
  }
}

colvec = c('red','green','blue','black','yellow','cyan','pink','gray','brown','magenta')
xlim = c(1, 10)
ylim = c(min(MSE.validation), max(MSE.validation))
plot(1:10, MSE.validation[,1], type = 'b', pch = 20, lwd = 2, col = colvec[1], xlab = 'degrees of freedom', ylab = 'MSE',
     xlim = xlim, ylim = ylim)
for(i in 2:10){
  points(1:10, MSE.validation[,i], type = 'b', pch = 20, lwd = 2, col = colvec[i])
}

