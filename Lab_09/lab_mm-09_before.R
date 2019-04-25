# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Математическое моделирование: Практика 9
#  Машины опорных векторов

library('e1071')     # SVM
library('ROCR')      # ROC-кривые
library('ISLR')      # данные по экспрессии генов

my.seed <- 1

# Классификатор на опорных векторах --------------------------------------------

# сгенерированные данные: два линейно неразделимых класса ======================

# создаём наблюдения
set.seed(my.seed)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
# данные не разделяются линейно
plot(x, pch = 19, col = (3 - y)) 

# таблица с данными, отклик -- фактор
dat <- data.frame(x = x, y = as.factor(y))
# классификатор на опорных векторах с линейной границей
svmfit <- 
# на графике опорные наблюдения показаны крестиками

# список опорных векторов

# сводка по модели


# уменьшаем штрафной параметр
svmfit <- 
plot(svmfit, dat)
svmfit$index

# делаем перекрёстную проверку, изменяя штраф (аргумент cost)
set.seed(my.seed)
tune.out <- 
summary(tune.out)
# лучшая модель -- с минимальной ошибкой
bestmod <- 
summary(bestmod)

# генерируем контрольные данные
xtest <- matrix(rnorm(20 * 2), ncol = 2)
set.seed(my.seed)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- 
# делаем прогноз по лучшей модели
ypred <- 
# матрица неточностей


# прогноз по модели с cost = 0.01
svmfit <- 
ypred <- predict(svmfit, testdat)
# матрица неточностей
table(predict = ypred, truth = testdat$y)

# сгенерированные данные: два линейно разделимых класса ========================

# создаём наблюдения
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

# таблица с данными, отклик -- фактор
dat <- data.frame(x = x, y = as.factor(y))
# очень большой cost (маленький зазор, высокая точность классификации)
svmfit <- 
summary(svmfit)
plot(svmfit, dat)

svmfit <- 
summary(svmfit)
plot(svmfit,dat)


# Машина опорных векторов ------------------------------------------------------

# сгенерированные данные: нелинейная граница между классами ====================

# создаём наблюдения
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))

# таблица с данными, отклик -- фактор 
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y, pch = 19)

# обучающая выборка
train <- sample(200, 100)

# SVM с радиальным ядром и маленьким cost
svmfit <- 
plot(svmfit, dat[train, ])
summary(svmfit)

# SVM с радиальным ядром и большим cost
svmfit <- 
plot(svmfit, dat[train, ])

# перекрёстная проверка
set.seed(1)
tune.out <- 
summary(tune.out)
# матрица неточностей для прогноза по лучшей модели
table(true = dat[-train, "y"], 
      pred = predict(tune.out$best.model, newdata = dat[-train, ]))


# ROC-кривые -------------------------------------------------------------------

# функция построения ROC-кривой: pred -- прогноз, truth -- факт
rocplot <- function(pred, truth, ...){
    predob = prediction(pred, truth)
    perf = performance(predob, "tpr", "fpr")
    plot(perf,...)}

# последняя оптимальная модель
svmfit.opt <- 
# количественные модельные значения, на основе которых присваивается класс
fitted <- 

# график для обучающей выборки
par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "Training Data")
# более гибкая модель (gamma выше)
svmfit.flex <-  
fitted <- attributes(predict(svmfit.flex, dat[train, ], 
                             decision.values = T))$decision.values
# ROC-кривая


# график для тестовой выборки
fitted <- attributes(predict(svmfit.opt, dat[-train, ], 
                             decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train, ], 
                             decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")


# SVM с несколькими классами ---------------------------------------------------

# генерируем данные
set.seed(my.seed)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))

# график и модель по методу "один против одного"
par(mfrow = c(1, 1))
plot(x, col = (y + 1))
svmfit <- 
plot(svmfit, dat)


# Анализ данных по уровню экспрессии генов -------------------------------------

# данные по образцам тканей четырёх типов саркомы
names(Khan)
dim(Khan$xtrain)     # обучающая выборка, предикторы
dim(Khan$xtest)      # тестовая выборка, предикторы
length(Khan$ytrain)  # обучающая выборка, отклик
length(Khan$ytest)   # тестовая выборка, отклик
table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
# SVM с линейным ядром
out <- 
summary(out)
# матрица неточностей
table(out$fitted, dat$y)

# тестовые данные
dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
# прогноз на тестовой выборке
pred.te <- predict(out, newdata = dat.te)
# матрица неточностей
table(pred.te, dat.te$y)
