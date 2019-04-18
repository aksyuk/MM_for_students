# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Математическое моделирование: Практика 8
#  Деревья решений

# Загрузка пакетов
library('tree')              # деревья tree()
library('ISLR')              # набор данных Carseats
library('GGally')            # матричный график разброса ggpairs()
library('MASS')              # набор данных Boston
library('randomForest')      # случайный лес randomForest()
library('gbm')               # бустинг gbm()

my.seed <- 2


# Данные по продажам автокресел ------------------------------------------------

# ?Carseats
head(Carseats)

# новая переменная
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")

# присоединяем к таблице данных
Carseats <- cbind(Carseats, High)
head(Carseats)

# матричный график разброса переменных
# ggpairs(Carseats[, c(12, 1:4)], aes(color = High))
# ggpairs(Carseats[, c(12, 5:8)], aes(color = High))
# ggpairs(Carseats[, c(12, 9:11)], aes(color = High))


# Деревья классификации --------------------------------------------------------


# Выращиваем ===================================================================

# модель бинарного  дерева
tree.carseats <- 
summary(tree.carseats)

# график результата
plot(tree.carseats)              # ветви
text(tree.carseats, pretty = 0)  # подписи
tree.carseats                    # посмотреть всё дерево в консоли


# Оцениваем точность ===========================================================

# ядро генератора случайных чисел
set.seed(my.seed)

# обучающая выборка
train <- 

# тестовая выборка
Carseats.test <- 
High.test <- 

# строим дерево на обучающей выборке
tree.carseats <- 


# Оцениваем точность ###########################################################

# делаем прогноз
tree.pred <- 

# матрица неточностей
tbl <- 
tbl

# ACC на тестовой
acc.test <- 
names(acc.test)[length(acc.test)] <- 'Carseats.class.tree.all'
acc.test


# Готовимся к обрезке дерева ===================================================

set.seed(my.seed)
cv.carseats <- 
# имена элементов полученного объекта
names(cv.carseats)
# сам объект
cv.carseats

# графики изменения параметров метода по ходу обрезки дерева ###################

# 1. ошибка с кросс-валидацией в зависимости от числа узлов
par(mfrow = c(1, 2))
plot(, , type = "b",
     ylab = 'Частота ошибок с кросс-вал. (dev)',
     xlab = 'Число узлов (size)')
# размер дерева с минимальной ошибкой
opt.size <- 
abline(, col = 'red', 'lwd' = 2)     # соотв. вертикальная прямая
mtext(opt.size, at = opt.size, side = 1, col = 'red', line = 1)

# 2. ошибка с кросс-валидацией в зависимости от штрафа на сложность
plot(, , type = "b",
     ylab = 'Частота ошибок с кросс-вал. (dev)',
     xlab = 'Штраф за сложность (k)')


# Обрезаем: дерево с 9 узлами ##################################################
prune.carseats <- 

# визуализация
plot(prune.carseats)
text(prune.carseats, pretty = 0)


# Оцениваем точность ###########################################################

# прогноз на тестовую выборку
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")

# матрица неточностей
tbl <- table(tree.pred, High.test)
tbl

# ACC на тестовой
acc.test <- c(acc.test, sum(diag(tbl))/sum(tbl))
names(acc.test)[length(acc.test)] <- 'Carseats.class.tree.9'
acc.test

# Обрезаем: дерево с 15 узлами #################################################
prune.carseats <- 

# визуализация
plot(prune.carseats)
text(prune.carseats, pretty = 0)


# Оцениваем точность ###########################################################

# прогноз на тестовую выборку
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")

# матрица неточностей
tbl <- table(tree.pred, High.test)
tbl

# ACC на тестовой
acc.test <- c(acc.test, sum(diag(tbl))/sum(tbl))
names(acc.test)[length(acc.test)] <- 'Carseats.class.tree.15'
acc.test

# сбрасываем графические параметры
par(mfrow = c(1, 1))


# Данные по ценам на жильё в Бостоне -------------------------------------------
# ?Boston
head(Boston)

# матричные графики
# ggpairs(Boston[, c(14, 1:4)])
# ggpairs(Boston[, c(14, 5:8)])
# ggpairs(Boston[, c(14, 9:13)])

# обучающая выборка
set.seed(my.seed + 1)
train <- sample(1:nrow(Boston), nrow(Boston)/2) # обучающая выборка -- 50%


# Регрессионные деревья --------------------------------------------------------


# Обучаем модель ###############################################################

tree.boston <- 
summary(tree.boston)

# визуализация
plot(tree.boston)
text(tree.boston, pretty = 0)

# обрезка дерева
cv.boston <- 

# размер дерева с минимальной ошибкой
plot(cv.boston$size, cv.boston$dev, type = 'b')
opt.size <- cv.boston$size[cv.boston$dev == min(cv.boston$dev)]
abline(v = opt.size, col = 'red', 'lwd' = 2)     # соотв. вертикальная прямая
mtext(opt.size, at = opt.size, side = 1, col = 'red', line = 1)

# дерево с 7 конечными узлами
prune.boston <-  

# визуализация
plot(prune.boston)
text(prune.boston, pretty = 0)


# Оцениваем точность ###########################################################

# прогноз по лучшей модели (8 конечных узлов)
yhat <- 
boston.test <- 

# график "прогноз -- реализация"
plot(, )
# линия идеального прогноза
abline(, )

# MSE на тестовой выборке
mse.test <- 
names(mse.test)[length(mse.test)] <- 'Boston.regr.tree.8'
mse.test


# Обучаем модель ###############################################################

# дерево с 5 узлами
prune.boston <- 


# Оцениваем точность ###########################################################

# прогноз по модели (5 узлов)
yhat <- predict(prune.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
# MSE на тестовой выборке
mse.test <- c(mse.test, mean((yhat - boston.test)^2))
names(mse.test)[length(mse.test)] <- 'Boston.regr.tree.5'
mse.test


# Бэггинг ----------------------------------------------------------------------


# Обучаем модель ###############################################################

# бэггинг с 13 предикторами
set.seed(my.seed)
bag.boston <- 
bag.boston


# Оцениваем точность ###########################################################

# прогноз
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

# график "прогноз -- реализация"
plot(yhat.bag, boston.test)
# линия идеального прогноза
abline(0, 1)

# MSE на тестовой
mse.test <- c(mse.test, mean((yhat.bag - boston.test)^2))
names(mse.test)[length(mse.test)] <- 'Boston.bag.13'
mse.test


# Обучаем модель ###############################################################

# бэггинг с 13 предикторами и 25 деревьями
bag.boston <- 


# Оцениваем точность ###########################################################

# прогноз
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

# MSE на тестовой
mse.test <- c(mse.test, mean((yhat.bag - boston.test)^2))
names(mse.test)[length(mse.test)] <- 'Boston.bag.13.25'
mse.test


# Случайный лес ----------------------------------------------------------------


# Обучаем модель ###############################################################
set.seed(my.seed)
rf.boston <- 


# Оцениваем точность ###########################################################

# прогноз
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])

# MSE на тестовой выборке
mse.test <- c(mse.test, mean((yhat.rf - boston.test)^2))
names(mse.test)[length(mse.test)] <- 'Boston.rf.6'
mse.test

# важность предикторов
importance(rf.boston)  # оценки 
varImpPlot(rf.boston)  # графики


# Бустинг ----------------------------------------------------------------------


# Обучаем модель ###############################################################

set.seed(my.seed)
boost.boston <- 
# график и таблица относительной важности переменных
summary(boost.boston)

# графики частной зависимости для двух наиболее важных предикторов
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")


# Оцениваем точность ###########################################################

# прогноз
yhat.boost <- 

# MSE на тестовой
mse.test <- c(mse.test, mean((yhat.boost - boston.test)^2))
names(mse.test)[length(mse.test)] <- 'Boston.boost.opt'
mse.test


# Обучаем модель ###############################################################

# меняем значение гиперпараметра (lambda) на 0.2 -- аргумент shrinkage
boost.boston <- 


# Оцениваем точность ###########################################################

# прогноз
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)

# MSE а тестовой
mse.test <- c(mse.test, mean((yhat.boost - boston.test)^2))
names(mse.test)[length(mse.test)] <- 'Boston.boost.0.2'
mse.test
