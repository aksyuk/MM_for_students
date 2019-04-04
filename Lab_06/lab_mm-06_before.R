# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# ..............................................................................
# Математическое моделирование: Практика 6
#   Регуляризация линейных моделей  
#   * как провести отбор оптимального подмножества переменных;     
#   * как отобрать предикторы методами пошагового включения и исключения;    
#   * как построить ридж- и лассо-регрессию;   
#   * как использовать снижение размерности: PCR и PLS;     
#   * как применять эти методы в сочетании с кросс-валидацией. 
# ..............................................................................

library('ISLR')              # набор данных Hitters
library('leaps')             # функция regsubset() -- отбор оптимального 
                             #  подмножества переменных
library('glmnet')            # функция glmnet() -- лассо
library('pls')               # регрессия на главные компоненты -- pcr()
                             #  и частный МНК -- plsr()

my.seed <- 1

# набор данных по зарплатам бейсбольных игроков
?Hitters

fix(Hitters)
names(Hitters)
dim(Hitters)

# считаем пропуски
sum(is.na(Hitters$Salary))

# убираем пропуски
Hitters <- na.omit(Hitters)

# проверяем результат
dim(Hitters)
sum(is.na(Hitters$Salary))


# 6.5 Лабораторная работа 1: методы отбора подмножеств переменных  -------------

# 6.5.1 Отбор оптимального подмножества ========================================

# подгоняем модели с сочетаниями предикторов до 8 включительно
regfit.full <- 
summary(regfit.full)

# подгоняем модели с сочетаниями предикторов до 19 (максимум в данных)
regfit.full <- 
reg.summary <- summary(regfit.full)
reg.summary

# структура отчёта по модели (ищем характеристики качества)
names(reg.summary)

# R^2 и скорректированный R^2

# на графике
plot(, 
     type = 'b', xlab = 'Количество предикторов', ylab = 'R-квадрат')
# сода же добавим скорректированный R-квадрат

# модель с максимальным скорректированным R-квадратом

### 11
points(which.max(reg.summary$adjr2), 
       reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col = 'red', cex = 2, pch = 20)
legend('bottomright', legend = c('R^2', 'R^2_adg'),
       col = c('black', 'red'), lty = c(1, NA),
       pch = c(1, 1))

# C_p

# число предикторов у оптимального значения критерия

### 10
# график
plot(reg.summary$cp, xlab = 'Число предикторов',
     ylab = 'C_p', type = 'b')
points(which.min(reg.summary$cp),
       reg.summary$cp[which.min(reg.summary$cp)], 
       col = 'red', cex = 2, pch = 20)
# BIC

# число предикторов у оптимального значения критерия

### 6
# график
plot(reg.summary$bic, xlab = 'Число предикторов',
     ylab = 'BIC', type = 'b')
points(which.min(reg.summary$bic),
       reg.summary$bic[which.min(reg.summary$bic)], 
       col = 'red', cex = 2, pch = 20)

# метод plot для визуализации результатов
?plot.regsubsets





# коэффициенты модели с наименьшим BIC



# 6.5.2 Отбор путём пошагового включения и исключения переменных ---------------

# Пошаговое включение ==========================================================

regfit.fwd <- 
    
summary(regfit.fwd)

# Пошаговое исключение =========================================================

regfit.bwd <- 
    
summary(regfit.bwd)

round(coef(regfit.full, 7), 3)

round(coef(regfit.fwd, 7), 3)

round(coef(regfit.bwd, 7), 3)


# 6.5.3 Методы проверочной выборки и перекрёстной проверки ---------------------

# Метод проверочной выборки ====================================================

set.seed(my.seed)
train <- sample(c(T, F), nrow(Hitters), rep = T)
test <- !train

# обучаем модели
regfit.best <- 
    
# матрица объясняющих переменных модели для тестовой выборки
test.mat <- 

# вектор ошибок
val.errors <- 
# цикл по количеству предикторов
for (i in 1:19){
    coefi <- 
    pred <- 
    # записываем значение MSE на тестовой выборке в вектор
    val.errors[i] <- 
}
round(val.errors, 0)
# находим число предикторов у оптимальной модели

### 10
# коэффициенты оптимальной модели


# функция для прогноза для функции regsubset()
predict.regsubsets <- function(object, newdata, id, ...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}

# набор с оптимальным количеством переменных на полном наборе данных
regfit.best <- 

    

# k-кратная кросс-валидация ====================================================

# отбираем 10 блоков наблюдений
k <- 
set.seed(my.seed)
folds <- 

# заготовка под матрицу с ошибками
cv.errors <- 

# заполняем матрицу в цикле по блокам данных
for (j in 1:k){
    best.fit <- 
        
    # теперь цикл по количеству объясняющих переменных
    for (i in 1:19){
        # модельные значения Salary
        pred <- 
        # вписываем ошибку в матрицу
        cv.errors[j, i] <- 
    }
}

# усредняем матрицу по каждому столбцу (т.е. по блокам наблюдений), 
#  чтобы получить оценку MSE для каждой модели с фиксированным 
#  количеством объясняющих переменных
mean.cv.errors <- 
round(mean.cv.errors, 0)

# на графике
plot(mean.cv.errors, type = 'b')
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)],
       col = 'red', pch = 20, cex = 2)

# перестраиваем модель с 11 объясняющими переменными на всём наборе данных
reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
round(coef(reg.best, 11), 3)


# Лабораторная работа 6.2: Гребневая регрессия и лассо -------------------------

# из-за синтаксиса glmnet() формируем явно матрицу объясняющих...
x <- model.matrix(Salary ~ ., Hitters)[, -1]

# и вектор значений зависимой переменной
y <- Hitters$Salary

# 6.6.1 Гребневая регрессия ====================================================

# вектор значений гиперпараметра лямбда
grid <- 

# подгоняем серию моделей ридж-регрессии
ridge.mod <- 

# размерность матрицы коэффициентов моделей


# значение лямбда под номером 50


# коэффициенты соответствующей модели


# норма эль-два


# всё то же для лямбды под номером 60
# значение лямбда под номером 50
round(ridge.mod$lambda[60], 0)

# коэффициенты соответствующей модели
round(coef(ridge.mod)[, 60], 3)

# норма эль-два
round(sqrt(sum(coef(ridge.mod)[-1, 60]^2)), 1)

# мы можем получить значения коэффициентов для новой лямбды
round(predict(ridge.mod, s = 50, type = 'coefficients')[1:20, ], 3)

# Метод проверочной выборки ####################################################

set.seed(my.seed)
train <- sample(1:nrow(x), nrow(x)/2)
test <- -train
y.test <- y[test]

# подгоняем ридж-модели с большей точностью (thresh ниже значения по умолчанию)
ridge.mod <- 
    
plot(ridge.mod)

# прогнозы для модели с лямбда = 4
ridge.pred <- 


# сравним с MSE для нулевой модели (прогноз = среднее)


# насколько модель с лямбда = 4 отличается от обычной ПЛР
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact = T,
                      x = x[train, ], y = y[train])
round(mean((ridge.pred - y.test)^2), 0)

# predict с лямбдой (s) = 0 даёт модель ПЛР
lm(y ~ x, subset = train)

round(predict(ridge.mod, s = 0, exact = T, type = 'coefficients',
              x = x[train, ], y = y[train])[1:20, ], 3)

# Подбор оптимального значения лямбда с помощью перекрёстной проверки ##########

# k-кратная кросс-валидация
set.seed(my.seed)
# оценка ошибки
cv.out <- 
plot(cv.out)
# значение лямбда, обеспечивающее минимальную ошибку перекрёстной проверки
bestlam <- 
round(bestlam, 0)

# MSE на тестовой для этого значения лямбды
ridge.pred <- 
round(mean((ridge.pred - y.test)^2), 0)

# наконец, подгоняем модель для оптимальной лямбды, 
#  найденной по перекрёстной проверке
out <- glmnet(x, y, alpha = 0)
round(predict(out, type = 'coefficients', s = bestlam)[1:20, ], 3)

# 6.6.2 Лассо ==================================================================

lasso.mod <- 
plot(lasso.mod)

# Подбор оптимального значения лямбда с помощью перекрёстной проверки ##########

set.seed(my.seed)
cv.out <- 
plot(cv.out)
bestlam <- 
lasso.pred <- 
round(mean((lasso.pred - y.test)^2), 0)

# коэффициенты лучшей модели
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = 'coefficients',
                      s = bestlam)[1:20, ]
round(lasso.coef, 3)

round(lasso.coef[lasso.coef != 0], 3)

# Лабораторная работа 3: регрессия при помощи методов PCR и PLS ----------------

# 6.7.1 Регрессия на главные компоненты ========================================

# кросс-валидация 
set.seed(2)   # непонятно почему они сменили зерно; похоже, опечатка
pcr.fit <- 
summary(pcr.fit)

# график ошибок


# Подбор оптиального M: кросс-валидация на обучающей выборке  ##################

set.seed(my.seed)
pcr.fit <- 
    
validationplot(pcr.fit, val.type = 'MSEP')

# MSE на тестовой выборке
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 7)
round(mean((pcr.pred - y.test)^2), 0)

# подгоняем модель на всей выборке для M = 7 
#  (оптимально по методу перекрёстной проверки)
pcr.fit <- pcr(y ~ x, scale = T, ncomp = 7)
summary(pcr.fit)

# 6.7.2 Регрессия по методу частных наименьших квадратов -----------------------

set.seed(my.seed)
pls.fit <- 
    
summary(pls.fit)

# теперь подгоняем модель для найденного оптимального M = 2 
#  и оцениваем MSE на тестовой
pls.pred <- predict(pls.fit, x[test, ], ncomp = 2)
round(mean((pls.pred - y.test)^2), 0)

# подгоняем модель на всей выборке
pls.fit <- 
summary(pls.fit)
