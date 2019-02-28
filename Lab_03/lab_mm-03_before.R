# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# ..............................................................................
# Математическое моделирование: Практика 3
#   Параметрические классификаторы для бинарной Y
#      * логистическая регрессия
#      * линейный дискриминантный анализ (LDA)
#      * квадатичный дискриминантный анализ (QDA)
#      * ROC-кривая
# ..............................................................................

library('ISLR')
library('GGally')
library('MASS')

my.seed <- 12345
train.percent <- 0.85

# Исходные данные: набор Default -----------------------------------------------
?Default
head(Default)
str(Default)

# графики разброса
ggally(Default)


# Отбираем наблюдения в обучающую выборку --------------------------------------

set.seed(my.seed)
inTrain <- sample(seq_along(Default$default),
                  nrow(Default) * train.percent)
df <- 

# фактические значения на обучающей выборке
Факт <- 


# Строим модели, чтобы спрогнозировать default ---------------------------------

# Логистическая регрессия ======================================================
model.logit <- 
    
    
summary(model.logit)

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.logit <- 
    
Прогноз <- 
    
    

# матрица неточностей
conf.m <- 
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])
# специфичность
conf.m[1, 1] / sum(conf.m[1, ])
# верность
sum(diag(conf.m)) / sum(conf.m)


# LDA ==========================================================================
model.lda <- 
    
model.lda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.lda <- predict(model.lda, df, 
                 type = 'response')
Прогноз <- factor(ifelse(p.lda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))

# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])
# специфичность
conf.m[1, 1] / sum(conf.m[1, ])
# верность
sum(diag(conf.m)) / sum(conf.m)


# QDA ==========================================================================
model.qda <- 
    
model.qda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.qda <- predict(model.qda, df, type = 'response')
Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))

# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])
# специфичность
conf.m[1, 1] / sum(conf.m[1, ])
# верность
sum(diag(conf.m)) / sum(conf.m)


# Подбор границы отсечения вероятностей классов --------------------------------

# ROC-кривая для LDA ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x <- NULL    # для (1 - SPC)
y <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.No', 'fact.Yes')
colnames(tbl) <- c('predict.No', 'predict.Yes')

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 501)){
    # прогноз
    Прогноз <- 
        
        
        
    
    # фрейм со сравнением факта и прогноза
    df.compare <- data.frame(Факт = Факт, Прогноз = Прогноз)
    
    # заполняем матрицу неточностей
    # TN
    tbl[1, 1] <- 
        
    # TP
    tbl[2, 2] <- 
        
    # FP
    tbl[1, 2] <- 
    
    # FN
    tbl[2, 1] <- 
        
    
    # считаем характеристики
    TPR <- tbl[2, 2] / sum(tbl[2, ])
    y <- c(y, TPR)
    SPC <- tbl[1, 1] / sum(tbl[1, ])
    x <- c(x, 1 - SPC)
}

# строим ROC-кривую
par(mar = c(5, 5, 1, 1))
# кривая
plot(x, y, 
     type = 'l', col = 'blue', lwd = 3,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
# прямая случайного классификатора
abline(a = 0, b = 1, lty = 3, lwd = 2)
# точка для вероятности 0.5
points(x[p.vector == 0.5], y[p.vector == 0.5], pch = 16)
text(x[p.vector == 0.5], y[p.vector == 0.5], 'p = 0.5', pos = 4)
# точка для вероятности 0.2
points(x[p.vector == 0.2], y[p.vector == 0.2], pch = 16)
text(x[p.vector == 0.2], y[p.vector == 0.2], 'p = 0.2', pos = 4)
