# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Математическое моделирование: Практика 7
#  Нелинейные модели

library('ISLR')              # набор данных Auto
library('splines')           # сплайны
library('gam')               # обобщённые аддитивные модели
library('akima')             # график двумерной плоскости

my.seed <- 1


# Данные по зарплатам ----------------------------------------------------------

attach(Wage)


# Полиномиальная регрессия -----------------------------------------------------

# Зависимость зарплаты от возраста =============================================

# полином четвёртой степени для зависимости зарплаты от возраста
fit <- 
coef(summary(fit))

# функция poly(age, 4) создаёт таблицу с базисом ортогональных полиномов: 
#  линейные комбинации значений переменной age в степенях от 1 до 4


# можно получить сами значения age в заданных степенях
head(poly(age, 4, raw = T))

# на прогноз не повлияет, но оценки параметров изменяются
fit.2 <- 
coef(summary(fit.2))

# границы изменения переменной age
agelims <- range(age)
# значения age, для которых делаем прогноз (от min до max с шагом 1)
age.grid <- 
# рассчитать прогнозы и их стандартные ошибки
preds <- 
# границы доверительного интервала
se.bands <- cbind(preds$fit + 2*preds$se.fit,
                  preds$fit - 2*preds$se.fit)

# левая панель графика со слайда 4 презентации (рис. 7.1 книги) ################
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
# наблюдения
plot(, , xlim = agelims, cex = 0.5, col = 'darkgrey')
# заголовок
title('Полином четвёртой степени', outer = T)
# модель
lines(, , lwd = 2, col = 'blue')
# доверительные интервалы прогноза
matlines(, , lwd = 1, col = 'blue', lty = 3)

# прогнозы по моделям с различными вызовами poly() совпадают
preds2 <- predict(fit.2, newdata = list(age = age.grid), se = T)
max(abs(preds$fit - preds2$fit))

# подбираем степень полинома, сравнивая модели со степенями от 1 до 5 
#  с помощью дисперсионного анализа (ANOVA)
fit.1 <- 
fit.2 <- 
fit.3 <- 
fit.4 <- 
fit.5 <- 

anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Зависимость вероятности величины зарплаты > 250 от возраста ==================

# подгоняем логистическую регрессию
fit <- 
# прогнозы
preds <- 
# пересчитываем доверительные интервалы и прогнозы в исходные ЕИ
pfit <- 
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit,
                        preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))

# правая панель графика с 4 слайда презентации (рис. 7.1 книги) ################
# сетка для графика (изображаем вероятности, поэтому интервал изменения y мал)
plot(, , xlim = agelims, type = 'n', ylim = c(0, 0.2))
# фактические наблюдения показываем засечками
points, , cex = 0.5, pch = '|', col = 'darkgrey')
# модель
lines(, , lwd = 2, col = 'blue')
# доверительные интервалы
matlines(, , lwd = 1, col = 'blue', lty = 3)


# Ступенчатые функции ----------------------------------------------------------

# нарезаем предиктор age на 4 равных интервала
table(cut(age, 4))

# подгоняем линейную модель на интервалах
fit <- 
coef(summary(fit))

# прогноз -- это средние по `wage` на каждом интервале
preds.cut <- predict(fit, newdata = list(age = age.grid), se = T)

# интервальный прогноз
se.bands.cut <- cbind(lower.bound = preds.cut$fit - 2*preds.cut$se.fit,
                      upper.bound = preds.cut$fit + 2*preds.cut$se.fit)


# график со слайда 7 (рис. 7.2) ################################################

# наблюдения
plot(, , xlim = agelims, cex = 0.5, col = 'darkgrey')

# модель
lines(, , lwd = 2, col = 'darkgreen')

# доверительные интервалы прогноза
matlines(, , lwd = 1, col = 'darkgreen', lty = 3)

# заголовок
title('Ступенчатая функция')

# правая часть графика, для вероятности того, что зарплата выше 250
fit <- 

# прогнозы
preds <- predict(fit, newdata = list(age = age.grid), se = T)

# пересчитываем доверительные интервалы и прогнозы в исходные ЕИ
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(lower.bound = preds$fit - 2*preds$se.fit,
                        upper.bound = preds$fit + 2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))

# результат - доверительный интервал для вероятности события 
#   "Заработная плата выше 250".   
round(head(se.bands), 3)

# сетка для графика (изображаем вероятности, поэтому интервал изменения y мал)
plot(age, I(wage > 250), xlim = agelims, type = 'n', ylim = c(0, 0.2),
     ylab = 'P(Wage > 250 | Age)')

# фактические наблюдения показываем засечками
points(jitter(age), I((wage > 250) / 5), cex = 0.5, pch = '|', col = 'darkgrey')

# модель
lines(age.grid, pfit, lwd = 2, col = 'darkgreen')

# доверительные интервалы
matlines(age.grid, se.bands, lwd = 1, col = 'darkgreen', lty = 3)

# заголовок
title('Ступенчатая функция')


# Сплайны ----------------------------------------------------------------------

# кубический сплайн с тремя узлами
fit <- 
# прогноз
preds <- predict(fit, newdata = list(age = age.grid), se = T)

# график сравнения кубического и натурального сплайнов #########################
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1, 8.5), 
    oma = c(0, 0, 0, 0), xpd = T)
# наблюдения
plot(age, wage, col = 'grey')
# модель
lines(age.grid, preds$fit, lwd = 2)
# доверительный интервал
lines(age.grid, preds$fit + 2*preds$se, lty = 'dashed')
lines(age.grid, preds$fit - 2*preds$se, lty = 'dashed')

# три узла -- это 6 степеней свободы


# если задать только степени свободы, функция bs() распределяет узлы равномерно
#  в данном случае узлы -- квартили распределения age
attr(bs(age, df = 6), 'knots')

# натуральный сплайн
fit2 <- 
preds2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, preds2$fit, col = 'red', lwd = 2)
legend("topright", inset = c(-1.15, 0),
       c('Кубический \n с 3 узлами', 'Натуральный'),
       lwd = rep(2, 2), col = c('black', 'red'))

# график со слайда 20 (рис. 7.8) ###############################################

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
# наблюдения
plot(age, wage, xlim = agelims, cex = 0.5, col = 'darkgrey')
# заголовок
title('Сглаживающий сплайн')
# подгоняем модель с 16 степенями свободы
fit <- 
# подгоняем модель с подбором лямбды с помощью перекрёстной проверки
fit2 <- 
fit2$df
# рисуем модель
lines(, col = 'red', lwd = 2)
lines(, col = 'blue', lwd = 2)
legend('topright', 
       c('16 df', '6.8 df'),
       col = c('red', 'blue'), lty = 1, lwd = 2, cex = 0.8)


# Локальная регрессия ----------------------------------------------------------

# график со слайда 24 (рис. 7.10) ##############################################
plot(age, wage, xlim = agelims, cex = 0.5, col = 'darkgrey')
title('Локальная регрессия')
# подгоняем модель c окном 0.2
fit <- 
# подгоняем модель c окном 0.5
fit2 <- 
# рисум модели
lines(age.grid, predict(fit, data.frame(age = age.grid)),
      col = 'red', lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
      col = 'blue', lwd = 2)
legend('topright', 
       c('s = 0.2', 's = 0.5'),
       col = c('red', 'blue'), lty = 1, lwd = 2, cex = 0.8)


# Обобщённые аддитивные модели (GAM) с непрерывным откликом --------------------

# GAM на натуральных сплайнах степеней 4 (year), 5 (age); 
#  предиктор edication -- категориальный
gam.ns <- 

# GAM на сглаживающих сплайнах
gam.m3 <- 

# график со слайда 28 (рис. 7.12) ##############################################
par(mfrow = c(1, 3))
plot(gam.m3, se = T, col = 'blue')

# график со слайда 27 (рис. 7.11) ##############################################
plot(gam.ns, se = T, col = 'red')

# график функции от year похож на прямую
#  делаем ANOVA, чтобы понять, какая степень для year лучше
gam.m1 <- 
gam.m2 <- 

anova(gam.m1, gam.m2, gam.m3, test = 'F')

# сводка по модели gam.m3
summary(gam.m3)

# прогноз по обучающей выборке
preds <- 

# GAM на локальных регрессиях
gam.lo <- 
  

par(mfrow = c(1, 3))
plot(gam.lo, se = T, col = 'green')

# модель со взаимодействием регрессоров year и age
gam.lo.i <- 
plot(gam.lo.i)


# Логистическая GAM ------------------------------------------------------------

gam.lr <- 
  
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = 'green')

# уровни образования по группам разного достатка
table(education, I(wage > 250))

# в категории с самым низким уровнем образования нет wage > 250, 
#  поэтому убираем её
gam.lr.s <- 
  
  
plot(gam.lr.s, se = T, col = 'green')


