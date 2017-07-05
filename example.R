

# install.packages('AER')
data(Affairs, package = 'AER')

summary((Affairs))
table(Affairs$affairs)

# 转化为二值型因子
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0

Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels = c(0,1),
                           labels = c('No','Yes'))

table(Affairs$ynaffair)


# Logistics 回归
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation + rating,
                data = Affairs, family = binomial())

summary((fit.full))


