# author： 许欢
# date： 20170615

# 自定义函数 -------------------------------------------------------------------
# normalizePath(readClipboard(), "/", mustWork = FALSE)
source(file="D:/XH/R_Project/Questionnaire/file/My_Scripts/my_scripts.R", 
       chdir = TRUE, encoding = 'UTF-8')

# 文件位置、工作目录 --------------------------------------------------------------------
data_folder <- 'D:/XH/R_Project/Questionnaire/data/guyuan_data'
result_folder <- 'D:/XH/R_Project/Questionnaire/result'
file_folder <- "D:/XH/R_Project/Questionnaire/file"
# getwd()
setwd(file_folder)

# 数据导入 --------------------------------------------------------------------
# 初中学生：SS
file_name <- paste(data_folder, '/guyuan 3 clean_data no_missing 初中.csv', sep = '')
SS_data <- read.csv(file_name, header = TRUE, encoding = "UTF-8",sep = ',')
print(class(SS_data)) # 数据类型 "data.frame"

# 基本分析及绘图 -------------------------------------------------------------------
## 数据准备 --------------------------------------------------------------------
print(names(SS_data)[1])
names(SS_data)[1] <- 'X1'  # 修改第一个变量的名称
### 建立新变量名，放入合并后数据
# str(datas) # head()/tail()
dim(SS_data) # 行列 41915   210  
SS_data_change <- data.frame('性别' = numeric(dim(SS_data)[1]))
# Make variables factors into factors
SS_data_change$'性别' <- factor(SS_data$'X1', labels = c('男','女'))
SS_data_change$'住址' <- factor(SS_data$'X3', labels = c('城市','农村'))


SS_data_change$'成绩排名' <- factor(SS_data$'X5', 
                                labels = c('第五','第四','第三','第二','第一'),
                                levels = c(5,4,3,2,1), ordered=TRUE)

# factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
#                  'Title','Surname','Family','FsizeD')
# SS_data_change[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

### 青少年创造性倾向问卷
creative_names <- list(names <- c('自信心','好奇心','探索性',
                                  '挑战性','意志力','创造性倾向'),
                       items <- list(list(45:47),list(48:51),list(52:55),
                                     list(56:58),list(59:62),list(45:62)))
for (i in 1:length(creative_names[[1]])){
  SS_data_change[creative_names[[1]][i]] <- apply(SS_data[,paste('X', 
                  unlist(creative_names[[2]][i]), sep = '')], 1, sum)
  }

### 自尊量表
SS_data_change['自尊'] <- apply(SS_data[,paste('X', 63:72, sep = '')], 1, sum)

## 分析与绘图 -------------------------------------------------------------------
### 类别
#### 单变量
table(SS_data_change$'性别') # 分别统计频数
barplot(table(SS_data_change$'自信心'))
rank <- table(SS_data_change$'成绩排名')
names(rank) <- c('第一','第二','第三','第四','第五')
pie(rank) # 饼图
hist(SS_data_change$'创造性倾向') # 直方图
hist(SS_data_change$'创造性倾向', prob = T) # 频率直方图
lines(density(SS_data_change$'创造性倾向'), col = 'red') # 密度曲线
# rug(SS_data_change$'创造性倾向') # 把各个数据竖线描绘在X轴上
boxplot(SS_data_change$'创造性倾向',horizontal = T)

#### 双变量
table(SS_data_change$'性别', SS_data_change$'住址')
# 边缘概率，margin = 1（行），margin（列），省略（总和）
# prop.table(SS_data_change$'性别', SS_data_change$'住址', margin = 1)
barplot(table(SS_data_change$'性别', SS_data_change$'住址'), legend =T) # 堆积条形图
barplot(table(SS_data_change$'性别', SS_data_change$'住址'), legend =T, beside = T)
boxplot(SS_data_change[creative_names[[1]]])
plot(SS_data_change$'自尊', SS_data_change$'创造性倾向')
abline(lm(SS_data_change$'创造性倾向' ~ SS_data_change$'自尊'))
cor(SS_data_change$'自尊', SS_data_change$'创造性倾向')

### 多变量
table(SS_data_change$'性别', SS_data_change$'住址', SS_data_change$'成绩排名')
# 点带图：比较各变量的分布情况，stripchart(z~t)，z 在 t 上的分布
stripchart(SS_data_change$'创造性倾向' ~ SS_data_change$'成绩排名')
pairs(SS_data_change[creative_names[[1]]]) # 矩阵式散点图

### 自尊
EDA <- function(x){ 
  dev.new()
  par(mfrow = c(2,2)) # 2*2 个图
  hist(x) # 直方图
  dotchart(x) # 点图
  boxplot(x) # 箱式图
  qqnorm(x);qqline(x) # 正态概率图
  par(mfrow = c(1,1))} # 恢复成单图

EDA(SS_data_change$'自尊')


# 参数检验 --------------------------------------------------------------------
## Z 检验：总体正态，总体均值、方差已知 ----
z.test <- function(data,mu,thegma, alternative = 'twoside'){ # 数据、均值、标准差、单双侧
  SE = thegma/sqrt(length(data))
  Z = (mean(data) - mu)/SE
  if (alternative == 'twoside') p = Z*(1-pnorm(abs(Z)))
  else if (alternative == 'less') p = pnorm(Z)
  else p = 1- pnorm(Z)
  return(list(Z=Z,p=p))}

z.test(SS_data_change$'创造性倾向', 52,15)

## 方差齐性检验 ----
### 原假设都为“变量的总体方差全部相同”；备择假设则为“至少有两个变量的总体方差时不同的”
### 单一变量 barrlett.test(count ~ spray, data = InsectSprays)
plot(创造性倾向 ~ 性别, data = SS_data_change)
bartlett.test(创造性倾向 ~ 性别, data = SS_data_change)
var.test(创造性倾向 ~ 性别, data = SS_data_change)

### 多变量
plot(创造性倾向 ~ interaction(性别, 成绩排名), data = SS_data_change)
bartlett.test(创造性倾向 ~ interaction(性别, 成绩排名), data = SS_data_change)

## 正态性检验 ----
normal_test(SS_data_change$创造性倾向) # 正态性检验

library(car)
dev.new()
# qqPlot()要求用lm()拟合
# 数据落在95%的置信区间范围内，说明满足正态性假设。
qqPlot(lm(SS_data_change$创造性倾向 ~ SS_data_change$成绩排名),
       simulate = T, main = 'Q-Q Plot')

## T 检验:总体正态、方差未知 ----
### 单样本
t.test(SS_data_change$创造性倾向)

### 配对样本
t.test(SS_data_change$创造性倾向 ~ SS_data_change$性别, 
       paired = FALSE, var.equal = FALSE,conf.level = 0.95)

### 独立样本
t.test(SS_data_change$创造性倾向 ~ SS_data_change$性别, 
       paired = FALSE, var.equal = FALSE,conf.level = 0.95)

## 方差检验 ----
### 单因素
oneway.test(SS_data_change$创造性倾向 ~ SS_data_change$性别, var.equal = FALSE)

### 两因素：无交互作用
par(mfrow = c(1,2))
boxplot(SS_data_change$创造性倾向 ~ SS_data_change$住址, xlab = '住址')
boxplot(SS_data_change$创造性倾向 ~ SS_data_change$成绩排名, xlab = '成绩排名')
par(mfrow = c(1,1))

(aov.result <- aov(SS_data_change$创造性倾向 ~ SS_data_change$住址 
                   + SS_data_change$成绩排名))
summary(aov.result) # 方差分析表

### 两因素：有交互作用
table(SS_data_change$住址,SS_data_change$成绩排名) # 各条件样本量
aggregate(SS_data_change$创造性倾向, by = list(SS_data_change$住址,  # 均值
                                          SS_data_change$成绩排名), FUN = mean)
aggregate(SS_data_change$创造性倾向, by = list(SS_data_change$住址,  # 标准差
                                          SS_data_change$成绩排名), FUN = sd)

aov.result <- aov(SS_data_change$创造性倾向 ~ SS_data_change$住址 
                  + SS_data_change$成绩排名 + SS_data_change$住址*SS_data_change$成绩排名)
summary(aov.result)
interaction.plot(SS_data_change$成绩排名, SS_data_change$住址, 
                 SS_data_change$创造性倾向, type = 'b', col = c('red','blue'),
                 pch = c(16,18), main = 'interaction effect') # 交互效应图

### 事后检验 ----
# 1. 如果主效应和交互作用均不显著，无需事后检验
# 2. 如果只有主效应显著，检验主效应
# 3. 如果交互效应显著，即便主效应显著，也只需检验交互效应
if(!require(car)){install.packages("car")}
if(!require(psych)){install.packages("psych")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(phia)){install.packages("phia")}

#### 只有主效应显著 ----
model = lm(创造性倾向 ~ 成绩排名 + 性别 + 成绩排名:性别,
                data = SS_data_change)

interaction.plot(x.factor = SS_data_change$成绩排名,
                 trace.factor = SS_data_change$性别,
                 response = SS_data_change$创造性倾向,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"), ### Colors for levels of trace var.
                 pch=c(19, 17, 15), ### Symbols for levels of trace var.
                 fixed=TRUE, ### Order by factor order in data
                 leg.bty = "o")

library(car)
Anova(model,type = "II")

# 事后检验
library(lsmeans)
lsmeans(model,pairwise ~ 成绩排名,
        adjust="tukey") ### Tukey-adjusted comparisons
lsmeans(model,pairwise ~ 性别,
        adjust="tukey") ### Tukey-adjusted comparisons

Sum = Summarize(创造性倾向 ~ 成绩排名 + 性别 + 成绩排名:性别,
                     data = SS_data_change,digits=3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)
### Order levels of the factor; otherwise R will alphabetize them
Sum$成绩排名 = factor(Sum$成绩排名,levels=unique(Sum$成绩排名))
Sum$性别 = factor(Sum$性别,levels=unique(Sum$性别))

library(ggplot2)
pd = position_dodge(.2)
ggplot(Sum, aes(x = 成绩排名,y = mean,color = 性别)) +
  geom_errorbar(aes(ymin = mean - se,ymax = mean + se),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  scale_colour_manual(values= c("black", "red")) +
  ylab("Mean weight change")
        
#### 交互作用显著 ----
model = lm(创造性倾向 ~ 成绩排名 + 住址 + 成绩排名:住址,
                data = SS_data_change)

interaction.plot(x.factor = SS_data_change$成绩排名,
                 trace.factor = SS_data_change$住址,
                 response = SS_data_change$创造性倾向,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"), ### Colors for levels of trace var.
                 pch=c(19, 17, 15), ### Symbols for levels of trace var.
                 fixed=TRUE, ### Order by factor order in data
                 leg.bty = "o")

library(car)
Anova(model,type = "II")

# 事后检验
library(lsmeans)
leastsquare <- lsmeans(model,pairwise ~ 成绩排名:住址,
        adjust="tukey") ### Tukey-adjusted comparisons
leastsquare$contrasts
CLD <- cld(leastsquare,alpha=0.05,
           Letters=letters, ### Use lower-case letters for .group
           adjust="tukey") ### Tukey-adjusted comparisons

# Interaction plot with error bars using ggplot2
CLD$成绩排名 = factor(Sum$成绩排名,levels=unique(Sum$成绩排名))
CLD$性别 = factor(Sum$住址,levels=unique(Sum$住址))
CLD$.group=gsub(" ", "", CLD$.group) # Remove spaces in .group

library(ggplot2)
pd = position_dodge(0.4) ### How much to jitter the points on the plot
ggplot(CLD,aes(x = 成绩排名,y = lsmean,color = 住址,label = .group)) +
  geom_point(shape = 15,size = 4,position = pd) +
  geom_errorbar(aes(ymin = lower.CL,ymax = upper.CL),
                width = 0.2,size = 0.7,position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Mean") +
  ggtitle ("交互效应显著\n之事后检验",
           subtitle = "成绩排名:住址") +
  labs(caption = paste0("\nMidichlorian counts for two distincts ",
                        "across five classes. Boxes indicate \n",
                        "the LS mean. ",
                        "Error bars indicate the 95% confidence ",
                        "interval ",
                        "of the LS \n",
                        "mean. Means sharing a letter are ",
                        "not significantly different \n",
                        "(Tukey-adjusted comparisons)."),hjust=0.5) +
  geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
            nudge_y = c(4.5, 4.5, 4.5, 4.5, 4.5 , 4.5, 4.5, 4.5),
            color = "black") +
  scale_color_manual(values = c("blue", "red"))

#### 多重比较 ----
# detach("package:HH") # TukeyHSD()函数与HH包存在兼容性问题
TukeyHSD(aov.result)
dev.new()
par(las = 2) # 坐标轴刻度数字标记方向
par(mar = c(3,10,4,2))
plot(TukeyHSD(aov.result)) # 图形中置信区间包含0的疗法说明差异不显著（p>0.05）

library(multcomp) # 既适用于线性模型，也适用于广义线性模型

#### 交互效应 ----
if(!require(gplots)){install.packages("gplots")}
library(gplots) 
# 展示交互效果:均值、误差棒（95%的置信区间）和样本大小
dev.new()
plotmeans(SS_data_change$创造性倾向 ~ interaction(SS_data_change$住址,
                                             SS_data_change$成绩排名, sep = ''), 
          connet = list(c(1,3,5,7,9),c(2,4,6,8,10)),
          col = c('red','darkgreen'),
          main = 'Interaction Plot with 95% CIs')

library(HH)
# 能展示任意复杂度设计（双因素方差分析、三因素方差分析等）的主效应（箱线图）和交互效应
dev.new()
interaction2wt(SS_data_change$创造性倾向 ~ SS_data_change$住址*SS_data_change$成绩排名)

dev.new()
par(mfrow = c(1,2))
interaction.plot(SS_data_change$住址,SS_data_change$成绩排名,
                 SS_data_change$创造性倾向,legend=T)
interaction.plot(SS_data_change$成绩排名,SS_data_change$住址,
                 SS_data_change$创造性倾向,legend=T)
par(mfrow = c(1,1))

### 重复测量方差分析 ----
# 通常处理的数据集是宽格式（wide format） ，即列是变量，行是观测值，而且一行一个
# 受试对象。在处理重复测量设计时，需要有长格式（long format）数据才能拟合模型。
# 在长格式中，因变量的每次测量都要放到它独有的行中， CO2数据集即该种形式。

# 因变量是二氧化碳吸收量（uptake）
# 二氧化碳浓度（conc）, Type是组间因子， conc是组内因子
wlbl <- subset(CO2, Treatment == 'chilled')
head(CO2)
head(wlbl)
fit <- aov(uptake ~ conc*Type + Error(Plant/conc), wlbl)
summary(fit)

par(las = 2)
par(mar = c(10, 4, 4, 2))
with(wlbl, interaction.plot(conc, Type, uptake, type = "b", 
    col = c("red", "blue"), pch = c(16, 18), 
    main = "Interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type * conc, data = wlbl, 
        col = (c("gold",  "green")), 
        main = "Chilled Quebec and Mississippi Plants", 
        ylab = "Carbon dioxide uptake rate (umol/m^2 sec)")

### 协方差分析 ----
#### 单因素
# 包含一个或多个定量的协变量

#### 多因素



### 多元方差分析 ----
#### 单因素
library(MASS)
attach(UScereal)
head(UScereal)

y <- cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN = mean)
cov(y) # 输出各谷物间的方差和协方差
fit <- manova(y ~ shelf) # 对组间差异进行多元检验
summary(fit) # 结果显著
summary.aov(fit) # 对每一个变量做单因素方差分析

#### 多因素



# 非参数检验 -------------------------------------------------------------------


# 相关分析 --------------------------------------------------------------------


# 回归分析 --------------------------------------------------------------------


# 因子分析 --------------------------------------------------------------------
# install.packages('psych')
library('psych')
fa.parallel(USJudgeRatings[,-1], fa ='pc', n.iter = 100,
            show.legend = FALSE, main = 'Scree plot with parallel analysis')

fa.parallel(Harman23.cor$cov, n.obs = 302, fa='pc', n.iter = 100)


# 主成分分析（PCA） -------------------------------------------------------------------

PC <- principal(Harman23.cor$cov, nfactors = 2, rotate = 'none')

PC <- principal(Harman23.cor$cov, nfactors = 2, rotate = 'varimax') # 方差极大旋转

# 提取成分得分
PC1 <- principal(USJudgeRatings[,-1], nfactors = 1, scores = TRUE)
head(PC1$scores)


# 探索性因子分析（EFA） ------------------------------------------------------------
PC1 <- principal(USJudgeRatings[,-1], nfactors = 1, scores = TRUE)
head(PC1$scores)
options(digits = 2)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)

fa.parallel(correlations, n.obs = 112, fa = "both", n.iter = 100,
            main = 'Scree plots with parallel analysis')


# 数据挖掘或机器学习 ---------------------------------------------------------------










