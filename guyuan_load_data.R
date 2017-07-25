# author： 许欢
# date： 20170615


# 导入包、库、自定义函数 -------------------------------------------------------------------
library(car)
library(MASS)

# 自定义函数
# normalizePath(readClipboard(), "/", mustWork = FALSE)
source(file="D:/XH/R_Project/Questionnaire/file/My_Scripts/my_scripts.R", chdir = TRUE)

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
## 因子分加总
print(names(SS_data)[1])
names(SS_data)[1] <- 'X1'  # 修改第一个变量的名称
### 建立新变量名，放入合并后数据
dim(SS_data) # 行列 41915   210  
SS_data_change <- data.frame('性别' <- numeric(dim(SS_data)[1]))
SS_data_change$'性别' <- SS_data['X1']
SS_data_change$'住址' <- SS_data['X3']
SS_data_change$'成绩排名' <- SS_data['X5']

### 青少年创造性倾向问卷
SS_data_change$'自信心' <- sum(SS_data['X1'])



### 自尊量表

SS_data_change


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

options(digits = 2)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)

fa.parallel(correlations, n.obs = 112, fa = "both", n.iter = 100,
            main = 'Scree plots with parallel analysis')









