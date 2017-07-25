# author： 许欢
# date： 20170615


# 导入包、库 -------------------------------------------------------------------
library(car)
library(MASS)

# 添加自定义函数
# normalizePath(readClipboard(), "/", mustWork = FALSE)
source(file="D:/XH/R_Project/Questionnaire/file/My_Scripts/my_scripts.R", chdir = TRUE)


# 获取与设置工作目录 ---------------------------------------------------------------
getwd()
setwd('D:/XH/R_Project/shunyi_questionaire/file')

# 文件位置 --------------------------------------------------------------------
data_folder <- 'D:/XH/R_Project/shunyi_questionaire/data'
result_folder <- 'D:/XH/R_Project/shunyi_questionaire/result'
file_folder <- "D:/XH/R_Project/shunyi_questionaire/file"


# 数据导入 --------------------------------------------------------------------
file_name <- paste(data_folder, '/S_P_fitness data part.csv', sep = '')
S_P_fitness_data <- read.csv(file_name)


# 建模：多自变量-单因变量 ------------------------------------------------------------
scatterplotMatrix(S_P_fitness_data,spread = FALSE,main = 'plot matrix')
attach(S_P_fitness_data)

par(mfrow = c(1,1))
plot(总分,BMI,xlab = '总分',ylab = '家长教育需求');abline(lm(总分 ~ BMI))

fit1 <- lm(总分 ~ BMI 
             + 家长教育观念 + 家长教育需求)
summary(fit1)

fit2 <- lm(总分 ~ . -X, data = S_P_fitness_data)
summary(fit2)

fit3 <- lm(总分 ~ . -X -家长教育观念 -家长教育理念 -家长身体健康 
             -家长对校评价 -家长心理健康 -学校体育设施 
             -学生饮食健康 -学生社会健康  , data = S_P_fitness_data)
summary(fit3)

fit1 <- lm(BMI ~ 总分 
             + 家长教育观念 + 家长教育需求)
summary(fit1)

fit2 <- lm(BMI ~ . -X, data = S_P_fitness_data)
summary(fit2)

fit3 <- lm(BMI ~ . -X -家庭教育情况 -家长教育需求 -家长身体健康 
                   -家长心理健康 -学生饮食健康 -学生社会倾向 
                   -学生心理健康 -家长社会健康 -学校体育设施  
                   -学校体育文化  , data = S_P_fitness_data)
summary(fit3)

fit2 <- lm(BMI ~ (. -X)^2, data = S_P_fitness_data)
summary(fit2)


# 模型选择 --------------------------------------------------------------------
stepAIC(fit1)



ptm <- proc.time() # 保存返回值
for(i in 1:100) mad(runif(1000))
proc.time() - ptm # 两者的差即运行程序所需时间

system.time(for(i in 1:100) mad(runif(1000)))

sprintf("%10.3f", 3.1415626)  # 宽度m为10,小数点n为3
sprintf("%.3f", 3.1415626)  # 不指定总宽度,小数点后3位




d <- data.frame(obs = c(1, 2, 3), treat = c("A", "B", "A"), weight = c(2.3, NA, 9))

# 空格分隔
write.table(d, file = "D:/XH/foo.txt", row.names = F, quote = F) 
# tab 分隔的文件
write.table(d, file = "D:/XH/foo.txt", row.names = F, quote = F, sep="\t")  

# 保存为逗号分割文本
write.csv(d, file = "D:/XH/foo.csv", row.names = F, quote = F)

# 保存为R格式文件
save(d, file = "D:/XH/foo.Rdata")

# 保存工作空间镜像
save.image( ) = save(list =ls(all=TRUE), file=".RData")








