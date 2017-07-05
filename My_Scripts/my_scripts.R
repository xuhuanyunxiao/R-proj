


# 获取文件所在路径及文件名 ----------------------------------------------------------------

# file_folder <- getwd()
file_folder <- "D:/XH/R_Project/My_Scripts"
file_list <- as.list(dir(file_folder))


# 循环添加到工作空间 ---------------------------------------------------------------

for (i in 1:length(file_list))
{ 
  if (file_list[i] != 'my_scripts.R')
  {
    source(file.path(file_folder,file_list[i]))
    }
  }












