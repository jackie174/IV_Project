# 导入需要的库
library(readxl)
library(dplyr)
library(gdata)
# 读取Excel文件
data <- read_excel("../Data/crime/LGA_Recorded_Offences_Year_Ending_June_2023.xlsx", sheet= "Table 04")


# 筛选满足条件的行
filter_data <- data[grep("Other Transport|public Transport", data$`Location Subdivision`, ignore.case = TRUE), ]
filter_data <- filter_data[grep("Melbourne", filter_data$`Local Government Area`, ignore.case = TRUE), ]
selected_data <-
  filter_data[, c('Year', 'Local Government Area', 'Location Subdivision', 'Location Group', 'Offence Count')]


# 对Location Group进行汇总
# summary_data <- selected_data %>%
#   group_by(Year, `Local Government Area`, `Location Subdivision`, `Location Group`) %>%
#   summarise(Total_Offence_Count = sum(`Offence Count`)) %>%
#   ungroup()

# 对Location Subdivision进行汇总
# summary_data1 <- selected_data %>%
#   group_by(Year, `Local Government Area`, `Location Subdivision`) %>%
#   summarise(Total_Offence_Count = sum(`Offence Count`)) %>%
#   ungroup()

# 对Location Subdivision进行汇总并添加新的列
# selected_data1 <- selected_data %>%
#   group_by(Year, `Local Government Area`, `Location Subdivision`) %>%
#   mutate(Total_Offence_by_Subdivision = sum(`Offence Count`)) %>%
#   ungroup()


# 使用gsub函数删除数字
selected_data$`Location Subdivision` <- gsub("\\d", "", selected_data$`Location Subdivision`)
selected_data$`Location Group` <- gsub("\\d", "", selected_data$`Location Group`)

# 打印处理后的数据
print(selected_data)






# 打印汇总后的数据
print(summary_data)
# 打印汇总后的数据
print(summary_data)








