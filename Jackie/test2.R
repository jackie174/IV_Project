library(tidyverse)
library(jsonlite)
library(plotly)
################# cleaning data ################
# 读取CSV文件
data <- read.csv("../Data/relation/off-street-car-parks-with-capacity-and-type1.csv", header=TRUE)

# 定义suburb列表
suburbs_lga_mel = c('Carlton', 'Carlton North', 'East Melbourne', 
                    'South Yarra', 'Melbourne', 'Southbank', 
                    'South Wharf', 'Docklands', 'Port Melbourne',
                    'West Melbourne', 'North Melbourne','Kensington', 
                    'Parkville', 'Flemington')

# 对"geography"列中的特定值进行替换
data$`Clue.Small.Area` <- gsub("Melbourne \\(CBD\\)", "Melbourne", data$`Clue.Small.Area`)
data$`Clue.Small.Area` <- gsub("Melbourne \\(Remainder\\)", "Melbourne", data$`Clue.Small.Area`)
data$`Clue.Small.Area` <- gsub("West Melbourne \\(Residential\\)", "West Melbourne", data$`Clue.Small.Area`)
data$`Clue.Small.Area` <- gsub("West Melbourne \\(Industrial\\)", "West Melbourne", data$`Clue.Small.Area`)

# 检查CSV文件中"geography"列的值是否与suburbs_lga_mel中的值不一致
mismatched_suburbs <- setdiff(data$`Clue.Small.Area`, suburbs_lga_mel)

# 打印不一致的suburb
print(mismatched_suburbs)

data_2020 <- data %>% filter(Census.Year == 2020)

result <- data_2020 %>%
  group_by(`Clue.Small.Area`) %>%
  summarise(total_parking_spaces = sum(`Parking.Spaces`))

########################## plot 1 #########
# individual plots
fig1 <- plot_ly(
  type = 'scatterpolar',
  r = ~result$total_parking_spaces,
  theta = ~result$Clue.Small.Area,
  fill = 'toself'
)

fig1 <- fig1 %>% layout(polar = list(radialaxis = list(visible = T, range = c(0,60000))),
                        showlegend = FALSE)

fig1



#######################
# library(raster)
# library(sp)

# rast_data <- raster("../Data/population/aus_pd_2020_1km.tif")
# plot(rast_data)

# 导入必要的库
library(tidyverse)

# 读取CSV文件
data <- read.csv("../Data/population/residents-profiles-by-clue-small-area.csv")

# 定义suburb列表
suburbs_lga_mel = c('Carlton', 'Carlton North', 'East Melbourne', 
                    'South Yarra', 'Melbourne', 'Southbank', 
                    'South Wharf', 'Docklands', 'Port Melbourne',
                    'West Melbourne', 'North Melbourne','Kensington', 
                    'Parkville', 'Flemington')



# 检查CSV文件中"geography"列的值是否与defined_suburbs中的值不一致
mismatched_suburbs <- setdiff(data$geography, suburbs_lga_mel)

# 打印不一致的suburb
print(mismatched_suburbs)

# 对"geography"列中的特定值进行替换
data$geography <- gsub("Melbourne \\(CBD\\)", "Melbourne", data$geography)
data$geography <- gsub("West Melbourne \\(Residential\\)", "West Melbourne", data$geography)
data$geography <- gsub("South Yarra \\(inc\\. Melbourne Remainder\\)", "South Yarra", data$geography)

# 检查CSV文件中"geography"列的值是否与suburbs_lga_mel中的值不一致
mismatched_suburbs <- setdiff(data$geography, suburbs_lga_mel)

# 打印不一致的suburb
print(mismatched_suburbs)
# 删除包含"Greater Melbourne"和"City of Melbourne"的所有行
data <- data[!data$geography %in% c("Greater Melbourne", "City of Melbourne"), ]
# 检查CSV文件中"geography"列的值是否与suburbs_lga_mel中的值不一致
mismatched_suburbs <- setdiff(data$geography, suburbs_lga_mel)

# 打印不一致的suburb
print(mismatched_suburbs)




