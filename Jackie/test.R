# 导入需要的库
library(readxl)
library(dplyr)
library(gdata)
# 读取Excel文件
data <- read_excel("../Data/crime/LGA_Recorded_Offences_Year_Ending_June_2023.xlsx", sheet= "Table 04")

# 筛选满足条件的行
filter_data <- data[grep("Other Transport|public Transport", data$`Location Subdivision`, ignore.case = TRUE), ]
filter_data <- filter_data[grep("Melbourne", filter_data$`Local Government Area`, ignore.case = TRUE), ]
filter_data <- filter_data[grep("2020", filter_data$`Year`, ignore.case = TRUE), ]
selected_data <-
  filter_data[, c('Year', 'Local Government Area', 'Location Subdivision', 'Location Group', 'Offence Count')]

# 更新Location Subdivision列的值
selected_data$`Location Subdivision`[grep("car", selected_data$`Location Group`, ignore.case = TRUE)] <- "Car"

# 对Location Subdivision进行汇总并添加新的列
selected_data <- selected_data %>%
  group_by(Year, `Local Government Area`, `Location Subdivision`) %>%
  mutate(Total_Offence_by_Subdivision = sum(`Offence Count`)) %>%
  ungroup()

# 使用gsub函数删除数字
selected_data$`Location Subdivision` <- gsub("\\d", "", selected_data$`Location Subdivision`)
selected_data$`Location Group` <- gsub("\\d", "", selected_data$`Location Group`)






# 
# library
library(tidyverse)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,17), sep=""),
  group=c( rep('Public Transport', 9), rep('Car', 4),rep('Other Transport', 4)) ,
  value=sample( seq(10,100), 17, replace=T)
)
data$group <- as.factor(data$group)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))
data$group <- as.character(data$group)

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  # ...
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(2,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p


nrow(base_data)

base_data$hjust







# 安装并加载必要的库
install.packages("plotly")
library(plotly)

# Create polar bar plot using plotly
plot_ly(data, type = "barpolar", r = ~value, theta = ~id, color = ~group, colors = c('red', 'blue')) %>%
  layout(polar = list(bargap = 0.1, radialaxis = list(tickangle = 45)))







library(plotly)
# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,17), sep=""),
  group=c( rep('A', 10), rep('B', 7)) ,
  value=sample( seq(10,100), 17, replace=T)
)
# 使用 plot_ly 创建基础图
p <- plot_ly()

# 为每个组添加条形图
colors <- c(A = 'red', B = 'blue')
p <- p %>% add_bars(data = subset(data, !is.na(value) & group == 'A'), 
                    r = ~value, theta = ~id, name = 'A', 
                    marker = list(color = colors['A'], opacity = 0.5))
p <- p %>% add_bars(data = subset(data, !is.na(value) & group == 'B'), 
                    r = ~value, theta = ~id, name = 'B', 
                    marker = list(color = colors['B'], opacity = 0.5))

# 添加文本标签
p <- p %>% add_text(data = label_data, 
                    r = ~value + 10, 
                    theta = ~id, 
                    text = ~individual, 
                    textposition = "top center",
                    showlegend = FALSE)

# 设定布局和外观
p <- p %>% layout(title = "Polar Bar Plot",
                  polar = list(radialaxis = list(visible = TRUE, range = c(0, 120)),
                               angularaxis = list(direction = "clockwise")),
                  showlegend = FALSE)

p













