################# Note this r script only execute once for file prepare #################
###### The large file has been removed from the repo due to unable for push ############
###### The large file (Road_Crashes_for_five_Years_Victoria.geojson) has been kept locally ####################################
library(sf)
library(dplyr)
library(lubridate)

# set current working directory to where this app.R file stores
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_directory)

################
# Read in Data #
################
# Victoria street line data
mel_traffic_lines <- st_read("../Data/transportation/mel_traffic_vol.geojson")

# LGA melbourne shape data
mel_suburbs_gda94 <- st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson")

# get last five years vic crash data (with point location)
vic_crash_data <- st_read('../Data/transportation/Road_Crashes_for_five_Years_Victoria.geojson')

#####################
# Data Manipulation #
#####################
# transform to the same coordinate system
mel_traffic_lines_wgs84 <- st_transform(mel_traffic_lines, crs = 4326)

mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

vic_crash_data_wgs84 <- st_transform(vic_crash_data, crs = 4326)


####### city_mel streets line traffic data #######

mel_st_lines_sf <- st_intersection(mel_traffic_lines_wgs84, mel_suburbs_wgs84)

# Convert the geometry column back to multilinestring
mel_st_lines_sf <- st_cast(mel_st_lines_sf, "MULTILINESTRING")

temp <- st_drop_geometry(mel_st_lines_sf) %>% select(-clue_area) %>% distinct()

mel_st_lines_sf <- mel_st_lines_sf[rownames(temp), ]


##### crash data ########

# 选择指定的列并进行日期提取
selected_columns <- vic_crash_data_wgs84 %>%
  select(
    ACCIDENT_DATE = ACCIDENT_DATE,
    ALCOHOLTIME,
    DAY_OF_WEEK,
    LIGHT_CONDITION,
    ROAD_GEOMETRY,
    SPEED_ZONE,
    LONGITUDE,
    LATITUDE,
    LGA_NAME,
    INJ_OR_FATAL,
    FATALITY,
    SERIOUSINJURY,
    BICYCLIST,
    PEDESTRIAN,
    MOTORIST,
    ALCOHOL_RELATED,
    HEAVYVEHICLE
  )

# 提取日期部分
selected_columns$ACCIDENT_DATE <- as.Date(selected_columns$ACCIDENT_DATE)

# 筛选 LGA_NAME 为 "MELBOURNE" 的行
mel_crash_data_wgs84 <- selected_columns[selected_columns$LGA_NAME == "MELBOURNE", ]

# join street (line) and crash data (point)

# keep required columns in mel_st_lines_sf only
mel_st_for_join <- mel_st_lines_sf %>%
  select(OBJECTID_1, LOCAL_ROAD_NM, clue_area)

# perform spatial join
crash_at_streets <- st_join(mel_st_for_join, mel_crash_data_wgs84, join = st_intersects)

# 输出结果
crash_at_streets <- crash_at_streets %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE) & !is.na(ACCIDENT_DATE))

# Extract crash year into a column
crash_at_streets$ACCIDENT_YEAR <- year(crash_at_streets$ACCIDENT_DATE)

# drop unnecessary columns
crash_at_streets <- st_drop_geometry(crash_at_streets)

crash_at_streets <- crash_at_streets %>%
  select(-LGA_NAME, -LONGITUDE, -LATITUDE, -ACCIDENT_DATE)

# statistics about crash at streets df
stats_crash_at_streets <- crash_at_streets %>%
  group_by(OBJECTID_1, ACCIDENT_YEAR, LOCAL_ROAD_NM, clue_area) %>%
  summarize(
    ACCIDENTS = n(),
    INJ_OR_FATAL = sum(INJ_OR_FATAL),
    FATALITY = sum(FATALITY),
    SERIOUSINJURY = sum(SERIOUSINJURY),
    BICYCLIST = sum(BICYCLIST),
    PEDESTRIAN = sum(PEDESTRIAN),
    MOTORIST = sum(MOTORIST),
    ALCOHOL_RELATED = sum(ALCOHOL_RELATED == 'Yes'),
    LIGHT_CONDITION_Unk = sum(LIGHT_CONDITION == 'Unk.'),
    LIGHT_CONDITION_DarkLightsUnknown = sum(LIGHT_CONDITION == 'Dark Street lights unknown'),
    LIGHT_CONDITION_DuskDawn = sum(LIGHT_CONDITION == 'Dusk/Dawn'),
    ROAD_GEOMETRY_CIntersection = sum(ROAD_GEOMETRY == 'Cross intersection'),
    ROAD_GEOMETRY_MultiIntersection = sum(ROAD_GEOMETRY == 'Multiple intersection'),
    ROAD_GEOMETRY_TIntersection = sum(ROAD_GEOMETRY == 'T intersection'),
    ROAD_GEOMETRY_YIntersection = sum(ROAD_GEOMETRY == 'Y intersection'),
    SPEED_ZONE_NotKnown = sum(SPEED_ZONE == 'Not known'),
    WEEKDAYS = sum(DAY_OF_WEEK %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')),
    WEEKENDS = sum(DAY_OF_WEEK %in% c('Saturday', 'Sunday'))
  ) %>%
  ungroup()

# To add missing year data
# 你的OBJECTID_1的列表
objectid_1_list <- unique(stats_crash_at_streets$OBJECTID_1)
all_years <- c(2015, 2016, 2017, 2018, 2019)

all_years_df <- expand.grid(OBJECTID_1 = objectid_1_list,
                            ACCIDENT_YEAR = all_years)

# 当前temp中有很多na值
temp <- all_years_df %>%
  left_join(stats_crash_at_streets, by = c("OBJECTID_1", "ACCIDENT_YEAR"))%>%
  mutate(across(-c(OBJECTID_1, ACCIDENT_YEAR, LOCAL_ROAD_NM, clue_area), ~replace(., is.na(.), 0))) %>%
  mutate_all(~ replace(., is.na(.), 0))

# 找到每个 OBJECTID_1 对应的非零 LOCAL_ROAD_NM 和 clue_area 值
non_zero_local_road <- temp %>%
  group_by(OBJECTID_1) %>%
  filter(LOCAL_ROAD_NM != 0, clue_area != 0) %>%
  select(OBJECTID_1, LOCAL_ROAD_NM, clue_area)%>%
  unique()


# 使用非零值填充对应 OBJECTID_1 的 LOCAL_ROAD_NM 列
stats_crash_at_streets <- temp %>%
  left_join(non_zero_local_road, by = "OBJECTID_1") %>%
  select(-LOCAL_ROAD_NM.x, -clue_area.x) %>%
  rename(LOCAL_ROAD_NM = LOCAL_ROAD_NM.y, clue_area = clue_area.y) %>%
  select(OBJECTID_1, ACCIDENT_YEAR, LOCAL_ROAD_NM, clue_area, everything())

st_write(stats_crash_at_streets, '../Data/transportation/stats_crash_at_streets.geojson')




