library(readxl)
library(dplyr)
library(doBy)

# file 공개데이터_초18
# health_m_gd5_11,health_m_gd6_11,health_fe_gd5_11,health_fe_gd6_11
# 5학년 남자, 6학년 남자, 5학년 여자, 6학년여자 체지방BMI수치값

bmi <- read_xlsx(file.choose())
bmi <- bmi %>% select(year,regname,health_m_gd5_11,health_m_gd6_11,health_fe_gd5_11,health_fe_gd6_11)
View(bmi)

# 결측치를 평균값으로 대체 
bmi %>% summarise(mean_health_m_gd5_11  = mean(health_m_gd5_11, na.rm = T))
bmi$health_m_gd5_11 <- ifelse(is.na(bmi$health_m_gd5_11),19.8,bmi$health_m_gd5_11)
bmi %>% summarise(mean_health_m_gd6_11  = mean(health_m_gd6_11, na.rm = T))
bmi$health_m_gd6_11 <- ifelse(is.na(bmi$health_m_gd6_11),20.3,bmi$health_m_gd6_11)
bmi %>% summarise(mean_health_fe_gd5_11 = mean(health_fe_gd5_11, na.rm = T))
bmi$health_fe_gd5_11 <- ifelse(is.na(bmi$health_fe_gd5_11),18.7,bmi$health_fe_gd5_11)
bmi %>% summarise(mean_health_fe_gd6_11 = mean(health_fe_gd6_11, na.rm = T))
bmi$health_fe_gd6_11 <- ifelse(is.na(bmi$health_fe_gd6_11),19.6,bmi$health_fe_gd6_11)

View(bmi)

bmi_region <- bmi %>% group_by(regname) %>% summarise(mean_health_m_gd5_11 = mean(health_m_gd5_11),
                                                      mean_health_m_gd6_11 = mean(health_m_gd6_11),
                                                      mean_health_fe_gd5_11 = mean(health_fe_gd5_11),
                                                      mean_health_fe_gd6_11 = mean(health_fe_gd6_11))

View(bmi_region)

# 지역별 평균 mean_health_all
bmi_region$mean_health_all <- (bmi_region$mean_health_m_gd5_11 + bmi_region$mean_health_m_gd6_11 + bmi_region$mean_health_fe_gd5_11 + bmi_region$mean_health_fe_gd6_11)/4
# 지역별 평균 반올림 round_mean_health_all
bmi_region$round_mean_health_all <- round(bmi_region$mean_health_all,2)
# 지역별 평균 순위 region_rank
bmi_region$region_rank <-rank(-(bmi_region$mean_health_all))

View(bmi_region)
write.csv(bmi_region, file = "지역별 비만지수.csv")

