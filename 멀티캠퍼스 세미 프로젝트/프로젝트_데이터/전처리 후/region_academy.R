# 학원교습소정보_2020년07월31일기준(1).xlsx
# 참고사이트 나이스 교육정보 개방포털(https://open.neis.go.kr/portal/mainPage.do)

library(readxl)
library(dplyr)
library(stringr)

region_academy <- read_xlsx(file.choose())
head(region_academy) 

# 교습소, 학원 중 학원만 선택
region_academy <- region_academy %>% filter(학원교습소명 =='학원')
View(region_academy)

region_academy_cnt <- region_academy %>% 
                        group_by(region_academy$시도교육청명) %>% 
                        summarise(n=n())

names(region_academy_cnt) <- c('region','academy')
View(region_academy_cnt)

#recode함수 값 변경 (substr하기전)
region_academy_cnt$region <- recode(region_academy_cnt$region, '충청남도교육청'='충남교육청')
region_academy_cnt$region <- recode(region_academy_cnt$region, '충청북도교육청'='충북교육청')
region_academy_cnt$region <- recode(region_academy_cnt$region, '전라남도교육청'='전남교육청')
region_academy_cnt$region <- recode(region_academy_cnt$region, '전라북도교육청'='전북교육청')
region_academy_cnt$region <- recode(region_academy_cnt$region, '경상남도교육청'='경남교육청')
region_academy_cnt$region <- recode(region_academy_cnt$region, '경상북도교육청'='경북교육청')

region_academy_cnt$region <- substr(region_academy_cnt$region,1,2)

# 지역 순서로 정렬
region_academy_cnt <- region_academy_cnt[c(9,8,6,12,5,7,11,10,2,1,17,16,14,13,4,3,15),]
View(region_academy_cnt)

# 텍스트 파일로 저장
write.table(region_academy_cnt,"region_academy.txt", quote=FALSE, row.names = FALSE)
            
            