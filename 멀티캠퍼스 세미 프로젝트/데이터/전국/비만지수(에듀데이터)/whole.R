### whole

#local <- read.table(file.choose(), sep='\t', header = T)           # (2018)광역시별_지역아동센터현황.txt
#local
#local <- local[3:18,]
#names(local) <- c("region","facilities","users")
#head(local)


local_2020 <- read.table(file.choose(), sep='\t', header = T)           # (2020)광역시별_지역아동센터현황.txt
local_2020 <- local_2020[1:18,]
local_20fac <- local_2020[3:18,1:2]
local_20fac
names(local_20fac) <- c("region","facilities")


finance <- read.table(file.choose(), sep='\t', header = T)       # (2020,2019,2018)광역시 재정자립도.txt
finance
fin_20 <- finance[2:17,c(1,4)]
names(fin_20) <- c("region","fin")
fin_20

cbind(fin_20, local)


bmi <- read.table(file.choose(), sep='\t', header = T)         # 지역별 비만지수_sorted.txt
bmi
names(bmi) <- c("region","bmi")
bmi
bmi[7,2] <- 19.73617021


dolbom <- read.table(file.choose(), sep='\t')       # 전국돌봄교실.txt
dolbom
names(dolbom) <- c("region","dolbom")
dolbom


students <- read.table(file.choose(), sep='\t', header = T)         # 전국초등학생수_2020.txt
students <- students[3:18,]


lib_whole <- read.table(file.choose(), sep='\t', header = T)  # 전국 시도별 도서관 수.txt
lib_whole
names(lib_whole) <- c("gu","lib_whole")
lib_whole <- lib_whole[2:17,]


gross_whole <- read.table(file.choose(), sep='\t', header=T) # 지자체별 지역내총생산 
gross_whole <- gross_whole[1:16,]
names(gross_whole) <- c("gu","gross")


whole$`bmi$bmi` <- bmi$bmi
whole <- cbind(dolbom, local_20fac$facilities, fin_20$fin, bmi$bmi)
whole$dolbomplus <- dolbom$dolbom + local_20fac$facilities
whole <- cbind(whole, students$students)
whole <- cbind(whole, gross_whole$gross)
whole <- cbind(whole,lib_whole$lib_whole)
whole_draft <- whole[,c(1,6,7,5,8,9)]
whole_draft
names(whole_draft) <- c("region","dolbomplus","students","bmi","gross","lib")
#whole_dr_sj <- whole_draft[-7,]

#whole_draft$bmi <- bmi$bmi

# seoul regression result
# dolbomplus = -6.106e+02 + 4.552e-03 * students + 3.263e+01 * bmi + (-8.704e-01) * lib + 4.719e-07 * gross

whole_draft$pred <- -6.106e+02 + 4.552e-03 * whole_draft$students + 
  3.263e+01 * whole_draft$bmi + (-8.704e-01) * whole_draft$lib + 
  4.719e-07 * whole_draft$gross
whole_draft$diff <- whole_draft$pred - whole_draft$dolbomplus
whole_draft[,c("region","bmi")]
