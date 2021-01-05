#세미프로젝트(~ 20.09.10)

""" 
외부데이터는 다음과 같습니다 
# 서울_학교돌봄현황.xlsx : 학교알리미서비스 공시정보 활용 자체파일제작
# 학생체력증진현황.xlsx : 공공데이터포털에서 교육부 통합제공 정보공시 초중등학교 학생 체력 증진 현황.zip 에서 2019년도 초등학교 파일만 조회
# 서울_마을돌봄현황.xlsx : 지역아동센터, 다함께돌봄센터, 방과후아카데미 사이트 활용 자체파일제작
# 행정구역별주요교육통계자료.xlsx : 교육통계서비스에서 알림서비스 > 주요요청자료 > 주제별공개데이터 > 시도별 행정구별 주요 교육 통계 현황(2010~2020)에서 2019년 파일 
# 서울_저소득및한부모가구수 : 국가통계포털 > 통계표 > 저소득 및 한부모 가정 2018년 조회
# 시도별행정구별사설학원현황.xlsx : 교육통계서비스에서 알림서비스 > 주요요청자료 > 주제별공개데이터 > 시도별 행정구별 사설학원 현황(2013~2019 )에서 2019년 파일 
# 서울_도서관현황.txt : 서울열린데이터광장에서 서울시 도서관 현황 통계자료에서 기간 2018년으로 조회
# 작은도서관현황.xls : 국가도서관통계시스템에서 도서관통계 > 작은도서관 > 통계보기에서 통계년도 2018년 지역구분 '서울'로 조회
# 간호사및간호조무사현황.xls : 보건의료빅데이터개방시스템에서 의료통계정보 > 임상간호인력 현황정보 카테고리에서, 간호사 및 간호조무사 현황 2019년 조회 
# 교육관련지원예산현황.xlsx: 지방재정365에서 지방재정전문통계 > 예산 > 주요경비 예산편성 카데고리에서 교육관련지원예산현황 회계연도 2019년도로 조회
# 재정운용상황개요.xlsx: 지방재정365에서 지방재정전문통계 > 지방재정 통합공시>항목별현황>예산기준>재정운용계획에서 재정운용상황개요 회계연도 2019년도로 조회
# 서울_나라살림리포트전처리.xlsx파일 : 나라살림리포트(https://www.narasallim.net/2450) 외부사이트 크롤링
# 서울_살림규모.xlsx  : 서울재정포털에서 각 구별로 접속한 후 행정정보 > 예산정보 > 재정공시에서 2019년 회계연도 결산기준 재정공시 파일 참고해 작성
# 서울_지역내총생산.txt : 서울열린데이터광장에서 자치구별 1인당 지역내 총생산및 수준지수 2016년 조회(서울시에서 2019년 2월 공표한 자료)
# 서울_월간음주율.txt : 국가통계포털
# 서울_현재흡연율.txt : 국가통계포털
# 전국_월간음주율.xlsx : 국가통계포털
# 전국_저소득및한부모가구수.txt : 한국사회보장정보원 > 알림마당 > 사회보장통계 > 복지서비스별 사회보장통계 > 한부모가족 카테고리에서 한부모가족 가족유형별 가구수, 수급자수에서 2019년 4월로 조회
# 전국_학생수현황.xlsx :행정구역별주요교육통계자료.xlsx 파일에서 엑셀 작업 후 자체파일제작
# 전국_학교돌봄현황.xlsx : 학교알리미서비스 공시정보 활용 자체파일제작
# 전국_마을돌봄현황.xlsx : 지역아동센터, 다함께돌봄센터, 방과후아카데미 사이트 활용 자체파일제작
"""


"""
변수명은 다음과 같습니다

gu              자치구 
sch_dolbom      학교돌봄개수 
schools         돌봄운영학교수 
vil_dolbom      마을돌봄개수 
bmi             비만지수
hanbumo         한부모가구수
base            저소득가구수 
students        학생수
multicul        다문화학생수
multiculper     학생수당 다문화학생수 비율
gross           서울시 구별 지역내총생산
academy         사설학원수
fin01           재정자립도
fin02           재정자주도
socialbud       사회복지예산비율
lib             서울시 구별 도서관(국립,공공,대학,전문 도서관의 합)수
little_lib      작은도서관수
libtotal        총 도서관수
Slibpub         공공도서관 + 작은도서관 수 
scale           서울시 구별 살림규모
edubud          교육경비보조금
bud             서울시 구별 세입예산 
edubudper       세입예산대비 교육경비보조금비율
medical         의료인력
air             미세먼지 
"""

install.packages("readxl")
install.packages("car")
install.packages("dplyr")
install.packages("MASS")
install.packages("leaps")
install.packages("corrgram")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("ggmap")
library(readxl)
library(car)
library(dplyr)
library(MASS)
library(leaps)
library(corrgram)
library(RColorBrewer)
library(ggplot2)
library(ggmap)



# 1. 데이터 수집 
seoul_total <- read_xlsx(file.choose())                                #서울_학교돌봄현황.xlsx
seoul_vil_dolbom <- read_xlsx(file.choose())                           #서울_마을돌봄현황.xlsx
bmi <- read_xlsx(file.choose())                                        #학생체력증진현황.xlsx
hanbumo_base  <- read.table(file.choose(), sep='\t', header=T)         #서울_저소득및한부모가구수.txt
students <- read_xlsx(file.choose())                                   #행정구역별주요교육통계자료.xlsx
seoul_gross <-read.table(file.choose(), sep='\t', header=T)            #서울_지역내총생산.txt
academy <-read_xlsx(file.choose())                                     #시도별행정구별사설학원현황.xlsx
fin <- read_xlsx(file.choose())                                        #재정운용상황개요.xlsx
seoul_lib <- read.table(file.choose(), sep='\t', header=T)             #서울_도서관현황.txt
little_lib <- read_xls(file.choose())                                  #작은도서관현황.xls  
seoul_scale <- read_xlsx(file.choose())                                #서울_살림규모.xlsx 
edubud <-    read_xlsx(file.choose())                                  #교육관련지원예산현황.xlsx
seoul_budget <- read_xlsx(file.choose())                               #서울_나라살림리포트전처리.xlsx파일
medical <- read_xls(file.choose())                                     #간호사및간호조무사현황.xls 
nationwide_total <- read_xlsx(file.choose())                           #전국_학교돌봄현황.xlsx
nationwide_vil_dolbom <-read_xlsx(file.choose())                       #전국_마을돌봄현황.xlsx 
nationwide_alcohol <- read_xlsx(file.choose())                         #전국_월간음주율.xlsx 
nationwide_hanbumobase<- read_xlsx(file.choose())                      #전국_저소득및한부모가구수.xlsx
nationwide_students <- read_xlsx(file.choose())                        #전국_학생수현황.xlsx
seoul_air <- read.table(file.choose(), sep='\t', header=T)             #서울_미세먼지.txt

# 2. 데이터 전처리
#서울_학교돌봄현황.xlsx 파일에서 서울시 구별 학교돌봄교실개수(sch_dolbom) 변수 돌봄운영학교수(schools) 변수 만들기 
View(seoul_total)
seoul_total <- seoul_total[-c(26),]
names(seoul_total) <- c("gu", "sch_dolbom", "schools")

#서울_마을돌봄현황.xlsx 파일에서 서울시 구별 마을돌봄개수(vil_dolbom) 변수 만들기 
View(seoul_vil_dolbom)
seoul_vil_dolbom <- seoul_vil_dolbom[-c(1,27),]
seoul_vil_dolbom$지역아동센터수 <- as.numeric(seoul_vil_dolbom$지역아동센터수)
seoul_vil_dolbom$다함께돌봄센터수<- as.numeric(seoul_vil_dolbom$다함께돌봄센터수)
seoul_vil_dolbom$초등청소년방과후아카데미수<- as.numeric(seoul_vil_dolbom$초등청소년방과후아카데미수)
seoul_vil_dolbom$vil_dolbom <- seoul_vil_dolbom$지역아동센터수+ seoul_vil_dolbom$다함께돌봄센터수+seoul_vil_dolbom$초등청소년방과후아카데미수

#학교돌봄교실개수(sch_dolbom)와 마을돌봄개수(vil_dolbom)를 활용해 종속변수(dolbomplus) 만들기 
View(seoul_total)
seoul_total$dolbomplus <- seoul_total$sch_dolbom + seoul_vil_dolbom$vil_dolbom

#학생체력증진현황.xlsx 파일에서 서울시 구별 비만지수(bmi) 변수 만들기 
View(bmi)
bmi <- bmi[ ,c(1,3,5,10,11,22)]
names(bmi) <- c("시도교육청명", "지역", "학교명","학년","성별","비만지수")
bmi <- bmi %>% filter(시도교육청명 == "서울특별시교육청")
bmi <- na.omit(bmi)
bmi <- bmi %>%  group_by(지역) %>% summarise(비만지수평균 = mean(비만지수))
names(bmi) <- c("gu", "bmi")
bmi$gu <- substr(bmi$gu, 7 , nchar(bmi$gu))

#서울_저소득및한부모가구수.txt 파일에서 서울시 구별 한부모가구수(hanbumo), 저소득가구(base) 변수 만들기 
View(hanbumo_base) 
hanbumo_base <- hanbumo_base[-1 ,c(1,4,6)]
names(hanbumo_base) <- c("gu", "base", "hanbumo")
hanbumo_base$base <- gsub(",","",hanbumo_base$base)

#행정구역별주요교육통계자료.xlsx 파일에서 서울시 구별 학생수(students), 다문화학생수(multicul) 변수 만들기
#학생수(students)당 다문화학생수(multicul)비율로 파생변수(multiculper) 만들기  
View(students)
students <- students %>% filter(...2 == "서울" & ...4 == '초등학교')
students <- students[-26,c(3,13,29)]
names(students) <- c("gu", "students", "multicul")
students$multiculper<-  as.numeric(students$multicul) / as.numeric(students$students) 
students <- students %>% arrange(gu)

#서울_지역내총생산.txt 파일에서 서울시 구별 지역내총생산(gross) 변수 만들기 
View(seoul_gross)
seoul_gross <- seoul_gross[-1,c(2,3)]
names(seoul_gross) <- c("gu", "gross")
seoul_gross <- seoul_gross %>% arrange(gu)

#시도별행정구별사설학원현황.xlsx 파일에서 서울시 구별 사설학원수(academy) 변수 만들기 
View(academy)
academy <- academy %>% filter(...2 =="서울" & ...4 == "합계")
academy <- academy[,c(3,6)]
names(academy) <- c("gu", "academy")
academy <- academy %>% arrange(gu)

#재정운용상황개요.xlsx 파일에서 서울시 구별 재정자립도(fin01), 재정자주도(fin02), 사회복지예산비율(socialbud) 변수 만들기
View(fin)
fin <- fin %>% filter(지역명 =="서울")
fin <- fin[-1,c(4,5,6,7)]
names(fin) <- c("gu", "fin01", "fin02","socialbud")
fin$gu <- substr(fin$gu, 3 , length(fin$gu))
fin <- fin %>% arrange(gu)

#서울_도서관현황.txt 파일에서 서울시 구별 도서관(lib)변수 만들기 
View(seoul_lib)
seoul_lib_raw <- seoul_lib[-1,]
seoul_lib <- seoul_lib[-1,c(2,3)]
names(seoul_lib) <- c("gu", "lib")
seoul_lib <-seoul_lib %>% arrange(gu)

#작은도서관현황.xlsx 파일에서 서울시 구별 작은 도서관(little_lib) 변수 만들기  
View(little_lib)
little_lib <- little_lib %>% filter(...3 =="서울")
little_lib <-little_lib %>% group_by(...4) %>% summarise(n=n())
names(little_lib) <- c("gu", "little_lib")

#서울_도서관현황.txt 파일의 공공도서관수와 작은도서관(little_lib)변수 합계로 파생변수(Slibpub)만들기 
View(seoul_lib_raw)
View(seoul_lib)
seoul_lib$Slibpub <- seoul_lib_raw$공공도서관 + little_lib$little_lib

#서울_도서관현황.txt 파일에서 서울시 구별 도서관(lib)변수에 작은도서관(little_lib)수 합계로 총도서관수(libtotal)변수만들기 
seoul_lib$libtotal <- seoul_lib$lib + little_lib$little_lib
View(seoul_lib)

#서울_살림규모.xlsx 파일에서 서울시 구별 살림규모(scale) 변수 만들기
View(seoul_scale)
names(seoul_scale) <- c("gu", "scale")

#교육관련지원예산현황.xlsx 파일에서 서울시 구별 교육경비보조금(edubud) 변수 만들기 
View(edubud)
edubud <- edubud %>% filter(지역명 =="서울")
edubud <- edubud %>% filter(구분 =="교육경비보조금") 
edubud <- edubud %>% group_by(자치단체명)  %>%  summarise(예산액합 = sum(예산액)) 
edubud <- edubud[-14,]
names(edubud) <- c("gu", "edubud")
edubud$gu <- substr(edubud$gu, 3 , nchar(edubud$gu))
edubud$edubud <- substr(edubud$edubud, 1, nchar(edubud$edubud)-6)      #단위 백만원으로 통일

#서울_나라살림리포트전처리.xlsx파일에서 세입예산(bud)변수, 예산대비 교육경비보조금 비율(eduper) 변수 만들기
View(seoul_budget)
seoul_budget <- seoul_budget[-c(1,2,3),c(1,3,4)]
names(seoul_budget) <- c("gu", "bud", "edubudper")
seoul_budget$gu <- substr(seoul_budget$gu, 3, nchar(seoul_budget$gu))


#간호사및간호조무사현황.xls 파일에서 서울시 구별 의료인력(medical) 변수 만들기  
View(medical)
medical <- medical[-(1:7),c(1,2,3,5)] 
medical$...3 <- gsub(",","",medical$...3)
medical$...5 <- gsub(",","",medical$...5)
medical <- medical %>% filter(medical[,1]== "서울특별시")
medical$medical <- as.numeric(medical$...3) + as.numeric(medical$...5)
medical <- medical[-1,c(2,5)]
names(medical) <- c("gu", "medical")
medical <- medical %>% arrange(gu)

#서울_미세먼지.txt 파일에서 서울시 구별 미세먼지(air) 변수 만들기
View(seoul_air)
seoul_air <- seoul_air[-c(1,2,3),c(2,6)]
names(seoul_air) <- c("gu","air")
seoul_air <- seoul_air %>% arrange(gu)

# Train 테이블(서울) 만들기 
seoul_table <- cbind(seoul_total, bmi$bmi, hanbumo_base$hanbumo, hanbumo_base$base, students$students, 
                     students$multicul,students$multiculper, seoul_gross$gross, academy$academy, fin$fin01, 
                     fin$fin02, fin$socialbud, seoul_lib$libtotal, little_lib$little_lib, seoul_lib$Slibpub, 
                     seoul_scale$scale, edubud$edubud, seoul_budget$bud, seoul_budget$edubudper, medical$medical, 
                     seoul_air$air)

colnames(seoul_table) <- c("gu","sch_dolbom","schools","dolbomplus","bmi",
                           "hanbumo","base","students","multicul","multiculper",
                           "gross","academy","fin01","fin02","socialbud",
                           "libtotal","little_lib","Slibpub","scale","edubud",
                           "bud","edubudper","medical","air")
View(seoul_table)

# Train 테이블(서울) 변수 타입 확인 및 수정 
summary(seoul_table)

seoul_table$hanbumo   <- as.numeric(seoul_table$hanbumo)
seoul_table$base      <- as.numeric(seoul_table$base)
seoul_table$students  <- as.numeric(seoul_table$students)
seoul_table$multicul  <- as.numeric(seoul_table$multicul)
seoul_table$academy   <- as.numeric(seoul_table$academy)
seoul_table$edubud    <- as.numeric(seoul_table$edubud)
seoul_table$edubudper <- as.numeric(seoul_table$edubudper)
seoul_table$bud       <- as.numeric(seoul_table$bud)

# 독립변수간 상관분석 
seoul_cor <- seoul_table[,-c(1,2)]
cor(seoul_cor)
corrgram(seoul_cor, upper.panel = panel.conf) 

""" 
corrgram보는 법) upper.panel : 우측상단에 상관계수의 값을 나타냄 , panel.conf : 신뢰구간을 함께 봄
                 음의 상관계수는 적색&우측 하단 방향으로 빗금
                 양의 상관계수는 청색&우측 상단 방향으로 빗금
                 절댓값이 1에 가까울수록 진해지고, 0에 가까울수록 옅어짐 
"""

# 서울시를 통해 만든 회귀모델에서 사용할 가장 좋은 예측변수 선택과정(유의미한 모델을 만들기 위한 과정 )
# 변수선택 2가지 방법을 이용하여 최적의 예측변수를 구해 회귀분석식을 구한다.
# 첫번째 방법 :  단계적 변수 선택(stepwise regression - forward/backward/both)
seoul_data <- seoul_table[,-c(1,2)]
View(seoul_data)

seoul_steplm.null <- lm(dolbomplus ~ 1,   data = seoul_data)      #null model
seoul_steplm      <- lm(dolbomplus ~ . ,  data = seoul_data)      #full model 

# 1-(1) forward
step(seoul_steplm.null , scope=list(lower = seoul_steplm.null, upper= seoul_steplm), direction = "forward")
model1 <- lm(formula = dolbomplus ~ schools + multicul + base + students, data = seoul_data)
model1_re <- lm(formula = dolbomplus ~ students + multicul + base , data = seoul_data)

summary(model1)       
vif(model1)

summary(model1_re)
vif(model1_re)

"""
1-(1) forward 결과 해석
첫번째 모델1의 경우 
vif 분석을 이용하였더니 schools 13.702292, students 11.872514 나왔고 
base와 students의 경우 신뢰수준이 0.06265, 0.19943으로 매우 떨어진다.
vif가장 높았던 schools를 제외한 후 다시 계산해보니 
신뢰수준과 vif 모두 안정적으로 나왔더
"""

# 1-(2) backward
step(seoul_steplm , data = seoul_data, direction = "backward")

model2 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base + students +  multicul + gross + academy + fin01 + fin02 + socialbud +  little_lib + Slibpub + scale + edubud + edubudper + medical,   data = seoul_data)

summary(model2)
vif(model2)

# 모델2 수정1
model2_re <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base + students + multicul + gross + academy + fin01 + fin02  +  little_lib + Slibpub + scale + edubud + edubudper + medical,  data = seoul_data)

summary(model2_re)
vif(model2_re)

# 모델2 수정2
model2_re1 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base  +  multicul + gross + academy + fin01 + fin02  +  little_lib + Slibpub + scale + edubud + edubudper + medical,  data = seoul_data)

summary(model2_re1)
vif(model2_re1)

# 모델2 수정3
model2_re2 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base  +  multicul + gross + academy + fin01 + fin02  + Slibpub + scale + edubud + edubudper + medical, data = seoul_data)

summary(model2_re2)
vif(model2_re2)

# 모델2 수정4
model2_re3 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base  +  multicul + gross + academy + fin01  +  Slibpub + scale + edubud + edubudper + medical, data = seoul_data)

summary(model2_re3)
vif(model2_re3)

# 모델2 수정5
model2_re4 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base  + multicul + gross + academy + fin01  + Slibpub + scale  + edubudper + medical, data = seoul_data)

summary(model2_re4)
vif(model2_re4)

# 모델2 수정6
model2_re5 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base  + multicul + gross + academy + fin01  + Slibpub  + edubudper + medical, data = seoul_data)

summary(model2_re5)
vif(model2_re5)

# 모델2 수정7
model2_re6 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo + base  + multicul + gross + academy  + Slibpub  + edubudper + medical, data = seoul_data)

summary(model2_re6)
vif(model2_re6)

# 모델2 수정8
model2_re7 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo  + multicul + gross + academy  + Slibpub  + edubudper + medical, data = seoul_data)

summary(model2_re7)
vif(model2_re7)

# 모델2 수정9
model2_re8 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo  + multicul + gross  + Slibpub  + edubudper + medical, data = seoul_data)

summary(model2_re8)
vif(model2_re8)

# 모델2 수정10
model2_re9 <- lm(formula = dolbomplus ~ schools + bmi + hanbumo  + multicul + gross  + Slibpub  + edubudper , data = seoul_data)

summary(model2_re9)
vif(model2_re9)



"""
1-(2) backward 결과해석

backward 결과 산출된 예측변수 총 17개 그 중에 vif가 가장 높은 social bud(348.620880)을 제거해 다시 시도   
vif가 가장 높은 social bud(348.620880)을 제거해 다시 시도   
vif가 가장 높은 little_lib(100.401054)을 제거해 다시 시도 
vif가 가장 높은 fin02(42.823160)을 제거해 다시 시도 
vif가 가장 높은 edubud(21.584589)을 제거해 다시 시도 
vif가 가장 높은 scale(15.821798)을 제거해 다시 시도 
vif가 가장 높은 fin01(14.059044)을 제거해 다시 시도 
vif가 가장 높은 fin01(14.059044)을 제거해 다시 시도 
vif가 가장 높은 base(7.168779)을 제거해 다시 시도 
vif가 가장 높은 academy(5.898760)을 제거해 다시 시도
vif값 전체적으로 안정됨
예측변수 8개 결정계수 0.9098 , 수정결정계수 0.8647로 나왓으나 예측변수 모두 신뢰수준이 떨어지는 결과가 나왓다 
신뢰수준이 가장 안 좋은 medical을 제거한 후 다시 구해보았지만 결과는 동일하다. 

"""

# 1-(3) stepwise
seoul_steplm.null <- lm(dolbomplus ~ 1,   data = seoul_data)      #null model
seoul_steplm      <- lm(dolbomplus ~ . ,  data = seoul_data)      #full model 

step(seoul_steplm.null, scope = list(upper = seoul_steplm) , data = seoul_data, direction = "both")                     

# full model(seoul_steplm)변경해서 계산
seoul_steplm_forward  <- lm(formula = dolbomplus ~ schools + multicul + base + students, 
                            data = seoul_data)   

step(seoul_steplm.null, scope = list(upper = seoul_steplm_forward) ,  data = seoul_data, direction = "both")       #결과 forward와 동일 


# full model(seoul_steplm)변경해서 계산
seoul_steplm_backward <- lm(formula = dolbomplus ~ schools + bmi + hanbumo  + 
                              multicul + gross  + 
                              Slibpub  + edubudper + medical, 
                            data = seoul_data)

step(seoul_steplm.null, scope = list(upper = seoul_steplm_backward) ,  data = seoul_data, direction = "both")

model3 <- lm(formula = dolbomplus ~ schools + multicul + gross, data = seoul_data)

summary(model3)
vif(model3)


"""
1-(3) stepwise 결과해석

전체예측변수를 full model로 사용하여 stepwise(both)할 경우, forward에서 나왔던 예측변수와 동일하다 따라서 결과값도 같기 때문에 회귀식을 만들지 않고
곧바로 backward에서 나왓던 예측변수를 사용해 stepwise(both)를 해보았다
예측변수3개(schools, multicul, gross) 결정계수 0.9068, 수정결정계수 0.8935으로 나왔으나 gross의 신뢰수준이 떨어진다.

"""

"""
(개념정리) 
예측변수의 개수가 21개로 꽤 많다. 따라서 변수가 많을 때는 자동 접근법 사용을 통해 구하였다. 3가지 방법을 서로 비교,
결과는 AIC로 나타나지며 가장 적은 변수를 선택하기로 한다.
backward elimination의 경우 신뢰할 수 없는 변수(p값)를 하나씩 제거하면서 유의미한 변수만 남기는 것이고 
forward selection의 경우 유의미한 변수들을 하나씩 늘려가며 모델을 만들어 가는 방법이고
stepwise selection의 경우 모든 변수가 포함된 모델에서 출발하고 기준통계치에 가장 도움이 되지 않는 변수를 삭제하거나 모델에서
빠져있는 변수 중에서 기준 통계치를 가장 개선시키는 변수를 추가함(추가&제거를 반복)

(개념정리) 
변수가 많을수록 더 좋은 회귀모델을 얻는 것은 아니다.
오컴의 면도날 : 모든 것이 동일한 조건에서는 복잡한 모델보다는 단순한 모델을 우선 사용해야 한다는 원리
변수를 추가하면 항상 RMSE는 감소하고 결정계수는 증가 

"""


"""
단계적회귀분석 stepwise regression의 문제점
전진, 후진, 단계를 통한 변수 선택은 전체 부분집합(subset)중 일부만 비교되므로 실제 최적의 변수를 간과할 수 있다
leaps패키지의 regsubsets함수를 사용하여 부분집합회귀분석(all subset regression)을 시도

"""

# 두번째 방법 : 부분집합회귀분석(all subset regression)
seoul_data <- seoul_table[,-c(1,2)]
View(seoul_table)
View(seoul_data)

seoul_allsubset<- regsubsets(dolbomplus ~., data = seoul_data, nvmax = 20)

summary(seoul_allsubset)
summary(seoul_allsubset)$rsq            # 결정계수 값 확인
summary(seoul_allsubset)$adjr2          # 수정결정계수 값 확인 
summary(seoul_allsubset)$bic            # BIC 값 확인

plot(seoul_allsubset, scale="bic", col = palette())
plot(seoul_allsubset, scale="adjr2",col=palette())


model4 <- lm(dolbomplus~ schools+ base+ multicul+ fin02+ socialbud+ libtotal+ Slibpub, data = seoul_data)
summary(model4)
vif(model4)


model5 <- lm(dolbomplus~ schools+ multicul + gross + edubud + edubudper, data = seoul_data)
summary(model5)
vif(model5)


"""
2- all subsets 결과해석
변수의 개수에 따른 BIC값과 수정결정계수값 비교확인후 최적예측변수 7개로 모델4을 생성하였는데 Vif 값이 매우 불안정하게 나왔다.
그래서 변수의 개수를 2개 줄였을 때 나올 수 있는 최적의 예측변수로 모델5를 생성하였는데 전체적으로 안정화되었음을 볼 수 있다. 
"""

# 돌봄수요(종속변수 dolbomplus) 확인을 위한 최적의 변수 선택
"""
schools         서울시 구별 돌봄운영학교수 
hanbumo         서울시 구별 한부모가구수
multicul        다문화학생수
gross           지역 내 총생산
edubud          교육경비보조금
edubudper       예산대비 교육경비보조금 비중
"""

# (정리) 각 모델의 정확도 확인
model1_re   <- lm(formula = dolbomplus ~ students + multicul + base , data = seoul_data)
model2_re9  <- lm(formula = dolbomplus ~ schools + bmi + hanbumo  + multicul + gross  + Slibpub  + edubudper + medical, data = seoul_data)
model3      <- lm(formula = dolbomplus ~ schools + multicul + gross, data = seoul_data)
model4      <- lm(dolbomplus~ schools+ base+ multicul+ fin02+ socialbud+ libtotal+ Slibpub, data = seoul_data)
model5      <- lm(dolbomplus~ schools+ multicul + gross + edubud + eduper, data = seoul_data)


summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

vif(model1)
vif(model2)
vif(model3)
vif(model4)
vif(model5)

"""
모델1    91.64%      신뢰수준 미달 변수 개수 : 1     VIF : 불안정       총예측변수:  2
모델2    90.98%      신뢰수준 미달 변수 개수 : 5     VIF : 안정         총예측변수:  8
모델3    90.68%      신뢰수준 미달 변수 개수 : 1     VIF : 안정         총예측변수:  3
모델4    94.78%      신뢰수준 미달 변수 개수 : 2     VIF : 불안정       총예측변수:  7
모델5    93.12%      신뢰수준 미달 변수 개수 : 1     VIF : 안정         총예측변수:  5

"""

#  최종 모델 선정 
"""
신뢰수준,결정계수,수정결정계수, AIC, BIC를 모두 고려한 결과
예측변수 5개(schools,multicul,gross,edubud,edubudper)일때의 모델5가 가장 최적의 모델로 드러났다.
"""
# 회귀진단
plot(model5, col = palette(),bg = palette())

# 변수간 중요도 확인
# 사용자정의함수 relweights (참고사이트 : https://bioinformaticsandme.tistory.com/290?category=807358)
relweights <- function(fit,...){
  
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}

# 변수간 상대적 중요도 시각화
model5 <- lm(dolbomplus~ schools+ multicul + gross + edubud + edubudper, data = seoul_data)
result = relweights(model5, col=palette())
result

# ggplot2를 사용하여 시각화 (참고사이트 : https://bioinformaticsandme.tistory.com/290?category=807358)
plotRelWeights=function(fit){
  data<-relweights(fit)
  data$Predictors<-rownames(data)
  p <-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+ 
       geom_bar(stat="identity",width=0.5)+
       ggtitle("Relative Importance of Predictor Variables")+
       ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
       geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
       guides(fill=FALSE)+
       coord_flip()+
       theme_classic()
  p
}

plotRelWeights(model5)


# normalization
seoul_norm <- transform(seoul_data, 
                        z.schools = scale(schools),
                        z.multicul = scale(multicul),
                        z.edubudper = scale(edubudper),
                        z.edubud = scale(edubud),
                        z.gross = scale(gross))
head(seoul_norm)

seoul_norm <- lm(dolbomplus ~ z.schools + z.multicul + z.edubudper + z.edubud + z.gross , data = seoul_norm)    
summary(seoul_norm)



# 전국 
# 전국 데이터 전처리
"""
서울모델을 통해 결정된 예측변수만을 사용하면 되기에 해당하는 데이터들만 전처리하기로 한다.
"""

#전국_학교돌봄현황.xlsx파일에서 사용할 돌봄운영학교수(schools) 변수
View(nationwide_total)
nationwide_total <- nationwide_total[-c(1,18), ]

#전국_마을돌봄현황.xlsx파일에서 사용할 마을돌봄개수(vil_dolbom)변수
View(nationwide_vil_dolbom)
nationwide_vil_dolbom <- nationwide_vil_dolbom[-c(1,2,19),]
nationwide_vil_dolbom$지역아동센터수 <- as.numeric(nationwide_vil_dolbom$지역아동센터수)
nationwide_vil_dolbom$다함께돌봄센터수 <- as.numeric(nationwide_vil_dolbom$다함께돌봄센터수)
nationwide_vil_dolbom$초등청소년방과후아카데미수 <- as.numeric(nationwide_vil_dolbom$초등청소년방과후아카데미수)
nationwide_vil_dolbom$vil_dolbom <- nationwide_vil_dolbom$지역아동센터수 + nationwide_vil_dolbom$다함께돌봄센터수 + nationwide_vil_dolbom$초등청소년방과후아카데미수

#학교돌봄교실개수(sch_dolbom)와 마을돌봄개수(vil_dolbom)를 활용해 종속변수(dolbomplus) 만들기 
View(nationwide_total)
nationwide_total$dolbomplus <- nationwide_total$sch_dolbom + nationwide_vil_dolbom$vil_dolbom


#학생수현황.xlsx 파일에서 사용할 다문화학생수(multicul)변수
View(nationwide_students)
names(nationwide_students) <- c("regname", "students", "multicul")
nationwide_students <- nationwide_students[-c(1,2,19),]
nationwide_students$multiculper <- as.numeric(nationwide_students$multicul) / as.numeric(nationwide_students$students)

#전국_나라살림리포트.xlsx 파일에서 예산대비교육경비보조금비율 (edubudper) 변수 
nationwide_edubudper <- read_xlsx(file.choose())
View(nationwide_edubudper)
nationwide_edubudper <- nationwide_edubudper[,c(1,3)]
names(nationwide_edubudper) <- c("regname", "edubudper")

#교육관련지원예산현황.xlsx 파일에서 서울시 구별 교육경비보조금(edubud) 변수 만들기 
nationwide_edubud <- read_xlsx(file.choose()) 
View(nationwide_edubud)

nationwide_edubud <- nationwide_edubud %>% filter(구분 =="교육경비보조금") 
nationwide_edubud <- nationwide_edubud %>% group_by(지역명)  %>%  summarise(예산액합 = sum(예산액)) 
nationwide_edubud <- nationwide_edubud[c(9,8,6,12,5,7,11,10,2,1,17,16,14,13,4,3,15),]
nationwide_edubud <- nationwide_edubud[-c(1),]

names(nationwide_edubud) <- c("regname", "edubud")
nationwide_edubud$edubud <- as.character(nationwide_edubud$edubud)
nationwide_edubud$edubud <- substr(nationwide_edubud$edubud, 1, nchar(nationwide_edubud$edubud)-6)       #단위 백만원으로 통일

#전국지역내총생산 gross 변수
nationwide_gross <- read_xlsx(file.choose())
View(nationwide_gross)
nationwide_gross <- nationwide_gross[-c(1,2,3),c(1,10)]
names(nationwide_gross) <- c("regname", "gross")

# Test 테이블(전국) 만들기 
nationwide_table <- cbind(nationwide_total$regname,nationwide_total$dolbomplus,
                          nationwide_total$schools, nationwide_edubudper$edubudper, 
                          nationwide_edubud$edubud,nationwide_gross$gross, nationwide_students$multicul)

colnames(nationwide_table) <- c("regname","schools","dolbomplus","edubudper","edubud","gross","multicul")
View(nationwide_table)

# Test 테이블(전국) 변수 타입 확인 및 수정 
summary(nationwide_table)

nationwide_table <- transform(nationwide_table, 
                                     schools = as.numeric(schools), 
                                     dolbomplus = as.numeric(dolbomplus),
                                     edubudper = as.numeric(edubudper),
                                     edubud  = as.numeric(edubud),
                                     gross = as.numeric(gross),
                                     multicul = as.numeric(multicul)
                                     )

summary(nationwide_table)
View(nationwide_table)

# 독립변수간 상관분석 
nationwide_cor <- nationwide_table[,-c(1,3)]
cor(nationwide_cor)
corrgram(nationwide_cor, upper.panel = panel.conf) 

# 모델5를 통해 전국데이터에 적용한 결과값 
# 모델5의 dolbomplus = -3.2411333994 +  3.8393838992*schools + 0.0258592211*multicul  + (-0.0000005387)*gross + 0.0021347061 * edubud
summary(model5) 
nationwide_table1 <- nationwide_table
nationwide_table1$pred <- -3.2411333994 +  3.8393838992*nationwide_table$schools + 0.0258592211*nationwide_table$multicul  + (-0.0000005387)*nationwide_table$gross + 0.0021347061 * nationwide_table$edubud
nationwide_table1$diff <-  nationwide_table1$pred - nationwide_table1$dolbomplus
View(nationwide_table1)
result_table_model5 <- nationwide_table1[,c(1,3,8,9)]
#모델5로 전국에 적용한 돌봄실제값, 예측값, 차이값 
View(result_table_model5)

# 결과차이가 너무 많이 나서 모델5대신 추가 모델을 생성해 전국 데이터에 적용한 결과값
# 추가모델의 dolbomplus = -4.678331 + 3.973735*schools + 0.026012*multicul 

new_model = lm(dolbomplus ~ schools + multicul, data = seoul_data )
summary(new_model)
vif(new_model)
nationwide_table_new <- nationwide_table
nationwide_table_new$pred <-  -4.678331 + 3.973735*nationwide_table$schools + 0.026012*nationwide_table$multicul 
nationwide_table_new$diff <-  nationwide_table_new$pred - nationwide_table_new$dolbomplus
View(nationwide_table_new)
result_table_new <- nationwide_table_new[,c(1,3,8,9)]
#추가모델로 전국에 적용한 돌봄실제값, 예측값, 차이값 
View(result_table_new)


"""
결론 : 경기, 경남, 전남, 인천, 전북, 인천, 경북(모델5 model5)이 가장 시급 
       경기, 경남, 전남, 경북, 전북, 인천, 경북(추가모델 newmodel)이 가장 시급 
"""

##############################################################################################
##############################################################################################

#2020.09.10
#PPT에 나와있는 모델

#전국전처리.xlsx 파일 불러들일 것 
region_total1 <- read_xlsx(file.choose())
View(region_total1)
region_total <- region_total1[,-c(2,3,4,5,8,12)]
region_total <- region_total[-c(1,2,19),]
names(region_total) <- c("regname","dolbomplus","schools","students","multicul","fin_19","little_lib","libtotal","lib")
View(region_total)

region_total$regname       = as.character(region_total$regname)
region_total$schools       = as.numeric(region_total$schools)
region_total$dolbomplus    = as.numeric(region_total$dolbomplus)
region_total$multicul      = as.numeric(region_total$multicul)
region_total$fin_19        = as.numeric(region_total$fin_19)
region_total$little_lib    = as.numeric(region_total$little_lib)
region_total$lib           = as.numeric(region_total$lib)
region_total$libtotal      = as.numeric(region_total$libtotal)


summary(region_total)

# PPT에 나온 최종모델 
# sl.xlsx 파일 불러들이기
sl_raw <- read_xlsx(file.choose())                                   
View(sl1)
sl <- sl_raw[c(-1)]
sl.null   <- lm(dolbomplus ~ 1, data = sl)
sl_model  <- lm(dolbomplus ~ ., data = sl)

step(sl.null, scope=list(lower=sl.null, upper=sl_model), direction="both")

sl_model<- lm(formula = dolbomplus ~ schools + Slibpub + multicul + edum_19 + 
                lib + fin_19 + gross + base, data = sl)

sl_model<- lm(formula = dolbomplus ~ schools + multicul + fin_19 + lib + little_lib, data = sl)

vif(sl_model)
summary(sl_model)

sl_result = relweights(sl_model, col=palette())
sl_result



#dolbomplus = -27.350717 + 3.092360*schools + 0.015365*multicul + 0.784119*fin_19  + (-0.677161)*lib +  0.339872*little_lib
nation_table_ppt<- region_total
nation_table_ppt$pred <-  -27.350717 + 3.092360 * region_total$schools + 0.015365 * region_total$multicul + 0.784119 * region_total$fin_19 + (-0.677161) * region_total$lib + 0.339872 * region_total$little_lib 
nation_table_ppt$diff <-  nation_table_ppt$pred - nation_table_ppt$dolbomplus

#PPT에 있는 모델로 전국에 적용한 돌봄실제값, 예측값, 차이값 
View(nation_table_ppt)
View(nation_table_ppt)
ppt_result_table  <- nation_table_ppt[ ,c(1,2,10,11)]
View(ppt_result_table)


"""
결론
대전과 세종을 제외하고 전체적으로 돌봄개수 부족
가장 시급한 지역 경기,경북,경남, 강원, 충남 순
"""

#PPT모델  normalization

sl_norm <- transform(sl, 
                        z.schools = scale(schools),
                        z.fin_19 = scale(fin_19),
                        z.lib = scale(lib),
                        z.little_lib = scale(little_lib),
                        z.multicul = scale(multicul))
head(sl_norm)
sl_norm <- lm(dolbomplus ~ z.schools + z.fin_19 + z.lib + z.little_lib + z.multicul , data = sl_norm)    
summary(sl_norm)

# PPT모델 회귀진단
plot(sl_model, col = palette())

# PPT모델 변수중요도 시각화
sl_result = relweights(sl_model, col=palette())
sl_result

plotRelWeights(sl_model)




# 2020.09.10.수정
# 종속변수(dolbomplus) 기입 오류로 인해 동일 예측변수조건으로 모델 새로 설정시도(PPT모델에서 수정된 값)
# sl_new.xlsx 파일 불러들일 것
sl_new_raw <- read_xlsx(file.choose())       
View(sl_new_raw)
sl_new <- sl_new_raw[,-c(1,4,5)]

model.null1       <- lm(dolbomplus ~ 1, data = sl_new)
sl_new_model      <- lm(dolbomplus ~., data =  sl_new)

step(model.null1 , scope=list(lower=model.null1 , upper= sl_new_model), direction="both")

sl_new_model <- lm(formula = dolbomplus ~ schools + multicul + fin_19 + lib + little_lib, data = sl_new)
sl_new_model_rev <- lm(formula = dolbomplus ~ schools + multicul + fin_19 + lib + libtotal, data = sl_new)

vif(sl_new_model)
summary(sl_new_model)

vif(sl_new_model_rev)
summary(sl_new_model_rev)

sl__new_result = relweights(sl__new_model, col=palette())
sl__new_result

sl__new_rev_result = relweights(sl__new_model_rev, col=palette())
sl__new_rev_result


#수정된 모델 normalization
# sl__new_model(예측변수 PPT에 나온 그대로 기존과 동일)
sl_new_norm <- transform(sl_new, 
                       z.schools = scale(schools),
                       z.fin_19 = scale(fin_19),
                       z.lib    = scale(lib),
                       z.little_lib = scale(little_lib),
                       z.multicul = scale(multicul))

head(sl_new_norm)

sl_new_norm <- lm(dolbomplus ~ z.schools + z.fin_19 + z.lib + z.little_lib + z.multicul , data = sl_new_norm)    
summary(sl_new_norm)


# sl__new_model_rev(예측변수 수정 기존 little_lib에서 libtotal로 수정, 갯수는 동일)
sl_new_rev_norm <- transform(sl_new, 
                         z.schools = scale(schools),
                         z.fin_19 = scale(fin_19),
                         z.lib = scale(lib),
                         z.libtotal = scale(libtotal),
                         z.multicul = scale(multicul))

head(sl_new_rev_norm)

sl_new_rev_norm <- lm(dolbomplus ~ z.schools + z.fin_19 + z.lib + z.libtotal + z.multicul , data = sl_new_rev_norm)    
summary(sl_new_rev_norm)


# 회귀진단
# sl__new_model
plot(sl_new_model, col = palette())
# sl__new_model_rev
plot(sl_new_model_rev, col = palette())


#PPT모델과 동일한 예측변수모델(sl__new_model) 전국에 적용할 식 
#dolbomplus =-5.830351 + 3.610526*schools +  0.023008 *multicul +  0.354712*fin_19  + (-0.593746)*lib +  0.297911*little_lib    
summary(sl_new_model)
vif(sl_new_model)
nation_table_ppt_rev1<- region_total
nation_table_ppt_rev1$pred <-  -5.830351 + 3.610526 * region_total$schools + 0.023008 * region_total$multicul + 0.354712 * region_total$fin_19 + (-0.593746) * region_total$lib + 0.297911 * region_total$little_lib 
nation_table_ppt_rev1$diff <-  nation_table_ppt_rev1$pred - nation_table_ppt$dolbomplus

#PPT에 있는 모델로 전국에 적용한 돌봄실제값, 예측값, 차이값 
View(nation_table_ppt_rev1)
ppt_rev1_result_table  <- nation_table_ppt_rev1[ ,c(1,2,10,11)]
View(ppt_rev1_result_table)

"""
결론
대전과 세종을 제외하고 전체적으로 돌봄개수 부족
가장 시급한 지역 경기,경북,경남, 강원, 충남 순
"""

#PPT모델에서 바뀐 예측변수모델(sl__new_model) 전국에 적용할 식 
#dolbomplus = -8.150920 + 3.652743*schools +  0.020592*multicul + 0.396540 *fin_19  + ( -0.925375)*lib +  0.309115*libtotal
summary(sl_new_model_rev)
vif(sl_new_model_rev)
nation_table_ppt_rev2<- region_total
nation_table_ppt_rev2$pred <-  -8.150920+ 3.652743 * region_total$schools + 0.020592** region_total$multicul + 0.396540 * region_total$fin_19 + ( -0.925375) * region_total$lib + 0.309115 * region_total$libtotal
nation_table_ppt_rev2$diff <-  nation_table_ppt_rev2$pred - nation_table_ppt$dolbomplus

#PPT모델에서 바뀐 예측변수모델(sl__new_model)로 전국에 적용한 돌봄실제값, 예측값, 차이값 
View(nation_table_ppt_rev2)
ppt_rev2_result_table  <- nation_table_ppt_rev2[ ,c(1,2,10,11)]
View(ppt_rev2_result_table)

"""
결론
세종을 제외하고 전체적으로 돌봄개수 부족
가장 시급한 지역 경기,경북,경남, 강원, 충남 순
"""


"""
적용모델들 모두 경기, 경북, 경남을 돌봄개수가 가장 부족한 지역이라는 것을 나타내고 있다.
"""
