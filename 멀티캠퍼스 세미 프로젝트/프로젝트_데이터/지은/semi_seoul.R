#### semi_seoul

local <- read.table(file.choose(), sep='\t')                  # 자치구별 지역돌봄센터.txt
head(local)
local <- local[5:29,c(2,3,4)]
names(local) <- c("gu","facilities","users")
head(local)
local$facilities
local_order <- local[order(local$gu),]
local_order
fac <- c(3 , 20, 22 ,19 ,27 ,17 ,21 ,27, 21 ,18 ,13, 23, 11, 8,  7,  11, 27, 19, 24, 15, 5,  27, 12, 5,  25)
class(fac)
local_order <- cbind(local_order, fac)
#local_order$facilities <- as.integer(local_order$facilities)
#local$users <- as.numeric(local$users)
#class(local_order$fac)


finance <- read.table(file.choose(), sep='\t', header=T)        # 자치구별 재정자립도.txt
finance
finance_gu <- finance[4:28,2:3]
finance_gu <- finance_gu[order(finance_gu$자치구별),]
names(finance_gu) <- c("gu", "fin")
finance_gu$fin
class(finance_gu$fin)

#test_table_01 <- cbind(finance_gu, slsl$dolbomplus)
#test_table_01[order(test_table_01$fin),]



seoulbmi <- read.table(file.choose(), sep='\t', header=T)    # 서울 구별 비만지수(초5,초6).txt
seoulbmi
class(seoulbmi$bmi)


#smoke <- read.table(file.choose(), sep='\t', header=T)                # 현재흡연율_서울시.txt
#smoke
#smoke$smnumbers <- smoke$응답자수..명. * smoke$조율.... / 100
#smoke <- smoke[2:26,c(1,4,8)]
#names(smoke) <- c("gu","smoper","sm")


#alcohol <- read.table(file.choose(), sep='\t', header=T)
#alcohol
#alcohol$alnumbers <- alcohol$응답자수..명. * alcohol$조율.... / 100
#alcohol <- alcohol[2:26,c(1,3,7)]
#alcohol
#names(alcohol) <- c("gu", "alper", "al")


#medi <- read.table(file.choose(), sep='\t', header=T)       # 서울시 의료기관종사 의료인력 (구별) 통계 2018.txt
#medi
#medi <- medi[2:26,c(2,4,9,10)]
#medi$doctt <- medi$의사 + medi$간호사 + medi$간호조무사


slsl <- read.table(file.choose(), sep='\t')       # slsl.txt
head(slsl)
local
slsl$dolbomplus <- slsl$dolbom_total + local_order$fac

slsl <- cbind(slsl, finance_gu$fin, seoulbmi$bmi)
#slsl <- cbind(slsl, smoke$smoper, smoke$sm, alcohol$alper, alcohol$al)
slsl <- cbind(slsl, medi$doctt)
head(slsl)
colnames(slsl) <- c("gu","schools","students","dolbom_avg","students_par_per", "hanbumo","base",
                    "dolbom_total","dolbomplus","fin","bmi", "medi")
#class(slsl$students)
#slsl$students
#cor(slsl$dolbomplus, slsl$fin)
#plot(slsl$dolbomplus, slsl$fin)

#slsl$dolbomplus

#regression
slsl_lm_plus.null <- lm(dolbomplus~1, data = slsl)
slsl_lm_plus <- lm(dolbomplus ~ students+bmi+fin+medi, data = slsl)
library(car)
vif(slsl_lm_plus)
step(slsl_lm_plus.null, scope=list(lower=slsl_lm_plus.null, upper=slsl_lm_plus), direction="both")
summary(slsl_lm_plus)

predict(slsl_lm_plus)

#cbind(slsl$gu, slsl$dolbomplus , slsl$fin)
#slsl$gu
#class(slsl$gu)

library(olsrr)
ols_plot_resid_stud(slsl_lm_plus)
ols_plot_resid_lev(slsl_lm_plus)            # checking outliers, leverages


slsl_checking <- slsl[,c("gu","dolbomplus","students",
        "fin","bmi")]
library(writexl)
write_xlsx(slsl_checking, path = 'C:/Users/kimje/Desktop/slsl_checking.xlsx')

class(slsl_checking)

# deleting outliers, leverages
slsl_wo <- slsl[c(-2,-14,-17),]
slsl_wo_lm_plus.null <- lm(dolbomplus~1, data = slsl_wo)
slsl_wo_lm_plus <- lm(dolbomplus ~ students+bmi+fin, data = slsl_wo)
library(car)
vif(slsl_wo_lm_plus)
step(slsl_wo_lm_plus.null, scope=list(lower=slsl_wo_lm_plus.null, upper=slsl_wo_lm_plus), direction="both")
summary(slsl_wo_lm_plus)


#normalization
slsl_norm <- transform(slsl, 
                       z.dolbomplus = scale(dolbomplus),
                       z.students = scale(students),
                       z.hanbumo = scale(hanbumo),
                       z.bmi = scale(bmi),
                       z.fin = scale(fin))
head(slsl_norm)
slsl_norm_lm <- lm(dolbomplus ~ z.students+z.bmi+z.fin , data = slsl_norm)    # 한부모는 사각지대     ######## finalfinal
summary(slsl_norm_lm)

slsl_norm_checking <- slsl_norm[,c("gu","dolbomplus","students","fin","bmi","z.dolbomplus","z.students","z.bmi","z.fin")]
write_xlsx(slsl_norm_checking, path = 'C:/Users/kimje/Desktop/slsl_normal_checking.xlsx')



# boxplot
boxplot(slsl$students)
boxplot(slsl$fin)
boxplot(slsl$bmi)
