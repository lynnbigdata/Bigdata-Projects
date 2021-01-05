library(readxl)
sl_elmnt <- read.table(file.choose(), sep='\t', header=T)
sl_elmnt

install.packages("writexl")
library(writexl)
write_xlsx(sl_elmnt, path = 'C:/Users/i/Desktop/sl_elmnt.xlsx')

length(sl_elmnt)
sl_elmnt_useful <- sl_elmnt[1:26,c(2,3,4,5)]
colnames(sl_elmnt_useful) <- c("gu","schools","classes","students")
sl_elmnt_useful
sl_elmnt_useful_sorted <- sl_elmnt_useful[order(sl_elmnt_useful$gu),]
seoul_students <- sl_elmnt_useful_sorted[,c(1,4)]



local <- read.table(file.choose(), sep='\t')                  # 자치구별 지역돌봄센터.txt
head(local)
local <- local[5:29,c(2,3,4)]
names(local) <- c("gu","facilities","users")
head(local)
local <- local[order(local$gu),]
local$facilities <- as.numeric(local$facilities)
local$users <- as.numeric(local$users)
class(local$facilities)

finance <- read.table(file.choose(), sep='\t', header=T)        # 자치구별 재정자립도.txt
finance
finance_gu <- finance[4:28,2:3]
finance_gu <- finance_gu[order(finance_gu$자치구별),]
names(finance_gu) <- c("gu", "fin")
finance_gu$fin

slsl$fin <- finance_gu$fin


slsl <- read.table(file.choose(), sep='\t', header=T)       # 서울시 구별 현황_hanbumo.txt
head(slsl)

# smoke <- read.table(file.choose(), sep='\t', header=T)                # 현재흡연율_서울시.txt
# smoke
#smoke$smnumbers <- smoke$응답자수..명. * smoke$조율.... / 100
#smoke <- smoke[2:26,c(1,4,8)]
#names(smoke) <- c("gu","smoper","sm")

#alcohol <- read.table(file.choose(), sep='\t', header=T)
#alcohol
#alcohol$alnumbers <- alcohol$응답자수..명. * alcohol$조율.... / 100
#alcohol <- alcohol[2:26,c(1,3,7)]
#alcohol
#names(alcohol) <- c("gu", "alper", "al")

seoulbmi <- read.table(file.choose(), sep='\t', header=T)
seoulbmi
class(seoulbmi$bmi)


slsl <- cbind(slsl, finance_gu$fin, seoulbmi$bmi)
head(slsl)
colnames(slsl) <- c("gu", "schools", "students", "dolbom", "dolbomper", "hanbumo","base", "fin", "bmi")
slsl


#dolbomplus
head(slsl)
cbind(slsl$gu,slsl$dolbom, local$facilities, slsl$dolbomplus)
slsl$dolbomplus <- slsl$dolbom + local$facilities
slsl_lm_plus.null <- lm(dolbomplus~1, data = slsl)
slsl_lm_plus <- lm(dolbomplus ~ fin+students+bmi , data = slsl)     ######## finalfinal
slsl_lm_plus
vif(slsl_lm_plus)
max(vif(slsl_lm_plus))
step(slsl_lm_plus.null, scope=list(lower=slsl_lm_plus.null, upper=slsl_lm_plus), direction="both")
summary(slsl_lm_plus)
vif(slsl_lm_plus)
ols_plot_resid_stud(slsl_lm_plus)
ols_plot_resid_lev(slsl_lm_plus)
slsl[c(14,25),]

### 돌봄교실만 했을 때보다 지역구시설을 합했을 때 정확도가 더 올라가고, 재정상태의 -정도도 훨씬 더 세짐
## 부자구일수록 사립을 더 많이 보내는 거 아닐가




# cor.test(slsl$students, slsl$hanbumo)
# cor.test(slsl$students, slsl$base)
# cor.test(slsl$hanbumo, slsl$base)
cor.test(slsl$hanbumo, slsl$bmi)
cor(slsl$hanbumo, slsl$bmi)

attach(slsl)
slsl_lm.null <- lm(dolbom~1)
# slsl_lm <- lm(dolbom ~ students+hanbumo+base+smoper+sm+alper+al, data = slsl)
slsl_lm <- lm(dolbom ~ students+hanbumo+base+fin , data = slsl)    
step(slsl_lm.null, scope=list(lower=slsl_lm.null, upper=slsl_lm), direction="both")
# step(slsl_lm.null, scope=list(lower=slsl_lm.null, upper=slsl_lm), direction="backward")
summary(slsl_lm)

#vif
library(car)
vif(slsl_lm)

install.packages("olsrr")
library(olsrr)
ols_plot_resid_stud(slsl_lm)
ols_plot_resid_lev(slsl_lm)

slsl[6,]
prd <- 2.515e+00 + 8.439e-06*19831 + 4.359e-05*407 + 2.462e-04* 947
(3.6-prd)/3.6

predict(slsl_lm_plus)
result <- cbind(slsl["dolbomplus"], predict(slsl_lm_plus))
names(result) <- c("dolbomplus", "predict")
result
errrate <- abs((result$dolbomplus - result$predict)/result$dolbomplus*100)
result <- cbind(result, errrate)
result
mean(errrate)




# normalization
slsl_norm <- transform(slsl, 
                       z.students = scale(students),
                       z.hanbumo = scale(hanbumo),
                       z.base = scale(base), 
                       z.bmi = scale(bmi),
                       z.fin = scale(fin))
head(slsl_norm)
slsl_norm_lm <- lm(dolbomplus ~ z.students+z.hanbumo+z.bmi+z.fin , data = slsl_norm)    # 한부모는 사각지대     ######## finalfinal
summary(slsl_norm_lm)




slsl[,c("gu","dolbomplus","dolbom","fin")]
finance_gu[order(finance_gu$fin),]





# shapiro
# check the normality of the residuals
res <- residuals(slsl_lm_plus)
shapiro.test(res)






head(slsl)
# vif dummy
sl_dum.null <- lm(dolbomplus ~ 1, data = slsl)
sl_dum.full <- lm(dolbomplus ~ students+hanbumo+fin+bmi, data = slsl)    
vif(sl_dum.full)
max(vif(sl_dum.full))
