
library(car)
# Source data is publicly available by Aalto universty course MS-C2128 - Ennustaminen ja aikasarja-analyysi
# https://mycourses.aalto.fi/mod/assign/view.php?id=70445
kansantalous = read.table("t38.txt", header = T, sep = " ")
attach(kansantalous)
# CONS kokonais kulutus (mrd mk)
# INC tulot (mrd mk)
# INFLAT inflaatio (%)
pairs(kansantalous)
tKT = ts(kansantalous, frequency = 4, start = 1953)

tCONS = ts(CONS, frequency = 4, start = 1953)
tINC = ts(INC, frequency = 4, start = 1953)
tINFLAT = ts(INFLAT, frequency = 4, start = 1953)

DtCONS = diff(tCONS)
DtINC = diff(tINC)
DtINFLAT = diff(tINFLAT)

DtKT = diff(tKT)
plot(DtKT)
plot(tKT)

malliN = lm(tCONS~tINC+tINFLAT)
summary(malliN)
plot(malliN$fit)



malliD = lm(DtCONS~DtINC+DtINFLAT)
summary(malliD)
plot(malliD$fit)

# evaluation of the model

hist(malliD$res)
shapiro.test(malliD$res)
plot(malliD$res)
plot(malliD$res, malliD$fit)

vif(malliD)

malli = diffinv(malliD$fit, xi = CONS[1])
plot(malli)
plot(CONS)

malli = diffinv(malliD$fit, xi = CONS[1]) 
tMalli = ts(malli, frequency = 4, start = 1953) 
plot(tMalli,ylab = "Consumption", col="red", lwd = 1.5)
lines(tCONS,col="blue",lwd=1.5) # adds a line for health expenditures
legend(1982,890,c("Real CONS","Modeled CONS"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))