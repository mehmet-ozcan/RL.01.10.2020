# R Ladies & Mülkiye Digital Lab
# Regresyon Analizine Giriş
# 1.11.2020
# Dr. Mehmet Özcan


############################
# ÇALIŞMA ORTAMI BELİRLEME #
#setwd("E:/Drive/Calismalar/Dersler/Uygulamali Ekonometri/Ders-1/")
setwd("C:/Users/Ozc/Desktop/R Ladies")


###############
# VERİ OKUTMA #

# Text Dosyasından
data <- read.table("data.txt") # Eğer Çalışma Dosyası Tanımlanmışsa
data <- read.table("C:/Users/Ozc/Desktop/R Ladies/data.txt") # Eğer Çalışma Dosyası Tanımlanmamışsa

# Excel .csv Dosyasından
data <- read.table("data.csv", header=TRUE ,sep=";")

# Zaman Serisine Dönüştürme
datat <-ts(data, start=c(2008,3), frequency=12) 


########################
# DEĞİŞKEN DÖNÜŞÜMLERİ #

# Doğal Logaritma
m2 <- log(data[,1])
p  <- log(data[,2])
y  <- m2-p

# 10 tabanında Logaritma
m2_10 <-log10(data[,1]) 
 p_10 <-log10(data[,2])
 y_10 <-m2_10-p_10 

# Fark Alma
fm2 <- diff(m2,1)
 fp <- diff(p,1)
 fy <- diff(y,1)

# Yüzde Değişim (Logaritmaya Göre)

# 1 ay
m2p <- diff(m2,1)*100
pp  <- diff(p,1)*100
yp  <- diff(y,1)*100
# 12 ay
m2p1 <- diff(m2,12)*100
pp1  <- diff(p,12)*100
yp1  <- diff(y,12)*100


#############################
# TANIMLAYICI İSTATİSTİKLER #

# ortalama
mean(m2p)
mean(pp)
mean(yp)

# medyan
median(m2p)
median(pp)
median(yp)

# varyans
var(m2p)
var(pp)
var(yp)

# standart sapma
sd(m2p)
sd(pp)
sd(yp)

# minimum değeri
min(m2p)
min(pp)
min(yp)

# maksimum değer
max(m2p)
max(pp)
max(yp)
##################
install.packages("moments")
library(moments)
##################
# çarpıklık
skewness(m2p)
skewness(pp)
skewness(yp)
# basıklık
kurtosis(m2p)
kurtosis(pp)
kurtosis(yp)


####################################
# VERİ GRAFİKLERİNİN OLUŞTURULMASI #

# zaman serisi olarak tanıtma
m2p<-ts(m2p, start=c(2008,4), frequency=12)
 pp<-ts(pp,  start=c(2008,4), frequency=12)
 yp<-ts(yp,  start=c(2008,4), frequency=12)

# çizgi grafiği

plot(m2p,type="l",xlab="Zaman", ylab="M2 Para Arzı (% Değişim)",main="Zaman Serisi Grafiği", lwd=1.5, col="red")
plot( pp,type="l",xlab="Zaman", ylab="Fiyatlar Genel Düzeyi (% Değişim)",main="Zaman Serisi Grafiği", lwd=1.5, col="blue")
plot( yp,type="l",xlab="Zaman", ylab="Reel Para Arzı (% Değişim)",main="Zaman Serisi Grafiği", lwd=1.5, col="orange")


########################
## BİRİM KÖK SINAMASI ##
# ADF Testi

install.packages("urca")
install.packages("fUnitRoots")
library(urca)
library(fUnitRoots)

# Gecikme Belirlemek için...
test1 <- ur.df(m2p, type = c("none"),  lags = 10, selectlags = c("BIC")) 
test2 <- ur.df(m2p, type = c("drift"), lags = 10, selectlags = c("BIC")) 
test3 <- ur.df(m2p, type = c("trend"), lags = 10, selectlags = c("BIC")) 

summary(test1)
summary(test2)
summary(test3)

# p değerini hesaplamak için
adfTest(m2p, lags = 3, type = c("nc"))
adfTest(m2p, lags = 2, type = c("c"))
adfTest(m2p, lags = 2, type = c("ct"))


###################################
# REGRESYON PARAMETRELERİ TAHMİNİ #

# Veri Matrisi Oluşturma
Xdata <- cbind(yp,m2p,pp)
Xdata <- as.matrix(Xdata)
# İki fonksiyon iç içe de yazılabilir.
Xdata <- as.matrix(cbind(yp,m2p,pp))

# Gecikme için paket
install.packages("dplyr")
library(dplyr)

# Veri Çerçevesi Oluşturma
Xveri <- data.frame(cbind(yp,m2p,pp))

# Gecikme almak
Xveri[,2] <- lag(Xveri[,2],1)
Xveri$m2p <- lag(Xveri$m2p,1)

# İlk satırı silmek
Xveri <- Xveri[-1,]

# Model Parametre tahmini #

# lm fonksiyonu

model1 <- lm(yp~m2p+pp,data=Xveri)
summary(model1)

model2 <- lm(yp~m2p+pp+0, data=Xveri) # Sıfır Noktasından Geçen Reg.
summary(model2)

# Trend Eklemek
length(Xveri[,1])
tt<-c(1:148)
Xveri[,4]<-tt
colnames(Xveri)<-c("yp","m2p","pp","t")

model3 <- lm(yp~m2p+pp+t,data=Xveri)
summary(model3)

# Kalıntıları elde etmek

eh <- as.matrix(model3$residuals)
eh <- ts(eh,start=c(2008,5), frequency=12)

# Kalıntı analizi

plot(eh,type="l",xlab="Zaman", ylab="Kalıntılar",main="Zaman Serisi Grafiği", lwd=1.5, col="red")
hist(eh,main="Kalıntıların Histogramı",xlab="Kalıntılar",ylab="Frekans")

mean(eh)
median(eh)
var(eh)
sd(eh)
min(eh)
max(eh)
library(moments)
skewness(eh)
kurtosis(eh)

# Normallik testi

install.packages("tseries")
library(tseries)
jarque.bera.test(eh)


##############################
## MATRİS CEBİRİ İLE TAHMİN ##

# Sabit terim için 1 değişkeni ekleme

ct <- rep(1, length(Xveri[,1]))
Xveri[,5] <- ct
colnames(Xveri) <- c("yp","m2p","pp","t","c")

# Matris ile tahmin

x <- as.matrix(cbind(Xveri[,5], Xveri[,4], Xveri[,2], Xveri[,3]))
# yada
x1 <- as.matrix(cbind(Xveri$c, Xveri$t, Xveri$m1p, Xveri$pp))

y <- as.matrix(cbind(Xveri[,1]))
# yada
y1 <- as.matrix(cbind(Xveri$yp))

# En Küçük Kareler Algoritması
n      <- length(x[,1])
kx     <- ncol(x)
xx     <- solve(t(x)%*%x)
bols   <- xx%*%(t(x)%*%y)
eols   <- y - x%*%bols
sols   <- t(eols)%*%eols                  # hata kareler toplamı
sigols <- sols/(n-kx)                     # hata varyansı tahmini
seols  <- sqrt(diag(xx)%*%sigols)         # parametre standart hataları
yhat   <- y-eols                          # y şapka
ess    <- sum(yhat^2)                     # açıklanan değişim
ssr    <- sols                            # açıklanamayan değişim (hata kareler toplamı)
tss    <- sum(y^2)                        # toplam değişim
tss    <- t(y)%*%y                         
Fstat  <- (ess/(kx-1))/(ssr/(n-kx))       # F istatistiği
Rsq    <- ess/tss                         # R2 değeri


####################
## KUKLA DEĞİŞKEN ##

# Covid-19 Kuklası #
# İlk Vaka 11 Mart 2020'de Açıklandı.
# 143. gözlem olduğunu tespit ediyoruz.

# Sabit Kuklası #

dd_cv <- rep(0,148) # Sayılar ile kukla oluşturma.
dd_cv[143:148] <- 1 
Xveri[,6] <- dd_cv
colnames(Xveri)<-c("yp","m2p","pp","t","c","dd_cv")

model4 <- lm(yp~m2p+pp+t+dd_cv,data=Xveri)
summary(model4)

# Trend Kuklası #

dt_cv <- rep(0,148)
dt_cv[143:148] <- (c(143:148)-142)
Xveri[,7] <- dt_cv
colnames(Xveri)<-c("yp","m2p","pp","t","c","dd_cv","dt_cv")

model5 <- lm(yp~m2p+pp+t+dt_cv,data=Xveri)
summary(model5)

# İdeal Model #

model6 <- lm(yp~m2p+pp+dd_cv,data=Xveri)
summary(model4)

# Diagnostics #
eh <- as.matrix(model4$residuals)
eh <- ts(eh,start=c(2008,5), frequency=12)
library(tseries)
jarque.bera.test(eh)

     ############
     ## UYARI! ##
     ############

#####################
## DEĞİŞEN VARYANS ##

# Scater Plot ile Görsel Tanı #

plot(Xveri[,2],Xveri[,1], type="p",xlab="M2 Para Arzı", ylab="Reel para Arzı")
abline(model4)

plot(Xveri[,3],Xveri[,1], type="p",xlab="Enflasyon", ylab="Reel para Arzı")
abline(model4)

# D. Varyans Sınamaları #

install.packages("lmtest")
library(lmtest)

# Breusch-Pagan Testi #

bptest(model4)

# White Testi #

bptest(model4, ~ m2p*pp + I(m2p^2) + I(pp^2), data=Xveri)

# D. Varyans Düzeltilmesi #
# White (1980) Dirençli Standart Hatalar
install.packages("car")
library(car)

modelW <- hccm(model4, type="hc1")
coeftest(model4, vcov.=modelW)


###################
## OTOKORELASYON ##

# Otokorelasyon Sınamaları #
install.packages("forecast")
library(forecast)

Arima(eh, order=c(1,0,0)) # 6 gecikmeye kadar bak.

# Breusch-Godfrey Testi #
bgtest(model4, order=3, type="Chisq", fill=0)

# Nihai Çözüm: Newey & West (1987) Tahmincisi #

install.packages("sandwich")
library(sandwich)
library(lmtest)

mod.X <- coeftest(model4, vcov.=NeweyWest(model4))

# Son #