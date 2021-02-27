#monthly S&P 500 log returns and 3 month T-bill returns
data1<-read.table('m_sp500ret_3mtcm.txt',header=T)
#monthly log returns of ten stocks
data2<-read.table('m_logret_10stocks-1.txt',header=T)


head(round(data1[2:3],4),5)
head(round(data2[2:11],4),5)

#Fit CAPM for each stock + Determine if CAPM applies to each
attach(data1)
attach(data2)
logsp500 <- diff(sp500)-X3mTCM[1:156]/(100*12)
logAAPL <- diff(AAPL)-X3mTCM[1:156]/(100*12)
logADBE <- diff(ADBE)-X3mTCM[1:156]/(100*12)
logADP <- diff(ADP)-X3mTCM[1:156]/(100*12)
logAMD <- diff(AMD)-X3mTCM[1:156]/(100*12)
logDELL <- diff(DELL)-X3mTCM[1:156]/(100*12)
logGTW <- diff(GTW)-X3mTCM[1:156]/(100*12)
logHP <- diff(HP)-X3mTCM[1:156]/(100*12)
logIBM <- diff(IBM)-X3mTCM[1:156]/(100*12)
logMSFT <- diff(MSFT)-X3mTCM[1:156]/(100*12)
logRCL <- diff(ORCL)-X3mTCM[1:156]/(100*12)

#fit linear regression models to stock on sp500 log returns 
lmAAPL<-lm(logAAPL~logsp500)
summary(lmAAPL)
lmADBE<-lm(logADBE~logsp500)
summary(lmADBE)
lmADP<-lm(logADP~logsp500)
summary(lmADP)
lmAMD<-lm(logAMD~logsp500)
summary(lmAMD)
lmDELL<-lm(logDELL~logsp500)
summary(lmDELL)
lmGTW<-lm(logGTW~logsp500)
summary(lmGTW)
lmHP<-lm(logHP~logsp500)
summary(lmHP)
lmIBM<-lm(logIBM~logsp500)
summary(lmIBM)
lmMSFT<-lm(logMSFT~logsp500)
summary(lmMSFT)
lmRCL<-lm(logRCL~logsp500)
summary(lmRCL)

