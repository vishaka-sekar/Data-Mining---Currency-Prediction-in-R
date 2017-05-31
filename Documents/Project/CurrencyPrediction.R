install.packages("rnn")
install.packages("XLConnect")
install.packages("readxl")
install.packages("forecast")
install.packages("tseries")
library(readxl)
library("XLConnect")
library("rnn")
library("forecast")
library("tseries")

Historical_Exchange_Rates <-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/Project papers/COEN 281 Term Project Papers -currency-exch-pred/Historical_Exchange_Rates.xlsm",sheet = 2 ,region = "B2:B10", header = FALSE)

X <-matrix(Historical_Exchange_Rates[,1], nrow=3)
Y <-X
#print(X)
#print(Y)
# train the model

model <-trainr(Y=Y,X=X,learningrate=0.5,hidden_dim=20,numepochs=10)
#print(model)

# plot(colMeans(model$error),type='o',
#      xlab='epoch',
#      ylab='errors'                  )

#predict

Yp <-predictr(model, X)
newY <-(c(Y))
newYp <-(c(Yp))
print(newY)
print(newYp)
plot(c(Y), type="o", col="blue", axes=FALSE, ann=FALSE)
lines(c(Yp), type="o", pch=22, lty=2, col="red")

plot(newY, type="o",col="blue", xlab = "X Values", ylab = "Y,Yp",xlim = c(0,10),ylim = c(0.95,1.07))
lines(newYp, col="red", pch=22, lty=2)
adf.test(Historical_Exchange_Rates[,1], alternative = "stationary")

arima(Historical_Exchange_Rates[,1], order=c(2,1,1))
arima.final<- arima(Historical_Exchange_Rates,c(2,1,1))
predicted <-predict (arima.final,n.ahead =10)
predicted


