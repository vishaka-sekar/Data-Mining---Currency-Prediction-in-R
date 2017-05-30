
install.packages("rnn")
install.packages("XLConnect")
install.packages("forecast")
install.packages("tseries")
library("XLConnect")
library("rnn")
library("forecast")
library("tseries")

Historical_Exchange_Rates <-readWorksheetFromFile("~/Historical_Exchange_Rates.xlsm",sheet = "Sheet1", 
region = "B2:B10", header = FALSE)

X<-matrix(Historical_Exchange_Rates[,1], nrow=2)
Y <- X

# train the model
 model <- trainr(Y=Y,
                X=X,
                learningrate   =  0.5,
                 hidden_dim     = 20  ,
                numepochs = 10)


# plot(colMeans(model$error),type='o',
#      xlab='epoch',
#      ylab='errors'                  )

 #predict

Yp <- predictr(model, X)
print(c(Y))
print(c(Yp))

plot(c(Y), type="o", col="blue", axes=FALSE, ann=FALSE)
lines(c(Yp), type="o", pch=22, lty=2, col="red")

#ADF Test for checking if it is  stationary series or not  

adf.test(Historical_Exchange_Rates[,1], alternative = "stationary")

fit <- Arima(Historical_Exchange_Rates[,1], order=c(0,0,3))
plot(forecast(fit))
plot(residuals(fit))

# another implementation of arima

# fit <- auto.arima(Historical_Exchange_Rates[,1], lambda=0, d=0, D=1, max.order=9,
#                   stepwise=FALSE, approximation=FALSE)
# plot(forecast(fit))