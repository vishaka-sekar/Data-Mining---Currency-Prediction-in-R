
install.packages("rnn")
install.packages("XLConnect")

library("XLConnect")
library("rnn")

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

plot(c(Y), type="o", col="blue", xlab="x values", ylab="y , yp" ) 
lines(c(Yp),type = "o",col="red")
