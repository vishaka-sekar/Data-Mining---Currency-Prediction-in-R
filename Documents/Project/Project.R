install.packages("EMD")
install.packages("readxl")
install.packages("rnn")
install.packages("forecast")
install.packages("gdata")
install.packages("tseries")
install.packages("xlsx")
library(EMD)
library(rnn)
library(forecast)
library(gdata)
library(readxl)
library(tseries)
library(rnn)
library(XLConnect)
library(xlsx)
#######################################################################################################################################################################################
##########################################Fetching Data's for training the models and Plotting the trained and actuals ################################################################
USDINRtrain_data_rnn<-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 1)
USDINRtest_data_rnn <-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 2,startCol=4,endCol=5)
USDINRactuals <- readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 2,region = "B2:B1001")
USDINRtrain_data_arima <-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 1,region = "B2:B998")
USDINRactuals_arima<-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 2,region = "B2:B10") 
USDJPYtrain_data_rnn <-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdataforUSDJPY.xlsx",sheet= 1)
USDJPYtest_data_rnn <-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdataforUSDJPY.xlsx",sheet= 2,startCol=4,endCol=5)
USDJPYactuals <- readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdataforUSDJPY.xlsx",sheet= 2,region = "B2:B1001")
USDJPYtrain_data_arima<-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdataforUSDJPY.xlsx",sheet= 1,region = "B2:B998")
USDJPYactuals_arima <-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdataforUSDJPY.xlsx",sheet= 2,region = "B2:B10") 
#######################################################################################################################################################################################
###########################################################EMD and IMF Coding #########################################################################################################
try <- emd(USDINRtrain_data_rnn$Values, USDINRtrain_data_rnn$Number, boundary="wave")
#par(mfrow=c(3,1), mar=c(2,1,2,1))
#X11(); par(mfrow=c(try$nimf+1, 1), mar=c(2,1,2,1))
A <-matrix(try$imf ,ncol =try$nimf)
G <- A
tryonJPY <-emd(USDJPYtrain_data_rnn$Values,USDJPYtrain_data_rnn$Number,boundary = "wave")
#par(mfrow=c(3,1), mar=c(2,1,2,1))
#X11(); par(mfrow=c(tryonJPY$nimf+1, 1), mar=c(2,1,2,1))
B <-matrix(tryonJPY$imf ,ncol = tryonJPY$nimf)
H <- B
for(i in 1:try$nimf) 
{
#######################################################################################################################################################################################
###########################################Applying RNN on USD-INR ####################################################################################################################
rnnValueforUSDINR <-trainr (Y =G, 
                             X=A,
                             learningrate = 0.5,
                             hidden_dim = 10,
                             seq_to_seq_unsync = TRUE,
                             numepochs = 10,network_type = "rnn")

}
predictedUSDINR <-predictr(rnnValueforUSDINR, G)
predictedMatrixUSDINR <-matrix(predictedUSDINR, ncol = try$nimf)
summatrix <-matrix(data = 0,nrow(predictedMatrixUSDINR),ncol =1)
for(i in 1:nrow(predictedMatrixUSDINR)){
  for(j in 1:ncol(predictedMatrixUSDINR))
  {
    summatrix [i] <- summatrix[i] + predictedMatrixUSDINR [i,j]
    j <-j+1;
  }
  i <-i+1;
}
basedata <- 56.72299957 
sum2matrix <- matrix(data = 0,nrow = nrow(summatrix-1))
sum2matrix[1] = basedata + summatrix[1]
for(i in 2:nrow(summatrix-2))
{
  sum2matrix[i] <-sum2matrix[i-1] + summatrix[i]
}
plot(c(sum2matrix), type="b",col="red", xlab = "Days", ylab = "Rate",xaxt ='n',ylim=c(55,80),xlim = c(1,1000))
axis(1,at = seq(1,1000,by =1))
lines(USDINRactuals, col="black", pch=22, lty=2)

# add a title and subtitle 
title("Currency Prediction Values(USD,INR) -vs- Days  Using RNN")
#add legend
legend("bottomright", legend=c("Predicted", "Actuals"),col=c("red", "black"), lty=1:1, cex=0.8)
ᐧ
for(i in 1:tryonJPY$nimf) 
{
########################################################################################################################################################
############################################## Applying RNN on USD-JPY #################################################################################
rnnValueforUSDJPY <-trainr (Y =H, 
                              X=B,
                              learningrate = 0.5,
                              hidden_dim = 10,
                              seq_to_seq_unsync = TRUE,
                              numepochs = 10,network_type = "rnn")
  
}
predictedUSDJPY <-predictr(rnnValueforUSDJPY, H)
predictedMatrixUSDJPY <-matrix(predictedUSDJPY, ncol = tryonJPY$nimf)
summatrixforJPY <-matrix(data = 0,nrow(predictedMatrixUSDJPY),ncol =1)
for(i in 1:nrow(predictedMatrixUSDJPY)){
  for(j in 1:ncol(predictedMatrixUSDJPY))
  {
    summatrixforJPY [i] <- summatrixforJPY[i] + predictedMatrixUSDJPY [i,j]
    j <-j+1;
  }
  i <-i+1;
}
basedataforJPY <- 98.237998962402
sum2matrixforJPY <- matrix(data = 0,nrow = nrow(summatrixforJPY-1))
sum2matrixforJPY[1] = basedataforJPY + summatrixforJPY[1]
for(i in 2:nrow(summatrixforJPY-2))
{
  sum2matrixforJPY[i] <-sum2matrixforJPY[i-1] + summatrixforJPY[i]
}
plot(c(sum2matrixforJPY), type="b",col="red", xlab = "Days", ylab = "Rate",xaxt ='n',ylim=c(95,120),xlim = c(1,1000))
axis(1,at = seq(1,1000,by =1))
lines(USDJPYactuals, col="black", pch=22, lty=2)

# add a title and subtitle 
title("Currency Prediction Values(USD/JPY) -vs- Days  Using RNN")
#add legend
legend("bottomright", legend=c("Predicted", "Actuals"),col=c("red", "black"), lty=1:1, cex=0.8)

################################################################################################################
###########ARIMA Coding For USD-INR ######################################################################################

#adf.test(USDINRtrain_data_arima,alternative = "stationary")
arima(USDINRtrain_data_arima[,1],order = c(1,0,0))
arima.final<- arima(USDINRtrain_data_arima,c(1,0,0))
predicted <-predict (arima.final,n.ahead =200)
#c(predicted$pred)
plot(c(predicted$pred),type="o",col="red", xlab = "Days", ylab = "Rate",xaxt ='n',ylim=c(55,60),xlim = c(1,7))
axis(1,at = seq(1,1000,by =1))
lines(USDINRactuals_arima, col="black", pch=22, lty=2)
# add a title and subtitle 
title("Currency Prediction Values(USD/INR) -vs- Days  Using ARIMA")
#add legend
legend("bottomright", legend=c("Predicted", "Actuals"),col=c("red", "black"), lty=1:1, cex=0.8)
ᐧ
####################################################################################################################################
#################################### ARIMA Coding for USD-JPY ##############################################################################
#adf.test(USDJPYtrain_data_arima,alternative = "stationary")
arima(USDJPYtrain_data_arima[,1],order = c(2,1,1))
arima.final<- arima(USDJPYtrain_data_arima,c(2,1,1))
predictedJPY <-predict (arima.final,n.ahead =200)
#c(predicted$pred)
plot(c(predictedJPY$pred),type="o",col="red", xlab = "Days", ylab = "Rate",xaxt ='n',ylim=c(95,120),xlim = c(1,7))
axis(1,at = seq(1,1000,by =1))
lines(USDJPYactuals_arima, col="black", pch=22, lty=2)
# add a title and subtitle 
title("Currency Prediction Values(USD/JPY) -vs- Days  Using ARIMA")
#add legend
legend("bottomright", legend=c("Predicted", "Actuals"),col=c("red", "black"), lty=1:1, cex=0.8)
ᐧ



