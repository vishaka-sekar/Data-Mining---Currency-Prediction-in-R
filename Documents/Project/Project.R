install.packages("EMD")
install.packages("readxl")
install.packages("rnn")
install.packages("forecast")
install.packages("gdata")
install.packages("tseries")
install.packages("rnn")
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
train_data<-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 1)
test_data <-readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 2,startCol=4,endCol=5)
actuals <- readWorksheetFromFile("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/testdata.xlsx",sheet= 2,region = "B2:B1003")
#sampleFile
#plot(sampleFile$Values ~ sampleFile$Number , type="l", lty=2 , col= "red")
try <- emd(train_data$Values, train_data$Number, boundary="wave")
par(mfrow=c(3,1), mar=c(2,1,2,1))
X11(); par(mfrow=c(try$nimf+1, 1), mar=c(2,1,2,1))
#rangeimf <- range(try$imf)
#for(i in 1:try$nimf) {
 #plot( sampleFile$Number, try$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
#        main=paste(i, "-th IMF", sep="")); abline(h=0)
#}

#n <-try$nimf
#Yp <-matrix(nrow= 10,ncol=try$nimf)
#print(n)
A <-matrix(try$imf ,ncol =try$nimf)
G <- A
for(i in 1:try$nimf) 
{
#print( try$imf[,i])
#print(try$imf)
rnnValueforUSDINR <-trainr (Y =G, 
                             X=A,
                             learningrate = 0.5,
                             hidden_dim = 10,
                             seq_to_seq_unsync = TRUE,
                             numepochs = 10,network_type = "rnn")
#Yp[ ,i] <-matrix(predictr(rnnValueforUSDINR, A))
#i <-i+1;
#print(Yp)
#Yp[ ,i] <-predictr(rnnValueforUSDINR, A)

}
predictedUSDINR <-predictr(rnnValueforUSDINR, G)
predictedMatrixUSDINR <-matrix(predictedUSDINR, ncol = try$nimf)
#write.xlsx(predictedMatrixUSDINR,"C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/predictedrnnvalue.xlsx")
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
#X <- seq(1,1000 , by = 1)

plot(c(sum2matrix), type="o",col="red", xlab = "Days", ylab = "Rate",xaxt ='n')
axis(1,at = seq(1,1000,by =1))
lines(actuals, col="black", pch=22, lty=2)