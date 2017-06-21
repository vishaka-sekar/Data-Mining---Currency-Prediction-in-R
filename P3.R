install.packages("EMD")
install.packages("readxl")
install.packages("rnn")
install.packages("forecast")
install.packages("gdata")
install.packages("tseries")
install.packages("XLConnect")
install.packages("xlsx")
install.packages("ModelMetrics")
#############################################################################################################
################################ Using the Installed Libraries ##############################################

################# Start Running the code from below after successful installation of the libraries
library(EMD)
library(rnn)
library(forecast)
library(gdata)
library(readxl)
library(tseries)
library(rnn)
library(XLConnect)
#############################################################################################################
# dataset of 1500 values
# min training data size from excel to the model should be 1500
NumOfDays=100 #No of days to be predicted
trainingDataSize=1500
startNumForOutPut=1+NumOfDays
endNumForOutPut= trainingDataSize+ NumOfDays
#test data set size is set as 2000 
predictionTestDataSize=1500
endForPrediction=predictionTestDataSize+NumOfDays
################################################################################################################
################################################# Inputting the File ###########################################
sampleFileTrain = read_xlsx("C:/Users/nithi/Desktop/Course Study/Spring 2017 Classes/Pattern Recognition and Data Mining/USDINR.xlsx")
sampleFileInput <- sampleFileTrain$`USD/INR`
##################################################################################################################
################################################## Applying EMD on RNN ###########################################
try <- emd(sampleFileInput, sampleFileTrain$Number)
################################################################################################################
#######################################Residue prediction ######################################################
normalizeResidue= (try$residue-min(try$residue))/(max(try$residue)-min(try$residue))
trainInputValues <- 1:trainingDataSize
trainOutputValues <- startNumForOutPut:endNumForOutPut
input=normalizeResidue[trainInputValues]
input<-matrix(input, ncol=trainingDataSize)
output=normalizeResidue[trainOutputValues]
output<-matrix(output, ncol=trainingDataSize)
rnnValueforUSDINR <-trainr (Y=output, 
                            X=input,
                            learningrate = 0.01,
                            hidden_dim = 10,numepochs = 350)
inputForRnnPredictr=normalizeResidue[1:predictionTestDataSize]
inputForRnnPredictrMatrix=matrix(inputForRnnPredictr, ncol=predictionTestDataSize) 
predictedUSDINR <-predictr(rnnValueforUSDINR,inputForRnnPredictrMatrix)
outputResidueFromRnn <- predictedUSDINR[1,]
denormalizedResidue = (outputResidueFromRnn)*(max(try$residue)-min(try$residue))+min(try$residue)
predictionOutput <- denormalizedResidue
##################################################################################################################
########################################IMF prediction############################################################
for(i in 1:try$nimf) {
  normalizeIMF= (try$imf[,i]-min(try$imf[,i]))/(max(try$imf[,i])-min(try$imf[,i]))
  trainInputValues <- 1:trainingDataSize
  trainOutputValues <- startNumForOutPut:endNumForOutPut
  input=normalizeIMF[trainInputValues]
  input<-matrix(input, ncol=trainingDataSize)
  output=normalizeIMF[trainOutputValues]
  output<-matrix(output, ncol=trainingDataSize)
    rnnValueforUSDINR <-trainr (Y=output, 
                              X=input,
                              learningrate = 0.01,
                              hidden_dim = 10,numepochs = 350)
inputForRnnPredictr=normalizeIMF[1:predictionTestDataSize]
  inputForRnnPredictrMatrix=matrix(inputForRnnPredictr, ncol=predictionTestDataSize) 
  predictedUSDINR <-predictr(rnnValueforUSDINR,inputForRnnPredictrMatrix)
  outputFromRnn <- predictedUSDINR[1,]
  denormalizedIMF = (outputFromRnn)*(max(try$imf[,i])-min(try$imf[,i]))+min(try$imf[,i])
  predictionOutput <- cbind(predictionOutput,denormalizedIMF)
  
}
finalPrediction<-  rowSums(predictionOutput)
par(mar = c(5,2,2,2))
sampleFileInputForGraph<-sampleFileInput[startNumForOutPut:endForPrediction]
length(sampleFileInputForGraph)
length(finalPrediction)
#########################################################################################################################################
########################################### Calculating RMSE ############################################################################
RMSE<-sqrt(mean((sampleFileInputForGraph-finalPrediction)^2))

######################################################################################################################################
########################################### Plotting RNN with EMD Graph ############################################################## 
plot(sampleFileInputForGraph,type="l",col="blue",ylab = "USD INR" ,xlab = "Days ->")
lines(finalPrediction,type="l",col="red", ylab = "USD INR" ,xlab = "Days ->")
title("Currency Prediction Values(USD/INR) Using EMD+RNN")
legend("bottomright", legend=c("Predicted", "Actuals"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

##############################################################################################################################################
##################################################### Final Prediction and RMSE for RNN + EMD ###############################################
print(finalPrediction)
print(RMSE)
############################################################# Raw RNN ###########################################################################
#################################################################################################################################################
normalizedForSimpleRNN = (sampleFileInput-min(sampleFileInput))/(max(sampleFileInput)-min(sampleFileInput))
normalizedForSimpleRNN 
trainInputValuesSimpleRNN <- 1:trainingDataSize
trainOutputValuesSimpleRNN <- startNumForOutPut:endNumForOutPut
inputforSimpleRNN=normalizedForSimpleRNN[trainInputValuesSimpleRNN]
inputforSimpleRNN<-matrix(inputforSimpleRNN, ncol=trainingDataSize)
outputforSimpleRNN=normalizedForSimpleRNN[trainOutputValuesSimpleRNN]
outputforSimpleRNN<-matrix(outputforSimpleRNN, ncol=trainingDataSize)
rnnValueforUSDINRSimple <-trainr (Y=outputforSimpleRNN, 
                                  X=inputforSimpleRNN,
                                  learningrate = 0.01,
                                  hidden_dim = 10,numepochs = 100)
inputForRnnPredictrSimple=normalizedForSimpleRNN[1:predictionTestDataSize]
inputForRnnPredictrMatrixSimple=matrix(inputForRnnPredictrSimple, ncol=predictionTestDataSize) 
predictedUSDINRNNSimple<-predictr(rnnValueforUSDINRSimple,inputForRnnPredictrMatrixSimple)
outputFromRnnSimple <- predictedUSDINRNNSimple[1,]
denormalizedFromRnnSimple = (outputFromRnnSimple)*(max(sampleFileInput)-min(sampleFileInput))+min(sampleFileInput)
denormalizedFromRnnSimple
sampleFileInputForGraphSimpleRnn<-sampleFileInput[startNumForOutPut:endForPrediction]
length(sampleFileInputForGraphSimpleRnn)
length(sampleFileInputForGraphSimpleRnn)
plot(sampleFileInputForGraphSimpleRnn,type="l",col="blue",ylab = "USD INR" ,xlab = "Days ->")
lines(denormalizedFromRnnSimple,type="l",col="red", ylab = "USD INR" ,xlab = "Days ->")
RMSEforSimple<-sqrt(mean((sampleFileInputForGraphSimpleRnn-denormalizedFromRnnSimple)^2))
title("Currency Prediction Values(USD/INR) Using RNN")
legend("bottomright", legend=c("Predicted", "Actuals"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

####################################################### Predicted and RMSE Value for RNN##########################
#################################################################################################################
print(denormalizedFromRnnSimple)
print(RMSEforSimple)
############################################################## ARIMA #####################################################
##########################################################################################################################
sampleFileInputforarima <- sampleFileTrain$`USD/INR`
trainData <- sampleFileInput[2:51]
testData <- sampleFileInput[52:102]
arima.final<- arima(trainData,c(2,2,2))
predicted <-predict (arima.final,n.ahead =100)
par(mar = c(5,5,3,2))
#c(predicted$pred)
plot(c(predicted$pred),type="l",col="red", xlab = "Days", ylab = "Rate",ylim=c(40,60), xlim = c(0,100))
lines(c(testData), col="black",type="l",ylim=c(40,60), xlim = c(0,100))
# add a title and subtitle 
title("Currency Prediction Values(USD/INR)Using ARIMA")
#add legend
RS <- sqrt(mean((testData-trainData)^2))
print(RS)
legend("topright",legend=c("Predicted", "Actuals"),col=c("red", "black"), lty = 1:1 , cex = 0.5)
#legend("topright", legend=c("Predicted", "Actuals"),col=c("red", "black"), lty=1:1)