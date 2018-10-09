#STAGE 1 ATTEMPTS TO SPOT ANTECEDENTS OF CYBER-INCIDENTS AS PRE-CURSORS TO THE KILL-CHAIN

#set working directories if any
#working.dir = '/media/ruth/Elements/PhdDataAnalysis/DataAnalysis/'
working.dir = '/Volumes/Elements/PhdDataAnalysis/DataAnalysis/'
setwd(working.dir)
source("GlobalFunctions.R")
source("ModelValidation.R")
set.seed(1000)

#set stage experiment
stage = 1

#read in data for stages
sd = '../Database/final/sd.csv'
ed = '../Database/final/ec.csv'
weather = '../Database/final/Weather.csv'

sd = read.csv(sd)
weather = read.csv(weather)
ed = read.csv(ed)


#format datetime variables
sd$datetime = strptime(as.character(sd$datetime), "%m/%d/%Y %H:%M")
sd$date = as.Date(sd$datetime)
time = sd$datetime
weather$Date = as.Date(weather$Date, "%m/%d/%Y")

#normalize sd
sd[,c(2:21)] = data.frame(sapply(2:21, function(dn) sd[,dn]/sd$wordcount))



#data merging and transformation
  #merge weather and social data
sd = merge(sd, weather, by.x='date', by.y='Date')
rm(weather)

  #clean ed data if necessary
ed[,3:9] = data.frame(sapply(ed[,3:9], repped))
ed = ed[complete.cases(ed),][,2:9]

names(ed) = tolower(names(ed))
edcols = c("open", "high","low","close","volume_usd","weighted_price")
ed = ed[,edcols]


sdcols = names(sd[c(3,4,6:13,16:25,27)])
sd = sd[,sdcols]

#rename columns properly 
names(sd) = c("Sentiment", "Cyber_Relate", "Entropy", "Fire_Relate", "Flu_Relate", "Negativity", "Neutrality", "Polarity","Positivity", "Subjectivity","Disgust","Fear","Joy","Sadness","Surprise","Trust","Anticipation","Chat_traffic","Chat_congestion","Datetime","Wind_Speed")

#get endogenous variable for this stage of experiment
endo = 'weighted_price'

#inegrate timelines

stage1 = data.frame(cbind(sd, tail(ed, nrow(sd))))
stage1cols = tolower(names(stage1)[c(1:19,21:27)])
names(stage1) = tolower(names(stage1))
stage1 = stage1[order(stage1$datetime),]


#get final data for this stage of experiment
s1 = stage1[,stage1cols]
s1 = s1[complete.cases(s1),]

s1 = data.frame(sapply(s1, as.numeric))
#s1 = s1[,c(1,2,4:26)]


#split data into training and test sets (70-30)
train = s1[1:round(0.9*nrow(s1)),]
test = tail(s1, nrow(s1)-nrow(train))

#correlation analysis

#inter-dimensional correlatioin plot
sdcor=cor(train[,1:20])
edcor = cor(train[,21:25])

png("Stage1/plots/inter_dim_cor_social.png", height=800, width=1000)
corrplot(sdcor,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

png("Stage1/plots/inter_dim_cor_eco.png", height=800, width=1000)
corrplot(edcor,method='number', order='hclust', addrect=2, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

#intra-dimensional correlation plot
s1cormat=cor(train)
png("Stage1/plots/intra_dim_cor.png", height=800, width=1000)
corrplot(s1cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

s1 = train
#feature selection for endogenous variable
s1vars = c("entropy", "negativity", "surprise", "anticipation","fire_relate","disgust","fear","trust","cyber_relate","volume_usd","chat_congestion", "flu_relate", "wind_speed", "weighted_price", "sentiment")
s1 = s1[,s1vars]

s1cormat=cor(s1)
png("Stage1/plots/correlation_elimination.png", height=800, width=1000)
corrplot(s1cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()


#recuresive feature selection
s1lm = lm(weighted_price ~., data=s1)
s1 = stepAIC(s1lm, direction="both")$model



#VAR Importance analysis
s1lm = lm(weighted_price ~., data=s1)
s1Imp = varImp(s1lm)

png("Stage1/plots/variable_importance.png", height=800, width=1000)
plot(as.factor(rownames(s1Imp)), s1Imp[,1], frame=F,s1imp[,1], las=2, cex.axis=0.7,main='Variable Importance for Predicting Weighted Price Index', ylab='Variable Importance Level',xlab='')
abline(h=1, col="red", lwd=1)
dev.off()

s1vars = c('weighted_price',rownames(s1Imp)[s1Imp >1])
s1 = s1[,s1vars]

#Model order selection
#ACF PLOTS
png("Stage1/plots/var_acf.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  acf(s1[,variable], main = paste("ACF ",variable), xlab='Lag', ylab = "Significance")
}
par(mfrow=c(1,1))
dev.off()
#pacf plots

#PACF PLOTS
png("Stage1/plots/var_pacf.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  pacf(s1[,variable], main = paste("PACF ",variable), xlab='Lag', ylab = "Significance")
}
par(mfrow=c(1,1))
dev.off()




s1order = VARselect(s1,lag.max=80, type='const')
s1order = s1order$selection[1]


#decompose endogenous variable order = model order


#stationarity and normality tests
s1pretests = data.frame(sapply(s1, pretest))
s1pretests = t(s1pretests)
write.csv(s1pretests,'Stage1/tests/s1tests.csv')

#transform data
s1 = data.frame(sapply(s1, norm))
test = data.frame(sapply(test, norm))
test = test[,names(s1)]

#co integration test
  




#Determine Jhansen's test for co-integrating rank
#johansen's co-integration test
s1jotest = ca.jo(s1, K=s1order, type='eigen', spec='transitory')
s1tt = ctest(s1jotest, cutoff=10)

#Determione which features are co-integrated with Engle Granger co-integration Matrix
s1coMatrix = cointMatrix(s1)   


#model Building
s1VAR = VAR(s1, type='both', p=s1order)
results = s1VAR$varresult

#VAR
#VECM

#Residual Analysis

#VAR
resids = data.frame(results$weighted_price$residuals)
for(i in 2:ncol(s1)){
  resids = data.frame(cbind(resids, results[[i]]$residuals))
}
names(resids) = names(s1)

#get fitted values data frame
fitted = data.frame(results$weighted_price$fitted.values)
for(i in 2:ncol(s1)){
  fitted = data.frame(cbind(fitted, results[[i]]$fitted.values))
}
names(fitted) = names(s1)

#Residual plots
png("Stage1/plots/residsVSfitted.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  len = length(results$weighted_price$residuals)
  plot(s1[,variable][1:len],results[[i]]$residuals,xlab='Actual',ylab='Residuals', main=paste('Residuals VS Fitted Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()

png("Stage1/plots/resids.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  len = length(results$Weighted_price$residuals)
  plot(results[[i]]$residuals, main = paste(variable, " Residuals"), xlab='Lag', ylab = "Significance")
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()



#ACF Residual PLots
png("Stage1/plots/acf_resids.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  len = length(results$Weighted_price$residuals)
  acf(results[[i]]$residuals, main = paste("ACF ",variable, " Residuals"), xlab='Lag', ylab = "Significance")
}
par(mfrow=c(1,1))
dev.off()

#PACF Residual PLots
png("Stage1/plots/pacf_resids.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  len = length(results$Weighted_price$residuals)
  pacf(results[[i]]$residuals, main = paste("PACF ",variable, " Residuals"), xlab='Lag', ylab = "Significance")
}
par(mfrow=c(1,1))
dev.off()

#get residuals data frame
resids = data.frame(results$weighted_price$residuals)
for(i in 2:ncol(s1)){
  resids = data.frame(cbind(resids, results[[i]]$residuals))
}
names(resids) = names(s1)

#get fitted values data frame
fitted = data.frame(results$weighted_price$fitted.values)
for(i in 2:ncol(s1)){
  fitted = data.frame(cbind(fitted, results[[i]]$fitted.values))
}
names(fitted) = names(s1)


residtest = data.frame(sapply(resids, pretest))
residtest = t(residtest)
write.csv(residtest, "Stage1/tests/residualtests.csv")

png("Stage1/plots/resids_bell_curve.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  plot(density(resids[,variable]), main = paste("Normality Of ",variable), xlab='', ylab = "")
}
par(mfrow=c(1,1))
dev.off()

png("Stage1/plots/resids_qnorm_curve.png", height=1000, width=1000)
par(mfrow=c(4,2))
for(i in 1:ncol(s1)){
  variable = names(s1)[i]
  qqnorm(resids[,variable], main = paste("Normality Of ",variable))
  qqline(resids[,variable], col='red', lwd=2)
}
par(mfrow=c(1,1))
dev.off()

#Insample Performance
inSampPerf = sapply(1:ncol(resids), function(dn)modelPerformance(residuals=resids[1:nrow(resids),dn], actuals = fitted[,dn]))
inSampPerf=data.frame(inSampPerf)
colnames(inSampPerf) = toupper(names(s1))
write.csv(t(inSampPerf),"Stage1/tests/InSampleperformance.csv")

#forecasting
predictions=predict(s1VAR, n.ahead=nrow(test))

#extract forecasts values
#fcst = data.frame(predictions)

fcst = data.frame(predictions$fcst$weighted_price[,1])
for(i in 2:ncol(s1)){
  fcst = data.frame(cbind(fcst, predictions$fcst[[2]][,1]))
}
names(fcst) = names(s1)



#Out of sample performance
fresids = data.frame(sapply(c(1:ncol(s1)), function(dn)fcst[,dn]-test[,dn]))
names(fresids) = names(s1)
outSampPerf = sapply(1:ncol(fresids), function(dn)modelPerformance(residuals=fresids[,dn], actuals=test[,dn]))
outSampPerf=data.frame(outSampPerf)
colnames(outSampPerf) = toupper(names(s1))
write.csv(t(outSampPerf),"Stage1/tests/OutSampleperformance.csv")




#granger 
gg = granger(s1, 1)
write.csv(gg,"Stage1/tests/granger.csv")













