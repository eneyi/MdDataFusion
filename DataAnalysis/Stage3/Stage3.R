#STAGE 3 ATTEMPTS TO LINK THE WEAPONIZATION PHASE OF THE KILL-CHAIN ON THE SOCIAL DIMENSION OF CYBERSPACE USING DATA FROM THE NEWORK LAYER

#set working directories if any
#working.dir = '/media/ruth/Elements/PhdDataAnalysis/DataAnalysis/'
working.dir = '/Volumes/Elements/PhdDataAnalysis/DataAnalysis/'
setwd(working.dir)
source("GlobalFunctions.R")
source("modelValidation.R")
set.seed(1000)

#set stage experiment
stage = 3

#read in data for stages
#data on the social dimension
sd = read.csv('../Database/final/sd.csv')

#data on the physical dimension
weather = read.csv('../Database/final/Weather.csv')
fw = read.csv("../Database/final/fw_permin.csv") #firewall
ids = read.csv("../Database/final/ids_permin.csv") #ids logs
pcap = read.csv("../Database/final/pcap_permin.csv") #pcap logs


#format datetime variables
sd$datetime = strptime(as.character(sd$datetime), "%m/%d/%Y %H:%M")
sd$date = as.Date(sd$datetime)
weather$Date = as.Date(weather$Date, "%m/%d/%Y")



#normalize sd
sd[,c(2:21)] = data.frame(sapply(2:21, function(dn) sd[,dn]/sd$wordcount))


#data merging and transformation
#merge weather and social data
sd = merge(sd, weather, by.x='date', by.y='Date')
rm(weather)

#clean ed data if necessary
#merge pcap and ids logs
nd = merge(pcap,ids, all.x=T)
nd[,26:31] = data.frame(sapply(nd[,26:31], repna))
#merge with firewall data
nd = merge(nd, fw)
#order by date
nd = nd[order(nd$permin),]
#format date
nd$permin = strptime(nd$permin, '%Y-%m-%d %H:%M')


#get endogenous variable for this stage of experiment
nd$scanner = (nd$undstip-nd$unsrcip)/nd$undstport
nd$recon = (nd$outbound/nd$unsrcip)/((nd$inbound+nd$outbound)/nd$undstip)
nd$cfr= (nd$inbound-nd$outbound)/nd$outbound


rm(pcap)
rm(fw)
rm(ids)

sdcols = names(sd[c(3:14,16:25,27)])
sd = sd[,sdcols]


#rename columns properly 
names(sd) = c("Sentiment", "Cyber_Relate", "Chat_Size", "Entropy", "Fire_Relate", "Flu_Relate", "Negativity", "Neutrality", "Polarity","Positivity", "Subjectivity","weaponization", "Disgust","Fear","Joy","Sadness","Surprise","Trust","Anticipation","Chat_traffic","Chat_congestion","Datetime", "Wind_Speed")

#endogenous variable
endo = 'weaponization'

#endogenous variable plots
wf = getFourier(data.frame(sd$weaponization))
dd1 = decompSeries(sd$weaponization, wf[1])
tt=dd1$trend[complete.cases(dd1$trend)]

png("Stage3/plots/endo_weaponization.png", width=1000, height=700)
plot(nd$permin, sd$Chat_congestion[1:nrow(nd)]-tt[1:nrow(nd)], type='l', frame =F, ylab='Weaponization', xlab='Datetime', main='Weaponization on The Social Dimension', col='brown', lwd=2)
dev.off()






#inegrate timelines
Stage3 = data.frame(cbind(nd, tail(sd, nrow(nd))))
time1 = Stage3$permin
time2 = Stage3$Datetime


Stage3cols = tolower(names(Stage3)[c(2:73,75)])
names(Stage3) = tolower(names(Stage3))
Stage3 = Stage3[order(Stage3$permin),]


#get final data for this stage of experiment
s3 = Stage3[,Stage3cols]
s3 = s3[complete.cases(s3),]
s3 = s3[,colSums(s3)!=0]
s3 = data.frame(sapply(s3, as.numeric))


#split data into training and test sets (70-30)
train = s3[1:(nrow(s3)-120),]
test = tail(s3, 120)

#correlation analysis
#inter-dimensional correlatioin plot
ndcor=cor(train[,1:49])
sdcor = cor(train[,50:71])

png("Stage3/plots/inter_dim_cor_social.png", height=800, width=1000)
corrplot(sdcor,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

png("Stage3/plots/inter_dim_cor_eco.png", height=800, width=1000)
corrplot(ndcor,method='number', order='hclust', addrect=2, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

#intra-dimensional correlation plot
s3cormat=cor(train)
png("Stage3/plots/intra_dim_cor.png", height=800, width=1000)
corrplot(s3cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

s3vars=c("joy", "tot_alerts", "meanbytes", "fragmentation", "cfr", "recon", "flu_relate", "sadness", "fire_relate", "entropy","outmail", "indns", "cyber_relate", "sentiment", "trust","disgust","fear","icmp","anticipation","surprise","weaponization","decoy","udp", "dbcons","outdns")
s3 = train[,s3vars]

s3cormat=cor(s3)
png("Stage3/plots/correlation_elimination.png", height=800, width=1000)
corrplot(s3cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()



#recuresive feature selection
s3lm = lm(weaponization ~., data=s3)
s3 = stepAIC(s3lm, direction="both")$model



#VAR Importance analysis
s3lm = lm(weaponization ~., data=s3)
s3Imp = varImp(s3lm)

png("Stage3/plots/variable_importance.png", height=800, width=1000)
plot(as.factor(rownames(s3Imp)), s3Imp[,1], frame=F,s3imp[,1], las=2, cex.axis=0.7,main='Variable Importance for Predicting The Weaponization Phase', ylab='Variable Importance Level',xlab='')
abline(h=2, col="red", lwd=3)
dev.off()


#decompose endogenous variable order = model order

s3vars = c('weaponization',rownames(s3Imp)[s3Imp >=1])
s3 = s3[,s3vars]


#stationarity and normality tests
s3pretests = data.frame(sapply(s3, pretest))
s3pretests = t(s3pretests)
write.csv(s3pretests,'Stage3/tests/s3tests.csv')


s3 = data.frame(sapply(s3, norm))
test = data.frame(sapply(s3, norm))
test = test[,names(s3)]

s3order = VARselect(s3,lag.max=80, type='const')
s3order = s3order$selection[1]

#decompose endogenous variable order = model order
#determine the ma order of each series in the data
s3mas = getMa(s3)
#transform dat awith ma values
s3ma = data.frame(sapply(1:ncol(s3), function(dn) ma(s3[,dn], order=s3mas[dn])))
names(s3ma) = cols
#decompose endogenous variable order = model order
orders = c(getFourier(s3[,1:6]), getFourier(s3[,7:15]))
orders


#decompose time series
png("Stage3/plots/decompose_fire_relate.png", height=1000, width=800)
decompSeries(s3[,9], 650)
dev.off()

png("Stage3/plots/decompose_joy.png", height=1000, width=800)
decompSeries(s3[,10], orders[10])
dev.off()

#co integration test


#Determine Jhansen's test for co-integrating rank
#johansen's co-integration test
s3VECM = VECM(s3, lag = s3order, estim='ML', include='both')
s3r = rank.test(s3VECM)

#cointegration plots

#PMi

#MI

#Determione which features are co-integrated with Engle Granger co-integration Matrix
s3coMatrix = cointMatrix(s3)   


#model Building

s3VAR = VAR(s3, type='both', p=s3order)
results = s3VAR$varresult
#VAR
#VECM
resids = s3VECM$residuals
fitted = s3VECM$fitted.values
#Residual Analysis


#VAR
resids = data.frame(results$weaponization$residuals)
for(i in 2:ncol(s3)){
  resids = data.frame(cbind(resids, results[[i]]$residuals))
}
names(resids) = names(s3)

#get fitted values data frame
fitted = data.frame(results$weaponization$fitted.values)
for(i in 2:ncol(s3)){
  fitted = data.frame(cbind(fitted, results[[i]]$fitted.values))
}
names(fitted) = names(s3)

#Residual Analysis

png("Stage3/plots/residsVSfitted1.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:8){
  variable = names(s3)[i]
  plot(fitted[,i],resids[,i],xlab='Fitted',ylab='Residuals', main=paste('Residuals VS Fitted Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()
png("Stage3/plots/residsVSfitted2.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 9:ncol(s3)){
  variable = names(s3)[i]
  plot(fitted[,i],resids[,i],xlab='Fitted',ylab='Residuals', main=paste('Residuals VS Fitted Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()



#ACF RESIDUALS

png("Stage3/plots/resids_acf1.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:8){
  variable = names(s3)[i]
  acf(resids[,i],xlab='Residuals', main=paste('ACF OF RESIDUALS: ', toupper(variable)))
}
par(mfrow=c(1,1))
dev.off()
png("Stage3/plots/resids_acf2.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 9:ncol(s3)){
  variable = names(s3)[i]
  acf(resids[,i],xlab='Residuals', main=paste('ACF OF RESIDUALS: ', toupper(variable)))
}
par(mfrow=c(1,1))
dev.off()





residtest = data.frame(sapply(data.frame(resids), pretest))
residtest = t(residtest)
write.csv(residtest, "Stage3/tests/residualtests.csv")

png("Stage3/plots/resids_bell_curve1.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 1:8){
  variable = names(s3)[i]
  plot(density(resids[,variable]), main = paste("Normality Of ",variable), xlab='', ylab = "")
}
par(mfrow=c(1,1))
dev.off()
png("Stage3/plots/resids_bell_curve2.png", height=800, width=1000)
par(mfrow=c(4,2))
for(i in 9:ncol(s3)){
  variable = names(s3)[i]
  plot(density(resids[,variable]), main = paste("Normality Of ",variable), xlab='', ylab = "")
}
par(mfrow=c(1,1))
dev.off()



png("Stage3/plots/resids_qnorm_curve1.png", height=1000, width=1000)
par(mfrow=c(4,2))
for(i in 1:8){
  variable = names(s3)[i]
  qqnorm(resids[,variable], main = paste("Normality Of ",variable))
  qqline(resids[,variable], col='red', lwd=2)
}
par(mfrow=c(1,1))
dev.off()
png("Stage3/plots/resids_qnorm_curve2.png", height=1000, width=1000)
par(mfrow=c(4,2))
for(i in 9:ncol(s3)){
  variable = names(s3)[i]
  qqnorm(resids[,variable], main = paste("Normality Of ",variable))
  qqline(resids[,variable], col='red', lwd=2)
}
par(mfrow=c(1,1))
dev.off()


#Insample Performance
#Insample Performance
inSampPerf = sapply(1:ncol(resids), function(dn)modelPerformance(residuals=resids[1:nrow(resids),dn], actuals = fitted[,dn]))
inSampPerf=data.frame(inSampPerf)
colnames(inSampPerf) = toupper(names(s3))
write.csv(t(inSampPerf),"Stage3/tests/InSampleperformance.csv")

#forecasting
predictions=predict(s3VAR, n.ahead=nrow(test))

#extract forecasts values
#fcst = data.frame(predictions)

fcst = data.frame(predictions$fcst$weaponization[,1])
for(i in 2:ncol(s3)){
  fcst = data.frame(cbind(fcst, predictions$fcst[[2]][,1]))
}
names(fcst) = names(s3)

#Out of sample performance
#Out of sample performance
fresids = data.frame(sapply(c(1:ncol(s3)), function(dn)fcst[,dn]-test[,dn]))
names(fresids) = names(s3)
outSampPerf = sapply(1:ncol(fresids), function(dn)modelPerformance(residuals=fresids[,dn], actuals=test[,dn]))
outSampPerf=data.frame(outSampPerf)
colnames(outSampPerf) = toupper(names(s3))
write.csv(t(outSampPerf),"Stage3/tests/OutSampleperformance.csv")




#granger 
gg = granger(s3, 1)
write.csv(gg,"Stage3/tests/granger.csv")




