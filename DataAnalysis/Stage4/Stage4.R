#STAGE 4 ATTEMPTS TO LINK 3 PHASES OF THE KILL-CHAIN ON THE NETWORK DIMENSION AND THE WEAPONIZATION PHASE ON THE SOCIAL DIMENSION OF CYBERSPACE

#set working directories if any
#working.dir = '/media/ruth/Elements/PhdDataAnalysis/DataAnalysis/'
working.dir = '/Volumes/Elements/PhdDataAnalysis/DataAnalysis/'
setwd(working.dir)
source("GlobalFunctions.R")
source("ModelValidation.R")
set.seed(1000)

#set stage experiment
stage = 4

#read in data for stages
#data on the social dimension
sd = read.csv('../Database/final/sd.csv')

#data on the physical dimension
weather = read.csv('../Database/final/Weather.csv')
fw = read.csv("../Database/final/fw_permin.csv") #firewall
ids = read.csv("../Database/final/ids_permin.csv") #ids logs
pcap = read.csv("../Database/final/pcap_permin.csv") #pcap logs

###########################################################PROPAGATE PRIORS FROM PREVIOUS STAGES


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

######################################################################

#rename columns properly 
sdcols = names(sd[c(3:14,16:25,27)])
sd = sd[,sdcols]
names(sd) = c("Sentiment", "Cyber_Relate", "Chat_Size", "Entropy", "Fire_Relate", "Flu_Relate", "Negativity", "Neutrality", "Polarity","Positivity", "Subjectivity","weaponization", "Disgust","Fear","Joy","Sadness","Surprise","Trust","Anticipation","Chat_traffic","Chat_congestion","Datetime", "Wind_Speed")


#endogenous variable selection
nd$injection = (nd$dbcons/(nd$inhttp+nd$outhttp+1))/nd$numcons



png("Stage4/plots/endo_injection.png", width=1000, height=700)
plot(nd$permin, nd$injection, type='l', frame =F, ylab='Injection', xlab='Datetime', main='Database Injection on The Physical Dimension', col='brown', lwd=2)
dev.off()


####foiurier
injf = getFourier(data.frame(nd$injection))
dd1 = decompSeries(nd$injection, injf[1])
tt=dd1$trend[complete.cases(dd1$trend)]




#inegrate timelines
Stage4 = data.frame(cbind(nd, tail(sd, nrow(nd))))
time1 = Stage4$permin
time2 = Stage4$Datetime


Stage4cols = tolower(names(Stage4)[c(2:74,76)])
names(Stage4) = tolower(names(Stage4))
Stage4 = Stage4[order(Stage4$permin),]


#get final data for this stage of experiment
s4 = Stage4[,Stage4cols]
s4 = s4[complete.cases(s4),]
s4 = s4[,colSums(s4)!=0]
s4 = data.frame(sapply(s4, as.numeric))


#split data into training and test sets (70-30)
train = s4[1:(nrow(s4)-120),]
test = tail(s4, 120)


#correlation analysis
#inter-dimensional correlatioin plot
ndcor=cor(train[,1:50])
sdcor = cor(train[,51:72])

png("Stage4/plots/inter_dim_cor_social.png", height=800, width=1000)
corrplot(sdcor,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

png("Stage4/plots/inter_dim_cor_eco.png", height=800, width=1000)
corrplot(ndcor,method='number', order='hclust', addrect=2, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

#intra-dimensional correlation plot
s4cormat=cor(train)
png("Stage4/plots/intra_dim_cor.png", height=800, width=1000)
corrplot(s4cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

s4vars = c("tot_alerts","meanbytes","fragmentation","cfr","flu_relate","sadness","recon","fire_relate","chat_congestion","injection","totalbytes","indns","trust","disgust","fear","anticipation","surprise","weaponization","decoy","udp","meanepoch","outmail","dbcons","outdns")
s4 = train[,s4vars]

s4cormat=cor(s4)
png("Stage4/plots/correlation_elimination.png", height=800, width=1000)
corrplot(s4cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()



#recuresive feature selection
#s4lm = lm(injection ~., data=s4)
#s4 = stepAIC(s4lm, direction="both")$model



#VAR Importance analysis
s4lm = lm(injection ~., data=s4)
s4Imp = varImp(s4lm)

png("Stage4/plots/variable_importance.png", height=800, width=1000)
plot(as.factor(rownames(s4Imp)), s4Imp[,1], frame=F,s4imp[,1], las=2, cex.axis=0.7,main='Variable Importance for Predicting Databse Injections', ylab='Variable Importance Level',xlab='')
abline(h=1, col="red", lwd=3)
dev.off()


#decompose endogenous variable order = model order

s4vars = c('injection',rownames(s4Imp)[s4Imp >=1])
s4 = s4[,s4vars]


#stationarity and normality tests
s4pretests = data.frame(sapply(s4, pretest))
s4pretests = t(s4pretests)
write.csv(s4pretests,'Stage4/tests/s4tests.csv')



s4order = VARselect(s4,lag.max=80, type='const')
s4order = s4order$selection[1]

#decompose endogenous variable order = model order
#determine the ma order of each series in the data
s4mas = getFourier(s4)
#transform dat awith ma values
s4ma = data.frame(sapply(1:ncol(s4), function(dn) ma(s4[,dn], order=s4mas[dn])))
names(s4ma) = cols
#decompose endogenous variable order = model order
orders = getFourier(s4)
orders

s4 = data.frame(sapply(s4, norm))

test[,c('chat_congestion','fear')] = data.frame(sapply(test[,c('chat_congestion','fear')], norm))
#Determine Jhansen's test for co-integrating rank
#johansen's co-integration test
s4VECM = VECM(s4, lag = s4order, estim='ML', include='both')
s4r = rank.test(s4VECM)

#cointegration plots

#PMi

#MI

#Determione which features are co-integrated with Engle Granger co-integration Matrix
s4coMatrix = cointMatrix(s4)   


#model Building

s4VAR = VAR(s4, type='both', p=s4order)
results = s4VAR$varresult


#VAR
resids = data.frame(results$injection$residuals)
for(i in 2:ncol(s4)){
  resids = data.frame(cbind(resids, results[[i]]$residuals))
}
names(resids) = names(s4)

#get fitted values data frame
fitted = data.frame(results$injection$fitted.values)
for(i in 2:ncol(s4)){
  fitted = data.frame(cbind(fitted, results[[i]]$fitted.values))
}
names(fitted) = names(s4)



png("Stage4/plots/residsVSfitted1.png", height=800, width=1000)
par(mfrow=c(2,2))
for(i in 1:ncol(s4)){
  variable = names(s4)[i]
  plot(fitted[,i],resids[,i],xlab='Fitted',ylab='Residuals', main=paste('Residuals VS Fitted Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()


png("Stage4/plots/resids_acf.png", height=800, width=1000)
par(mfrow=c(2,2))
for(i in 1:ncol(s4)){
  variable = names(s4)[i]
  acf(resids[,i],xlab='Residuals', main=paste('ACF OF RESIDUALS: ', toupper(variable)))
}
par(mfrow=c(1,1))
dev.off()


residtest = data.frame(sapply(data.frame(resids), pretest))
residtest = t(residtest)
write.csv(residtest, "Stage4/tests/residualtests.csv")

png("Stage4/plots/resids_bell_curve.png", height=800, width=1000)
par(mfrow=c(2,2))
for(i in 1:ncol(s4)){
  variable = names(s4)[i]
  plot(density(resids[,variable]), main = paste("Normality Of ",variable), xlab='', ylab = "")
}
par(mfrow=c(1,1))
dev.off()



png("Stage4/plots/resids_qnorm_curve1.png", height=1000, width=1000)
par(mfrow=c(2,2))
for(i in 1:ncol(s4)){
  variable = names(s4)[i]
  qqnorm(resids[,variable], main = paste("Normality Of ",variable))
  qqline(resids[,variable], col='red', lwd=2)
}
par(mfrow=c(1,1))
dev.off()



#Insample Performance
inSampPerf = sapply(1:ncol(resids), function(dn)modelPerformance(residuals=resids[1:nrow(resids),dn], actuals = fitted[,dn]))
inSampPerf=data.frame(inSampPerf)
colnames(inSampPerf) = toupper(names(s4))
write.csv(t(inSampPerf),"Stage4/tests/InSampleperformance.csv")

#forecasting
predictions=predict(s4VAR, n.ahead=nrow(test))

#extract forecasts values
#fcst = data.frame(predictions)

fcst = data.frame(predictions$fcst$injection[,1])
for(i in 2:ncol(s4)){
  fcst = data.frame(cbind(fcst, predictions$fcst[[2]][,1]))
}
names(fcst) = names(s4)



#Out of sample performance
fresids = data.frame(sapply(c(1:ncol(s4)), function(dn)fcst[,dn]-test[,dn]))
outSampPerf = sapply(1:ncol(fcst), function(dn)modelPerformance(residuals=fresids[,dn], actuals=test[,dn]))
outSampPerf=data.frame(outSampPerf)
colnames(outSampPerf) = toupper(names(s4))
write.csv(t(outSampPerf),"Stage4/tests/OutSampleperformance.csv")




#granger 
gg = granger(s4, 1)
write.csv(gg,"Stage4/tests/granger.csv")













