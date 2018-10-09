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

##order datasets by datetime
sd = sd[order(sd$datetime),]
nd=nd[order(nd$permin),]

#propagate features from previous stages
nd$scanner = (nd$undstip-nd$unsrcip)/nd$undstport
nd$recon = (nd$outbound/nd$unsrcip)/((nd$inbound+nd$outbound)/nd$undstip)
nd$cfr= (nd$inbound-nd$outbound)/nd$outbound
nd$injection = (nd$dbcons/(nd$inhttp+nd$outhttp+1))/nd$numcons
nd$botnets = ((nd$indns+nd$outdns)/nd$UDP)/(nd$inbound+nd$outbound)

#get endo genous variable
nd$dos =(nd$indns)- (nd$tcp+nd$ICMP)






png("Stage6/plots/endo_dos.png", width=1000, height=700)
plot(nd$permin[2:nrow(nd)], diff(nd$dos), type='l', frame =F, ylab='Scaled DOS Requests', xlab='Datetime', main='Denial Of Service on The Physical Dimension', col='brown', lwd=2)
dev.off()


####foiurier
#dos = getFourier(data.frame(nd$dos))
#dd1 = decompSeries(nd$dos, dos[1])
#tt=dd1$trend[complete.cases(dd1$trend)]
#ts=dd1$seasonal[complete.cases(dd1$seasonal)]




#inegrate timelines
Stage6 = data.frame(cbind(nd, tail(sd, nrow(nd))))
time1 = Stage6$permin
time2 = Stage6$Datetime


Stage6cols = tolower(names(Stage6)[c(2:55,58:69,71:79,82)])
names(Stage6) = tolower(names(Stage6))
#Stage6 = Stage6[order(Stage6$permin),]


#get final data for this stage of experiment
s6 = Stage6[,Stage6cols]
s6 = s6[complete.cases(s6),]
s6 = s6[,colSums(s6)!=0]
s6 = data.frame(sapply(s6, as.numeric))


#split data into training and test sets (70-30)
train = s6[1:(nrow(s6)-120),]
test = tail(s6, 120)




#correlation analysis
#inter-dimensional correlatioin plot
ndcor=cor(train[,1:52])
sdcor = cor(train[,53:74])

png("Stage6/plots/inter_dim_cor_social.png", height=800, width=1000)
corrplot(sdcor,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

png("Stage6/plots/inter_dim_cor_nd.png", height=800, width=1000)
corrplot(ndcor,method='number', order='hclust', addrect=2, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

#intra-dimensional correlation plot
s6cormat=cor(train)
png("Stage6/plots/intra_dim_cor.png", height=800, width=1000)
corrplot(s6cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

s6vars = c("recon","chat_traffic","botnets", "fragmentation","dos","tot_alerts","cyber_rel","injection","fire_rel","icmp","flu_rel","numcons","outmail","dbcons","polarity","joy","meanbytes","meaniplen","trust","weaponization","disgust","fear","surprise","anticipation","meanepoch","undstips")
s6 = train[,s6vars]

s6cormat=cor(s6)
png("Stage6/plots/correlation_elimination.png", height=800, width=1000)
corrplot(s6cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()



#recuresive feature selection
s6lm = lm(dos ~., data=s6)
s6 = stepAIC(s6lm, direction="both")$model



#VAR Importance analysis
s6lm = lm(dos ~., data=s6)
s6Imp = varImp(s6lm)

png("Stage6/plots/variable_importance.png", height=800, width=1000)
plot(as.factor(rownames(s6Imp)), s6Imp[,1], frame=F,s6imp[,1], las=2, cex.axis=0.7,main='Variable Importance for Predicting DOS Attacks', ylab='Variable Importance Level',xlab='')
abline(h=1, col="red", lwd=3)
dev.off()


#decompose endogenous variable order = model order

s6vars = c('dos',rownames(s6Imp)[s6Imp >=1])
s6 = s6[,s6vars]


#stationarity and normality tests
s6pretests = data.frame(sapply(s6, pretest))
s6pretests = t(s6pretests)
write.csv(s6pretests,'Stage6/tests/s6tests.csv')



s6order = VARselect(s6,lag.max=250, type='const')
s6order = s6order$selection[1]

#decompose endogenous variable order = model order
#determine the ma order of each series in the data
s6mas = getFourier(s6)
#transform dat awith ma values
s6ma = data.frame(sapply(1:ncol(s6), function(dn) ma(s6[,dn], order=s6mas[dn])))
names(s6ma) = cols
#decompose endogenous variable order = model order
orders = getFourier(s6)
orders

s6 = data.frame(sapply(s6, norm))



s6 = data.frame(sapply(s6, norm))
test = data.frame(sapply(test[,names(s6)], norm))
#Determine Jhansen's test for co-integrating rank
#johansen's co-integration test
s6VECM = VECM(s6, lag = s6order, estim='ML', include='both')
s6r = rank.test(s6VECM)

#cointegration plots

#PMi

#MI

#Determione which features are co-integrated with Engle Granger co-integration Matrix
s6coMatrix = cointMatrix(s6)   


#model Building

s6VAR = VAR(s6, type='both', p=s6order)
results = s6VAR$varresult



#VAR
resids = data.frame(results$dos$residuals)
for(i in 2:ncol(s6)){
  resids = data.frame(cbind(resids, results[[i]]$residuals))
}
names(resids) = names(s6)

#get fitted values data frame
fitted = data.frame(results$dos$fitted.values)
for(i in 2:ncol(s6)){
  fitted = data.frame(cbind(fitted, results[[i]]$fitted.values))
}
names(fitted) = names(s6)



png("Stage6/plots/residsVSfitted1.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s6)){
  variable = names(s6)[i]
  plot(fitted[,i],resids[,i],xlab='Fitted',ylab='Residuals', main=paste('Residuals VS Fitted Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()


png("Stage6/plots/resids_acf.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s6)){
  variable = names(s6)[i]
  acf(resids[,i],xlab='Residuals', main=paste('ACF OF RESIDUALS: ', toupper(variable)))
}
par(mfrow=c(1,1))
dev.off()


residtest = data.frame(sapply(data.frame(resids), pretest))
residtest = t(residtest)
write.csv(residtest, "Stage6/tests/residualtests.csv")

png("Stage6/plots/resids_bell_curve.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s6)){
  variable = names(s6)[i]
  plot(density(resids[,variable]), main = paste("Normality Of ",variable), xlab='', ylab = "")
}
par(mfrow=c(1,1))
dev.off()



png("Stage6/plots/resids_qnorm_curve1.png", height=1000, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s6)){
  variable = names(s6)[i]
  qqnorm(resids[,variable], main = paste("Normality Of ",variable))
  qqline(resids[,variable], col='red', lwd=2)
}
par(mfrow=c(1,1))
dev.off()



#Insample Performance
inSampPerf = sapply(1:ncol(resids), function(dn)modelPerformance(residuals=resids[1:nrow(resids),dn], actuals = fitted[,dn]))
inSampPerf=data.frame(inSampPerf)
colnames(inSampPerf) = toupper(names(s6))
write.csv(t(inSampPerf),"Stage6/tests/InSampleperformance.csv")

#forecasting
predictions=predict(s6VAR, n.ahead=nrow(test))

#extract forecasts values
#fcst = data.frame(predictions)

fcst = data.frame(predictions$fcst$dos[,1])
for(i in 2:ncol(s6)){
  fcst = data.frame(cbind(fcst, predictions$fcst[[2]][,1]))
}
names(fcst) = names(s6)



#Out of sample performance
fresids = data.frame(sapply(c(1:ncol(s6)), function(dn)fcst[,dn]-test[,dn]))
outSampPerf = sapply(1:ncol(fcst), function(dn)modelPerformance(residuals=fresids[,dn], actuals=test[,dn]))
outSampPerf=data.frame(outSampPerf)
colnames(outSampPerf) = toupper(names(s6))
write.csv(t(outSampPerf),"Stage6/tests/OutSampleperformance.csv")




#granger 
gg = granger(s6, 1)
write.csv(gg,"Stage6/tests/granger.csv")





