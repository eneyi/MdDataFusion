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


#get endo genous variable
nd$botnets = ((nd$indns+nd$outdns)/nd$UDP)/(nd$inbound+nd$outbound)






png("Stage5/plots/endo_botnets.png", width=1000, height=700)
plot(nd$permin[2:nrow(nd)], diff(nd$botnets), type='l', frame =F, ylab='Scaled DNS Requests', xlab='Datetime', main='Actions of Botnets on The Physical Dimension', col='brown', lwd=2)
dev.off()


####foiurier
bot = getFourier(data.frame(nd$botnet))
dd1 = decompSeries(nd$botnets, bot[1])
tt=dd1$trend[complete.cases(dd1$trend)]
ts=dd1$seasonal[complete.cases(dd1$seasonal)]




#inegrate timelines
Stage5 = data.frame(cbind(nd, tail(sd, nrow(nd))))
time1 = Stage5$permin
time2 = Stage5$Datetime


Stage5cols = tolower(names(Stage5)[c(2:54,57:68,70:78,81)])
names(Stage5) = tolower(names(Stage5))
#Stage5 = Stage5[order(Stage5$permin),]


#get final data for this stage of experiment
s5 = Stage5[,Stage5cols]
s5 = s5[complete.cases(s5),]
s5 = s5[,colSums(s5)!=0]
s5 = data.frame(sapply(s5, as.numeric))


#split data into training and test sets (70-30)
train = s5[1:(nrow(s5)-120),]
test = tail(s5, 120)




#correlation analysis
#inter-dimensional correlatioin plot
ndcor=cor(train[,1:51])
sdcor = cor(train[,52:73])

png("Stage5/plots/inter_dim_cor_social.png", height=800, width=1000)
corrplot(sdcor,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

png("Stage5/plots/inter_dim_cor_nd.png", height=800, width=1000)
corrplot(ndcor,method='number', order='hclust', addrect=2, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

#intra-dimensional correlation plot
s5cormat=cor(train)
png("Stage5/plots/intra_dim_cor.png", height=800, width=1000)
corrplot(s5cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

s5vars = c("recon","chat_traffic","botnets", "fragmentation","scanner","tot_alerts","cyber_rel","injection","fire_rel","icmp","flu_rel","numcons","outmail","dbcons","polarity","joy","meanbytes","meaniplen","average_wind_speed","trust","weaponization","disgust","fear","surprise","anticipation","meanepoch","undstips")
s5 = train[,s5vars]

s5cormat=cor(s5)
png("Stage5/plots/correlation_elimination.png", height=800, width=1000)
corrplot(s5cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()



#recuresive feature selection
s5lm = lm(botnets ~., data=s5)
s5 = stepAIC(s5lm, direction="both")$model



#VAR Importance analysis
s5lm = lm(botnets ~., data=s5)
s5Imp = varImp(s5lm)

png("Stage5/plots/variable_importance.png", height=800, width=1000)
plot(as.factor(rownames(s5Imp)), s5Imp[,1], frame=F,s5imp[,1], las=2, cex.axis=0.7,main='Variable Importance for Predicting Databse Injections', ylab='Variable Importance Level',xlab='')
abline(h=1, col="red", lwd=3)
dev.off()


#decompose endogenous variable order = model order

s5vars = c('botnets',rownames(s5Imp)[s5Imp >=1])
s5 = s5[,s5vars]


#stationarity and normality tests
s5pretests = data.frame(sapply(s5, pretest))
s5pretests = t(s5pretests)
write.csv(s5pretests,'Stage5/tests/s5tests.csv')

##%intra-dim correlation
(sum(s5cormat>0.7)/length(s5cormat))*100

s5order = VARselect(s5,lag.max=250, type='const')
s5order = s5order$selection[1]

#decompose endogenous variable order = model order
#determine the ma order of each series in the data
s5mas = getFourier(s5)
#transform dat awith ma values
s5ma = data.frame(sapply(1:ncol(s5), function(dn) ma(s5[,dn], order=s5mas[dn])))
names(s5ma) = cols
#decompose endogenous variable order = model order
orders = getFourier(s5)
orders

s5 = data.frame(sapply(s5, norm))



s5 = data.frame(sapply(s5, norm))
test = data.frame(sapply(test[,names(s5)], norm))
#Determine Jhansen's test for co-integrating rank
#johansen's co-integration test
s5VECM = VECM(s5, lag = s5order, estim='ML', include='both')
s5r = rank.test(s5VECM)

#cointegration plots

#PMi

#MI

#Determione which features are co-integrated with Engle Granger co-integration Matrix
s5coMatrix = cointMatrix(s5)   


#model Building

s5VAR = VAR(s5, type='both', p=s5order)
results = s5VAR$varresult



#VAR
resids = data.frame(results$botnets$residuals)
for(i in 2:ncol(s5)){
  resids = data.frame(cbind(resids, results[[i]]$residuals))
}
names(resids) = names(s5)

#get fitted values data frame
fitted = data.frame(results$botnets$fitted.values)
for(i in 2:ncol(s5)){
  fitted = data.frame(cbind(fitted, results[[i]]$fitted.values))
}
names(fitted) = names(s5)



png("Stage5/plots/residsVSfitted1.png", height=800, width=1000)
par(mfrow=c(3,3))
for(i in 1:ncol(s5)){
  variable = names(s5)[i]
  plot(fitted[,i],resids[,i],xlab='Fitted',ylab='Residuals', main=paste('Residuals VS Fitted Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()


png("Stage5/plots/resids_acf.png", height=800, width=1000)
par(mfrow=c(3,3))
for(i in 1:ncol(s5)){
  variable = names(s5)[i]
  acf(resids[,i],xlab='Residuals', main=paste('ACF OF RESIDUALS: ', toupper(variable)))
}
par(mfrow=c(1,1))
dev.off()


residtest = data.frame(sapply(data.frame(resids), pretest))
residtest = t(residtest)
write.csv(residtest, "Stage5/tests/residualtests.csv")

png("Stage5/plots/resids_bell_curve.png", height=800, width=1000)
par(mfrow=c(3,3))
for(i in 1:ncol(s5)){
  variable = names(s5)[i]
  plot(density(resids[,variable]), main = paste("Normality Of ",variable), xlab='', ylab = "")
}
par(mfrow=c(1,1))
dev.off()



png("Stage5/plots/resids_qnorm_curve1.png", height=1000, width=1000)
par(mfrow=c(3,3))
for(i in 1:ncol(s5)){
  variable = names(s5)[i]
  qqnorm(resids[,variable], main = paste("Normality Of ",variable))
  qqline(resids[,variable], col='red', lwd=2)
}
par(mfrow=c(1,1))
dev.off()



#Insample Performance
inSampPerf = sapply(1:ncol(resids), function(dn)modelPerformance(residuals=resids[1:nrow(resids),dn], actuals = fitted[,dn]))
inSampPerf=data.frame(inSampPerf)
colnames(inSampPerf) = toupper(names(s5))
write.csv(t(inSampPerf),"Stage5/tests/InSampleperformance.csv")

#forecasting
predictions=predict(s5VAR, n.ahead=nrow(test))

#extract forecasts values
#fcst = data.frame(predictions)

fcst = data.frame(predictions$fcst$botnets[,1])
for(i in 2:ncol(s5)){
  fcst = data.frame(cbind(fcst, predictions$fcst[[2]][,1]))
}
names(fcst) = names(s5)



#Out of sample performance
fresids = data.frame(sapply(c(1:ncol(s5)), function(dn)fcst[,dn]-test[,dn]))
outSampPerf = sapply(1:ncol(fcst), function(dn)modelPerformance(residuals=fresids[,dn], actuals=test[,dn]))
outSampPerf=data.frame(outSampPerf)
colnames(outSampPerf) = toupper(names(s5))
write.csv(t(outSampPerf),"Stage5/tests/OutSampleperformance.csv")




#granger 
gg = granger(s5, 1)
write.csv(gg,"Stage5/tests/granger.csv")





