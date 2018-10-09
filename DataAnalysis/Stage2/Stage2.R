#STAGE 2 ATTEMPTS TO PREDICT AN ACTIVE RECON OR PORT SCAN ON THE NETWORK LAYER OF CYBERSPACE USING DATA FROM THE SOCIAL DIMENSION

#set working directories if any
#working.dir = '/media/ruth/Elements/PhdDataAnalysis/DataAnalysis/'
working.dir = '/Volumes/Elements/PhdDataAnalysis/DataAnalysis/'
setwd(working.dir)
set.seed(1000)
source("GlobalFunctions.R")
source("modelValidation.R")

#set stage experiment
stage = 2

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




rm(pcap)
rm(fw)
rm(ids)

sdcols = names(sd[c(3,4:13,16:25,27)])
sd = sd[,sdcols]

#rename columns properly 
names(sd) = c("Sentiment", "Cyber_Relate", "Chat_Size", "Entropy", "Fire_Relate", "Flu_Relate", "Negativity", "Neutrality", "Polarity","Positivity", "Subjectivity","Disgust","Fear","Joy","Sadness","Surprise","Trust","Anticipation","Chat_traffic","Chat_congestion","Datetime", "Wind_Speed")

#get endogenous variable for this stage of experiment
nd$scanner = (nd$undstip-nd$unsrcip)/nd$undstport
nd$recon = (nd$outbound/nd$unsrcip)/((nd$inbound+nd$outbound)/nd$undstip)
nd$cfr= (nd$inbound-nd$outbound)/nd$outbound

wf = getFourier(data.frame(nd$portscan+nd$portsweep))
dd1 = decompSeries(nd$portscan+nd$portsweep, 183)
tt=dd1$trend[complete.cases(dd1$trend)]

#endogenous variable plots
png("Stage2/plots/endo_scanner.png", width=900, height=400)
plot(nd$permin, nd$scanner, type="l", xlab="Datetime", ylab="Scanner", main="Network Scanner", col="brown", lwd=2, frame=F)
dev.off()

png("Stage2/plots/endo_recon.png", width=900, height=400)
plot(nd$permin, nd$recon, type="l", xlab="Datetime", ylab="Recon", main="Active Reconnaissance", col="brown", lwd=2, frame=F)
dev.off()


png("Stage2/plots/endo_cfr.png", width=900, height=400)
plot(nd$permin, nd$cfr, type="l", xlab="Datetime", ylab="Connection Failure Rate", main="Connection Failure Rate", col="brown", lwd=2, frame=F)
dev.off()

nd = nd[,c(1:21,26:52)]

ndcols = c("datetime", "database_cons", "incoming_dns", "incoming_http", "incoming_https", "incoming_mail", "meanbytes", "mean_captured_bytes", "mean_epoch", "mean_ip_length", "mean_time_to_live", "total_packets", "outgoing_dns", "outgoing_http", "outgoing_https", "outgoing_mail", "totalbytes", "total_captured_bytes", "total_epoch", "total_ip_length", "total_time_to_live", "portscans", "portsweep", "decoy", "fragmentation", "total_ids_alerts", "total_ids_prio_level", "connections_built", "connections_torn", "inbound_cons", "outbound_cons","tcp_cons","icmp_cons", "udp_cons","info_alerts", "warning_alerts","emergency_alerts", "denied_cons", "permitted_cons", "source_ips", "destination_ips", "source_ports", "destination_ports", "services", "total_connections", "scanner", "recon", "cfr")
names(nd) = ndcols 


#inegrate timelines

stage2 = data.frame(cbind(nd, head(sd, nrow(nd))))
time1 = stage2$permin
time2 = stage2$Datetime


stage2cols = tolower(names(stage2)[c(2:68,70)])
names(stage2) = tolower(names(stage2))
stage2 = stage2[order(stage2$permin),]


#get final data for this stage of experiment
s2 = stage2[,stage2cols]
s2 = s2[complete.cases(s2),]
s2 = s2[,colSums(s2)!=0]
s2 = data.frame(sapply(s2, as.numeric))


#split data into training and test sets (70-30)
train = s2[1:(nrow(s2)-120),]
test = tail(s2, 120)

#correlation analysis
#inter-dimensional correlatioin plot
ndcor=cor(train[,1:45])
sdcor = cor(train[,46:66])

png("Stage2/plots/inter_dim_cor_social.png", height=800, width=1000)
corrplot(sdcor,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

png("Stage2/plots/inter_dim_cor_eco.png", height=800, width=1000)
corrplot(ndcor,method='number', order='hclust', addrect=2, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()

#intra-dimensional correlation plot
s2cormat=cor(train)
png("stage2/plots/intra_dim_cor.png", height=800, width=1000)
corrplot(s2cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()


s2vars = c("database_cons", "incoming_dns", "incoming_mail", "meanbytes", "mean_captured_bytes", "mean_ip_length", "total_packets", "outgoing_mail", "decoy", "fragmentation", "total_ids_alerts", "icmp_cons", "udp_cons", "warning_alerts", "mean_epoch", "recon", "cfr","disgust","fear","trust","flu_relate","sadness","entropy","chat_traffic", "anticipation","cyber_relate", "joy","sentiment","surprise", "chat_size" )
s2=s2[,s2vars]

#feature selection for endogenous variable
s2cormat=cor(s2)
png("Stage2/plots/correlation_elimination.png", height=800, width=1000)
corrplot(s2cormat,method='number', order='hclust', addrect=6, tl.col='black', tl.cex=0.5, number.cex=0.5)
dev.off()




#recuresive feature selection
s2lm = lm(recon ~., data=s2)
s2 = stepAIC(s2lm, direction="both")$model





#VAR Importance analysis
s2lm = lm(recon ~., data=s2)
s2Imp = varImp(s2lm)

png("Stage2/plots/variable_importance.png", height=800, width=1000)
plot(as.factor(rownames(s2Imp)), s2Imp[,1], frame=F,s2imp[,1], las=2, cex.axis=0.7,main='Variable Importance for Predicting Active Reconnaissance', ylab='Variable Importance Level',xlab='')
abline(h=1, col="red", lwd=3)
dev.off()

s2vars = c('recon',rownames(s2Imp)[s2Imp >=1])
s2 = s2[,s2vars]

#Model order selection
#ACF PLOTS
png("Stage2/plots/var_acf.png", height=1000, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]
  acf(s2[,variable], main = paste("ACF ",variable), xlab='Lag', ylab = "Significance")
}
par(mfrow=c(1,1))
dev.off()

#pacf plots

#PACF PLOTS
png("Stage2/plots/var_pacf1.png", height=1000, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]
  pacf(s2[,variable], main = paste("PACF ",variable), xlab='Lag', ylab = "Significance")
}
par(mfrow=c(1,1))
dev.off()

s2 = data.frame(sapply(s2, norm))
test = data.frame(sapply(test, norm))
test = test[,names(s2)]


s2order = VARselect(s2,lag.max=80, type='const')
s2order = s2order$selection[1]

#stationarity and normality tests
s2pretests = data.frame(sapply(s2, pretest))
s2pretests = t(s2pretests)
write.csv(s2pretests,'Stage2/tests/s2tests.csv')




#decompose endogenous variable order = model order
#determine the ma order of each series in the data
s2mas = getMa(s2)
#transform dat awith ma values
s2ma = data.frame(sapply(1:ncol(s2), function(dn) ma(s2[,dn], order=s2mas[dn])))
names(s2ma) = cols
#decompose endogenous variable order = model order
orders = getFourier(s2)
orders


#decompose seasonal series
png("Stage2/plots/decompose_recon.png", height=1000, width=800)
dd1=decompSeries(s2[,1], orders[1])
dev.off()


png("Stage2/plots/decompose_frag.png", height=1000, width=800)
dd2=decompSeries(s2[,2], orders[2])
dev.off()

png("Stage2/plots/decompose_udp.png", height=1000, width=800)
dd3=decompSeries(s2[,4], orders[4])
dev.off()

png("Stage2/plots/decompose_cfr.png", height=1000, width=800)
dd4=decompSeries(s2[,6], orders[6])
dev.off()

png("Stage2/plots/decompose_sadness.png", height=1000, width=800)
dd5=decompSeries(s2[,8], orders[8])
dev.off()

png("Stage2/plots/decompose_cyb_relate.png", height=1000, width=800)
dd6=decompSeries(s2[,9], orders[9])
dev.off()

png("Stage2/plots/decompose_sadness.png", height=1000, width=800)
dd7=decompSeries(s2[,10], orders[10])
dev.off()

png("Stage2/plots/co_integrating.png", height=700, width=1000)
plot(norm(dd3$trend), type='l', ylim=c(-2.5,2.5), col='purple',lwd=2, frame=F,xlab='Lag', ylab='', main='Stage 2 Co-integrating Relationships')
#lines(norm(dd4$trend), col='red')
lines(norm(dd5$trend), col='green', lwd=2)
lines(norm(dd6$trend), col='blue', lwd=2)
legend("topright", legend=c("Ids Alerts", "Sadness", "Cyber-Relatedness"), fill=c("purple", "green", "blue"))
dev.off()





#co integration test
#Determine Jhansen's test for co-integrating rank
#johansen's co-integration test
s2VECM = VECM(s2, lag = s2order, estim='ML', include='both')
s2r = rank.test(s2VECM)

#cointegration plots

#PMi

#MI

#Determione which features are co-integrated with Engle Granger co-integration Matrix
s2coMatrix = cointMatrix(s2)   


#model Building

s2VAR = VAR(s2, type='both', p=s2order)
results = s2VAR$varresult


#VAR
resids = data.frame(results$recon$residuals)
for(i in 2:ncol(s2)){
  resids = data.frame(cbind(resids, results[[i]]$residuals))
}
names(resids) = names(s2)

#get fitted values data frame
fitted = data.frame(results$recon$fitted.values)
for(i in 2:ncol(s2)){
  fitted = data.frame(cbind(fitted, results[[i]]$fitted.values))
}
names(fitted) = names(s2)

#Residual Analysis

png("Stage2/plots/residsVSfitted.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]
  plot(fitted[,i],resids[,i],xlab='Fitted',ylab='Residuals', main=paste('Residuals VS Fitted Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()


png("Stage2/plots/resids.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]
  plot(resids[,i],xlab='Residuals', main=paste('Residuals Values: ', toupper(variable)))
  abline(h=0, col='red')
}
par(mfrow=c(1,1))
dev.off()

#ACF RESIDUALS

png("Stage2/plots/resids_acf.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]
  acf(resids[,i],xlab='Residuals', main=paste('ACF OF RESIDUALS: ', toupper(variable)))
}
par(mfrow=c(1,1))
dev.off()


png("Stage2/plots/resids_Pacf.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]  
  pacf(resids[,i],xlab='Residuals', main=paste('PACF OF RESIDUALS: ', toupper(variable)))
}
par(mfrow=c(1,1))
dev.off()



residtest = data.frame(sapply(data.frame(resids), pretest))
residtest = t(residtest)
write.csv(residtest, "Stage2/tests/residualtests.csv")

png("Stage2/plots/resids_bell_curve.png", height=800, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]
  plot(density(resids[,variable]), main = paste("Normality Of ",variable), xlab='', ylab = "")
}
par(mfrow=c(1,1))
dev.off()

png("Stage2/plots/resids_qnorm_curve.png", height=1000, width=1000)
par(mfrow=c(5,2))
for(i in 1:ncol(s2)){
  variable = names(s2)[i]
  qqnorm(resids[,variable], main = paste("Normality Of ",variable))
  qqline(resids[,variable], col='red', lwd=2)
}
par(mfrow=c(1,1))
dev.off()

#Insample Performance
#Insample Performance
inSampPerf = sapply(1:ncol(resids), function(dn)modelPerformance(residuals=resids[1:nrow(resids),dn], actuals = fitted[,dn]))
inSampPerf=data.frame(inSampPerf)
colnames(inSampPerf) = toupper(names(s2))
write.csv(t(inSampPerf),"Stage2/tests/InSampleperformance.csv")

#forecasting
predictions=predict(s2VAR, n.ahead=nrow(test))

#extract forecasts values
#fcst = data.frame(predictions)

fcst = data.frame(predictions$fcst$recon[,1])
for(i in 2:ncol(s2)){
  fcst = data.frame(cbind(fcst, predictions$fcst[[2]][,1]))
}
names(fcst) = names(s2)

#Out of sample performance
#Out of sample performance
fresids = data.frame(sapply(c(1:ncol(s2)), function(dn)fcst[,dn]-test[,dn]))
names(fresids) = names(s2)
outSampPerf = sapply(1:ncol(fresids), function(dn)modelPerformance(residuals=fresids[,dn], actuals=test[,dn]))
outSampPerf=data.frame(outSampPerf)
colnames(outSampPerf) = toupper(names(s2))
write.csv(t(outSampPerf),"Stage2/tests/OutSampleperformance.csv")




#granger 
gg = granger(s2, 1)
write.csv(gg,"Stage2/tests/granger.csv")




