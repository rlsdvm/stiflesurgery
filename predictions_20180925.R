
load('/Users/rlsdvm/Box Sync/R projects/stifle/fitted_model_20180925.R')
load('/Users/rlsdvm/Box Sync/R projects/stifle/data_for_fitted_model_20180925.Rdata')
library(brglm)
datamin=data_in[which(complete.cases(data_in)==TRUE),]

pfm=predict(finalmod,type='response',newdata=datamin)	#predicted probability of returning to work
returns=which(datamin$return_to_work==1);rl=length(returns)
nonrs=which(datamin$return_to_work==0);nrl=length(nonrs)

cutoffs=seq(0,1,by=0.01)
Se=Sp=rep(NA,length(cutoffs))
for(i in 1:length(cutoffs)){
	Se[i]=length(which(pfm[returns]>cutoffs[i]))/rl
	Sp[i]=length(which(pfm[nonrs]<cutoffs[i]))/nrl
}

library(flux)
auc_fm=auc(1-Sp,Se)

tiff('ROC_curve_fitted_model_20180925.tif')
plot(1-Sp,Se,type='l',xlab="1-Specificity",ylab="Sensitivity",lwd=3)
text(0.4,0.7,paste('AUC =',round(auc_fm,2)))
lines(c(0,1),c(0,1),lty=2)
dev.off()


ndata=data.frame(lameness_degree=c(2,2,4,4),lameness_duration=rep(4,4),age=c(2,6,2,6),
cart_lesion_full=rep(0,4),cart_lesion_partial=rep(0,4))
ndata$lameness_degree=as.factor(ndata$lameness_degree)
newpred=predict(finalmod,newdata=ndata,type='response')

cf=finalmod$coefficients
cf1=cf[2]+cf[6]*2+cf[14]*2
cf2=cf[2]+cf[6]*6+cf[14]*6
cf3=cf[4]+cf[6]*2+cf[16]*2
cf4=cf[4]+cf[6]*6+cf[16]*6


######
no_int_mod=brglm(return_to_work~lameness_degree+lameness_duration+age+cart_lesion_full+cart_lesion_partial,family=binomial,data=data_in)
	cutoffs=seq(0,1,by=0.01)
pfm=predict(no_int_mod,type='response',newdata=datamin)	#predicted probability of returning to work
Se_n=Sp_n=rep(NA,length(cutoffs))
for(i in 1:length(cutoffs)){
	Se_n[i]=length(which(pfm[returns]>cutoffs[i]))/rl
	Sp_n[i]=length(which(pfm[nonrs]<cutoffs[i]))/nrl
}

auc_n=auc(1-Sp_n,Se_n)
tiff('ROC_no_interactions_20180925.tif')
plot(1-Sp_n,Se_n,type='l',xlab="1-Specificity",ylab="Sensitivity",lwd=3)
text(0.4,0.7,paste('AUC =',round(auc_n,2)))
lines(c(0,1),c(0,1),lty=2)
dev.off()

tiff('ROC_both_20180925.tif')
plot(1-Sp_n,Se_n,type='l',xlab="1-Specificity",ylab="Sensitivity",lwd=3)
lines(1-Sp,Se,lty=3,lwd=3)
legend(0.2,0.5,c(paste('with interactions, AUC =',round(auc_fm,2)),paste('without interactions AUC =',round(auc_n,2))),lty=c(3,1),lwd=3)
lines(c(0,1),c(0,1),lty=2)
dev.off()
