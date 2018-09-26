data=read.csv('/Users/rlsdvm/Box Sync/R projects/stifle/stifle/stifledata092518.csv')
#changing surgeon names to group by number of surgeries and less training
data$surgeon=as.character(data$surgeon)
data$surgeon[which(data$surgeon=='Joyce')]=data$surgeon[which(data$surgeon=='Hendrix')]='resident'
data$surgeon[which(data$surgeon=='Hubert')]=data$surgeon[which(data$surgeon=='Trumble')]=data$surgeon[which(data$surgeon=='Frisbie')]=data$surgeon[which(data$surgeon=="Kawcak")]='<10'
data$surgeon[which(data$surgeon=='Baxter')]=data$surgeon[which(data$surgeon=='Goodrich')]=data$surgeon[which(data$surgeon=='Hendrickson')]=data$surgeon[which(data$surgeon=='Kawack')]=data$surgeon[which(data$surgeon=='McIlwraith')]='>10'
data$surgeon=as.factor(data$surgeon)

#few in degree 0 or 5, so combining categories
data$lameness_degree[which(data$lameness_degree==0)]=1
data$lameness_degree[which(data$lameness_degree==5)]=4
data$lameness_degree=as.factor(data$lameness_degree)

#combining discipline categories to simplify
data$discipline=as.character(data$discipline)
data$discipline[which(data$discipline=='barrel_racing')]=data$discipline[which(data$discipline=='working_cow_horse')]='other'
data$discipline[which(data$discipline=='roping')]='other';data$discipline[which(data$discipline=='show')]='western_pleasure'
data$discipline=as.factor(data$discipline)

#no sex, no flexion, only SBC_TCA=1
#no variation in SBC, narrow_jt_space, fray_tear_cruciate_lig, flexion response; no residents, no effusion=none
data=data[which(data$SBC_TCA==1),]
data=data[,-c(3,11,13:15,24)]
data$surgeon=factor(as.character(data$surgeon))
data$effusion=factor(as.character(data$effusion))

#remove id
data=data[,-1]

library(brglm)
library(MASS)

mod=brglm(return_to_work~1,data=data,family=binomial)

#stepwise
	#trial all additions
	#trial all exits
	#if p-value above threshold, exit
		#else lowest p-value below threshold enters
##Round 1
	ps=rep(NA,23)
		for(i in 1:23){
			datamin=data[which(is.na(data[,i])==FALSE),]
				ymin=data$return_to_work[which(is.na(data[,i])==FALSE)]
			unull=brglm(ymin~1,family=binomial)
			um=brglm(ymin~datamin[,i],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps[i]=1-pchisq(lrt,1)
		}
#	belowentry=which(ps<entry);minbelow=which.min(belowentry);add=belowentry[minbelow]
add=which.min(ps);nameadd=names(data)[add]
min(ps);nameadd
	
data_in=data.frame(return_to_work=data$return_to_work,lameness_duration=data[,(add)])	#all data included in the model
data_out=data[,-add]	#all data eligible for inclusion

##Round 2
	ps=rep(NA,ncol(data_out))
		for(i in 1:ncol(data_out)){
			datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
				datamin_out=data_out[which(is.na(data_out[,i])==FALSE),]
			unull=brglm(datamin_in[,1]~datamin_in[,2],family=binomial)
			um=brglm(datamin_in[,1]~datamin_in[,2]+datamin_out[,i],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps[i]=1-pchisq(lrt,1)
		}
	add=which.min(ps);nameadd=names(data)[add]
	min(ps);nameadd
	
	data_in=cbind(data_in,data_out[,add])	#all data included in the model
	names(data_in)[3]=nameadd
	data_out=data_out[,-add]	#all data eligible for inclusion
	
	##Round 3
	ps=rep(NA,ncol(data_out))
	for(i in 1:ncol(data_out)){
	  datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
	  datamin_out=data_out[which(is.na(data_out[,i])==FALSE),]
	  unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3],family=binomial)
	  um=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_out[,i],family=binomial)
	  lrt=unull$penalized.deviance-um$penalized.deviance
	  ps[i]=1-pchisq(lrt,1)
	}
	ps2=rep(NA,ncol(data_in)-1)
	for(i in 2:(ncol(data_in))){j=ifelse(i==2,3,2)
	datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
	um=brglm(datamin_in[,1]~datamin_in[,j],family=binomial)
	unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3],family=binomial)
	lrt=unull$penalized.deviance-um$penalized.deviance
	ps2[i-1]=1-pchisq(lrt,1)
	}
	min(ps);ps2
	
	#none <0.05

#Interaction
	
unull=brglm(return_to_work~lameness_duration+systemic_suppl,family=binomial,data=data_in)
um=brglm(return_to_work~lameness_duration*systemic_suppl,family=binomial,data=data_in)
lrt=unull$penalized.deviance-um$penalized.deviance
ps=1-pchisq(lrt,1)

finalmod=brglm(return_to_work~lameness_duration+systemic_suppl,family=binomial,data=data_in)
sfm=summary(finalmod)
cisfm=confint(finalmod,ci.method='union')

library(car)
tiff("residual_plot_final_model_SBC.tif")
residualPlots(finalmod)
dev.off()


outtable=cbind(sfm$coefficients,exp(sfm$coefficients[,1]),exp(cisfm))
outtable=signif(outtable,2)
colnames(outtable)[5]='OR'

write.csv(outtable,file='results_of_model_SBC_2018_2.csv')

save(finalmod,file='fitted_model_SBC_2018_2.R')
save(data_in,file='data_for_fitted_model_SBC_2018_2.Rdata')

#predictions
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

tiff('ROC_curve_fitted_model_SBC_2.tif')
plot(1-Sp,Se,type='l',xlab="1-Specificity",ylab="Sensitivity",lwd=3)
text(0.5,0.5,paste('AUC =',round(auc_fm,2)))
dev.off()
