data=read.csv('/Users/rlsdvm/Box Sync/R projects/stifle/stifle/stifledata092518.csv')

#changing surgeon names to group by number of surgeries and less training
data$surgeon=as.character(data$surgeon)
data$surgeon[which(data$surgeon=='Joyce')]=data$surgeon[which(data$surgeon=='Hendrix')]='resident'
data$surgeon[which(data$surgeon=='Hubert')]=data$surgeon[which(data$surgeon=='Trumble')]=data$surgeon[which(data$surgeon=='Frisbie')]='<10'
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
	
#no sex, no flexion, no SBC_TCA 
data=data[,-11]
data=data[,-3]
data=data[,-13]
library(brglm)
library(MASS)

#null model
mod=brglm(return_to_work~1,data=data,family=binomial)

# #permutations
# 	#for 100-1000 iterations
# 	minp=rep(NA,1000)
# 	for(k in 1:1000){
# 	#shuffle y
# 		y=data$return_to_work[order(runif(nrow(data)))]
# 	#fit univariate model to each x
# 		ps=rep(NA,27)
# 		for(i in 2:28){
# 			datamin=data[which(is.na(data[,i])==FALSE),]
# 			ymin=y[which(is.na(data[,i])==FALSE)]
# 			unull=brglm(ymin~1,family=binomial)
# 			um=brglm(ymin~datamin[,i],family=binomial)
# 			#an=anova(unull,um,test='Chisq')
# 			#ps[i-1]=an$"Pr(>Chi)"[2]
# 			lrt=unull$penalized.deviance-um$penalized.deviance
# 			ps[i-1]=1-pchisq(lrt,1)
# 		}
# 		#record lowest p-value
# 			minp[k]=min(ps,na.rm=TRUE)
# 	}
# 	#5th percentile of lowest p-values is entry threshold
# 	entry=quantile(minp,probs=0.05,na.rm=TRUE)
# 	#2xentry threshold is exit threshold
# 	exit=entry*2
	
#stepwise
	#trial all additions
	#trial all exits
	#if p-value above threshold, exit
		#else lowest p-value below threshold enters
##Round 1
	ps=rep(NA,26)
		for(i in 2:27){
			datamin=data[which(is.na(data[,i])==FALSE),]
				ymin=data$return_to_work[which(is.na(data[,i])==FALSE)]
			unull=brglm(ymin~1,family=binomial)
			um=brglm(ymin~datamin[,i],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps[i-1]=1-pchisq(lrt,1)
		}
#	belowentry=which(ps<entry);minbelow=which.min(belowentry);add=belowentry[minbelow]
add=which.min(ps);nameadd=names(data)[add+1]
min(ps);nameadd
	
data_in=data.frame(return_to_work=data$return_to_work,lameness_degree=data[,(add+1)])	#all data included in the model
data_out=data[,2:26];data_out=data_out[,-add]	#all data eligible for inclusion

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
#	belowentry=which(ps<entry);minbelow=which.min(belowentry);add=belowentry[minbelow]
add=which.min(ps);nameadd=names(data_out)[add]
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
#	belowentry=which(ps<entry);minbelow=which.min(belowentry);add=belowentry[minbelow]
add=which.min(ps);nameadd=names(data_out)[add]
min(ps);nameadd;ps2

data_in=cbind(data_in,data_out[,add])	#all data included in the model
	names(data_in)[4]=nameadd
data_out=data_out[,-add]	#all data eligible for inclusion

##Round 4
	ps=rep(NA,ncol(data_out))
		for(i in 1:ncol(data_out)){
			datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
				datamin_out=data_out[which(is.na(data_out[,i])==FALSE),]
			unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4],family=binomial)
			um=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4]+datamin_out[,i],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps[i]=1-pchisq(lrt,1)
		}
	ps2=rep(NA,ncol(data_in)-1)
	js=2:4
		for(i in 2:(ncol(data_in))){j=js[-i]
			datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
			um=brglm(datamin_in[,1]~datamin_in[,j[1]]+datamin_in[,j[2]],family=binomial)
			unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps2[i-1]=1-pchisq(lrt,1)
}
add=which.min(ps);nameadd=names(data_out)[add]
min(ps);nameadd;ps2

data_in=cbind(data_in,data_out[,add])	#all data included in the model
	names(data_in)[5]=nameadd
data_out=data_out[,-add]	#all data eligible for inclusion

##Round 5
	ps=rep(NA,ncol(data_out))
		for(i in 1:ncol(data_out)){
			datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
				datamin_out=data_out[which(is.na(data_out[,i])==FALSE),]
			unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4]+datamin_in[,5],family=binomial)
			um=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4]+datamin_in[,5]+datamin_out[,i],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps[i]=1-pchisq(lrt,1)
		}
	ps2=rep(NA,ncol(data_in)-1)
	js=2:5
		for(i in 2:(ncol(data_in))){j=js[-i]
			datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
			um=brglm(datamin_in[,1]~datamin_in[,j[1]]+datamin_in[,j[2]]+datamin_in[,j[3]],family=binomial)
			unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4]+datamin_in[,5],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps2[i-1]=1-pchisq(lrt,1)
}
	add=which.min(ps);nameadd=names(data_out)[add]
	min(ps);nameadd;ps2
	
	data_in=cbind(data_in,data_out[,add])	#all data included in the model
	names(data_in)[6]=nameadd
	data_out=data_out[,-add]	#all data eligible for inclusion
	##Round 6
	ps=rep(NA,ncol(data_out))
	for(i in 1:ncol(data_out)){
	  datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
	  datamin_out=data_out[which(is.na(data_out[,i])==FALSE),]
	  unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4]+datamin_in[,5]+datamin_in[,6],family=binomial)
	  um=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4]+datamin_in[,5]+datamin_in[,6]+datamin_out[,i],family=binomial)
	  lrt=unull$penalized.deviance-um$penalized.deviance
	  ps[i]=1-pchisq(lrt,1)
	}
	ps2=rep(NA,ncol(data_in)-1)
	js=2:6
	for(i in 2:(ncol(data_in))){j=js[-i]
	datamin_in=data_in[which(is.na(data_out[,i])==FALSE),]
	um=brglm(datamin_in[,1]~datamin_in[,j[1]]+datamin_in[,j[2]]+datamin_in[,j[3]]+datamin_in[,j[4]],family=binomial)
	unull=brglm(datamin_in[,1]~datamin_in[,2]+datamin_in[,3]+datamin_in[,4]+datamin_in[,5]+datamin_in[,6],family=binomial)
	lrt=unull$penalized.deviance-um$penalized.deviance
	ps2[i-1]=1-pchisq(lrt,1)
	}
	add=which.min(ps);nameadd=names(data_out)[add]
	min(ps);nameadd;ps2
	
#no p<0.05, consider interactions

##Interactions
	ps=matrix(NA,nrow=5,ncol=5)
	colnames(ps)=rownames(ps)=names(data_in)[2:6]
		for(i in 2:5){for(j in (i+1):6){
			unull=brglm(data_in[,1]~data_in[,2]+data_in[,3]+data_in[,4]+data_in[,5]+data_in[,6],family=binomial)
			um=brglm(data_in[,1]~data_in[,2]+data_in[,3]+data_in[,4]+data_in[,5]+data_in[,6]+data_in[,i]*data_in[,j],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps[(i-1),(j-1)]=1-pchisq(lrt,1)
	}}
	ps;

	ps=matrix(NA,nrow=5,ncol=5)
	colnames(ps)=rownames(ps)=names(data_in)[2:6]
		for(i in 2:5){for(j in (i+1):6){
			unull=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
				data_in$lameness_degree*data_in$lameness_duration,family=binomial)
			um=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
			           data_in$lameness_degree*data_in$lameness_duration+data_in[,i]*data_in[,j],family=binomial)
			lrt=unull$penalized.deviance-um$penalized.deviance
			ps[(i-1),(j-1)]=1-pchisq(lrt,1)
	}}
ps

	ps=matrix(NA,nrow=5,ncol=5)
	colnames(ps)=rownames(ps)=names(data_in)[2:6]
	for(i in 2:5){for(j in (i+1):6){
	  unull=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
	                (data_in$age+data_in$lameness_degree)*data_in$lameness_duration,family=binomial)
	  um=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
	             (data_in$age+data_in$lameness_degree)*data_in$lameness_duration+data_in[,i]*data_in[,j],family=binomial)
	  lrt=unull$penalized.deviance-um$penalized.deviance
	  ps[(i-1),(j-1)]=1-pchisq(lrt,1)
	}}
ps
	
	ps=matrix(NA,nrow=5,ncol=5)
	colnames(ps)=rownames(ps)=names(data_in)[2:6]
	for(i in 2:5){for(j in (i+1):6){
	  unull=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
	                (data_in$age+data_in$lameness_degree+data_in$cart_lesion_full)*data_in$lameness_duration,family=binomial)
	  um=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
	             (data_in$age+data_in$lameness_degree+data_in$cart_lesion_full)*data_in$lameness_duration+data_in[,i]*data_in[,j],family=binomial)
	  lrt=unull$penalized.deviance-um$penalized.deviance
	  ps[(i-1),(j-1)]=1-pchisq(lrt,1)
	}}
ps

	ps=matrix(NA,nrow=5,ncol=5)
	colnames(ps)=rownames(ps)=names(data_in)[2:6]
	for(i in 2:5){for(j in (i+1):6){
	  unull=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
	                (data_in$age+data_in$lameness_degree+data_in$cart_lesion_full+data_in$cart_lesion_partial)*data_in$lameness_duration,family=binomial)
	  um=brglm(data_in$return_to_work~data_in$lameness_degree+data_in$lameness_duration+data_in$age+data_in$cart_lesion_partial+data_in$cart_lesion_full+
	             (data_in$age+data_in$lameness_degree+data_in$cart_lesion_full+data_in$cart_lesion_partial)*data_in$lameness_duration+data_in[,i]*data_in[,j],family=binomial)
	  lrt=unull$penalized.deviance-um$penalized.deviance
	  ps[(i-1),(j-1)]=1-pchisq(lrt,1)
	}}
ps	

ps=matrix(NA,nrow=5,ncol=5)
colnames(ps)=rownames(ps)=names(data_in)[2:6]
for(i in 2:5){for(j in (i+1):6){
  unull=brglm(data_in$return_to_work~data_in$age*data_in$lameness_degree+(data_in$age+data_in$lameness_degree+data_in$cart_lesion_full+data_in$cart_lesion_partial)*data_in$lameness_duration,family=binomial)
  um=brglm(data_in$return_to_work~data_in$age*data_in$lameness_degree+(data_in$age+data_in$lameness_degree+data_in$cart_lesion_full+data_in$cart_lesion_partial)*data_in$lameness_duration+
             data_in[,i]*data_in[,j],family=binomial)
  lrt=unull$penalized.deviance-um$penalized.deviance
  ps[(i-1),(j-1)]=1-pchisq(lrt,1)
}}
ps	

	
finalmod=brglm(return_to_work~age*lameness_degree+(age+lameness_degree+factor(cart_lesion_full)+factor(cart_lesion_partial))*lameness_duration,family=binomial,data=data)
sfm=summary(finalmod)
cisfm=confint(finalmod,ci.method='mean')

library(car)
tiff("residual_plot_final_model.tif")
residualPlots(finalmod)
dev.off()

outtable=cbind(sfm$coefficients,exp(sfm$coefficients[,1]),exp(cisfm))
outtable=signif(outtable,2)
colnames(outtable)[5]='OR'

write.csv(outtable,file='results_of_model_20180925.csv')

save(finalmod,file='fitted_model_20180925.R')
save(data_in,file='data_for_fitted_model_20180925.Rdata')

finalmodnoint=brglm(return_to_work~lameness_degree+lameness_duration+age+factor(cart_lesion_partial)+factor(cart_lesion_full),family=binomial,data=data)
sfmn=summary(finalmodnoint)
cisfmn=confint(finalmodnoint,ci.method='mean')

tiff("residual_plot_final_model_model_noint.tif")
residualPlots(finalmodnoint)
dev.off()

outtablen=cbind(sfmn$coefficients,exp(sfmn$coefficients[,1]),exp(cisfmn))
outtablen=signif(outtablen,2)
colnames(outtablen)[5]='OR'

write.csv(outtablen,file='results_of_model_nointeractions_20180925.csv')
save(finalmodnoint,file='fitted_model_nointeractions_20180925.R')
