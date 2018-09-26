data=read.csv('/Users/rlsdvm/Box Sync/R projects/stifle/stifle/stifledata092518.csv')

#changing surgeon names to group by less training
data$surgeon=as.character(data$surgeon)
data$surgeon[which(data$surgeon=='Joyce')]=data$surgeon[which(data$surgeon=='Hendrix')]='resident'
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

#only SBC_TCA=1
data=data[which(data$SBC_TCA==1),]

for(i in 10:30){data[,i]=as.factor(data[,i])}

#no variation in SBC, narrow_jt_space, fray_tear_cruciate_lig, flexion response; no residents, no effusion=none
data=data[,-c(11,13:15,24)]
data$surgeon=factor(as.character(data$surgeon))
data$effusion=factor(as.character(data$effusion))

#categorical: discipline2(6),sex3(3),surgeon5(10),degree9(6),effusion10(4)
#2-level:unilat6,work7,
	#flexion11,osteophytes12,narrow_jt_space13,MFC_SBC14,fibrillation15,partial16,full17,
	#clefts18,chondromalacia19,meniscal_tear_degen20,fray_tear_cranial21,
	#fray_tear_cruciate22,steroids23,adequan24,irap25,stem_cells26,nsaids27,
	#systemic_suppl28
#numeric: age4,duration8
load('/Users/rlsdvm/Box Sync/R projects/stifle/stifle/summary_for_stifle_data_20180925.R')

summ=summ[-c(13,16:17,29,31:32,35:40,57:58),]
ps=rep(NA,24)
ps_all=rep(NA,nrow(summ))

library(brglm)

#categoric
start=list(NULL)
start[[1]]=2:4;start[[2]]=6:7;start[[4]]=10:14;start[[5]]=16;start[[6]]=18
	start[[8]]=21:23;start[[9]]=25:26;start[[10]]=28;start[[11]]=30
	start[[12]]=32;start[[13]]=34;start[[14]]=36;start[[15]]=38;start[[16]]=40
	start[[17]]=42;start[[18]]=44;start[[19]]=46;start[[20]]=48;start[[21]]=50
	start[[22]]=52;start[[23]]=54;start[[24]]=56
for(i in 2:25){
	datamin=data[which(is.na(data[,i])==FALSE),]
	unull=brglm(datamin$return_to_work~1,family=binomial)
	um=brglm(datamin$return_to_work~datamin[,i],family=binomial)
		lrt=unull$penalized.deviance-um$penalized.deviance
		ps[i-1]=1-pchisq(lrt,1)
	sm=summary(um)
	if(is.factor(datamin[,i])){
	ps_all[start[[i-1]]]=sm$coefficients[2:ifelse(length(start[[i-1]])==2,2,(length(start[[i-1]])+1)),4]
	}
}

p_cols=matrix(NA,nrow=length(ps_all),ncol=2);colnames(p_cols)=c('level p-value','overall p_value')
p_cols[,1]=ps_all
start=c(1,5,8,9,15,17,19,20,24,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)
p_cols[start,2]=ps
p_cols=round(p_cols,3)

big_summ=cbind(summ,p_cols)
big_summ[which(is.na(ps_all)),5]='ref'
save(big_summ,file='full_summary_for_stifle_data_SBC_20180926.R')

pretty_summ=matrix(NA,nrow=length(ps_all),ncol=4)
rownames(pretty_summ)=rownames(summ)
colnames(pretty_summ)=c('N return to work (%)','N no return (%)','level p-value','overall p-value')
pretty_summ[,1]=paste(big_summ[,1],' (',big_summ[,2],'%)',sep="")
pretty_summ[,2]=paste(big_summ[,3],' (',big_summ[,4],'%)',sep="")
	pretty_summ[c(8,22),1]=paste(big_summ[c(8,22),1],' (',big_summ[c(8,22),2],')',sep="")
	pretty_summ[c(8,22),2]=paste(big_summ[c(8,22),3],' (',big_summ[c(8,22),4],')',sep="")
pretty_summ[,3]=big_summ[,5]
pretty_summ[,4]=big_summ[,6]
	pretty_summ[which(is.na(pretty_summ[,4])),4]=""
	
write.csv(pretty_summ,file='pretty_summary_for_stifle_data_SBC_20180926.csv')

