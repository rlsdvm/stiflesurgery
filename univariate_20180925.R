data=read.csv('/Users/rlsdvm/Box Sync/R projects/stifle/stifle/stifledata092518.csv')
data$surgeon=as.character(data$surgeon)  #changing surgeon names to resident for those with fewer surgeries and less training
	data$surgeon[which(data$surgeon=='Joyce')]=data$surgeon[which(data$surgeon=='Hendrix')]='resident'
	data$surgeon=as.factor(data$surgeon)

data$lameness_degree[which(data$lameness_degree==0)]=1  #few in degree 0 or 5, so combining categories
	data$lameness_degree[which(data$lameness_degree==5)]=4
	data$lameness_degree=as.factor(data$lameness_degree)

data$discipline=as.character(data$discipline) #combining discipline categories to simplify
	data$discipline[which(data$discipline=='barrel_racing')]=data$discipline[which(data$discipline=='working_cow_horse')]='other'
	data$discipline[which(data$discipline=='roping')]='other';data$discipline[which(data$discipline=='show')]='western_pleasure'
	data$discipline=as.factor(data$discipline)

#categorical: discipline2(6),sex3(3),surgeon5(10),degree9(6),effusion10(4)
#2-level:unilat6,work7,
	#flexion11,osteophytes12,narrow_jt_space13,MFC_SBC14,fibrillation15,partial16,full17,curettage18
	#clefts19,chondromalacia20,meniscal_tear_degen21,fray_tear_cranial22,
	#fray_tear_cruciate23,steroids24,adequan25,irap26,stem_cells27,nsaids28,
	#systemic_suppl29
#numeric: age4,duration8
load('summary_for_stifle_data_20180925.R')

ps=rep(NA,27)
ps_all=rep(NA,nrow(summ))

library(brglm)

#categoric
start=c(1,5,8,9,18,20,22,23,27,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71)
for(i in 2:30){if(i!=11){
	datamin=data[which(is.na(data[,i])==FALSE),]
	unull=brglm(datamin$return_to_work~1,family=binomial)
	um=brglm(datamin$return_to_work~datamin[,i],family=binomial)
		lrt=unull$penalized.deviance-um$penalized.deviance
		ps[i-1]=1-pchisq(lrt,1)
	sm=summary(um)
	if(is.factor(datamin[,i])){
	ps_all[(start[i-1]+1):(start[i]-1)]=sm$coefficients[2:length(levels(factor(data[,i]))),4]
	}
}}



p_cols=matrix(NA,nrow=70,ncol=2);colnames(p_cols)=c('level p-value','overall p_value')
p_cols[,1]=ps_all
start2=c(1,5,8,9,18,20,22,23,27,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69)
p_cols[start2,2]=ps
p_cols=round(p_cols,3)

big_summ=cbind(summ,p_cols)
big_summ[which(is.na(ps_all)),5]='ref'
save(big_summ,file='full_summary_for_stifle_data_20180925.R')

pretty_summ=matrix(NA,nrow=70,ncol=4)
rownames(pretty_summ)=rownames(summ)
colnames(pretty_summ)=c('N return to work (%)','N no return (%)','level p-value','overall p-value')
pretty_summ[,1]=paste(big_summ[,1],' (',big_summ[,2],'%)',sep="")
pretty_summ[,2]=paste(big_summ[,3],' (',big_summ[,4],'%)',sep="")
	pretty_summ[c(8,22),1]=paste(big_summ[c(8,22),1],' (',big_summ[c(8,22),2],')',sep="")
	pretty_summ[c(8,22),2]=paste(big_summ[c(8,22),3],' (',big_summ[c(8,22),4],')',sep="")
pretty_summ[,3:4]=big_summ[,5:6]
	pretty_summ[which(is.na(pretty_summ[,4])),4]=""
	
write.csv(pretty_summ,file='pretty_summary_for_stifle_data_20180925.csv')

