#bootstrap sampling funciton
#to run, clustervar should be the variables by which to hierarchicaly sample from top to bottom
#ie clustervar=c('university', 'major', 'GPA')
#replace can also be a logical vector by level to sample with or without replacement
bootsample <- function(dat, clustervar, replace = TRUE, reps = 1){
  cid=unique(dat[, clustervar[1]])
  ncid=length(cid)
  recid=sample(cid, size = ncid * reps, replace = TRUE)
  if (replace){
    rid=lapply(seq_along(recid), function(i){
      cbind(NewID=i, RowID=sample(which(dat[,clustervar] == recid[i]),
                                  size=length(which(dat[,clustervar]==recid[i])), replace=TRUE))
      })
  } else{
    rid=lapply(seq_along(recid), function(i){
      cbind(NewID=i, RowID=which(dat[,clustervar]==recid[i]))
    })
  }
  dat=as.data.frame(do.call(rbind, rid))
  dat$Replicate=factor(cut(dat$NewID, breaks=c(1, ncid *1:reps), include.lowest = TRUE,
                           labels=FALSE))
  dat$NewID=factor(dat$NewID)
  return(dat)
}




  
#sample with bootstrap function
set.seed(20)
tmp=bootsample(hdp, "DID", reps = 100)
bigdata=cbind(tmp, hdp[tmp$RowID, ])


#refit model with sampled data
f=fixef(m)
r=getME(m, "theta")
cl=makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))
#fitfunction
myboot=function(i){
  object=try(glmer(remission ~IL6 + CRP + CancerStage +LengthofStay +
                     Experience +(1 | NewID), data=bigdata, subset=Replicate==i, 
                   family=binomial, nAGQ = 1, start=list(fixef=f, theta=r)), 
             silent = TRUE)
  if(class(object)=="try-error")
    return(object)
  c(fixef(object), getME(object, "theta"))
}

#aggregate results into list
start=proc.time()
res=parLapplyLB(cl, X= levels(bigdata$Replicate), fun=myboot)
end=proc.time()
#shut down cluster
stopCluster(cl)

#summarize bootstrap results
success=sapply(res, is.numeric)
mean(success)

#convert list of bootstrap results to matrix
bigres=do.call(cbind, res[success])
#calculate 2.5 and 97,5 percentile for 95%CI
(ci=t(apply(bigres, 1, quantile, probs=c(0.025, 0.975))))

#concatenate results together
finaltable=cbind(Est=c(f, r), SE=c(se, NA), BootMean=rowMeans(bigres), ci)
round(finaltable,3)

 
      