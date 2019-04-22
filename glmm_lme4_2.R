#estimate mixed effects logisitc regssion with 
  #Il6, CRP, and LengthofStay, (doctor) Experience as continuous predictors,
  #CancerStage as categoriacal predictor, and 
  #random intercept by DID (doctor ID)

m<-glmer(remission ~ IL6 +CRP +CancerStage +LengthofStay +Experience +
           (1| DID), data =hdp, family=binomial, control=glmerControl(optimizer = "bobyqa"),
         nAGQ = 10)
#print without correlations (I don't see a difference with or without corr???)
print(m, corr=FALSE)

#find confidence intervals 
#here Est is the standard error and the CI have a 
  #lower limit and upper limit applied by 1.96 times this standard error
se= sqrt(diag(vcov(m)))
(tab=cbind(Est=fixef(m), LL= fixef(m) -1.96 * se, UL=fixef(m) + 1.96 *
             se))
