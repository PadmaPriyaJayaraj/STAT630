
data=read.csv("Classifications_1000.csv")


## make sure actuals and classifications are 0 (no) or 1 (yes) only 
##  Built by Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);


    tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
    fp=nrow(df[df$classifications==1 & df$actuals==0,]);
    fn=nrow(df[df$classifications==0 & df$actuals==1,]);
    tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
    

    recall=tp/(tp+fn)
    precision=tp/(tp+fp)
    accuracy=(tp+tn)/(tp+fn+fp+tn)
    tpr=recall
    fpr=fp/(fp+tn)
    fmeasure=2*precision*recall/(precision+recall)
    ckddollars=1300*tp-100*(fp+tp)
    churndollars=1600*tp-100*(fp+tp)
    scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,ckddollars,churndollars)
    names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","ckd profit","churn profit")
       
  #print(scores)
  return(scores);
}

#Random Guessing
rg=seq(.01,.99,length.out=20)
rg_mat=matrix(nrow=length(rg),ncol=8)
colnames(rg_mat)=c("recall","precision","accuracy","tpr","fpr","fmeasure","ckd profit","churn profit")

for (ff in 1:length(rg)){
  f=rbinom(1000,1,rg[ff])
  c=data$churn
  rg_mat[ff,]=c_accuracy(c,f)
  
}

## write.csv(rg_mat,"RandomGuessing_Churning.csv")


