
pullutantmean<-function(directory,pollutant,id=1:322){
  setwd("~/UMD/R/datasciencecoursera")
  setwd(directory)
  filels<-list.files(pattern="*.csv")
  pmean=vector(mode = "numeric",length=length(id))
  for(i in id){
  df<-read.csv(filels[i])
  good<-!is.na(df[,pollutant])
    pmean[i]<-mean(df[good,][,pollutant])
  }
  mean<-mean(pmean)
  mean
}
###################################
pollutantcomp<-function(directory,id=1:322){
  setwd("~/UMD/R/datasciencecoursera")
  directory<-paste("./",directory,sep="")
  setwd(directory)
  filels<-list.files(pattern="*.csv")
  good<-data.frame(id,rep(0,length(id)))
  for(i in id){
    df<-read.csv(filels[i])
    good[i,2]<-table(complete.cases(df))[2]
  }
  good
}
###################################
corr<-function(directory,threshold=0){
  setwd("~/UMD/R/datasciencecoursera")
  directory<-paste("./",directory,sep="")
  setwd(directory)
  filels<-list.files(pattern="*.csv")
  n<-length(filels)
  dat<-vector(mode="numeric",length = 0)
  for(i in 1:n){
    df<-read.csv(filels[i])
    csum <- sum((!is.na(df$sulfate)) & (!is.na(df$nitrate)))
    if(csum>threshold){
      dat<-c(dat,cor(df$sulfate,df$nitrate,use = "complete.obs"))
    }
  }
  dat
  
}
