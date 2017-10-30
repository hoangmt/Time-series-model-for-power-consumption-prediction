Utilities.SubstationIDList<-function(ss){
  #To be updated with online way to get substation list
  PathToMeterFile = file.path('substation','Lib','substation_relations.csv')
  meters=read.csv(PathToMeterFile,encoding='latin-1')
  #meters = meters[complete.cases(meters), ]
  assets=meters[meters['SubstationId']==ss,'MeterAssetId'] # take id of all houses registered to the substation ss
  return(assets)
}

Utilities.DownloadHouseholdDataAPI <-function(id='12911',from='2017-09-10T10:00:00', to='2017-09-11T10:00:00',typeClass='1',resolution='7',Debug=FALSE){
  print('Read meter reading data and return the total consumption of the hour')
  #browser()
  print(id)
  require(jsonlite)
  username = "f454617b-9f46-4da0-b539-064faac3995c";
  password = "806eee51-5e91-43b1-b05b-edaebe43fc37";
  secret <- jsonlite::base64_enc(paste(username, password, sep = ":"))
  url <- paste("https://rikprodwebapi.esmartapi.com/api/timeseries/",id,"/typeClass/",typeClass,"/resolution/",resolution,"?from=",from,"&to=",to,sep='')
  #url <- paste("https://esmartprodwebapi.azurewebsites.net/api/timeseries/",id,"/typeClass/",typeClass,"/resolution/",resolution,"?from=",from,"&to=",to,sep='')
  print(url)
  req <- httr::GET(url, httr::add_headers(
    "Authorization" = paste("Basic", gsub("\n", "", secret))
  ))
  
  json <- httr::content(req, as = "text")
  data1 <- (fromJSON(json))$Values
  #save('data1',file='data1.RData')
  data1 =data1[!duplicated(data1[,2]),]#remove duplicated values if there is.
  
  #browser()
  if (Debug)
  {
    timeIDs=as.POSIXct(data1$ValueTime,format="%Y-%m-%d T %H:%M:%S", tz="UTC")
    x11()
    plot(timeIDs,data1$Value,xlab='Time',ylab='Power consumption',type='l')
    x11()
    plot(timeIDs,data1$Status,xlab='Time',ylab='Status of the measurement',type='l')
    print('here')
  }
  if (as.numeric(typeClass)==100){
  x=diff(data1$Value)
  status=data1$Status
  status=cbind(drop(coredata(status[1:length(x)])),drop(coredata(status[2:(length(x)+1)])))
  status=apply(status,1,max)
  
  output=data.frame(c(x,status,data1$ValueTime[1:(length(data1$ValueTime)-1)]),stringsAsFactors=FALSE)
  colnames(output)=id}
  
  if (as.numeric(typeClass)==1){
    x=data1$Value
    status=data1$Status
    output=data.frame(c(x,status,data1$ValueTime),stringsAsFactors=FALSE)
    colnames(output)=id
  }
  return(output)
}

Utilities.PlotMatching <- function(ytrain, ytrainpred, yTestObserved, yTestHat,tTrain=NULL){
  library(zoom)
  
  x=index(rbind(ytrain,yTestObserved))
  plot(x,rbind(ytrain,yTestObserved),type='l',ylab='y',xlab='t')
  
  lines(x[1:length(ytrain)],ytrain, col='blue')
  lines(x[1:length(ytrainpred)],ytrainpred, col='blue',lty=2)
  
  lines(x[(length(ytrain)+1):length(x)],yTestObserved, col='red')
  lines(x[(length(ytrain)+1):length(x)],yTestHat, col='green',lty=2)
  
  legend(x="topright",legend=c("y","yhat"), lty=c(1,2))
}

Utilities.DownloadSubstationData <-function(ss='44820',from='2017-01-25T10:00:00', to='2017-10-25T10:00:00',typeClass='100',resolution='7',ncores=3,OutlierRate=0.2,Debug=TRUE,TimeUnit='hours'){
  assets=Utilities.SubstationIDList(ss)
  print(assets)
  #print(assets)
  require(doParallel)
  require(imputeTS)
  require(xts)
  cl <- makeCluster(ncores)
  print('insider substation data func')
  print(from)
  print(to)
  Utilities.DownloadHouseholdDataAPI(id=toString(assets[1]),from=from, to=to)
  registerDoParallel(cl)
  df=foreach(i=1:length(assets),.packages=c('RCurl','xts'), .export=c('Utilities.DownloadHouseholdDataAPI'), .combine=cbind) %dopar% Utilities.DownloadHouseholdDataAPI(id=toString(assets[i]),from=from, to=to)
  stopCluster(cl)
  NMeasurements=dim(df)[1]
  y=df[1:(NMeasurements/3),]
  y=data.frame(lapply(y,as.numeric))
  y=as.matrix(y)
  status=df[(1+NMeasurements/3):(2*NMeasurements/3),]
  status=data.frame(lapply(status,as.numeric))
  status=as.matrix(status)
  Time=df[(1+2*NMeasurements/3):NMeasurements,]
  print('Finished downloading data for the substation, start cleaning data')
  
  BadMeasurements=matrix(rep(0,dim(status)[1]*dim(status)[2]),nrow=dim(status)[1],ncol=dim(status)[2])
  BadMeasurements[status>3999]=1
  BadRate=colMeans(BadMeasurements)
  
  y=y[,which(BadRate<OutlierRate)]
  status=status[,which(BadRate<OutlierRate)]
  y[status>3999]=NA
  y=apply(y,2,na.interpolation)#remove na by interpolation along the column
  timeIDs=as.POSIXct(Time[,1],format="%Y-%m-%d T %H:%M:%S", tz="UTC")
  #browser()
  NumHours=as.numeric(difftime(as.POSIXct(to,format="%Y-%m-%d T %H:%M:%S", tz="UTC"),as.POSIXct(from,format="%Y-%m-%d T %H:%M:%S", tz="UTC"),units=TimeUnit))+1
  if (NumHours<dim(y)[1]){
    print('!!!!!!!!!!!!!!!!There are missing values, need to check the api that load data!!!!!!!!!!!!!!!!!!!!!!!')
    #Find the missing hour
    #Insert NA values
    #Impute
  }
  
  yss=xts(x=rowSums(y),order.by = timeIDs)
  if (Debug)
    return(structure(list(yss=yss,yAllHousehold=y,status=status)))
  else 
    return(structure(list(yss=yss,status=status,BadRate=BadRate)))
}

Utilities.PeformanceEvaluation <- function(ss,Npoints,PointType,PointRange,Method){
  ####This function will evaluate the performance of a Method (method) at different type of time (PointType): weekend, weekday, night, general, each hour when applying to a substation ss
  #Randomly generate Npoints of PointType in the PointRange. PointRange[1]: starting hour,     PointRange[2]: Last hour, format: "Y-m-dTH:M:SZ"
  LastTrainingPoint=Utilities.TimeSet(PointType,Npoints=3,PointRange)
  cl <- makeCluster(3)
  registerDoParallel(cl)
  df=foreach(LastPoint=LastTrainingPoint,.packages=c('RCurl','xml2','digest','httr','tseries','forecast','imputeTS')) %dopar% train_predict(ss,LastPoint,168)
  stopCluster(cl)
  save('df',file=paste(ss,'.RData'))
}

Utilities.TimeSet<-function(PointType,Npoints=30,PointRange=c('2017-01-01T00:01:00Z','2017-09-01T00:01:00Z')){
  #type = -2 for weekend, -1 for weekday, -3 for night time(6pm to 6am),-4 for day time(6am to 6pm), h for hour h of the day, -5 for general performance, a vector for a certain set of time
  #This function will generate all hours of certain type in a range of time
  PointRangeStart=as.POSIXct(PointRange[1],format="%Y-%m-%d T %H:%M:%S Z", tz="UTC")
  PointRangeEnd=as.POSIXct(PointRange[2],format="%Y-%m-%d T %H:%M:%S Z", tz="UTC")
  IndexRange=as.numeric(difftime(PointRangeEnd,PointRangeStart,units='hour'))
  AllTimeSet=PointRangeStart+3600*c(1:IndexRange)
  if (PointType==-1){#weekday
    HourSet=AllTimeSet[which(weekdays(AllTimeSet)%in%c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))]
  }
  if (PointType==-2){#weekend
    HourSet=AllTimeSet[which(weekdays(AllTimeSet)%in%c('Saturday', 'Sunday'))]
  }
  if (PointType==-3){#night time
    HourSet=AllTimeSet[which(as.numeric(format(as.POSIXct(AllTimeSet,format="%H:%M:%S"),"%H"))%in%c(18:23,0:6))]
  }
  if (PointType==-4){#day time
    HourSet=AllTimeSet[which(as.numeric(format(as.POSIXct(AllTimeSet,format="%H:%M:%S"),"%H"))%in%c(6:18))]
  }
  if (PointType==-5){#general hour
    HourSet=AllTimeSet
  }
  if (PointType>=0){#each hour
    HourSet=AllTimeSet[which(as.numeric(format(as.POSIXct(AllTimeSet,format="%H:%M:%S"),"%H"))%in%c(PointType))]
  }
  if (length(PointType)>1){#certain set of time in the day
    HourSet=AllTimeSet[which(as.numeric(format(as.POSIXct(AllTimeSet,format="%H:%M:%S"),"%H"))%in%PointType)]
  }
  if (Npoints<length(HourSet)){
    IndexToSample=sample(1:length(HourSet), Npoints, replace=F)
    HourSet=HourSet[IndexToSample]
  }
  return(HourSet)
}



Utilities.PlotSubstationData<- function(i,SubIDs=SubstationIDs,from='2015-08-11T10:00:00', to='2017-10-11T10:00:00'){
  ss=toString(SubIDs[i]);
  tem=Utilities.DownloadSubstationData(ss,from=from,to=to)
  x11();plot(tem$yss,main=paste('Substation ',toString(i),':',ss))
}

Utilities.OutlierRemoval<-function(y,lower=0.001,upper=0.999, LowerYQuantile=0.001){
  yy=y
  dy=diff(yy)
  quant=quantile(dy,probs=c(lower,upper),na.rm=TRUE)
  OutlierID=which(dy<quant[1]|dy>=quant[2])# outliers in the derivative
  IDtoRemove=OutlierID
  diffOutlierID=diff(OutlierID)
  smallID=which(diffOutlierID<24&diffOutlierID>1)
  if (length(smallID)>0){
  for (i in (1:length(smallID)))
    IDtoRemove=c(IDtoRemove,(OutlierID[smallID[i]]+1):(OutlierID[smallID[i]+1]-1))
  yy[IDtoRemove]=NA
  yy=na.interpolation(yy)}
  
  quant=quantile(yy,probs=LowerYQuantile,na.rm=TRUE)
  LowerBoundOutlier=quant
  OutlierID1=which(yy<LowerBoundOutlier)#Outliers in the series itself - values that are too small
  
  #The code below remove the outage outliers if any. This code can be further optimized!
  IDtoRemove=OutlierID1
  for (i in OutlierID1)
  {
    tem=which(dy>0)
    tem=tem[which(tem<i)]
    if (length(tem)>0)
      IDtoRemove=c(max(tem):(i-1),IDtoRemove)
    tem=which(dy<0)
    tem=tem[which(tem>i)]
    if (length(tem)>0)
      IDtoRemove=c((i+1):min(tem),IDtoRemove)
  }
  IDtoRemove=unique(IDtoRemove)
  yy[IDtoRemove]=NA
  
  yy=na.interpolation(yy)
  return(yy)
}

Utilities.PlotFilter <- function(filenumber,LowerYQuantile = 0.001){
  mainpath="C:/Users/Hoang Tran/Dropbox/temp_esmart/tem" #C:\Users\tran\Dropbox\temp_esmart
  load(paste(mainpath,toString(filenumber),".RData",sep=''))
  p1<-plot(Utilities.OutlierRemoval(tem$yss,LowerYQuantile=LowerYQuantile),main=paste('Filtered -file',toString(filenumber)))
  p2<-plot(tem$yss,main='Original')
  p3<-plot(tem$yss-Utilities.OutlierRemoval(tem$yss,LowerYQuantile=LowerYQuantile),main='Difference: original - filtered')
  p4<-plot(diff(Utilities.OutlierRemoval(tem$yss,LowerYQuantile=LowerYQuantile)),main='Filtered - derivative')
  p5<-plot(diff(tem$yss),main='Derivative - original')
  return(structure(list(p1=p1,p2=p2,p3=p3,p4=p4,p5=p5)))
}

Utilities.ComputeMapeOne<- function(df){
  #structure of df: df[[i]] - result from run i, it has 3 components:
  #df[[i]]$yHat,df[[i]]$yObserved, df[[i]]$ytrain
  Nahead=length(df[[1]]$yHat)
  Nruns=length(df)
  MapeMeanAll=matrix(nrow=Nruns,ncol=Nahead)
  MapeMedianAll=matrix(nrow=Nruns,ncol=Nahead)
  MapeAll=matrix()
  
  yHat=matrix(nrow=length(df),ncol=Nahead)
  yObserved=matrix(nrow=length(df),ncol=Nahead)
  for (i in 1:length(df)){
    yHat[i,]=df[[i]]$yHat
    yObserved[i,]=df[[i]]$yObserved
  }
  delta=yHat-yObserved
  Mape=abs(delta/yObserved)
  MapeMedian=apply(abs(t(Mape)),1,median)
  MapeMean=colMeans(abs(Mape))
  
  return(structure(list(Mape=Mape,Median=MapeMedian,Mean=MapeMean)))
}


Utilities.DownloadTemperatureHousehold <- function(id=88401,FromDate='2014-09-10T10:00:00', ToDate='2016-12-22T10:00:00',tsType="450"){
  require(xts)
  require(httr)
  require(RCurl)
  require(imputeTS)
  
  token = "BwtMhdv2ygILPRNrYwlBLMpLWBSLGnnI0W0CBXf7AsI="
  
  API = "https://esmartprodcloudservice.cloudapp.net/TsValues.svc"
  UrlLink = paste(API,"/GetTsData?request={%22to%22",":%20%22",ToDate,"%22", sep = '')
  UrlLink = paste(UrlLink,",%20%22from%22",":%20%22",FromDate,"%22", sep = '')
  UrlLink = paste(UrlLink,",%20%22id%22",":%20%22",id,"%22", sep = '')
  UrlLink = paste(UrlLink,",%20%22tsType%22",":%20%22",tsType,"%22", sep = '')
  UrlLink = paste(UrlLink,"}&token=",token, sep = '')
  #browser()
  print(UrlLink)
  
  tem_data=getURLContent(UrlLink,ssl.verifypeer=FALSE)
  
  tem_data=gsub("\"","",tem_data)
  tem_data = read.csv(text=tem_data,header=FALSE)
  tem_data = head(tem_data,-1)
  
  unique_ids = duplicated(tem_data[,2])
  tem_data =tem_data[!unique_ids,]
  y=tem_data[,3]
  
  y=na.interpolation(y)
  timeIDs=as.POSIXct(tem_data[!unique_ids,2],format="%Y-%m-%d T %H:%M:%S", tz="UTC")
  IdTimeNA=which(is.na(timeIDs))
  for (IdNa in IdTimeNA)
    timeIDs[IdNa]=timeIDs[IdNa-1]+3600
  y=xts(x=y,order.by = timeIDs)
  
  return(y)
}

Utilities.DownloadTemperatureSubstation<- function(ss='420H',FromDate='2014-09-10T10:00:00', ToDate='2017-10-20T10:00:00',ncores=3,outliner_level=10){
  require(xts)
  require("RCurl")
  PathToMeterFile = file.path('substation','Lib','substation_relationsold.csv')
  
  meters=read.csv(PathToMeterFile,encoding='latin-1')
  meters = meters[complete.cases(meters), ]
  print(PathToMeterFile)
  assets=meters[meters['SubstationName']==ss,'MeterAssetId'] # take id of all houses registered to ss
  
  require(doParallel)
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  df_tem=foreach(i=1:length(assets),.packages=c('RCurl','xts'), .export=c('Utilities.DownloadTemperatureHousehold','FromDate','ToDate'), .combine=cbind) %dopar% Utilities.DownloadTemperatureHousehold(assets[i],FromDate,ToDate,tsType="450")
  stopCluster(cl)
  
  Temperature=xts(x=rowMeans(df_tem),order.by = index(df_tem))
  return(Temperature)
}


Utilities.UploadModel <- function(Model,ss){
  #ModelFileName = file.path(getwd(), 'substation',local_model_folder,paste0(ss,'.R'))
  require(httr)
  require(RCurl)
  ModelFileName = paste0('modelforsubstation',ss,'s.R')
  print(ModelFileName)
  dump('Model',ModelFileName)
  
  print('Start uploading Model to blob')
  
  saname = "esmartprodstorage"
  blobcontainer = "model"
  blobname = ss
  sak = 'e2VxYkn7AlsHInKUbbjpSWa1qdewR92dOS/loVmvc/KuU8V6CxejJZJQZ8XvL51LpZdAaJi8TqYvBIsH9nk5og=='
  
  print('Creating container...')
  #create the container
  browser()
  url = paste("https://",saname,".blob.core.windows.net/",blobcontainer,"?restype=container", sep ="")
  azure_blob_call(url, "PUT", sak,headers = c("x-ms-blob-type"="BlockBlob")) #upload_file
  
  #Save the blob to the container
  print('Saving...')
  url = paste("https://",saname,".blob.core.windows.net/",blobcontainer,"/",blobname, sep ="")
  
  azure_blob_call(url, "PUT", sak,headers = c("x-ms-blob-type"="BlockBlob"), requestBody = upload_file(ModelFileName))
}

Utilities.DownloadModel <- function(ss,blobcontainer)
{
  require(httr)
  require(RCurl)
  print('starting R prediction function------------------------------------------')
  blobname = ss
  sak="e2VxYkn7AlsHInKUbbjpSWa1qdewR92dOS/loVmvc/KuU8V6CxejJZJQZ8XvL51LpZdAaJi8TqYvBIsH9nk5og=="
  saname = "esmartprodstorage"
  print('starting R prediction function--------------------------------1')
  url = paste("https://",saname,".blob.core.windows.net/",blobcontainer,"/",blobname, sep ="")
  tem=azure_blob_call(url, "GET", sak)
  #browser()
  tem=gsub("\r\n","",tem)
  print(tem)
  return(tem)
  #return(eval(parse(text=tem)))# This function will load the structure dshw_model into memory!!!!!
}
