Rfunc <-function(myArgs){
  ServiceID=as.numeric(myArgs[1])
  ss = myArgs[2]
  
  #ServiceID can be Training(1) or Prediction(2)
  #If call Prediction then do both training and prediction
  
require(xts)
#source(file.path(getwd(), 'Lib','dshw_functions.R'))
#source(file.path(getwd(), 'Lib','GamModels.R'))
#source(file.path(getwd(), 'Lib','Utilities.R'))
#source(file.path(getwd(), 'Lib','azure_blob_call.R'))
  source(file.path('substation', 'Lib','dshw_functions.R'))
  source(file.path('substation', 'Lib','GamModels.R'))
  source(file.path('substation', 'Lib','Utilities.R'))
  source(file.path('substation', 'Lib','TrainPred.R'))
  source(file.path('substation', 'Lib','azure_blob_call.R'))
#load(file.path(getwd(), 'Lib','Overloaded.RData'))
#Load all house holds that belong to a substation

PathToMeterFile = file.path('substation','Lib','substation_relations.csv')
SubstationMeterRelation=read.csv(PathToMeterFile,encoding='latin-1')
SubstationMeterRelation=data.frame(SubstationId=SubstationMeterRelation$SubstationId,MeterId=SubstationMeterRelation$MeterId)


if (ServiceID==1){#####If call train:args = ['1',substation_id, First_Training_Date, Last_Training_Date,nahead]
  print('here')
  from = myArgs[3]#"2017-08-21"#Sys.Date()
  to =myArgs[4]#"2017-01-21"#Sys.Date() "2017-01-21"
  Nahead=as.numeric(myArgs[5])
  print(ss)
  print(from)
  print(to)
  yRaw=Utilities.DownloadSubstationData(ss=ss,typeClass='100',from=from,to=to)$yss
  y=Utilities.OutlierRemoval(yRaw)
  ytrain=y
  Model=TrainPred.Train(y=ytrain,Nahead=Nahead,method=3)#3 looks good
  #Save model to blob
  Utilities.UploadModel(Model,ss)
  result=0
}

if (ServiceID==2){#####If call prediction:
  StartTime = myArgs[3]#"2017-08-21"#Sys.Date()
  #to = as.numeric(myArgs[4])#"2017-01-21"#Sys.Date() "2017-01-21"
  Nahead=as.numeric(myArgs[4])
  
  from=as.POSIXct(StartTime,format="%Y-%m-%d T %H:%M:%S", tz="UTC")-3600*24*365
  print(StartTime)
  print(as.POSIXct(StartTime,format="%Y-%m-%d T %H:%M:%S", tz="UTC"))
  
  from=paste0(toString(format(from,"%Y-%m-%d")),"T",format(from,"%H:%M:%S"),sep='')
  
  to=StartTime
  print(ss)
  print(from)
  print(to)
  yRaw=Utilities.DownloadSubstationData(ss=ss,typeClass='100',from=from,to=to)$yss
  print('hereeeee')
  y=Utilities.OutlierRemoval(yRaw)
  ytrain=y
Model=TrainPred.Train(y=ytrain,Nahead=Nahead,method=3)#3 looks good
#Save model to blob
Utilities.UploadModel(Model,ss)

#download model from blob
tem=Utilities.DownloadModel(ss,'model')
eval(parse(text=tem))
#make prediction
test1=TrainPred.Predict(Model=Model,StartTime=Model$ModelTime+3600,ss=ss,Nahead=Nahead)
y_pred=test1$mean
print(getwd())
write.csv(y_pred,file=file.path('substation','data','y_pred.csv'),row.names=FALSE)
save(y_pred,file=file.path('substation','data','y_pred.RData'))
cat(toString(y_pred))
}
return(result)
}

myArgs <- commandArgs(trailingOnly = TRUE) #This contains the parameters from python command

result=Rfunc(myArgs)