TrainPred.Train <-function(y,Nahead=72,method=1){
  #This function will return a generic model. Apply this model to some request should give prediction value
  
  #1)	Meanf Mean historical data
  #2)	Naive All forecasts are simply set to be the value of the last observation 
  #3)	Snaive: each forecast to be equal to the last observed value at the same hour from yesterday 
  #4)	RWF: previous + average changes in historical data
  #5)	Modified DSHW 
  #6)	TBATS: Exponential Smoothing State Space Model With Box-Cox Transformation, ARMA Errors, Trend And Seasonal Components
  #7)	 Average ARIMA 
  #8)GAM
  require('forecast')
  ytrain=ts(drop(y),freq=24)
  if (method==1)#Meanf
  {#yHatMeanf=meanf(drop(coredata(ytrain)),Nahead) #simply use mean
    Meanf=meanf(drop(coredata(ytrain)),Nahead) #simply use mean
  #yHat=yHatMeanf$mean
  Model=structure(list(model=Meanf,info='Mean of historical data',method=method,ModelTime=end(y)))
  }
  #naive or ARIMA(0,1,0) 
  if (method==2)#Naive use the last data point to predict future
  {Naive=naive(drop(coredata(ytrain)),Nahead)
  #yHat=as.vector(yHatNaive$mean)
  Model=structure(list(model=Naive,info ='All forecasts are simply set to be the value of the last observation',method=method,ModelTime=end(y)))
  }
  if (method==3){#snaive: forecast of y_{T+h} is y_{T+h-km} where m is seasonal period, k = integer part of (h-1)/m +1. For example, with monthly data, the forecast for all future February values is equal to the last observed February value. With quarterly data, the forecast of all future Q2 values is equal to the last observed Q2 value (where Q2 means the second quarter)
    #testing
    Snaive=snaive(ts(drop(coredata(ytrain)),freq=24),Nahead)#freq value should be defined by the user, either 24 hours, a week, a month or a year.
    Model=structure(list(model=Snaive, name='snaive', info='Snaive: each forecast to be equal to the last observed value at the same hour from yesterday',method=method,ModelTime=end(y)))
  }
  
  if (method==4){#rwf(y, h, drift=TRUE), equivalent to ARIMA(0,1,0)
    Rwf = rwf(ts(drop(coredata(ytrain)),freq=24), Nahead, drift=TRUE)
    #browser()
    Model=structure(list(model=Rwf,info='previous + average changes in historical data',method=method,ModelTime=end(y)))
  }
  if (method==5){#dshw method
    print('dshw method')
    dshw_model=ModifiedDSHW(ytrain,period1=24,period2=24*7)
    #browser()
    Model=structure(list(model=ModifiedDSHW, para= dshw_model,ytrain=ytrain, name='dshw_model', info='Modified DSHW method',method=method,ModelTime=end(y)))
    #dshw_model1=ModifiedDSHW(y=ytrain,h= Nahead, model=dshw_model)# or tem1=dshw_predict(NULL, dshw_model, 72)
    #yHat=as.vector(dshw_model1$mean)
  }
  if(method==6){#tbats
    print('tbats')
    Tbats = tbats(drop(coredata(ytrain)), seasonal.periods = 24, 
                  use.box.cox = TRUE, use.parallel = FALSE, num.cores = 1)
    Model=structure(list(model=Tbats, info='Exponential Smoothing State Space Model With Box-Cox Transformation, ARMA Errors, Trend And Seasonal Components',method=method,ytrain=ytrain,ModelTime=end(y)))
  }
  if(method==7){#AverageARIMA
    print('AverageARIMA')
    AverageArima = auto.arima(as.ts(ytrain), seasonal=TRUE)
    Model=structure(list(model=AverageArima, info='AverageArima',method=method,ModelTime=end(y)))
  }
  #semi parametric, GAM model w/o temperature, using day of the week and hour of the day. reference:https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
  if(method==8)
  {
    require(data.package)
    matrix_gam <- data.table(Load = drop(coredata((ytrain))),
                             Daily = as.numeric(format(as.POSIXct(index(ytrain),format="%H:%M:%S"),"%H")),
                             Weekly = as.integer(factor(weekdays(index(ytrain)))))
    model <- gam(Load ~ t2(Daily, Weekly,
                           k = c(period1, 7),
                           bs = c("cr", "ps"),
                           full = TRUE),
                 data = matrix_gam,
                 family = gaussian)
    
    CurrentTime = as.POSIXct(index(ytrain[length(ytrain)]),format="%H:%M:%S")
    TimeToPredict = CurrentTime+3600*c(1:Nahead)
    TimeToPredict <- data.frame(Daily=as.numeric(format(as.POSIXct(TimeToPredict,format="%H:%M:%S"),"%H")),Weekly = as.integer(factor(weekdays(TimeToPredict))))
    yHat = as.vector(predict.gam(model,TimeToPredict))
    Model=structure(list(model=predict.gam,para= model, name='GAM', info='GAM model',method=method,ModelTime=end(y)))
  }
  if(method==9){#stl - Seasonal and Trend decomposition using Loess. https://www.otexts.org/fpp/6/1
    model<-stl(as.ts(ytrain),s.window="periodic",method=method,ModelTime=end(y))
    forecast(model,h=Nahead)
  }
  
  return(Model)
}

TrainPred.Predict <-function(Model,Nahead=168,StartTime=NULL,ss=NULL){
  #Use Model to make prediction
  #Load xgap if the model is older than StartTime more than 1 step.
  require('forecast')
  from=paste0(toString(format(Model$ModelTime,"%Y-%m-%d")),"T",format(Model$ModelTime,"%H:%M:%S"))
  to=paste0(toString(format(StartTime,"%Y-%m-%d")),"T",format(StartTime,"%H:%M:%S"))
  browser()
  if (as.numeric(StartTime-Model$ModelTime)>1)
    xgap=Utilities.DownloadSubstationData(ss=ss,from=from,to=to,typeClass='100')
  else
    xgap=NULL
  
  print(Model$ModelTime)
  print(StartTime)
  #browser()
  method=Model$method
  if (method<=4)#Meanf
  {
    if (!is.null(xgap))y=c(as.vector(Model$model$x),as.vector(xgap$yss))
    else y=as.vector(Model$model$x)
  y=ts(y,freq=24)
  }
  
  if (method==1)#Meanf
  {#yHatMeanf=meanf(drop(coredata(ytrain)),Nahead) #simply use mean
    Meanf=meanf(y,Nahead) #simply use mean
    #yHat=as.vector(Meanf$mean)
    yHat=Meanf
    #Model=structure(list(model=Meanf,info='Mean of historical data',method=method))
  }
  #naive or ARIMA(0,1,0) 
  if (method==2)#Naive use the last data point to predict future
  {Naive=naive(y,Nahead)
  yHat=Naive#as.vector(Naive$mean)
  }
  if (method==3){#snaive: forecast of y_{T+h} is y_{T+h-km} where m is seasonal period, k = integer part of (h-1)/m +1. For example, with monthly data, the forecast for all future February values is equal to the last observed February value. With quarterly data, the forecast of all future Q2 values is equal to the last observed Q2 value (where Q2 means the second quarter)
    #testing
    Snaive=snaive(y,Nahead)
    yHat=Snaive#as.vector(Snaive$mean)
  }
  
  if (method==4){#rwf(y, h, drift=TRUE), equivalent to ARIMA(0,1,0)
    Rwf = rwf(y, Nahead, drift=TRUE)
    yHat = Rwf#as.vector(Rwf$mean)
  }
  
  if (method==5){#dshw method
    print('dshw method')
    #dshw_model=ModifiedDSHW(ytrain,period1=24,period2=24*7)
    #print('hererrrrrrr')
    #browser()
    #refit=ModifiedDSHW(y=ytrain,h= Nahead, model=dshw_model)# or 
    refit=dshw_predict(xgap, Model$model, Nahead)
    yHat=refit#as.vector(refit$mean)
  }
  if(method==6){#tbats
    print('tbats')
    if (!is.null(xgap))
      refit = tbats(as.ts(xgap), model=Model$model)
    else 
      refit = tbats(drop(coredata(Model$ytrain)), model=Model$model)
    yHat = forecast(refit, h=Nahead)#as.vector(forecast(refit, h=Nahead)$mean)
  }
  if(method==7){#AverageARIMA
    print('AverageARIMA')
    #model = auto.arima(as.ts(ytrain), seasonal=TRUE)
    if (!is.null(xgap))
      refit = Arima(as.ts(xgap), model=Model$model)
    else refit = Arima(as.ts(Model$model$x), model=Model$model)
    yHat = forecast(refit, h=Nahead)#as.vector(forecast(refit, h=Nahead)$mean)
  }
  #semi parametric, GAM model w/o temperature, using day of the week and hour of the day. reference:https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
  if(method==8)
  {
    require(data.table)
    require(mgcv)
    matrix_gam <- data.table(Load = drop(coredata((ytrain))),
                             Daily = as.numeric(format(as.POSIXct(index(ytrain),format="%H:%M:%S"),"%H")),
                             Weekly = as.integer(factor(weekdays(index(ytrain)))))
    model <- gam(Load ~ t2(Daily, Weekly,
                           k = c(24, 7),
                           bs = c("cr", "ps"),
                           full = TRUE),
                 data = matrix_gam,
                 family = gaussian)
    #yHat = as.vector(forecast(model, h=Nahead)$mean)
    
    CurrentTime = as.POSIXct(index(ytrain[length(ytrain)]),format="%H:%M:%S")
    TimeToPredict = CurrentTime+3600*c(1:Nahead)
    TimeToPredict <- data.frame(Daily=as.numeric(format(as.POSIXct(TimeToPredict,format="%H:%M:%S"),"%H")),Weekly = as.integer(factor(weekdays(TimeToPredict))))
    yHat = as.vector(predict.gam(model,TimeToPredict))}
  if(method==9){#stl - Seasonal and Trend decomposition using Loess. https://www.otexts.org/fpp/6/1
    model<-stl(as.ts(ytrain),s.window="periodic")
    forecast(model,h=Nahead)
  }
  return(yHat)
}