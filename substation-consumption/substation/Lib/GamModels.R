Gam.GAMTemperature<- function(yDf,model=NULL){

  #function(SS='50K',
  #         Nahead=168,
  #         from='2015-09-10T10:00:00',
  #         to='2016-12-22T10:00:00'){
  
  #Nahead=(24*7)
  #horizons=1:Nahead
  #SS='50K'
  #from='2015-09-10T10:00:00'
  #to='2016-12-22T10:00:00'
  
  #This function will train a Gam model for each hour
  stopifnot(require('KernSmooth'))
  stopifnot(require('bbemkr'))
  stopifnot(require('mgcv'))
  stopifnot(require('timeDate'))
  stopifnot(require("MASS"))
  stopifnot(require('forecast'))
  stopifnot(require('dplyr'))
  stopifnot(require('lubridate'))
  stopifnot(require('xts'))
  stopifnot(require('rlist'))
  
  
  predictions = rep(list(y), Nahead);
  #Create Time Features
  startYear = year(yDf$DateTime[1])
  endYear = year(tail(yDf$DateTime, 1))
  years = seq(startYear, endYear)
  NorwayHolidays = c(EasterMonday(years), 
                     Ascension(years), 
                     PentecostMonday(years), 
                     LaborDay(years), 
                     GoodFriday(years), 
                     BoxingDay(years), 
                     GoodFriday(years)-86400);
  yDf = yDf %>% mutate(Holiday= isHoliday(as.timeDate(DateTime), NorwayHolidays, wday=0:6)) %>%
    mutate(ChristmasDay= isHoliday(as.timeDate(DateTime), ChristmasDay(years), wday=0:6)) %>%
    mutate(ChristmasEve= isHoliday(as.timeDate(DateTime), ChristmasEve(years), wday=0:6)) %>%
    mutate(NewYearsDay= isHoliday(as.timeDate(DateTime), NewYearsDay(years), wday=0:6)) %>%
    mutate(DoW = factor(wday(DateTime))) %>%
    mutate(ToY = as.numeric(strftime(DateTime, format = "%j"))
           +as.numeric(strftime(DateTime, format="%H"))/24)
  
  
  #Data type
  featureDf = yDf %>% dplyr::select(one_of(c("DateTime", 
                                                      "Holiday", 
                                                      "ChristmasDay", 
                                                      "ChristmasEve", 
                                                      "NewYearsDay", 
                                                      "DoW", "ToY", "T",'y')))
  
    #featureDf$E = featureDf[[zone]]
    featureDf$SmoT = featureDf$T
    for (i in 2:length(featureDf$T)){
      featureDf$SmoT[i] = 0.15*featureDf$T[i] + 0.85*featureDf$SmoT[i-1]
    }
    if (!is.null(model)) featureDf$LT=yDf$LT
    
    if (is.null(model)){
    featureDf$LT = rep(0, nrow(featureDf))
    featureDf$Residuals = rep(0, nrow(featureDf)) #This is used to train an Arima on residuals
    
    trainData = featureDf # %>% filter(DateTime < startDate)
    trainData$LT = Gam.longTermTrend(trainData$y, trainData$T, trainData$DateTime)
    
    spec = y ~ LT + s(T) + s(SmoT) + s(ToY,bs="cc", k = 100) + 
        DoW + ChristmasDay + ChristmasEve + NewYearsDay + Holiday
    GamModel=list()
      for (hour in 0:23){ #different model for each hour
        trainingIdx = which(hour(trainData$DateTime)==hour) 
        trainDataAtH = trainData[trainingIdx, ]
        model = gam(spec, data=trainDataAtH)
        featureDf$Residuals[trainingIdx] = model$model$y - model$fitted.values #residuals used for training arima
        GamModel=list.append(GamModel,model)
      }
  return(structure(list(LT=trainData$LT,GamModel=GamModel,Residuals=xts(featureDf$Residuals,order.by=featureDf$DateTime))))
    }else{
      #prediction part
      
      #testingIdx = which((hour(featureDf$DateTime)==hour) & (featureDf$DateTime >= startDate) & (featureDf$DateTime <= endDate))
      #featureDf$LT[testingIdx] = rep(tail(trainData$LT, 1), length(testingIdx))
      #testData = featureDf[testingIdx, ]
      TrainedModel=model[hour(head(yDf$DateTime[1],1))][[1]]
      prediction = predict(TrainedModel, featureDf)
      return(prediction)
    }
}

Gam.longTermTrend <- function(E, T, DateTime, BANDWIDTH=12){
  E.xts = xts(E, DateTime)
  T.xts = xts(T, DateTime)
  E.month.xts = apply.monthly(E.xts, FUN="mean", na.rm = TRUE)
  I = as.numeric(format(index(E.month.xts), "%m"))
  #I = quarters (index(E.month.xts), "%m")
  if (length(I) > 24){
    I = factor(I)
  }
  T = apply.monthly(T.xts[index(E.xts)], FUN="mean", na.rm = TRUE)
  E.model = gam(E.month.xts ~ I + s(T))#ERROR!!
  E.est = E.model$fitted.values
  E.residuals = E.model$residuals
  a = NadarayaWatsonkernel(1:length(E.residuals), E.residuals, BANDWIDTH, 1:length(E.residuals))
  a$mh
  month = as.numeric(format(index(E.xts), "%m"))
  year = as.numeric(format(index(E.xts), "%y"))
  
  idx = (year-year[1])*12+(month-month[1])+1
  
  days = rep(0, length(E.xts))
  for (i in 1:length(a$mh)){
    days[which(idx == i)]  = sum(idx==i)
  }
  
  fraction = as.numeric(format(index(E.xts), "%d"))*24 + as.numeric(format(index(E.xts), "%H"))
  
  trend.month = c(a$mh[1]-(a$mh[2]-a$mh[1]), a$mh)
  trend = rep(0, length(E.xts))
  trend = (trend.month[idx+1]-trend.month[idx])*fraction/days + trend.month[idx]
  trend.xts = xts(trend, order.by = index(E.xts))
  return(drop(coredata(trend.xts)))
}