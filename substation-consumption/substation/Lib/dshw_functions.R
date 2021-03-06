par_dshw <- function(y, period1, period2, pars)
{
  
  start <- c(0.1,0.01,0.001,0.001,0.9)[is.na(pars)]
  print('inside par_dshw - training the dshw model')
  out <- optim(start, dshw.mse, y=y, period1=period1, period2=period2, pars=pars, control = list(maxit = 20000, trace = TRUE))
  #out <- optim(start, dshw.mse, y=y, period1=period1, period2=period2, pars=pars, control = list(maxit = 20000, trace = FALSE))
  pars[is.na(pars)] <- out$par
  return(pars)
}

dshw.mse <- function(par, y, period1, period2, pars)
{
  #print(par)
  pars[is.na(pars)] <- par
  if(max(pars) > 0.99 | min(pars) < 0 | pars[5] > .99)
    return(1e20)
  else
    return(ModifiedDSHW(y, period1, period2, h=period1, pars[1], pars[2], pars[3], pars[4], pars[5], armethod=(abs(pars[5]) >1e-7))$model$mse)
}

seasindex <- function(y,p)
{
  #require(zoo)
  n <- length(y)
  n2 <- 2*p
  shorty <- y[1:n2]
  average <- numeric(n)
  simplema <- zoo::rollmean.default(shorty, p)
  if (identical(p%%2,0)) # Even order
  {
    centeredma <- zoo::rollmean.default(simplema[1:(n2-p+1)],2)
    average[p/2 + 1:p] <- shorty[p/2 + 1:p]/centeredma[1:p]
    si <- average[c(p+(1:(p/2)),(1+p/2):p)]
  }
  else # Odd order
  {
    average[(p-1)/2 + 1:p] <- shorty[(p-1)/2 + 1:p]/simplema[1:p]
    si <- average[c(p+(1:((p-1)/2)),(1+(p-1)/2):p)]
  } 
  return(si)
}

####################################################################
## Modified version of Double Seasonal Holt Winters method as per Taylor (2003)
####################################################################
### Double Seasonal Holt-Winters smoothing parameter optimization

ModifiedDSHW <- function(y, period1=NULL, period2=NULL, h=period1, alpha=NULL, beta=NULL, gamma=NULL, omega=NULL, phi=NULL, lambda=NULL, armethod=TRUE, model = NULL,LastTrainingDate=NULL)
{
  
  if(min(y,na.rm=TRUE) <= 0)
    stop("dshw not suitable when data contain zeros or negative numbers")
  if (!is.null(model) && model$method == "DSHW") {
    period1 <- model$period1
    period2 <- model$period2
  } else if(any(class(y) == "msts") & (length(attr(y, "msts")) == 2)) {
    period1<-as.integer(sort(attr(y, "msts"))[1])
    period2<-as.integer(sort(attr(y, "msts"))[2])
  } else if(is.null(period1) | is.null(period2)) {
    stop("Error in dshw(): y must either be an msts object with two seasonal periods OR the seasonal periods should be specified with period1= and period2=")
  } else {
    if(period1 > period2)
    {
      tmp <- period2
      period2 <- period1
      period1 <- tmp
    }
  }
  if(any(class(y) != "msts"))
    y <- msts(y, c(period1, period2))
  
  if(!armethod)
  {
    phi <- 0
  }
  
  if(period1 < 1 | period1 == period2)
    stop("Inappropriate periods") 
  ratio <- period2/period1
  if(ratio-trunc(ratio) > 1e-10)
    stop("Seasonal periods are not nested")
  
  if (!is.null(model)) {
    lambda <- model$model$lambda
  }
  
  if (!is.null(lambda))
  {
    origy <- y
    y <- BoxCox(y, lambda)
  }
  
  if (!is.null(model)) {
    pars <- model$model
    alpha <- pars$alpha
    beta <- pars$beta
    gamma <- pars$gamma
    omega <- pars$omega
    phi <- pars$phi
  } else {
    pars <- rep(NA,5)
    if(!is.null(alpha))
      pars[1] <- alpha
    if(!is.null(beta))
      pars[2] <- beta
    if(!is.null(gamma))
      pars[3] <- gamma
    if(!is.null(omega))
      pars[4] <- omega
    if(!is.null(phi))
      pars[5] <- phi
  }
  
  # Estimate parameters
  if(sum(is.na(pars)) > 0)
  {
    #print('here 1')
    #pars <- par_dshw(y=y,period1=period1,period2=period2,pars=pars, model1=model)#Hoang modified code, original: 
    pars <- par_dshw(y,period1,period2,pars)
    #print('here 2')
    alpha <- pars[1]
    beta <- pars[2]
    gamma <- pars[3]
    omega <- pars[4]
    phi <- pars[5]
  }
  
  ## Allocate space
  n <- length(y)

  yhat <- matrix(0, nrow=n, ncol=h)
  
  ## Starting values
  I <- seasindex(y,period1)
  wstart <- seasindex(y,period2)
  wstart <- wstart / rep(I,ratio)
  w <- wstart
  x <- c(0,diff(y[1:period2]))
  t <- t.start <- mean(((y[1:period2]- y[(period2+1):(2*period2)])/period2 ) + x )/2
  s <- s.start <- (mean(y[1:(2*period2)])-(period2+0.5)*t)
  
  ## In-sample fit
  
  #browser()
  for(i in 1: n)
  {
    
    yhatNow <- (s+t) * I[i]*w[i]
    err <- y[i] - yhatNow
    yhat[i, ] <- (s+(1:h)*t) * rep(I[i-1+(1:period1)],h/period1 + 1)[1:h] * rep(w[i-1+(1:period2)],h/period2 + 1)[1:h] + phi^(1:h)*err
    snew <- alpha*(y[i]/(I[i]*w[i]))+(1-alpha)*(s+t)
    tnew <- beta*(snew-s)+(1-beta)*t
    I[i+period1] <- gamma*(y[i]/(snew*w[i])) + (1-gamma)*I[i]
    w[i+period2] <- omega*(y[i]/(snew*I[i])) + (1-omega)*w[i]
    s <- snew
    t <- tnew
  }
  err0=err

  # Forecasts
  fcast <- (s + (1:h)*t) * rep(I[n+(1:period1)],h/period1 + 1)[1:h] * rep(w[n+(1:period2)],h/period2 + 1)[1:h] + phi^(1:h)*err
  fcast <- ts(fcast,frequency=frequency(y),start=tsp(y)[2]+1/tsp(y)[3])
  
  # Calculate MSE and MAPE
  #yhat <- ts(yhat)
  #tsp(yhat) <- tsp(y)
  #yhat<-msts(yhat, c(period1, period2))
  #e <- y - yhat
  #e<-msts(e, c(period1, period2))
  #if(armethod)
  #{
  #	yhat <- yhat + phi * c(0,e[-n])
  #	e <- y - yhat
  #  fcast <- fcast + phi^(1:h)*e[n]
  #}
  mse = 0
  mape = numeric(h)
  err = list()
  for (k in 1:h){
    if (k == 1) {
      predictedY = yhat[, k]
      actualY = y
    }
    else {
      predictedY = head(yhat[, k], -k+1)
      actualY = tail(y, -k+1)
    }
    err[[k]] =  actualY - predictedY
    mse = mse + mean(err[[k]]^2)
    mape[k] = mean(abs(err[[k]])/actualY)*100
  }
  
  end.y <- end(y)
  if(end.y[2] == frequency(y)) {
    end.y[1]<-end.y[1]+1
    end.y[2]<-1
  } else {
    end.y[2]<-end.y[2]+1
  }
  
  fcast <- msts(fcast, c(period1, period2))
  #print(mape)
  if(!is.null(lambda))
  {
    y <- origy
    fcast <- InvBoxCox(fcast,lambda)
    yhat <- InvBoxCox(yhat,lambda)
  }
  #return(structure(list(mean=fcast,method="DSHW",x=y,residuals=err,fitted=yhat,
  #                      model=list(mape=mape,mse=mse,alpha=alpha,beta=beta, gamma=gamma,omega=omega,phi=phi,
  #                                 lambda = lambda, l0=s.start,b0=t.start,s10=wstart,s20=I), period1 = period1,
  #                      period2 = period2),class="forecast"))
  return(structure(list(mean=fcast,method="DSHW",y=y,frequencyy=frequency(y),tspvals=tsp(y),residual=err,LastTrainingDate=LastTrainingDate,
                        model=list(mape=mape,mse=mse,alpha=alpha,beta=beta, gamma=gamma,omega=omega,phi=phi,
                                   lambda = lambda, l0=s.start,b0=t.start,s10=wstart,s20=I, err0=err0,ratio=ratio,If=I[(length(I)-period1+1):length(I)], wf=w[(length(w)-period2+1):length(w)], sf=s, tf=t), period1 = period1,
                        period2 = period2),class="forecast"))
  
}

dshw_predict <- function(ygap,dshw_model,h){
  #ygap is the data from the last training data point to the starting prediction point
  #h is the horizon. 
  #Consider the case there is no gap!
  
  tspvals=dshw_model$tspvals
  
  period1=dshw_model$period1
  period2=dshw_model$period2
  t = dshw_model$model$tf
  phi = dshw_model$model$phi
  I = dshw_model$model$If
  s = dshw_model$model$sf
  w = dshw_model$model$wf
  ratio <- dshw_model$model$ratio
  gamma=dshw_model$model$gamma
  omega=dshw_model$model$omega
  alpha=dshw_model$model$alpha
  beta = dshw_model$model$beta
  err= dshw_model$model$err0
  Y=c(dshw_model$y,ygap)
  #Ymsts <- msts(Y, c(period1, period2))
  browser()
  if(!(is.null(ygap))){
  ygap=(as.data.frame(ygap))$V1
  tspvals[2]=end(ygap)
  #Ymsts <- msts(Y, c(period1, period2))
  
  n=length(ygap)
  for(i in 1: n)
  {
    yhatNow <- (s+t) * I[i]*w[i]
    err <- ygap[i] - yhatNow
    snew <- alpha*(ygap[i]/(I[i]*w[i]))+(1-alpha)*(s+t)
    tnew <- beta*(snew-s)+(1-beta)*t
    I[i+period1] <- gamma*(ygap[i]/(snew*w[i])) + (1-gamma)*I[i]
    w[i+period2] <- omega*(ygap[i]/(snew*I[i])) + (1-omega)*w[i]
    s <- snew
    t <- tnew
  }}
  else
    n=0
  
  fcast <- (s + (1:h)*t) * rep(I[n+(1:period1)],h/period1 + 1)[1:h] * rep(w[n+(1:period2)],h/period2 + 1)[1:h] + phi^(1:h)*err
  fcast <- ts(fcast,frequency=dshw_model$frequencyy,start=tspvals[2]+1/tspvals[3])
  yhat <- msts(fcast, c(period1, period2))
  return(yhat)
}