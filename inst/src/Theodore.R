dates <- normomo_daily$date
temp <- weather[date %in% normomo_daily$date & location_code=="norge"]$tg
deaths <-normomo_daily$nbc
yearweek <-spuls_restricted$yrwk_number
proxyH1 <- spuls_restricted$ili

library(FluMoDL)


  dat <- subset(data.frame(dates, deaths, temp))
  dat$yearweek <- isoweek(dat$dates)
  dat$proxyH1 <- proxyH1[match(dat$yearweek, yearweek)]
  dat$proxyH1 <- smooth.spline(dat$proxyH1, df=floor(nrow(dat)/7))$y

  lg <- 30  # 30 days maximum lag (fixed)

  # Create cross-basis matrices
  basis.temp <- crossbasis(dat$temp, lag=lg,
                           argvar=list(fun="bs", degree=2, knots=quantile(dat$temp, c(0.1,0.75,0.9))),
                           arglag=list(fun="ns", knots=logknots(lg,3)))
  basis.proxyH1 <- crossbasis(dat$proxyH1, lag=lg,
                              argvar=list(fun="poly", degree=1),
                              arglag=list(fun="ns", knots=logknots(lg,3)))


  dat$t <- 1:nrow(dat)   # Linear trend
  dat$doy <- as.integer(format(dat$dates, "%j"))   # Day of the year
  dat$dow <- as.factor(format(dat$dates,"%u"))   # Day of the week
    model <- glm(deaths ~ basis.temp + basis.proxyH1 +
                   dow + t + pbs(doy, knots=c(91,182,274)), data=dat, family="quasipoisson")


  predTemp <- crosspred(basis.temp, model,
                        at = seq(ceiling(min(dat$temp)), floor(max(dat$temp)), 1),
                        bylag=0.2, cen=round(median(dat$temp)), cumul=TRUE)
  # Calculate Minimum Mortality Point
  MMP <- as.integer(names(which(predTemp$allfit==min(predTemp$allfit))))
  # Refit prediction for temperature, centered at the MMP
  predTemp <- crosspred(basis.temp, model,
                        at = seq(ceiling(min(dat$temp)), floor(max(dat$temp)), 1),
                        bylag=0.2, cen=MMP, cumul=TRUE)
  # Predictions (linear) for influenza proxies
  pL <- pretty(c(0,ceiling(max(dat[,c("proxyH1")]))), 30)
  predProxyH1 <- crosspred(basis.proxyH1, model, at=pL, bylag=0.2, cen=0, cumul=TRUE)



  fitSP <- list(
    data = dat, model = model,
    basis = list(temp = basis.temp, proxyH1 = basis.proxyH1),
    MMP = MMP,
    pred = list(temp = predTemp, proxyH1 = predProxyH1),
    blup = NULL
  )
  class(fitSP) <- c("FluMoDL")

  am <- FluMoDL::attrMort(
    fitSP,
    par = c("H1", "temp"),
    sel = s_cold,
    temprange = c(-200,-10),
    progress = F
  )


  plot(fitSP$pred$temp, "overall")
