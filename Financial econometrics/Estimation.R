source("https://raw.githubusercontent.com/jnordeman/Course-work/master/Financial%20econometrics/GARCH/Set-up.R")

model_data <-as.xts(KVIK[,.(date,return)])

## Fit GARCH
## -----------------------------------------------
## Model Selection on GARCH(p,q) models
## -----------------------------------------------

## Fit ARCH 1 to 5
## -----------------------------------------------

arch_order <- 1:5
arch_names <- paste0("arch", arch_order)

# fit all arch models with p <= 5
arch_list <-list()
for (p in arch_order) {
  arch_spec <- rugarch::ugarchspec(
    variance.model = list(garchOrder=c(p,0)), 
    mean.model = list(armaOrder=c(0,0)),
    distribution.model = "sstd"
  )
  arch_fit <- rugarch::ugarchfit(
    spec=arch_spec, data=model_data,
    solver.control=list(trace = 0)
  )
  arch_list[[p]] <- arch_fit
}
names(arch_list) <- arch_names

arch_list

## Fit GARCH(1,1)
## -----------------------------------------------
garch11_spec <- rugarch::ugarchspec(
  variance.model = list(garchOrder=c(1,1)), 
  mean.model = list(armaOrder=c(0,0)),
  distribution.model = "sstd"
)

garch11_fit <- rugarch::ugarchfit(
  spec=garch11_spec, data=model_data,
  solver.control=list(trace = 0)
)

arch_list$garch11 <- garch11_fit

## Fit Integrated-GARCH(1,1)
## -----------------------------------------------
Igarch11_spec <- rugarch::ugarchspec(
  variance.model = list(model = "iGARCH",garchOrder=c(1,1)), 
  mean.model = list(armaOrder=c(0,0)),
  distribution.model = "sstd"
)

Igarch11_fit <- rugarch::ugarchfit(
  spec=Igarch11_spec, data=model_data,
  solver.control=list(trace = 0)
)

arch_list$Igarch11 <- Igarch11_fit

## Fit T-GARCH
## -----------------------------------------------
gjrgarch11_spec <- rugarch::ugarchspec(
  variance.model =list(model="gjrGARCH",
  garchOrder=c(1,1)),  
  mean.model = list(armaOrder=c(0,0)),
  distribution.model = "sstd"
)

gjrgarch11_fit <- rugarch::ugarchfit(spec=gjrgarch11_spec, data=model_data)

arch_list$gjrgarch11 <- gjrgarch11_fit

# extract information criteria for all models
info_mat <- sapply(arch_list, infocriteria)
rownames(info_mat) <- rownames(infocriteria(arch_list[[1]]))

info_mat

colnames(info_mat)[which.min(info_mat["Akaike",])]
colnames(info_mat)[which.min(info_mat["Bayes",])]
colnames(info_mat)[which.min(info_mat["Shibata",])]
colnames(info_mat)[which.min(info_mat["Hannan-Quinn",])]

# gjrGARCH 1,1 seem to be the best model 

## Extract information from GARCH Fit 
## -----------------------------------------------
## Coefficients 
garch11coef <- round(coef(garch11_fit),4)
Igarch11coef <- round(coef(Igarch11_fit),4)
gjrgarch11coef <- round(coef(gjrgarch11_fit),4)

## Unconditional variance 
garch11uncvar <-round(uncvariance(garch11_fit),6)
Igarch11uncvar <-round(uncvariance(Igarch11_fit),6)
gjrgarch11uncvar <-round(uncvariance(gjrgarch11_fit),6)


## Regression summary
round(garch11_fit@fit$matcoef,6)
round(Igarch11_fit@fit$matcoef,6)
round(gjrgarch11_fit@fit$matcoef,6)

## Likelihood
round(likelihood(garch11_fit),2) 
round(likelihood(Igarch11_fit),2) 
round(likelihood(gjrgarch11_fit),2) 

## News impact
## -----------------------------------------------
NewsImpact_gjr <-newsimpact(gjrgarch11_fit)
NewsImpact_garch <-newsimpact(garch11_fit)


NewsImpact_gjr<-as.data.frame(
  cbind(
    as.data.frame(NewsImpact_gjr$zx),
    as.data.frame(NewsImpact_gjr$zy)
  )
)
colnames(NewsImpact_gjr)<-c("zx","zy")

NewsImpact_gjr$model <- "TGARCH"

NewsImpact_garch<-as.data.frame(cbind(
  as.data.frame(NewsImpact_garch$zx),
  as.data.frame(NewsImpact_garch$zy)))
colnames(NewsImpact_garch)<-c("zx","zy")

NewsImpact_garch$model <-"GARCH"

NewsImpact <-rbind(NewsImpact_garch,NewsImpact_gjr)

plt_levEffect<-ggplot(NewsImpact,aes(x=zx,y=zy, col=model,linetype=model))+
  niceTheme+
  geom_line(size=0.5)+
  labs(x="Predicted error", y="Predicted variance")+
  scale_color_manual(values=c("black","black"))+
  scale_linetype_manual(values = c("solid","dashed"))+
  theme(
    legend.position = "bottom",
    legend.key = element_rect(fill="transparent",colour ="transparent"),
    legend.title = element_blank()
  )

plt_levEffect


## Test for Asymmetry 
## -----------------------------------------------
leverage_effect <- residuals(Igarch11_fit,standardize=TRUE)
colnames(leverage_effect)<-"sResid"
leverage_effect$sqrdResid <- leverage_effect$sResid^2

leverage_effect1<-dynlm(sqrdResid ~ L(sResid, 1:1), data = as.zoo(leverage_effect))
summary(leverage_effect1)

leverage_effect2<-dynlm(sqrdResid ~ L(sResid, 1:2), data = as.zoo(leverage_effect))
summary(leverage_effect2)

leverage_effect3<-dynlm(sqrdResid ~ L(sResid, 1:3), data = as.zoo(leverage_effect))
summary(leverage_effect3)

leverage_effect4<-dynlm(sqrdResid ~ L(sResid, 1:4), data = as.zoo(leverage_effect))
summary(leverage_effect4)

leverage_effect5<-dynlm(sqrdResid ~ L(sResid, 1:5), data = as.zoo(leverage_effect))
summary(leverage_effect5)

## No Leverage effect  


## Backtesting 
## -----------------------------------------------
start_forecast<-TradingDays*4

garch11_roll <- ugarchroll(
  garch11_spec, 
  data=model_data,
  n.start = start_forecast,
  refit.window = "moving",
  refit.every = TradingDays
)


gjrgarch11_roll <- ugarchroll(
  gjrgarch11_spec,
  data=model_data,
  n.start = start_forecast,
  refit.window = "moving",
  refit.every = TradingDays
)

coef(garch11_roll)
coef(gjrgarch11_roll)

## Merge to 1 
preds_garch11_roll <-as.data.frame(garch11_roll)
preds_garch11_roll$model <- "GARCH(1,1)"
preds_garch11_roll$date <-as.Date(rownames(preds_garch11_roll))

preds_gjrgarch11_roll <-as.data.frame(gjrgarch11_roll)
preds_gjrgarch11_roll$model <- "TGARCH"
preds_gjrgarch11_roll$date <-as.Date(rownames(preds_gjrgarch11_roll))

preds<-rbind(preds_garch11_roll,preds_gjrgarch11_roll)

plt_sigma <- ggplot(preds, aes(x=date,y=Sigma,col=model,linetype=model))+
  geom_line(size=1)+
  niceTheme+
  scale_color_manual(values=c( "Black", "steel blue"))+
  scale_linetype_manual(values=c("solid","dotted"))+
  theme(
    legend.position = "bottom",
    legend.key = element_rect(fill="transparent",colour ="transparent"),
    legend.title = element_blank()
  )+
  scale_x_date(
    breaks = "1 year",
    date_labels =  "%Y"
  )

plt_sigma

## Evaluate Fit 
## -----------------------------------------------
evaluate.fit<-function(x){
  #Mean 
  e<-x$Realized - x$Mu
  #Variance
  d<-e^2-x$Sigma^2

  return(
    list(
      squared_e = mean(e^2),
      squared_d = mean(d^2)
    )
  )
  
}

evaluate.fit(preds_garch11_roll)
evaluate.fit(preds_gjrgarch11_roll)

## Forecasting
## -----------------------------------------------
garch11_fcst <- ugarchforecast(garch11_fit, n.ahead=TradingDays)

garch11_fcst_df<-as.data.frame(sigma(garch11_fit)[(nrow(model_data)-round(TradingDays/3,0)):nrow(model_data)])

colnames(garch11_fcst_df)<-"sigma"

row.names(garch11_fcst_df)<-row(garch11_fcst_df)

garch11_fcst_df$date <-index(model_data[(nrow(model_data)-round(TradingDays/3,0)):nrow(model_data)])
garch11_fcst_df$model <- "Actual"

## extract mean using fitted() to the forecasting  
temp.tail<-as.data.frame(tail(garch11_fcst_df[,1],1))
colnames(temp.tail)<-"sigma"
fcst_df_temp<-as.data.frame(garch11_fcst@forecast$sigmaFor)
colnames(fcst_df_temp)<-"sigma"
fcst_df_temp<-rbind(temp.tail,fcst_df_temp)
fcst_df_temp$date <- as.Date(tail(KVIK$date,1))+row(fcst_df_temp)[,1]
fcst_df_temp$model<-"Forecast"

garch11_fcst_df<-rbind(garch11_fcst_df,fcst_df_temp)
garch11_fcst_df$uncvar <- sqrt(garch11uncvar)


plt_fcst<-ggplot(garch11_fcst_df,aes(x=date,y=sigma, col=model,linetype=model))+
  geom_hline(yintercept = sqrt(garch11uncvar),col="red")+
  geom_line(size=1)+
  niceTheme+
  scale_color_manual(values=c("black","steel blue"))+
  scale_linetype_manual(values=c("solid","dotted"))+
  ggtitle("Unconditional variance forecast")+
  theme(
    legend.position = "bottom",
    legend.key = element_rect(fill="transparent",colour ="transparent"),
    legend.title = element_blank()
  )+
  scale_x_date(
    expand=c(0,0)
  )


plt_fcst


# h-day return variance forecast = sum of h-day ahead variance forecasts
fcst.var.hDay = as.data.frame(cumsum(fcst_df_temp$sigma^2))
fcst.vol.hDay = sqrt(fcst.var.hDay)
colnames(fcst.vol.hDay)<-"volatility"
fcst.vol.hDay$date<- fcst_df_temp$date  

# plot h-day vol forecasts
plt_hday<-ggplot(fcst.vol.hDay,aes(x=date,y=volatility))+
  geom_line()+
  niceTheme

plt_hday

## Value-at-Risk 
## -----------------------------------------------
garch_VAR<-quantile(garch11_roll, probs=0.05)

actual <- fortify(xts(as.data.frame(garch11_roll)$Realized, time(garch_VAR)))
colnames(actual)<-c("date","actual")

garch_VAR_df <-fortify(garch_VAR)
colnames(garch_VAR_df)<-c("date","VaR")

VaR_df <- merge(actual,garch_VAR_df,by="date",all=TRUE)
VaR_df$out <- ifelse(VaR_df$actual>=VaR_df$VaR,NA,0)
VaR_df$date<- as.Date(VaR_df$date)

plt_VaR <- ggplot(VaR_df,aes(x=date,y=actual))+
  geom_line(colour="grey")+
  geom_line(aes(x=date,y=VaR), size=1)+
  niceTheme+
  geom_point(aes(x=date,y=out),col="Red")+
  scale_x_date(
    breaks = "1 year",
    date_labels =  "%Y"
  )
  
plt_VaR
