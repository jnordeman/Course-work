

## IDENTIFICATION
## -----------------------------------------------
plt_price <- ggplot(KVIK, aes(x=date, y=price))+
  geom_line()+
  niceTheme+
  ggtitle("Price (in SEK)")+
  scale_x_date(
    expand = c(0,0),
    breaks = "1 year",
    date_labels =  "%Y"
  )+
  scale_y_continuous(
    sec.axis = dup_axis()
  )

plt_price

plt_return <- ggplot(KVIK, aes(x=date, y=return*100))+
  geom_line()+
  niceTheme+
  ggtitle("Daily return (in %)")+
  scale_x_date(
    expand = c(0,0),
    breaks = "1 year",
    date_labels =  "%Y"
  )+
  scale_y_continuous(
    sec.axis = dup_axis()
  )

plt_return

plt_price_return <- grid.arrange(
  plt_price, plt_return,nrow=2,ncol=1
)

plt_price_return

# Histogram to visualize Skewness and Kurtosis 
plt_return_hist<- ggplot(KVIK,aes(x=return*100))+
  geom_histogram(binwidth=.5,colour="black", fill="grey")+
  niceTheme+
  scale_y_continuous(
    expand = c(0,0.05),
    sec.axis = dup_axis()
  )

plt_return_hist

## Visualize ARCH 
## -----------------------------------------------
## Squared Return
plt_sqrd_return <- ggplot(KVIK, aes(x=date, y=return^2))+
  geom_line()+
  niceTheme+
  ggtitle("Squared return")+
  scale_x_date(
    expand = c(0,0),
    breaks = "1 year",
    date_labels =  "%Y"
  )+
  scale_y_continuous(
    sec.axis = dup_axis(),
    expand = c(0,0.05)
  )


acf_sqrd_return <- ggAcf(
  KVIK[,return^2], 
  lag.max = 30,
  main="ACF squared return"
  )+
  niceTheme


## Absolute return
plt_abs_return <- ggplot(KVIK, aes(x=date, y=abs(return)))+
  geom_line()+
  niceTheme+
  ggtitle("Absolute return")+
  scale_x_date(
    expand = c(0,0),
    breaks = "1 year",
    date_labels =  "%Y"
  )+
  scale_y_continuous(
    sec.axis = dup_axis(),
    expand = c(0,0.05)
  )

acf_abs_return<- ggAcf(
    KVIK[,abs(return)], 
    lag.max = 30,
    main="ACF absolute return"
  )+
niceTheme

lay <- rbind(
  c(1,1,1,1,1),
  c(2,2,2,2,2),
  c(3,3,3,4,4),
  c(5,5,5,6,6)
)

ARCH_effect <- grid.arrange(
  plt_price,plt_return,
  plt_sqrd_return,acf_sqrd_return,
  plt_abs_return,acf_abs_return,
  layout_matrix = lay
)

ARCH_effect


## ESTIMATION
## -----------------------------------------------
arma_model<-forecast::auto.arima(KVIK[,return], ic="aic")

arma_model
## Model Check (ARMA(0,0))
## -----------------------------------------------
arma_model_resid <- arma_model$residuals

#  ACF and PACF
acf_resid <- ggAcf(
  arma_model_resid, 
  lag.max = 30,
  main="ACF"
)+niceTheme

acf_resid
pacf_resid <- ggPacf(
  arma_model_resid,
  lag.max = 30, 
  main="PACF"
)+niceTheme

pacf_resid

## Formal test 
ljung_box_arma_resid<-as.data.frame(matrix(ncol = 3, nrow = 30))
colnames(ljung_box_arma_resid)<-c("QM","pval","lag")

for(i in 1:30){
  temp<-Box.test(arma_model$residuals,
                 lag=i,type="Ljung-Box",fitdf=0)
  ljung_box_arma_resid[i-0,1]<-temp$statistic
  ljung_box_arma_resid[i-0,2]<-temp$p.value
  ljung_box_arma_resid[i-0,3]<-temp$parameter
}

plt_lb_resid<-ggplot(ljung_box_arma_resid,aes(x=lag,y=pval))+
  geom_point(size=2, shape= 1)+
  niceTheme+
  geom_line(aes(y=0.05),linetype="dotted", size=1, colour="Navy blue")+labs(y="P-value")+
  ggtitle("P-values from Ljung-Box statistics")

plt_lb_resid

