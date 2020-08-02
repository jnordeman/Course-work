## REQUIRED PACKAGES
## -----------------------------------------------
library(data.table)
library(quantmod)
library(ggplot2)
library(gridExtra)
library(forecast)
library(rugarch)
library(dynlm)



## DATA RETRIEVAL
## -----------------------------------------------
StartDate <- as.Date("2011-01-01","%Y-%m-%d")
LastDate <- as.Date("2018-12-31","%Y-%m-%d")
TradingDays <- 253


# Retrieve the data from Yahoo Finance
getSymbols("KINV-B.ST", from =StartDate, to =LastDate)
KVIK <- as.data.table(`KINV-B.ST`[,6])
colnames(KVIK) <- c("date","price")

KVIK[,return:=(log(price)-shift(log(price)))]
KVIK<-na.omit(KVIK)

# Summary statistics
KVIK[,.(
  annualised_mean = mean(return,na.rm = TRUE)*TradingDays,
  annualised_sd = sd(return, na.rm = TRUE)*sqrt(TradingDays)
)]


## THEME FOR DATA VISUALISATION
## -----------------------------------------------
niceTheme <- niceTheme <-theme(
  axis.title.x=element_blank(),
  axis.text.x=element_text(color = "black",size=8),
  axis.title.y = element_blank(),
  axis.text.y=element_text(color="black",size=8),
  panel.background=element_rect(fill = "white"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line = element_line(colour = "black",size = 0.5),
  plot.title = element_text(lineheight=.8, hjust = 0.5)
)
