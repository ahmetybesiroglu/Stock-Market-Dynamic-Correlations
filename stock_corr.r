x<-c("quantmod", "tidyverse", "xts", "PerformanceAnalytics", "plotly", "TSstudio", "ggthemes",
     "tidyr","forecast", "tseries", "urca", "zoo", "ggridges", "viridis", "hrbrthemes", "pastecs",
     "funModeling", "FinTS", "rugarch", "e1071", "rmgarch")

lapply(x, require, character.only = TRUE)


Symbols <- unique(sort(c("AKBNK.IS", "ARCLK.IS", "ASELS.IS", "BIMAS.IS", 
                         "EKGYO.IS", "EREGL.IS", "FROTO.IS", "GARAN.IS",
                         "GUBRF.IS", "HALKB.IS", "HEKTS.IS", "ISCTR.IS",
                         "KCHOL.IS", "KOZAA.IS", "KOZAL.IS", "KRDMD.IS",
                         "PETKM.IS", "PGSUS.IS", "SAHOL.IS", "SASA.IS",
                         "SISE.IS", "TAVHL.IS", "TCELL.IS", "THYAO.IS", 
                         "TKFEN.IS", "TOASO.IS", "TTKOM.IS", "TUPRS.IS",
                         "VESTL.IS", "YKBNK.IS")))

k <- length(Symbols)

ENV.STOCK <- new.env()

getSymbols(Symbols, 
           from = "2020-01-01", to = Sys.Date(), 
           auto.assign = TRUE,
           env = ENV.STOCK)


XTS.ADJUSTED <- do.call(merge, eapply(ENV.STOCK, Ad))
XTS.ADJUSTED <- XTS.ADJUSTED[,sort(names(XTS.ADJUSTED))]

XTS.VOLUME <- do.call(merge, eapply(ENV.STOCK, Vo))
XTS.VOLUME <- XTS.VOLUME[,sort(names(XTS.VOLUME))]

ts_plot(XTS.ADJUSTED, slider = TRUE)

#daily_returns
XTS.DRETURNS <- lapply(XTS.ADJUSTED, dailyReturn)
XTS.DRETURNS <- do.call(merge, XTS.DRETURNS)
names(XTS.DRETURNS) <- Symbols
XTS.DRETURNS <- XTS.DRETURNS[-1,]

ts_plot(XTS.DRETURNS, slider = TRUE)


# specify i.i.d. model for the univariate time series
ugarch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                          variance.model = list(model = "sGARCH", garchOrder = c(1,1)))

# specify DCC model
dcc_spec <- dccspec(uspec = multispec(replicate(ugarch_spec, n = k)),
                    VAR = TRUE, lag = 10,
                    model = "DCC", dccOrder = c(1,1))

garchdcc_fit <- dccfit(dcc_spec, data = XTS.DRETURNS[,1:k], solver = "nlminb")
garchdcc_fit


# extract time-varying covariance and correlation matrix
dcc_cor <- rcor(garchdcc_fit)
dim(dcc_cor)


#plot
initial <- c()
for(i in 2:k) {
  initial <- cbind(initial, dcc_cor[1, i, ])
}

corr_t <- xts(initial, order.by = index(XTS.DRETURNS))

colnames(corr_t) <- 1:ncol(corr_t)
for (i in 1:ncol(corr_t)) {
  colnames(corr_t)[i] <- paste("vs", colnames(XTS.DRETURNS)[i+1])
}

ts_plot(corr_t, slider = TRUE, title = "Dynamic Correlations of AKBNK")




# Creating function -------------------------------------------------------


dynamic_corr <- function(ticker) {
  new.xts <-merge(XTS.DRETURNS[,Symbols == ticker],
                  XTS.DRETURNS[,Symbols != ticker])
  # specify i.i.d. model for the univariate time series
  ugarch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                            variance.model = list(model = "sGARCH", garchOrder = c(1,1)))
  # specify DCC model
  dcc_spec <- dccspec(uspec = multispec(replicate(ugarch_spec, n = k)),
                      VAR = TRUE, lag = 10,
                      model = "DCC", dccOrder = c(1,1))
  garchdcc_fit <- dccfit(dcc_spec, data = new.xts[,1:k], solver = "nlminb")
  # extract time-varying covariance and correlation matrix
  dcc_cor <- rcor(garchdcc_fit)
  #plot
  initial <- c()
  for(i in 2:k) {
    initial <- cbind(initial, dcc_cor[1, i, ])
  }
  corr_t <- xts(initial, order.by = index(new.xts))
  colnames(corr_t) <- 1:ncol(corr_t)
  for (i in 1:ncol(corr_t)) {
    colnames(corr_t)[i] <- paste("vs", colnames(new.xts)[i+1])
  }
  return(ts_plot(corr_t, slider = TRUE, title = paste("Dynamic Correlations of", ticker)))
}

dynamic_corr("EREGL.IS")

lapply(Symbols, dynamic_corr)
