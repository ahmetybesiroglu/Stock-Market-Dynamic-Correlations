x<-c("quantmod", "tidyverse", "xts", "PerformanceAnalytics", "plotly", "TSstudio", "ggthemes",
     "tidyr","forecast", "tseries", "urca", "zoo", "ggridges", "viridis", "hrbrthemes", "pastecs",
     "funModeling", "FinTS", "rugarch", "e1071", "rmgarch")

lapply(x, require, character.only = TRUE)

symbols <- c("USDTRY=X", "AKBNK.IS", "ARCLK.IS", "ASELS.IS", "BIMAS.IS", 
                         "EKGYO.IS", "EREGL.IS", "FROTO.IS", "GARAN.IS",
                         "GUBRF.IS", "HALKB.IS", "HEKTS.IS", "ISCTR.IS",
                         "KCHOL.IS", "KOZAA.IS", "KOZAL.IS", "KRDMD.IS",
                         "PETKM.IS", "PGSUS.IS", "SAHOL.IS", "SASA.IS",
                         "SISE.IS", "TAVHL.IS", "TCELL.IS", "THYAO.IS", 
                         "TKFEN.IS", "TOASO.IS", "TTKOM.IS", "TUPRS.IS",
                         "VESTL.IS", "YKBNK.IS", "XU100.IS")

ENV.STOCK <- new.env()


getSymbols(symbols, 
           from = "2013-01-01", to = Sys.Date(), 
           auto.assign = TRUE,
           env = ENV.STOCK,
           src="yahoo")

XTS.ADJUSTED <- do.call(merge, eapply(ENV.STOCK, Ad))
XTS.ADJUSTED <- XTS.ADJUSTED[,sort(names(XTS.ADJUSTED))]
names(XTS.ADJUSTED) <- unique(sort(symbols))
XTS.ADJUSTED <- na.omit(XTS.ADJUSTED)

if(("XU100.IS" %in% colnames(XTS.ADJUSTED) | "XU030.IS" %in% colnames(XTS.ADJUSTED)) & index(head(XTS.ADJUSTED, 1)) < "2020-07-27") {
  
  XTS.ADJUSTED[index(XTS.ADJUSTED)< "2020-07-27", colnames(XTS.ADJUSTED)  %in% "XU100.IS" | colnames(XTS.ADJUSTED)  %in% "XU100.IS"] <- XTS.ADJUSTED[index(XTS.ADJUSTED)< "2020-07-27", colnames(XTS.ADJUSTED)  %in% "XU100.IS" | colnames(XTS.ADJUSTED)  %in% "XU100.IS"]/100
}

ts_plot(XTS.ADJUSTED, slider = TRUE)

#daily_returns
XTS.DRETURNS <- lapply(XTS.ADJUSTED, dailyReturn)
XTS.DRETURNS <- do.call(merge, XTS.DRETURNS)
names(XTS.DRETURNS) <- symbols
XTS.DRETURNS <- XTS.DRETURNS[-1,]

ts_plot(XTS.DRETURNS, slider = TRUE)


dynamic_corr <- function(ticker, sample, lag, from, to) {
  sample <- unique(sort(sample))
  XTS.ADJUSTED <-  XTS.ADJUSTED[paste0(from,"/",to),unique(sort(sample))]
  XTS.DRETURNS <- lapply(XTS.ADJUSTED, dailyReturn)
  XTS.DRETURNS <- do.call(merge, XTS.DRETURNS)
  names(XTS.DRETURNS) <- sample
  XTS.DRETURNS <- XTS.DRETURNS[-1,]
  
  # specify i.i.d. model for the univariate time series
  ugarch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                            variance.model = list(model = "sGARCH", garchOrder = c(1,1)))
  
  # specify DCC model
  dcc_spec <- dccspec(uspec = multispec(replicate(ugarch_spec, n = length(sample))),
                      VAR = TRUE, lag = lag,
                      model = "DCC", dccOrder = c(1,1))
  
  garchdcc_fit <- dccfit(dcc_spec, data = XTS.DRETURNS[,1:length(sample)], solver = "nlminb")
  # extract time-varying covariance and correlation matrix
  dcc_cor <- rcor(garchdcc_fit)
  dcc_cor <- dcc_cor[c(sample[sample == ticker], sample[sample != ticker]),c(sample[sample == ticker], sample[sample != ticker]),] 
  #plot
  initial <- c()
  for(i in 2:length(sample)) {
    initial <- cbind(initial, dcc_cor[1, i, ])
  }
  corr_t <- xts(initial, order.by = index(XTS.DRETURNS))
  colnames(corr_t) <- 1:ncol(corr_t)
  for (i in 1:ncol(corr_t)) {
    colnames(corr_t)[i] <- paste("vs", c(sample[sample == ticker], sample[sample != ticker])[i+1])
  }
  return(ts_plot(corr_t, slider = TRUE, title = paste("Dynamic Correlations of", ticker)))
}


sample <- c("AKBNK.IS", "ARCLK.IS", "ASELS.IS", "BIMAS.IS", 
            "EKGYO.IS", "EREGL.IS", "FROTO.IS", "GARAN.IS",
            "GUBRF.IS", "HALKB.IS", "HEKTS.IS", "ISCTR.IS",
            "KCHOL.IS", "KOZAA.IS", "KOZAL.IS", "KRDMD.IS",
            "PETKM.IS", "PGSUS.IS", "SAHOL.IS", "SASA.IS",
            "SISE.IS", "TAVHL.IS", "TCELL.IS", "THYAO.IS", 
            "TKFEN.IS", "TOASO.IS", "TTKOM.IS", "TUPRS.IS",
            "VESTL.IS", "YKBNK.IS", "USDTRY=X")

dynamic_corr("USDTRY=X", sample, lag = 10, from = "2013-01-01", to = Sys.Date())


