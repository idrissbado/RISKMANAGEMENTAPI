require("opencpu")
require("broom")
require("PerformanceAnalytics")
require("plumber")
require("ggmap")
require("ggplot2")
require("dplyr")
require("rjson")
require("jsonlite")
require("RCurl")
require("readr")
require("Rook")
require("tseries")
require("purrr")
require(fPortfolio)
library("rsconnect")
library(tseries)
library(SWIM)
library(purrr)
library(broom)
library(tidyquant)
library(tibble)
library(timetk)
library(actuar)
library(Hmisc )
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#
#
require("opencpu")
require("PerformanceAnalytics")
require("plumber")
require("ggmap")
require("ggplot2")
require("dplyr")
require("rjson")
require("jsonlite")
require("RCurl")
require("readr")
require("cvar")
require("Rook")
require("tidyr")
require("tseries")
library(xts)
library(PerformanceAnalytics)
library(tidyr)
require("opencpu")
require("highcharter")
library(highcharter)
library(rsconnect)
library(cvar)

interval_sd_by_hand <-
  function(returns_df,
           start = 1,
           window = 24,
           weights){
    # First create start date.
    start_date <-
      returns_df$date[start]
    # Next an end date that depends on start date and window.
    end_date <-
      returns_df$date[c(start + window)]
    # dplyr::dplyr::filter on start and end date.
    returns_to_use <-
      returns_df %>%
      dplyr::filter(date >= start_date & date < end_date) %>%
      select(-date)
    # Portfolio weights.
    w <- weights
    # Call our original custom function.
    # We are nesting one function inside another.
    component_percentages <-
      component_contr_matrix_fun(returns_to_use, w)
    # Add back the end date as date column
    results_with_date <-
      component_percentages %>%
      mutate(date = ymd(end_date)) %>%
      select(date, everything()) %>%
      spread(asset, contribution) %>%
      mutate_if(is.numeric, function(x) x * 100)
  }
simulation_accum_1 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
    `colnames<-`("returns") %>%
    mutate(sim =
             accumulate(returns,
                        function(x, y) x * y)) %>%
    dplyr::select(sim)
  
}



#* @apiTitle RISKS MANAGEMENT API
#* @apiDescription TOOLS FOR RISK MANAGEMENT PROPOSED BY IDRISS OLIVIER BADO,RESEARCH DEVELOPMENT ENGINEER AT KYRIA Consulting
library(lubridate)
library(rgeos)
library(rgdal)
library(sp)
library(raster)
library(cleangeo)
library(PerformanceAnalytics)
library(anytime)
#* return checkData
#* @param data
#*@serializer csv	
#* @get /checkData
function(req){
  
  data <- req$postBody
  FactorR_xts <- xts(x = data[, -1], # use all columns except for first column (date) as data
                     order.by = as.Date(data$Date) # Convert Date column from factor to Date and use as time index
  )
  return(checkData(FactorR_xts))
  
}
#* Log some information about the incoming request
#* @dplyr::filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  forward()
}


#* @apiTitle Plumber Example API



#* VAR CALCULATION
#*@param req
#*@param do.this
#*@post /VAR
function(req,do.this){
  data <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  ##datacsv.df$nomColonne
  data<-ts(data[,-1])
  ## historical
  switch(do.this,
         T1={ P<<-jsonlite::toJSON(PerformanceAnalytics::VaR(data, p=.95, method="historical")
         )},
         T2={P<<-jsonlite::toJSON(PerformanceAnalytics::VaR(data, p=.95, method="gaussian")
         )},
         T3={P<<-jsonlite::toJSON(PerformanceAnalytics::VaR(data, p=.95, method="modified")
         )},
         T4={P<<-jsonlite::toJSON(PerformanceAnalytics::VaR(data, p=.01)
         )},
         T5={P<<-jsonlite::toJSON(PerformanceAnalytics::VaR(data, p=.99)
         )},
         T6={P<<-jsonlite::toJSON(PerformanceAnalytics::VaR(data, clean="boudt")
         )},
         T7={P<<-jsonlite::toJSON(PerformanceAnalytics::VaR(data, clean="boudt", portfolio_method="component")
         )},
         stop("Enter something that switches me!")
  )
  return(P) 
}
#*return TVAR 
#*@param req 
#*@param op
#*@serializer json
#*@post /TVAR
function(req,op){
  data <- read.csv(text = req$postBody, header = TRUE,sep=";")
    data<-ts(data)
  # first do normal ES calc
  switch(op,T1={ P<-ES(data, p=.95, method="historical")},
         # now use Gaussian
         T2={P<-ES(data, p=.95, method="gaussian")},
         # now use modified Cornish Fisher calc to take non-normal distribution into account
         T3={P<-ES(data, p=.95, method="modified")},
         # now use p=.99
         T4={P<-ES(data, p=.99)},
         # or the equivalent alpha=.01
         T5={ P<-ES(data, p=.01)},
         # now with outliers squished
         T6= {P<-ES(data, clean="boudt")},
         # add Component ES for the equal weighted portfolio
         
         T7={P<-ES(edhec, clean="boudt", portfolio_method="component")},
         stop("Enter something that switches me !!"))
  return(P)
}
#* Return the CDD
#*@param req
#*@serializer json
#*@post /CDD
function(req){
  datacsv.df <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  ##datacsv.df$nomColonne
  datacsv.ts = xts(x=datacsv.df[,-1], frequency = 12,order.by = as.Date((datacsv.df$Date)))
  return(t(round(CDD(as.data.frame(datacsv.ts)) ,4)))
}
#*return informationratio
#*@param req :data an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns and bechmark
#*@param n : column number of benchmark in req
#*@serializer csv
#*@post /informationratio
function(req,n){
  data<-read.csv(text=req$postBody,header=TRUE,sep=",")
  data<-ts(data)
  return(table.InformationRatio(data[,-c(1,as.numeric(n))], data[,as.numeric(n)]))
}
#*return informationratio
#*@param req :data an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns and bechmark
#*@param n : column number of benchmark in req
#*@serializer png
#*@post /informationratioplot
function(req,n){
  data<-read.csv(text=req$postBody,header=TRUE,sep=",")
  data<-ts(data)
  result<-table.InformationRatio(data[,-c(1,as.numeric(n))], data[,as.numeric(n)])
  textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
           rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
           row.valign="center", wrap.rownames=20, wrap.colnames=10,
           col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
  title(main="Portfolio information ratio")
  #textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
          # rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
          # row.valign="center", wrap.rownames=20, wrap.colnames=10,
          # col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
  #title(main="Portfolio information ratio")
}
#* return mean
#* @param req data of price
#* @serializer json
#* @post /rendementmoyen
function(req){
  data<-read.csv(text=req$postBody,header=TRUE,sep=",")
  data<-ts(data)
  data<-data[,-1]
  calculateReturns<-data %>%
    sapply(.,function(x){
      diff(x)/lag(x)
    }) %>%
    na.omit() %>%
    sapply(.,mean)
  return(calculateReturns)
}
#*return M2
#* @param req data of price
#* @param n :column for benchmark
#* @param MAR :value
#* @serializer json
#* @post /indicateurM2
function(req,n,MAR){
  data<-read.csv(text=req$postBody,header=TRUE,sep=",")
  data<-ts(data)
  Ra<-data[,-c(1,as.numeric(n))]
  Rb<-data[,as.numeric(n)]
  M2Sortino(Ra, Rb, as.numeric(MAR ))
}
#*return The return on an investment's annualized return minus the benchmark's annualized return
#*@param req :data
#*@param n : benchmark column
#*serializer json
#*@post /activepremium
function(req,n){
  data<-read.csv(text=req$postBody,header=TRUE,sep=",")
  data<-ts(data)
  Ra<-data[,-c(1,as.numeric(n))]
  Rb<-data[,as.numeric(n)]
  ActivePremium(Ra, Rb)
}
#* return tracking error
#*@param req :data
#*@param n : benchmark column
#*serializer json
#*@post /trackingerror
function(req,n){
  data<-read.csv(text=req$postBody,header=TRUE,sep=",")
  data<-ts(data)
  Ra<-data[,-c(1,as.numeric(n))]
  Rb<-data[,as.numeric(n)]
    TrackingError(Ra, Rb)
}
#*allocationwithomega
#*@param req :an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#*@param method one of: simple, interp, binomial, blackscholes
#*@param output one of: point (in time), or full (distribution of Omega)
#*@param Rf:risk free value
#*@post /allocationwithomega
function(req,method,output,Rf){
  R<-read.csv(text=req$postBody,header = TRUE,sep=",")
  R<-ts(R)
 Omega(R,L=0,  method="method", output="output" ,as.numeric(Rf) )
      
}
#*return Net selectivity is the remaining selectivity after deducting the amount of return require to justify not
#being fully diversified:If net selectivity is negative the portfolio manager has not justified the loss of diversification
#*@param req :an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#*@param num :column for benchmark
#*@param Rf :free risk
#*@serializer json
#*@post /Netselectivity
function(req,num,Rf){
  data<-ts(read.csv(text=req$postBody,header = TRUE,sep=","))
  Ra<-data[,-as.numeric(num),drop=FALSE]
  Rb<-data[,as.numeric(num),drop=FALSE]
  NetSelectivity(Ra, Rb, as.numeric(Rf ))
}

#* Return the SMP.epsilon
#* @param req
#* @param assetr
#* @param assetm
#* @param Rf
#* @serializer json
#*@post /SFM.epsilon
function(req,assetr,assetm,Rf){
  #asset1= body$asset1
  #asset2= body$asset2
  #data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  data<-ts(data)
  
  # data <- ts(read.delim(text =req$postBody
  #, header = TRUE,stringsAsFactors = FALSE,sep=","))
  
  #row.names(data)=date
  
  
  return(SFM.epsilon(data[,as.numeric(assetr),drop=FALSE], data[,as.numeric(assetm),drop=FALSE],as.numeric(Rf)))
  
}
#* Return the CAMP.RiskPremium
#* @param req
#* @param Rf
#*@post /CAMP.RISKPremium
function(req,Rf){
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  datacsv.ts<-ts(data)
  return(CAPM.RiskPremium(datacsv.ts, as.numeric(Rf)))
}
#* Return the CAMP.SML.slope
#* @param req
#* @param numero
#* @param Rf
#*@post /CAMP.SML.slope
function(req,numero,Rf){
  data <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  #write.table(data,file='C:/Users/Yapi Emma/Desktop/example1.csv')
  #data <- read.zoo(text = data, sep = ",", header = TRUE, index = 1:1, tz = "", format = "%Y-%m-%d")
  #print(data)
  #return(data)
  #datas = xts(data[,-1], order.by=as.Date(data$Date))
  
  #datav.dfcs <- read.csv(text = csv, header = TRUE,stringsAsFactors = FALSE)
  ## print(data)
  #return(csv)
  datacsv.ts<-ts(data)
  #datacsv.ts = xts(x=datacsv.df$CTA.Global, frequency = 12,order.by = as.Date((titre.df$Date)))
  ## 
  return(CAPM.SML.slope(datacsv.ts[,as.numeric(numero),drop=FALSE], as.numeric(Rf)))
}
#* Return the CAMP.CML
#*@param req for data
#*@param req1 la rentabilite de l'actif
#*@param req2 la rentabilite du benchmark
#*@post /CAMP.CML
function(req,req1,req2,Rf){
  datacsv.df <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  datacsv.ts<-ts(datacsv.df)
  
  Ra<-datacsv.ts[,as.numeric(req1),drop=FALSE]
  Rb<-datacsv.ts[,as.numeric(req2),drop=FALSE]
  return(CAPM.CML(Ra,Rb, as.numeric(Rf)))
}
#* Return the BurkeRatio
#* @param req
#* @param Rf of asset
#* @param modified  FALSE OR TRUE 
#*@post /BurkeRatio
function(req,Rf,modified){
  #data <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  
  return(BurkeRatio(data,as.numeric(Rf),modified))
}
#* Return the BetaCoSkewness
#* @param req
#* @param req1 le numero du benchmark dans la data

#*@post /BetaCoSkewness
function(req,req1){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  Ra<-data[,-as.numeric(req1)]
  Rb<-data[,as.numeric(req1)]
  return(BetaCoSkewness(Ra, Rb))
}
#* Return the BetaCoKurtosis
#*@param req
#* @param req1
#*@post /BetaCoKurtosis
function(req,req1){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  Ra<-data[,-as.numeric(req1)]
  Rb<-data[,as.numeric(req1)]
  return(BetaCoKurtosis(Ra,Rb))
}
#*Return the BetaCoVariance
#*@param req
#* @param req1

#*@post /BetaCoVariance
function(req,req1){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  Ra<-data[,-as.numeric(req1)]
  Rb<-data[,as.numeric(req1)]
  #datacsv.df <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  return(BetaCoVariance(Ra, Rb))
}
#* Return the BernardoLedoitRatio
#* @param req
#*@post /BernardoLedoitRatio
function(req){
  #datacsv.df <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  return(BernardoLedoitRatio(data))
}
#* Return Total Risk
#* @param req
#* @param num numero de la colonne du  benchmark
#*@post /TotalRisk
function(req,num){
  #datacsv.df <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  return(TotalRisk(data[,-as.numeric(num)], data[,as.numeric(num)]))
  
}
#* Return Selectivity the same as Jensen's alpha
#* @param req of asset
#* @param num numero de la colonne du  benchmark
#* @param Rf risk free rate
#*@post /Selectivity
function(req,num,Rf){
  #data <- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  Ra<-data[,-as.numeric(num)]
  Rb<-data[,as.numeric(num)]
  return(Selectivity(Ra,Rb,as.numeric(Rf)))
}
#*Return TimingRatio may help assess whether the manager is a good timer of asset allocation decisions. 
#* @param req of asset
#* @param num numero de la colonne du  benchmark
#* @param Rf risk free rate
#*@post /TimingRatio
function(req,num,Rf){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  Ra<-data[,-as.numeric(num)]
  Rb<-data[,as.numeric(num)]
  return(TimingRatio(Ra, Rb, as.numeric(Rf)))
}
#* Return the CAMP.CML.slope
#* @param num le numero de la colonne de la la rentabilite du benchmark
#* @param Rf 
#*@post /CAMP.CML.slope
function(req,num,Rf){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  Ra<-data[,as.numeric(num)]
  
  return(CAPM.CML.slope(Ra, as.numeric(Rf)))
  
}
#*Returns table.ProbOutPerformance a table that contains the counts and probabilities of outperformance relative to benchmark for the various period_lengths
#*@param  req 
#*@param no for R an xts, timeSeries or zoo object of asset returns
#*@param nu for Rb the number of column of an xts, timeSeries or zoo object of the benchmark returns
#*@post /table.ProboutPermance
function(req,no,nu){
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  #data<-ts(data)
  R<-data[,as.numeric(no)]
  Rb<-data[,as.numeric(nu)]
  table.ProbOutPerformance(R,Rb)
  title(main='Table of Convertible Arbitrage vs Benchmark')
}
#*Returns chart.VaRSensitivity which Creates a chart of Value-at-Risk and/or Expected Shortfall estimates by confidence interval for multiple methods.
#*example data managers structures
#*@param req
#*@param num
#*@png
#*@post /chart.VaRSensitivity
function(req,num){
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  #data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  data<-ts(data)
  par(mar = rep(2, 4))
  
  return(chart.VaRSensitivity(data[,as.numeric(num),drop=FALSE], 
                              methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), 
                              colorset=bluefocus, lwd=2)
  )
}
#*Return CalmarRatio and SterlingRatio
#*@param req
#*@param op
#*@post /camaSertlingRatio
function(req,op){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  switch(op,
         T1={ P<-CalmarRatio(as.data.frame(data))
         
         },
         T2={ P<-SterlingRatio(as.data.frame(data))
         
         
         },
         stop("Enter something that switches me!"))
  return(P)
}
#* Return the Treynor Ratio
#* @param req data
#* @param num numero de la colonne du benchmark 
#* @param Rf la rentabilite au taux sans risque 
#*@post /Treynor
function(req,num,Rf){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  Ra<-data[,-as.numeric(num)]
  Rb<-data[,as.numeric(num)]
  return( round(TreynorRatio(Ra, Rb, as.numeric(Rf)),4))
  
}
#* Return the TrackingError
#* @param req 
#* @param num numero de la colonne du benchmark
#*@post /TrackingError
function(req,num){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  Ra<-data[,-as.numeric(num)]
  Rb<-data[,as.numeric(num)]
  return( TrackingError(Ra, Rb, scale = NA))
  
}
#* Return the CAPM.beta.bear
#* @param req 
#* @param num numero de la colonne  la rentabilite du benchmark
#*@param Rf
#*@post /CAPM.beta.bear
function(req,num,Rf){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  Ra<-data[,-as.numeric(num)]
  Rb<-data[,as.numeric(num)]
  return(CAPM.beta.bear(Ra,
                        Rb,
                        as.numeric(Rf )))
}

#* Return the SFM.jensenAlpha
#*@param req
#*@param num
#*@post /SFM.jensenAlpha
function(req,num){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  Ra<-data[,-as.numeric(num)]
  Rb<-data[,as.numeric(num)]
  return(SFM.jensenAlpha(Ra, Rb))
}
#*Return the AdjustedSharpe
#* @param req
#*@post /AdjustedSharpe
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  data<-as.data.frame(data)
  return(AdjustedSharpeRatio(data))
}
#*Return calculate a function over an expanding window always starting from
#* @param req
#*@post /apply.fromstart
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  data<-as.data.frame(data)
  jsonlite::toJSON(as.data.frame( apply.fromstart(data, FUN="mean", width=36)))
  
}
#* Return Rebalancing of Portfolio
#*@param req An xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#*@param periode the calendar period for rebalancing
#*@post /Rebalancing
function(req,periode){
  #data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  R<-ts(data)
  switch(periode,
         T1={ P=jsonlite::toJSON(as.data.frame(Return.portfolio(
           R,
           
           rebalance_on =  "years",verbose=TRUE
         )))
         },
         T2={P= jsonlite::toJSON(as.data.frame(Return.portfolio(
           R,
           
           rebalance_on = "quarters",verbose = TRUE
           
         )))
         },
         T3={P= jsonlite::toJSON(as.data.frame(Return.portfolio(
           R,
           rebalance_on =  "months",verbose = TRUE
           
         )))
         },
         T4={P= jsonlite::toJSON(as.data.frame(Return.portfolio(
           R,
           rebalance_on = "weeks",verbose=TRUE
           
           
         )))
         },
         T5={P= jsonlite::toJSON(as.data.frame(Return.portfolio(
           R,
           rebalance_on =  "days"
           
           
           
         )))
         },
         
         stop("Enter something that switches me!")
  )
  return(P)
  
}
#*Return some performance ratio 
#*Return Chart that cumulates the periodic returns given and draws a line graph of the results as a "wealth index
#*@param req 
#*@param num 
#*@param scale number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4)
#*@param Rf risk free rate, in same period as your returns
#*@post /table.CAPM
function(req,num,Rf,scale){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  #data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  Ra<-data[,-as.numeric(num),drop=FALSE]
  Rb<-data[,-as.numeric(num),drop=FALSE]
  
  return(table.CAPM(Ra, Rb, as.numeric(scale) , as.numeric(Rf) , digits =4))
}
#* Return chart.Cumeturns
#* @param req
#* @param do.this :method
#*@post /chart.Cumeturns
function(req,do.this){
  #data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  switch(do.this,
         T1={ P=chart.CumReturns(data,main="Cumulative Returns",begin="first")
         },
         
         T2={ P=chart.CumReturns(data,main="Cumulative Returns",begin="axis")
         },
         stop("Enter something that switches me!")
  )
  return(P)
}


#*Return calculate a function over a rolling window
#*@param req
#*@param num 
#*@post /apply.rolling
function(req,num){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  data<-as.data.frame(data)
  jsonlite::toJSON(as.data.frame(apply.rolling(data[,as.numeric(num),drop=FALSE], FUN="mean", width=36)))
}
#*Return VolatilitySkewness
#*@param req
#*@param MAR=0 or 0.005
#*@post /VolatilitySkewness
function(req,MAR){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  data<-as.data.frame(data)
  return(VolatilitySkewness(data, as.numeric(MAR), stat="volatility"))
}
#*Return UpsideRisk
#*@param req
#*@param MAR=0 or 0.005
#*@post /UpsideRisk
function(req,MAR){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  data<-as.data.frame(data)
  return(UpsideRisk(data, as.numeric(MAR), stat="risk"))
}
#*Return UpsidePotentialRatio
#*@param req
#*@param MAR=0 or 0.005
#*@post /UpsidePotentialRatio
function(req,MAR){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  data<-as.data.frame(data)
  return( UpsidePotentialRatio(data, as.numeric(MAR)))
}
#*Return UpsideFrequency
#*@param req
#*@post /UpsideFrequency
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  return(UpsideFrequency(as.data.frame(data)))
}
#*Return table variability
#*@param req
#*@post /table.variability
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  return(table.Variability(as.data.frame(data)))
}

#*Benchmarking with market index 
#*@param req  for stock or portfolio
#*@param num 
#*@serializer jpeg
#*@post /chart.RelativePerformance
function(req,num){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  Ra<-data[,-as.numeric(num)]
  Rb<-data[,as.numeric(num)]
  par(mar = rep(2, 4))
  
  chart.RelativePerformance(Ra, 
                            Rb, 
                            colorset=rich8equal, legend.loc="bottomright", 
                            main="BENCHMARK")
  
}
#*return benchmarking sectorial index 
#*@param req 
#*@param symbol1 the name of the column which contains the asset
#*@param symbol2 the name of index in the dataset
#*@serializer png 
#*@post /performancesectorielindex
function(req,symbol1,symbol2){
  #data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  highchart(type = "stock") %>%
    hc_title(text = "Dynamic visualisation") %>%
    hc_add_series(data[,as.numeric(symbol1)],
                  name = symbol1 )%>%
    hc_add_series(data[, as.numeric(symbol2)],
                  name = symbol2) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE)
}

#*Return correlation with plot
#*@param req
#*@serializer png 
#*@post /chart.correlation
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  data<-as.data.frame(data)
  chart.Correlation(data, histogram=TRUE, pch="+")
}
library(readr)
library(dplyr)
require(reshape2)
require(ggplot2)
#*Return virtual model portfolio
#*@param req
#*@post /Virtualportfolio
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  data<-as.data.frame(data)
  weight=matrix(0,ncol=length(data),nrow=NROW(data))
  for (i in 1:NROW(data)){
    for( j in 2:length(data)){
      weight[i,j]=data[i,j]/sum(data[,j])
    }
  }
  weight<-as.data.frame(weight)
  data<-data[,2:length(data)]
  virtual<-data*weight[,2:length(weight)]
  data$frame="1"
  virtual$frame="2"
  
  df=cbind(data,virtual) 
  df <- melt(df)
  
  
  return(df)
}

#* prediction anlysis 
#* @param req data
#* @param n asset column
#* @param m return column
#* @param r market return column 
#* @serializer png
#* @post /predictio
function(req,n,m,r){
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE,sep=",")
  
  #data<- read.csv(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  #data<- read.delim.zoo(text = req, sep=";", header = TRUE,tz =  "UTC",  format = "%Y-%m-%d")
  #data$index(data)<-data$Date
  #data<-as.data.frame(data)
  asset<-data[,as.numeric(n)]
  return<-data[,as.numeric(m)]
  marketreturn<-data[,as.numeric(r)]
  dat<-cbind(asset,return,marketreturn)
  dat<-as.data.frame(dat)
  dat<-ts(dat)
  colnames(dat)<-c("asset","return","marketreturn")
  portfolio_returns_xts_rebalanced_monthly <-Return.portfolio(dat,  #weights = edhec[,1],
                                                              rebalance_on = "months") %>%
    `colnames<-`("returns") %>%
    return(portfolio_returns_xts_rebalanced_monthly)
  portfolio_model_augmented <-
    as.data.frame(portfolio_returns_xts_rebalanced_monthly) %>%
    do(model =
         lm(returns ~
              as.numeric(marketreturn), data = .)) %>%
    map(~ .x[[1]][5])
  fitted<-portfolio_model_augmented[["model"]][["fitted.values"]]
  datt<-cbind(portfolio_returns_xts_rebalanced_monthly$returns,fitted)
  s<-  as.data.frame(datt)%>%
    ggplot(aes(x=index(datt), y = datt, color = type)) +
    geom_line() +
    xlab("date")
  return(s)
}

#* prediction analysis
#* @param req data
#* @param n asset column
#* @param m return column
#* @param r market return column 
#* @param p weight return column
#* @post /prediction
function(req,n,m,r,p){
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  #data<- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  asset<-data[,as.numeric(n)]
  return<-data[,as.numeric(m)]
  marketreturn<-data[,as.numeric(r)]
  weight<-data[,as.numeric(p)]
  dat<-cbind(asset,return,marketreturn,weight) 
  colnames(dat)<-c("asset","return","marketreturn","weight")
  dat$asset<-asset
  portfolio_returns_xts_rebalanced_monthly <-Return.portfolio(dat,  weights = dat$weight,
                                                              rebalance_on = "months") %>%
    `colnames<-`("returns")
  as.data.frame(portfolio_returns_xts_rebalanced_monthly) %>%
    mutate(market_return =
             dat$marketreturn) %>%
    ggplot(aes(x = market_return,
               y = returns)) +
    geom_point(color = "cornflowerblue") +
    geom_smooth(method = "lm",
                se = FALSE,
                color = "green") +
    ylab("portfolio returns") +
    xlab("market returns")
}
#* prediction analysis beta
#* @param req data
#* @param n asset column
#* @param m return column
#* @param r market return column 
#* @post /betaestimated
function(req,n,m,r){
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE,sep=",")
  
  #data<- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  asset<-data[,as.numeric(n)]
  return<-data[,as.numeric(m)]
  marketreturn<-data[,as.numeric(r)]
  dat<-cbind(asset,return,marketreturn)
  dat<-as.data.frame(dat)
  Beta<- dat %>%
    lm(return~marketreturn,.)%>%
    tidy()
  
  return(jsonlite::toJSON(Beta,force=TRUE))
}
#*Return stats
#*@param req
#*@serializer png
#*@post /table.Stats
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  result=t(table.Stats(as.data.frame(data)))
  require("Hmisc")
  textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(rep(1,2),rep(3,14))),
           rmar = 0.8, cmar = 1.5, max.cex=.9, halign = "center", valign = "top",
           row.valign="center", wrap.rownames=10, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
  title(main="Statistics ")
}
#*return contribution of risk
#*@param req data of asset (Date,assetname,returnsvalue or price)
#*@param  w vector same size with asset number 
#*@post /component_contr_matrix_fun

function(req, w){
  W<-as.numeric(as.vector(w))
  prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
  prices$date<-as.Date(prices$date)
  # create covariance matrix
  prices<-ts(prices)
  prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
  asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))
  covariance_matrix <-
    cov(asset_returns_xts[,-1])
  # calculate portfolio standard deviation
  sd_portfolio <-
    sqrt(t(w) %*% covariance_matrix %*% w)
  # calculate marginal contribution of each asset
  marginal_contribution <-
    w %*% covariance_matrix / sd_portfolio[1, 1]
  # multiply marginal by weights vecotr
  component_contribution <-
    marginal_contribution * w
  # divide by total standard deviation to get percentages
  component_percentages <-
    component_contribution / sd_portfolio[1, 1]
  component_percentages %>%
    as_tibble() %>%
    gather(asset, contribution)
}

#*return contribution of risk
#*@param req data of asset (Date,assetname,returnsvalue or price)
#*@param w weight 
#*@post /contributionplot
testtt<-function(req,w){
  w<-as.numeric(w)
  prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
  prices$date<-as.Date(prices[,1])
  asset_returns_dplyr_byhand <- 
    ts(prices) %>% 
    to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    # convert the index to a date
    data.frame(date = index(.)) %>%
    # now remove the index because it got converted to row names
    remove_rownames() %>% 
    gather(asset, returns, -date) %>% 
    group_by(asset) %>%  
    mutate(returns = log(returns) ) %>%
    spread(asset, returns) %>% 
    dplyr::select(date, symbols) %>%
    na.omit()
  
  percentages_tibble <-
    asset_returns_dplyr_byhand %>%
    dplyr::select(-date) %>%
    component_contr_matrix_fun(., w)
  percentages_tibble %>%
    ggplot(aes(x = asset, y = contribution)) +
    geom_col(fill = 'cornflowerblue',
             colour = 'pink',
             width = .6) +
    ggtitle("Percent Contribution to Standard Deviation") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Asset") +
    ylab("Percent Contribution to Risk")
}

#*return contributionplot2
#*@param req 
#*@param w weight 
#*@post /contributionplot2
function(req,w){
  w<-as.numeric(w)
  prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
  
  asset_returns_dplyr_byhand <- 
    prices %>% 
    to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    # convert the index to a date
    data.frame(date = index(.)) %>%
    # now remove the index because it got converted to row names
    remove_rownames() %>% 
    gather(asset, returns, -date) %>% 
    group_by(asset) %>%  
    mutate(returns = log(returns) ) %>%
    spread(asset, returns) %>% 
    dplyr::select(date, symbols) %>%
    na.omit()
  
  percentages_tibble <-
    asset_returns_dplyr_byhand %>%
    dplyr::select(-date) %>%
    component_contr_matrix_fun(., w)
  
  percentages_tibble %>%
    mutate(weights = w) %>%
    gather(type, contribution, -asset) %>%
    group_by(type) %>%
    ggplot(aes(x = asset,
               y = contribution,
               fill = type)) +
    geom_col(position='dodge') +
    ggtitle("Percent Contribution to Volatility") +
    theme(plot.title = element_text(hjust = 0.5))
}
#*return interval contribution 
#*@param req
#*@param n column for weights
#*@post /interval_sd
interval_sd_by_hand <-
  function(req,
           start = 1,
           window = 24,
           w){
    weights<-as.vector(as.numeric(w))
    prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
    
    asset_returns_dplyr_byhand <- 
      prices %>% 
      to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
      # convert the index to a date
      data.frame(date = index(.)) %>%
      # now remove the index because it got converted to row names
      remove_rownames() %>% 
      gather(asset, returns, -date) %>% 
      group_by(asset) %>%  
      mutate(returns = log(returns) ) %>%
      spread(asset, returns) %>% 
      dplyr::select(date, symbols) %>%
      na.omit()
    returns_df<-asset_returns_dplyr_byhand
    # First create start date.
    start_date <-
      returns_df$date[start]
    # Next create an end date that depends
    # on start date and window.
    end_date <-
      returns_df$date[c(start + window)]
    # Filter on start and end date.
    returns_to_use <-
      returns_df %>%
      filter(date >= start_date & date < end_date) %>%
      raster::select(-date)
    # Portfolio weights
    w <- weights
    # Call our original custom function
    # We are nesting one function inside another
    component_percentages <-
      component_contr_matrix_fun(returns_to_use, w)
    # Add back the end date as date column
    results_with_date <-
      component_percentages %>%
      mutate(date = lubridate::ymd(end_date)) %>%
      dplyr::select(date, everything()) %>%
      spread(asset, contribution) %>%
      # Round the results for better presentation
      mutate_if(is.numeric, function(x) x * 100)
  }

#*return portfoliocontribution
#*@param req
#*@param  w for weights
#*@post /portfolio_voplot
function(req,w){
  weights<-as.vector(as.numeric(w))
  prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
  prices$date<-as.Date(prices[,1])
  prices<-ts(prices)
  asset_returns_dplyr_byhand <- 
    prices %>% 
    to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    # convert the index to a date
    data.frame(date = index(.)) %>%
    # now remove the index because it got converted to row names
    remove_rownames() %>% 
    gather(asset, returns, -date) %>% 
    group_by(asset) %>%  
    mutate(returns = log(returns) ) %>%
    spread(asset, returns) %>% 
    dplyr::select(date, symbols) %>%
    na.omit()
  # First argument:
  # tell map_df to start at date index 1
  # This is the start argument to interval_sd_by_hand()
  # and it is what map() will loop over until we tell
  # it to stop at the date that is 24 months before the
  # last date.
  portfolio_vol_components_tidy_by_hand<- map_df(1:(nrow(asset_returns_dplyr_byhand) -24),
                                                 # Second argument:
                                                 # tell it to apply our rolling function
                                                 interval_sd_by_hand,
                                                 # Third argument:
                                                 # tell it to operate on our returns
                                                 returns_df = asset_returns_dplyr_byhand[,-1],
                                                 # Fourth argument:
                                                 # supply the weights
                                                 weights = w,
                                                 # Fifth argument:
                                                 # supply the rolling window
                                                 window = 24)
  portfolio_vol_components_tidy_by_hand %>%
    gather(asset, contribution, -date) %>%
    group_by(asset) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = contribution,
                  color = asset)) +
    scale_x_date(breaks =
                   pretty_breaks(n = 8)) +
    scale_y_continuous(labels =
                         function(x) paste0(x, "%"))
}
#*return contribution plot2
#*@param req 
#*@param w  
#*@post /contribuplot
function(req,w){
  weights<-as.vector(as.numeric(w))
  prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
  prices$date<-as.Date(prices[,1])
  prices<-ts(prices)
  asset_returns_dplyr_byhand <- 
    prices %>% 
    to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    # convert the index to a date
    data.frame(date = index(.)) %>%
    # now remove the index because it got converted to row names
    remove_rownames() %>% 
    gather(asset, returns, -date) %>% 
    group_by(asset) %>%  
    mutate(returns = log(returns) ) %>%
    spread(asset, returns) %>% 
    dplyr::select(date, symbols) %>%
    na.omit()
  # First argument:
  # tell map_df to start at date index 1
  # This is the start argument to interval_sd_by_hand()
  # and it is what map() will loop over until we tell
  # it to stop at the date that is 24 months before the
  # last date.
  portfolio_vol_components_tidy_by_hand<- map_df(1:(nrow(asset_returns_dplyr_byhand) -24),
                                                 # Second argument:
                                                 # tell it to apply our rolling function
                                                 interval_sd_by_hand,
                                                 # Third argument:
                                                 # tell it to operate on our returns
                                                 returns_df = asset_returns_dplyr_byhand[,-1],
                                                 # Fourth argument:
                                                 # supply the weights
                                                 weights = w,
                                                 # Fifth argument:
                                                 # supply the rolling window
                                                 window = 24)
  portfolio_vol_components_tidy_by_hand %>%
    gather(asset, contribution, -date) %>%
    group_by(asset) %>%
    ggplot(aes(x = date,
               y = contribution)) +
    geom_area(aes(colour = asset,
                  fill= asset),
              position = 'stack') +
    scale_x_date(breaks =
                   pretty_breaks(n = 8)) +
    scale_y_continuous(labels =
                         function(x) paste0(x, "%"))
}
#*return contributionhighcharter plot 
#*@param req 
#*@param w
#*@post /contrihighplot
function(req,w){
  weights<-as.vector(as.numeric(w))
  prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
  prices$date<-as.Date(prices[,1])
  prices<-ts(prices)
  asset_returns_dplyr_byhand <- 
    prices %>% 
    to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    # convert the index to a date
    data.frame(date = index(.)) %>%
    # now remove the index because it got converted to row names
    remove_rownames() %>% 
    gather(asset, returns, -date) %>% 
    group_by(asset) %>%  
    mutate(returns = log(returns) ) %>%
    spread(asset, returns) %>% 
    dplyr::select(date, symbols) %>%
    na.omit()
  # First argument:
  # tell map_df to start at date index 1
  # This is the start argument to interval_sd_by_hand()
  # and it is what map() will loop over until we tell
  # it to stop at the date that is 24 months before the
  # last date.
  portfolio_vol_components_tidy_by_hand<- map_df(1:(nrow(asset_returns_dplyr_byhand) -24),
                                                 # Second argument:
                                                 # tell it to apply our rolling function
                                                 interval_sd_by_hand,
                                                 # Third argument:
                                                 # tell it to operate on our returns
                                                 returns_df = asset_returns_dplyr_byhand[,-1],
                                                 # Fourth argument:
                                                 # supply the weights
                                                 weights = w,
                                                 # Fifth argument:
                                                 # supply the rolling window
                                                 window = 24)
  
  portfolio_vol_components_tidy_xts <-
    portfolio_vol_components_tidy_by_hand %>%
    tk_xts(date_var = date,
           silent = TRUE)
  highchart(type = "stock") %>%
    hc_title(text = "Volatility Contribution") %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 1],
                  name = symbols[1]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 2],
                  name = symbols[2]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 3],
                  name = symbols[3]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 4],
                  name = symbols[4]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 5],
                  name = symbols[5]) %>%
    hc_yAxis(labels = list(format = "{value}%"),
             max = max(portfolio_vol_components_tidy_xts) + 5,
             min = min(portfolio_vol_components_tidy_xts) - 5,
             opposite = FALSE) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE)
}
#*return contributionbeautyplot
#*@param req 
#*@param w 
#*@post /beautiplot
function(req,w){
  weights<-as.vector(as.numeric(w))
  prices<-read.csv(text=req$postBody,sep=",",header = TRUE)
  prices$date<-as.Date(prices[,1])
  prices<-ts(prices)
  asset_returns_dplyr_byhand <- 
    prices %>% 
    to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
    # convert the index to a date
    data.frame(date = index(.)) %>%
    # now remove the index because it got converted to row names
    remove_rownames() %>% 
    gather(asset, returns, -date) %>% 
    group_by(asset) %>%  
    mutate(returns = log(returns) ) %>%
    spread(asset, returns) %>% 
    dplyr::select(date, symbols) %>%
    na.omit()
  # First argument:
  # tell map_df to start at date index 1
  # This is the start argument to interval_sd_by_hand()
  # and it is what map() will loop over until we tell
  # it to stop at the date that is 24 months before the
  # last date.
  portfolio_vol_components_tidy_by_hand<- map_df(1:(nrow(asset_returns_dplyr_byhand) -24),
                                                 # Second argument:
                                                 # tell it to apply our rolling function
                                                 interval_sd_by_hand,
                                                 # Third argument:
                                                 # tell it to operate on our returns
                                                 returns_df = asset_returns_dplyr_byhand[,-1],
                                                 # Fourth argument:
                                                 # supply the weights
                                                 weights = w,
                                                 # Fifth argument:
                                                 # supply the rolling window
                                                 window = 24)
  
  highchart() %>%
    hc_chart(type = "area") %>%
    hc_title(text = "Volatility Contribution") %>%
    hc_plotOptions(area = list(
      stacking = "percent",
      lineColor = "#ffffff",
      lineWidth = 1,
      marker = list(
        lineWidth = 1,
        lineColor = "#ffffff"
        
      ))
    ) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 1],
                  name = symbols[1]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 2],
                  name = symbols[2]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 3],
                  name = symbols[3]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 4],
                  name = symbols[4]) %>%
    hc_add_series(portfolio_vol_components_tidy_xts[, 5],
                  name = symbols[5]) %>%
    hc_yAxis(labels = list(format = "{value}%"),
             opposite = FALSE) %>%
    hc_xAxis(type = "datetime") %>%
    hc_tooltip(pointFormat =
                 "<span style=\"color:{series.color}\">
{series.name}</span>:<b>{point.percentage:.1f}%</b><br/>",
               shared = TRUE) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE)
}
#*return histogram
#*@param  req dataset of asset
#*@serializer png 
#*@post /histogram
function(req,num){
  #data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  
  hchart(hist(data[,as.numeric(num)],
              breaks = 50,
              plot = FALSE), color = "cornflowerblue") %>%
    hc_title(text =
               paste(colnames(data)[[as.numeric(num)]]
                     ,
                     "Log Returns Distribution",
                     sep = " ")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
}
##########"strest test et etude de sentivité
#* return strest VAR
#* @param req data or object to stress
#* @param alpha Numeric vector, the levels of the stressed VaR.
#* @param q_ratio Numeric vector, the ratio of the stressed VaR to the baseline VaR. If alpha and q_ratio are vectors, they must have the same length.
#*@post /strestestVAR
function(req,alpha,q_ratio){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  q_ratio = as.numeric(q_ratio)
  alpha=as.numeric(alpha)
  
  rev.stress<-stress(type = "VaR", x = data, alpha , q_ratio )
  P<-summary(rev.stress)
  return(P)
}
#* return strest VAR
#* @param req data or object to stress
#* @param alpha Numeric vector, the levels of the stressed VaR.
#* @param q_ratio Numeric vector, the ratio of the stressed VaR to the baseline VaR. If alpha and q_ratio are vectors, they must have the same length.
#* @param s_ratio
#*@post /strestVARES
function(req,alpha,q_ratio,s_ratio){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  q_ratio = as.numeric(q_ratio)
  alpha=as.numeric(alpha)
  s_ratio=as.numeric(s_ratio)
  #data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  res1<-stress(type = "VaR ES", data[,-1],  alpha , q_ratio , s_ratio )
  return(summary(res1))
}
#*return strestest of mean
#*@param req data to strest
#*@post /strestmean
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  # data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  res1 <- stress(type = "mean", data,k=1:ncol(data),new_means = sapply(data,mean))
  return(summary(res1))
}
#* return strest by prob
#*@param req 
#*@param alpha
#*@param upper 
#*@param k 
#*@post /strest_prob 
function(req,alpha,upper,k){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  #data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  #strest_prob(edhec,0.05,-0.006,k=1)
  
  
  return(summary(stress_prob(data, as.numeric(alpha),lower=NULL, as.numeric(upper), as.numeric(k))))
  
}
#* @param req
#* @post /strest_user 
function(req){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  summary(stress(type = "user", x = data, new_weightsfun = function(x)x^2, k = 1))
}
#* return sensitivityVAR
#* @param req data or object to stress
#* @param alpha Numeric vector, the levels of the stressed VaR.
#* @param q_ratio Numeric vector, the ratio of the stressed VaR to the baseline VaR. If alpha and q_ratio are vectors, they must have the same length.
#* @post /sentivityanalysis
function(req,q_ratio,alpha){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  q_ratio = as.numeric(q_ratio)
  alpha=as.numeric(alpha)
  
  rev.stress<-stress(type = "VaR", x = data, alpha , q_ratio )
  P<-rev.stress
  return(sensitivity(P, type = "all"))
}
#* return sensitivityVAR
#* @param req data or object to stress
#* @param alpha Numeric vector, the levels of the stressed VaR.
#* @param q_ratio Numeric vector, the ratio of the stressed VaR to the baseline VaR. If alpha and q_ratio are vectors, they must have the same length.
#* @param num the number of column
#* @serializer png
#* @post /sentivityplot
function(req,q_ratio,alpha,num){
  data <- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  #data<- read.csv(text = req$postBody, header = TRUE,stringsAsFactors = FALSE)
  q_ratio = as.numeric(q_ratio)
  alpha=as.numeric(alpha)
  
  rev.stress<-stress(type = "VaR", x = data, alpha , q_ratio )
  P<-rev.stress
  plot_sensitivity(P ,1:as.numeric(num), "all",type = "all")
}
##############simulation portefeuille et actif
#*return montecarlo simulation 
#*@param req data 
#*@param  N number of simulation
#* @param sims number of replication
#* @param m the returns colunms 
#*@post /montecarlosimulation
function(req,sims,N,m){
  data<- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  starts <-rep(1, as.numeric(sims))
  monte_carlo_sim_51 <-
    map_dfc(starts, simulation_accum_1,
            as.numeric(N) ,
            mean = mean(data[,as.numeric(m)]),
            stdev = sd(data[,as.numeric(m)]))
  return(monte_carlo_sim_51 )
}
  
  
#*return montecarlo modified value
#*@param N number of simulation 
#*@param req data 
#*@param num column number of returns
#*@serializer pdf
#*@post /simulationmontecarlomodified
function(req, N,num){
  dat<- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  data<-dat[,as.numeric(num)]
  tibble(c(data[1], 1 + rnorm(as.numeric(N), mean(data),sd(data)))) %>%
    `colnames<-`("returns") %>%
    mutate(growth1 = accumulate(returns, function(x, y) x * y),
           growth2 = accumulate(returns, `*`),
           growth3 = cumprod(returns)) %>%
    dplyr::select(-returns)
  
  
}
#*return simulation plot 
#*@param sims number of replication 
#*@param N number of simulation 
#*@param m column number for returns
#*@serializer png
#*@post /montecarloplot
function(req,sims,N,m){
  data<- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  monte_carlo_sim_51<-montecarlo(data,as.numeric(sims),as.numeric(N),as.numeric(m))
  monte_carlo_sim_51<-cbind(index(monte_carlo_sim_51),monte_carlo_sim_51)
  colnames(monte_carlo_sim_51)[1]<-"Date"
  monte_carlo_sim_51 %>%
    gather(sim, growth,-Date) %>%
    group_by(sim) %>%
    ggplot(aes(x = Date, y = growth, color = sim))+
    geom_line() +
    theme(legend.position="none")
}
#* return simulation summary
#*@param sims number of replication 
#*@param N number of simulation 
#*@param m column number for returns
#*@post /simulationsummary
 function(req,sims,N,m){
   data<- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
    monte_carlo_sim_51<-montecarlo(data,sims,N,m) 
    monte_carlo_sim_51<-montecarlo(data,as.numeric(sims),as.numeric(N),as.numeric(m))
    monte_carlo_sim_51<-cbind(index(monte_carlo_sim_51),monte_carlo_sim_51)
    colnames(monte_carlo_sim_51)[1]<-"Date"
   sim_summary <-
     monte_carlo_sim_51 %>%
     gather(sim, growth, -Date) %>%
     group_by(sim) %>%
     summarise(final = dplyr::last(growth)) %>%
     summarise(
       max = max(final),
       min = min(final),
       median = median(final))
   return(sim_summary)
 }
#* return simulationplot including indicator
#*@param sims number of replication 
#*@param N number of simulation 
#*@param m column number for returns
#*@post /simulationplot2
function(req,sims,N,m){
  data<- read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  monte_carlo_sim_51<-montecarlo(data,as.numeric(sims),as.numeric(N),as.numeric(m))
  monte_carlo_sim_51<-cbind(Date=data[,0],monte_carlo_sim_51)
    monte_carlo_sim_51<-montecarlo(data,as.numeric(sims),as.numeric(N),as.numeric(m))
    monte_carlo_sim_51 %>%
    gather(sim..., growth, -Date) %>%
    group_by(sim...) %>%
      dplyr::filter(
      dlpyr::last(growth) == sim_summary$max ||
      dplyr::last(growth) == sim_summary$median ||
     dplyr::last(growth) == sim_summary$min) %>%
    ggplot(aes(x = Date, y = growth)) +
    geom_line(aes(color = sim...))
  
}
#* return simulationplot3
#*@param sims number of replication 
#*@param N number of simulation 
#*@param m column number for returns
#*@post /simulationplot3
 function(req,sims,N,m){
  data<- read.csv(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
  
  monte_carlo_sim_51<-montecarlo(data,sims,N,m)
  monte_carlo_sim_51<-montecarlo(data,as.numeric(sims),as.numeric(N),as.numeric(m))
  monte_carlo_sim_51<-cbind(index(monte_carlo_sim_51),monte_carlo_sim_51)
  colnames(monte_carlo_sim_51)[0]<-"Date"
mc_gathered <-
  monte_carlo_sim_51 %>%
  gather(sim, growth, -Date) %>%
  group_by(sim)
hchart(mc_gathered,
       type = 'line',
       hcaes(y = growth,
             x = Date,
             group = sim)) %>%
  hc_title(text = " Simulations") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = " growth"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)
 }
######### portfeuille modèle et creation de portefeuille 
#* create portfolio
#* @param req data
#* @param column number for weight
#*@post /create_portfolio
function(req,n){
  set_returs_long<-read.delim.zoo(text = req$postBody, sep = ",", header = TRUE,tz = "UTC",  format = "%Y-%m-%d")
    colnames(set_returs_long)[as,numeric(n)]="w"
  set_returs_long<-set_returs_long %>%
    dplyr::group_by(asset) %>%
    mutate(weights = case_when(asset == symbols ~ w
    ),weighted_returns<-as.numeric(returns)*as.numeric(weights))
  #asset_returns_long<-as.data.frame(cbind(asset=c("mtn","orange","wave"),symbols=c("mtn","orange","wave"),w=c(0.25,0.5,0.25),returns=c(1000,2000,5000),marketreturn=c(1400,2500,800)))
}
