library(R6)
library(Quandl)

BacktestDataFeedQuandl <- R6Class("BacktestDataFeedQuandl",
   public = list(
     data.source = 'quandl',
     start.date = NULL,
     end.date = NULL,
     hist.data = NULL,
     current.cursor = 1,
     #------------------- constructor -----------------------#
     initialize = function(start.date=NULL, end.date=NULL) {
       if (is.null(end.date)) {
         self$end.date = as.character(Sys.Date())
       }
       else {
         self$end.date <- end.date
       }
       if (is.null(start.date)) {
         self$start.date <- as.character(Sys.Date() - 365)
       }
       else {
         self$start.date <- start.date
       }
     },
     #---------------- function SubscribeMarketData --------------#
     SubscribeMarketData = function(syms) {
       data <- list()
       N <- length(syms)

       for (n in 1:N) {
         # data[[n]] <- Quandl.datatable('WIKI/PRICES', ticker='C', date='1999-11-19,1999-11-22,1999-11-23')
         data[[n]] <- Quandl(paste('WIKI', syms[n], sep='/'), start_date=self$start.date, end_date=self$end.date)
         data[[n]]$Symbol <- syms[n]
       }
       # data[[1]]
       self$hist.data <- do.call(rbind, data)             # to a single data frame
       self$hist.data <- self$hist.data[order(self$hist.data$Date),]        # sort data frame

       # reset current index
       self$current.cursor <- 1
     },
     #---------------- function Reset --------------#
     Reset = function() {
       self$current.cursor <- 1
     },
     #---------------- function StreamNext --------------#
     StreamNext = function() {
       if (self$current.cursor <= dim(self$hist.data)[1]) {
         bar <- BarEvent$new()
         bar$start.time <- self$hist.data[self$current.cursor,]$Date
         bar$full.symbol <- self$hist.data[self$current.cursor,]$Symbol
         bar$open.price <- self$hist.data[self$current.cursor,]$Open
         bar$high.price <- self$hist.data[self$current.cursor,]$High
         bar$low.price <- self$hist.data[self$current.cursor,]$Low
         bar$close.price <- self$hist.data[self$current.cursor,]$Close
         bar$volume <- self$hist.data[self$current.cursor,]$Volume
         bar$adj.close.price <- self$hist.data[self$current.cursor,]$'Adj. Close'

         self$current.cursor = self$current.cursor+1
         return(bar)
       } else {
         return(NULL)
       }
     }
   ),
   private = list(
     #---------------- function RetrieveOnlineHistoricalData --------------#
     RetrieveOnlineHistoricalData = function(syms) {

     },
     #---------------- function RetrieveLocalHistoricalData --------------#
     RetrieveLocalHistoricalData = function(syms) {

     }
   )
)
