library(R6)

BacktestDataFeedLocal <- R6Class("BacktestDataFeedLocal",
   public = list(
     local.dir = NULL,
     start.date = NULL,
     end.date = NULL,
     hist.data = NULL,
     current.cursor = 1,
     #------------------- constructor -----------------------#
     initialize = function(local.dir, start.date=NULL, end.date=NULL) {
       self$local.dir <- local.dir
       if (is.null(end.date)) {
         self$end.date <- as.character(Sys.Date())
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
         fn <- paste0(self$local.dir, '/', syms[n], '.csv')
         data[[n]] <- read.csv(file=fn, header=TRUE, sep=',')
         data[[n]][,1] <- as.Date(data[[n]][,1])
         data[[n]] <-subset(data[[n]], (Date >= as.Date(self$start.date)) & (Date <= as.Date(self$end.date)) )
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
         bar$adj.close.price <- self$hist.data[self$current.cursor,]$'Adj.Close'

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
