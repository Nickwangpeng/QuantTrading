library(R6)

DataBoard <- R6Class("DataBoard",
  public = list(
     symbols = list(),   # a list of list, indexed by symbol
     #-------------------- function OnTick -------------------------#
     OnTick = function(tick.event) {
       if (!(tick.event$full.symbol %in% names(self$symbols))) {
         self$symbols[[tick.event$full.symbol]] <- list()
       }
       self$symbols[[tick.event$full.symbol]][['timestamp']] <- tick.event$timestamp
       self$symbols[[tick.event$full.symbol]][['last.price']] <- tick.event$price
     },
     #---------------- function OnBar --------------#
     OnBar = function(bar.event) {
       if (!(bar.event$full.symbol %in% names(self$symbols))) {
         self$symbols[[bar.event$full.symbol]] <- list()
       }
       self$symbols[[bar.event$full.symbol]][['timestamp']] <- bar.event$BarEndTime()
       self$symbols[[bar.event$full.symbol]][['last.price']] <- bar.event$close.price
       self$symbols[[bar.event$full.symbol]][['last.adj.price']] <- bar.event$adj.close.price
     },
     #-------------- function GetLastPrice -----------------#
     GetLastPrice = function(symbol) {
       if (symbol %in% names(self$symbols)) {
         return(self$symbols[[symbol]][['last.adj.price']])
       }
       else {
         browser()
         print(paste('last.adj.price for symbol', symbol, 'not found'))
         return(NULL)
       }
     },
     #-------------- function GetLastTimestamp -----------------#
     GetLastTimestamp = function(symbol) {
       if (symbol %in% names(self$symbols)) {
         return(self$symbols[[symbol]][['timestamp']])
       }
       else {
         browser()
         print(paste('timestamp for symbol', symbol, 'not found'))
         return(NULL)
       }
     }
  )
)
