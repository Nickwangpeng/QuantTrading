library(R6)

Position <- R6Class("Position",
   public = list(
     full.symbol = '',
     average.price = 0,
     size = 0,
     realized.pnl = 0,
     unrealized.pnl = 0,
     #------------------- constructor -----------------------#
     initialize = function(full.symbol, average.price, size, realized.pnl=0) {
       self$full.symbol <- full.symbol
       self$average.price <- average.price
       self$size <- size
       self$realized.pnl <- realized.pnl
       self$unrealized.pnl <- 0
     },
     #-------------------- function MarktoMarket -------------------------#
     MarktoMarket = function(last.price) {
       self$unrealized.pnl <- (last.price - self$average.price) * self$size *
         RetrieveMultiplierFromFullSymbol(self$full.symbol)
     },
     #-------------------- function OnFill -------------------------#
     OnFill = function(fill.event) {
       if (self$full.symbol != fill.event$full.symbol) {
         print(paste('position fill errror', self$full.symbol, 'and', fill.event$full.symbol, 'do not match'))
       }

       if (self$size > 0) {          # existing long
         if (fill.event$fill.size > 0) {  # long more
           self$average.price <- (self$average.price * self$size + fill.event$fill.price * fill.event$fill.size
                                  + fill.event$commission / RetrieveMultiplierFromFullSymbol(self$full.symbol)) /
                                  (self$size + fill.event$fill.size)
         }
         else { # flat long
           if (abs(self$size) >= abs(fill.event$fill.size)) {  # stay long
             self$realized.pnl <- self$realized.pnl + (self$average.price - fill.event$fill.price) * fill.event$fill.size *
                                  RetrieveMultiplierFromFullSymbol(self$full.symbol) - fill.event$commission
           }
           else {  # flip to short
             self$realized.pnl <- self$realized.pnl + (fill.event$fill.price - self$average.price) * self$size *
                                  RetrieveMultiplierFromFullSymbol(self$full.symbol) - fill.event$commission
             self$average.price <- fill.event$fill.price
           }
         }
       }
       else {  # existing short
         if (fill.event$fill.size < 0) {  # short more
           self$average.price <- (self$average.price * self$size + fill.event$fill.price * fill.event$fill.size +
                                    fill.event$commission / RetrieveMultiplierFromFullSymbol(self$full.symbol)) /
                                    (self$size + fill.event$fill.size)
         }
         else { # flat short
           if (abs(self$size) >= abs(fill.event$fill.size)) {  # stay short
             self$realized.pnl <- self$realized.pnl + (self$average.price - fill.event$fill.price) * fill.event$fill.size *
                                  RetrieveMultiplierFromFullSymbol(self$full.symbol) - fill.event$commission
           }
           else {  # flip to long
             self$realized.pnl <- self$realized.pnl + (fill.event$fill.price - self$average.price) * self$size *
                                  RetrieveMultiplierFromFullSymbol(self$full.symbol) - fill.event$commission
             self$average.price <- fill.event$fill.price
           }
         }
       }

       self$size = self$size + fill.event$fill.size
     }
   )
)
