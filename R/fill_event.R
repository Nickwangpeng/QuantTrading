library(R6)

FillEvent <- R6Class("FillEvent",
   public = list(
     event.type = 'FILL',
     internal.order.id = -1,
     broker.order.id = -1,
     timestamp = NULL,
     full.symbol = '',
     fill.price = 0,
     fill.size = 0,
     exchange = '',
     commission = 0,
     #-------------------- function ToPosition -------------------------#
     ToPosition = function() {
       average.price.including.commission <- 0
       if (self$fill.size > 0) {
         average.price.including.commission <- self$fill.price
            + self$commission / RetrieveMultiplierFromFullSymbol(self$full.symbol)
       }
       else {
         average.price.including.commission <- self$fill.price
            - self$commission / RetrieveMultiplierFromFullSymbol(self$full.symbol)
       }

       new.position <- Position$new(self$full.symbol, average.price.including.commission, self$fill.size)
       return(new.position)
     }
   )
)
