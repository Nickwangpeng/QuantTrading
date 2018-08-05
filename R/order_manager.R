library(R6)

OrderManager <- R6Class("OrderManager",
   public = list(
     internal.order.id = 0,
     order.dict = NULL,
     #------------------- constructor -----------------------#
     initialize = function() {
     },
     #-------------------- function PlaceOrder -------------------------#
     PlaceOrder = function() {
     }
   )
)
