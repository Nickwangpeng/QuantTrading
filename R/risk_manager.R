library(R6)

RiskManager <- R6Class("RiskManager",
   public = list(
     #-------------------- function OnPosition -------------------------#
     OrderinCompliance = function(original.order) {
       return(original.order)
     }
   )
)
