library(R6)

PortfolioManager <- R6Class("PortfolioManager",
   public = list(
     cash = 0,
     positions = list(),  # a list of positions, indexed by symbols = names(positions)
     #------------------- constructor -----------------------#
     initialize = function(initial.cash) {
       self$cash <- initial.cash
     },
     #-------------------- function OnPosition -------------------------#
     OnPosition = function(symbol, price, quantity, commission=0) {
	   browser()
       if (!(symbol %in% names(self$positions))) {
         position <- Position$new(symbol, price, quantity)
         self$positions[[symbol]] <- position
       }
       else {
        print(paste(symbol, 'already exists in the portfolio'))
       }
     },
     #-------------------- function OnFill -------------------------#
     OnFill = function(fill.event) {
       self$cash = self$cash - (fill.event$fill.size * fill.event$fill.price + fill.event$commission)

       if (fill.event$full.symbol %in% names(self$positions)) {
         self$positions[[fill.event$full.symbol]]$OnFill(fill.event)
       }
       else {
         self$positions[[fill.event$full.symbol]] <- fill.event$ToPosition()
       }
     },
     #-------------------- function MarktoMarket -------------------------#
     MarktoMarket = function(current.time, symbol, last.price) {
       if (symbol %in% names(self$positions)) {
         self$positions[[symbol]]$MarktoMarket(last.price)
       }
     }
   )
)
