library(R6)

BacktestBrokerage <- R6Class("BacktestBrokerage",
  public = list(
    data.board = NULL,
    events.engine = NULL,
    #------------------- constructor -----------------------#
    initialize = function(events.engine, data.board) {
      self$data.board <- data.board
      self$events.engine <- events.engine
    },
    #-------------------- function PlaceOrder -------------------------#
    PlaceOrder = function(order.event) {
      order.event$order.status <- 'FILLED'
      fill <- FillEvent$new()
      fill$internal.order.id <- order.event$internal.order.id
      fill$broker.order.id <- order.event$broker.order.id
      fill$timestamp <- self$data.board$GetLastTimestamp(order.event$full.symbol)
      fill$full.symbol <- order.event$full.symbol
      fill$fill.size <- order.event$size
      fill$fill.price <- self$data.board$GetLastPrice(order.event$full.symbol)
      fill$exchange <- 'BACKTEST'
      #fill$commission <- self$CalculateCommission(fill$full.symbol, fill$fill.price, fill$fill.size)

      self$events.engine$Put(fill)
    },
    #---------------- function CancelOrder --------------#
    CancelOrder = function(order.id) {

    }
  ),
  private = list(
    CalculateCommission = function(full.symbol, fill.price, fill.size) {
      if (length(grep("STK",full.symbol))>0) {
        return( max(0.005*abs(fill.size), 1))
      } else if (length(grep("FUT",full.symbol))>0) {
        return( 2.01*abs(fill_size) )
      } else if(length(grep("OPT",full.symbol))>0) {
        return( max(0.7*abs(fill_size), 1.0) )
      } else if(length(grep("CASH",full.symbol))>0) {
        return( max(0.000002*abs(fill.price*fill.size), 2.0) )
      } else {
        return(0)
      }
    }
  )
)
