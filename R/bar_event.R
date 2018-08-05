library(R6)

BarEvent <- R6Class("BarEvent",
    public = list(
      event.type = 'BAR',
      start.time = NULL,
      interval = as.integer(86400),
      full.symbol = NULL,
      open.price = 0,
      high.price = 0,
      low.price = 0,
      close.price = 0,
      adj.close.price = 0,
      volume = as.integer(0),
    #-------------------- function BarEndTime -------------------------#
    BarEndTime = function() {
      return(self$start.time)
    }
  )
)
