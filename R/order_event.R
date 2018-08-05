library(R6)

##-------------- OrderStatus -------------------##
# NONE = -1
# NEWBORN = 0
# PENDING_SUBMIT = 1
# PENDING_CANCEL = 2
# SUBMITTED = 3
# ACKNOWLEDGED = 4
# CANCELED = 5
# FILLED = 6
# PARTIALLY_FILLED = 8

##-------------- OrderType -------------------##
# MARKET = 0
# LIMIT = 2
# STOP = 5
# STOP_LIMIT = 6
# TRAIING_STOP = 7


OrderEvent <- R6Class("OrderEvent",
   public = list(
     event.type = 'ORDER',
     internal.order.id = -1,
     broker.order.id = -1,
     full.symbol = NULL,
     order.type = 'MARKET',
     order.status = 'NONE',
     limit.price = 0,
     stop.price = 0,
     size = 0,
     fill.price = 0,
     fill.size = 0
   )
)
