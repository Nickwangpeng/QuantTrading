library(R6)

# TICKTYPE.BID <<- 0
# TICKTYPE.ASK <<- 1
# TICKTYPE.TRADE <<- 2
#
# TICKTYPE <<- c(
#   "BID",
#   "ASK",
#   "TRADE"
# )
#
# GetTickTypeString <- function(i) { TICKTYPE[i+1] }

TickEvent <- R6Class("TickEvent",
   public = list(
     event.type = 'TICK',
     tick.type = 'TRADE',
     full.symbol = NULL,
     timestamp = NULL,
     price = 0.0,
     size = as.integer(0)
   )
)
