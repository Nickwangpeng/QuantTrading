library(R6)

# EVENTTYPE.TICK <<- 0
# EVENTTYPE.BAR <<- 1
# EVENTTYPE.ORDER <<- 2
# EVENTTYPE.FILL <<- 3
# EVENTTYPE.CANCEL <<- 4
# EVENTTYPE.ACCOUNT <<- 5
# EVENTTYPE.POSITION <<- 6
# EVENTTYPE.TIMER <<- 7
# EVENTTYPE.GENERAL <<- 8
#
# EVENTTYPE <<- c(
#   "TICK",
#   "BAR",
#   "ORDER",
#   "FILL",
#   "CANCEL",
#   "ACCOUNT",
#   "POSITION",
#   "TIMER",
#   "GENERAL"
# )

# GetEventTypeString <- function(i) { EVENTTYPE[i+1] }

GeneralEvent <- R6Class("GeneralEvent",
    public = list(
      event.type = 'GENERAL',
      time = NULL,
      content = NULL
    )
)
