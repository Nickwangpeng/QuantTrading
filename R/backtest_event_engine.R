library(R6)

BacktestEventEngine <- R6Class("BacktestEventEngine",
  public = list(
     active = TRUE,
     queue = NULL,
     datafeed = NULL,
     handlers = NULL,
     general.handlers = NULL,
     #------------------- constructor -----------------------#
     initialize = function(datafeed) {
       self$queue = Queue$new()
       self$datafeed = datafeed
       self$handlers = list()
       self$general.handlers = list()
     },
     #-------------------- function run -------------------------#
     Run = function() {
       print('Running Backtest ...')
       self$datafeed$Reset()   # reset
       self$active <- TRUE

       while (self$active) {
         event <- self$queue$remove()
         if (is.null(event)) {
           event <- self$datafeed$StreamNext()
           if (is.null(event)) {
             self$active <- FALSE
           }
           else {
             self$queue$add(event)
           }
         }
         else {  # call event handlers
           for (fn in names(self$handlers[[event$event.type]])) {
             self$handlers[[event$event.type]][[fn]](event)
           }
           for (fn in names(self$general.handlers[[event$event.type]])) {
             self$general.handlers[[event$event.type]][[fn]](event)
           }
         }
       }
     },
     #---------------- function put --------------#
     Put = function(event) {
       self$queue$add(event)
     },
     #-------------- function RegisterHandler -----------------#
     RegisterHandler = function(event.type, handler.name, handler) {
       if (is.null(self$handlers[[event.type]])) {
         self$handlers[[event.type]] <- list()
       }
	   
       if (!(handler.name %in% self$handlers[[event.type]])) {
         self$handlers[[event.type]][[handler.name]] <- handler
       }
       else {
         print(paste('handler', handler.name,'is already registered'))
       }
     },
     #-------------- function UnregisterHandler --------------#
     UnregisterHandler = function(event.type, handler.name) {
       self$handlers[[event.type]][[handler.name]] <- NULL
     },
     #-------------- function RegisterGeneralHandler --------------#
     RegisterGeneralHandler = function(handler.name, handler) {

       if (!(handler.name %in% self$general.handlers)) {
         self$handlers[[handler.name]] = handler
       }
       else {
         print(paste('general handler', handler.name,'is already registered'))
       }
     },
     #-------------- function UnregisterGeneralHandler --------------#
     UnregisterGeneralHandler = function(handler.name) {
       self$handlers[[handler.name]] <- NULL
     }
  )
)
