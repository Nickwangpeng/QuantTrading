library(R6)

StrategyBase <- R6Class("StrategyBase",
  public = list(
     symbols = NULL,
     events.engine = NULL,
     name = '',
     author = '',
     initialized = FALSE,
     #------------------- constructor -----------------------#
     initialize = function(symbols) {
       self$symbols <- symbols
     },
     #---------------- function OnStart --------------#
     OnStart = function() {
       self$initialized <- TRUE
     },
     #-------------- function OnStop -----------------#
     OnStop = function() {
       self$initialized <- FALSE
     },
     #-------------- function OnTick -----------------#
     OnTick = function(tick.event) {
     },
     #-------------- function OnStop -----------------#
     OnBar = function(bar.event) {
     },
     #-------------- function OnOrder -----------------#
     OnOrder = function() {
     },
     #-------------- function OnCancel -----------------#
     OnCancel = function() {
     },
     #-------------- function OnFill -----------------#
     OnFill = function() {
     },
     #-------------- function Reset -----------------#
     Reset = function() {
     },
     #-------------- function PlaceOrder -----------------#
     PlaceOrder = function(order.event) {
       self$events.engine$Put(order.event)
     },
     #-------------- function CancelOrder -----------------#
     CancelOrder = function(fill.event) {
     },
     #-------------------- function SetEventEngine -------------------------#
     SetEventsEngine = function(events.engine) {
       self$events.engine <- events.engine
     }
  )
)
