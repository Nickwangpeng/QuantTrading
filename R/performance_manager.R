library(R6)
library(xts)
library(PerformanceAnalytics)

PerformanceManager <- R6Class("PerformanceManager",
   public = list(
     symbols = list(),
     realized.pnl = 0,
     unrealized.pnl = 0,
     equity.line = NULL,
     #TODO: add position table and trade table
     #------------------- constructor -----------------------#
     initialize = function(symbols) {
       self$symbols <- symbols
       self$Reset()
     },
     #-------------------- function Reset -------------------------#
     Reset = function() {
       self$realized.pnl <- 0
       self$unrealized.pnl <- 0
       self$equity.line <- xts(NA, Sys.Date())
     },
     #-------------------- function UpdatePerformance -------------------------#
     UpdatePerformance = function(current.time, position.manager, data.board) {
       if (is.na(self$equity.line[1])) {
         index(self$equity.line)[1] <- current.time
         self$equity.line[1] <- 0.0
       }
       else if (current.time != tail(index(self$equity.line),1)) {        # in case of multiple symbols, only update on new date
         equity <- 0.0
         for (pos in position.manager$positions) {
           equity = equity + pos$size * data.board$GetLastPrice(pos$full.symbol)
         }

         self$equity.line[index(last(self$equity.line))] <- equity + position.manager$cash   # update last value
         self$equity.line <- c(self$equity.line, xts(as.double(0), current.time))
       }
     },
     #-------------------- function OnFill -------------------------#
     OnFill = function(fill.event) {
     },
     #-------------------- function UpdateFinalPerformance -------------------------#
     UpdateFinalPerformance = function(current.time, position.manager, data.board) {
       equity <- 0.0

       for (pos in position.manager$positions) {
         equity = equity + pos$size * data.board$GetLastPrice(pos$full.symbol)
       }

       self$equity.line[index(last(self$equity.line))] <- equity + position.manager$cash   # update last value
     },
     #-------------------- function CreateTearsheet -------------------------#
     CreateTearsheet = function() {
       library(PerformanceAnalytics)

       port.rets <- Return.calculate(self$equity.line, method='discrete')
       charts.PerformanceSummary(port.rets)
     },
     #-------------------- function SaveResults -------------------------#
     SaveResults = function(output.dir) {
       write.zoo(self$equity.line, file=paste0(output.dir, '/equity_line.csv'), sep=",")
     }
   )
)
