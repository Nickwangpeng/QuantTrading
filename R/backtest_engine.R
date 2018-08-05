library(R6)
library(yaml)

BacktestEngine <- R6Class("BacktestEngine",
  public = list(
     current.time = as.Date('1900-01-01'),
     initial.cash = 0,
     symbols = list(),
     benchmark = '',
     start.date = NULL,
     end.date = NULL,
     strategy.name = '',
     output.dir = '',
     hist.dir = '',

     strategy = NULL,
     datafeed = NULL,
     events.engine = NULL,
     data.board = NULL,
     backtest.brokerage = NULL,
     portfolio.manager = NULL,
     performance.manager = NULL,
     risk.manager = NULL,
     #------------------- constructor -----------------------#
     initialize = function(strategy) {
       library(yaml)
       config.yaml <- yaml.load_file("config_backtest.yaml")
       self$initial.cash <- config.yaml$cash
       self$symbols <- config.yaml$tickers
       self$start.date <- as.Date(config.yaml$start_date)
       self$end.date <- as.Date(config.yaml$end_date)
       self$output.dir <- config.yaml$output_dir
       self$hist.dir <- config.yaml$hist_dir
       datasource <- config.yaml$datasource

       ## 1. data feed
       if (datasource == 'local') {
         self$datafeed <- BacktestDataFeedLocal$new(self$hist.dir, self$start.date, self$end.date)
       }
       else {
         self$datafeed <- BacktestDataFeedQuandl$new(self$start.date, self$end.date)
       }
       self$datafeed$SubscribeMarketData(self$symbols)

       ## 2. event engine
       self$events.engine <- BacktestEventEngine$new(self$datafeed)

       ## 3. brokerage
       self$data.board <- DataBoard$new()
       self$backtest.brokerage <- BacktestBrokerage$new(self$events.engine, self$data.board)

       ## 4. portfolio manager
       self$portfolio.manager <- PortfolioManager$new(self$initial.cash)

       ## 5. performance manager
       self$performance.manager <- PerformanceManager$new(self$symbols)

       ## 6. risk manager
       self$risk.manager <- RiskManager$new()

       ## 7. load strategy
       self$strategy <- strategy
       self$strategy$SetEventsEngine(self$events.engine)

       ## 8. trade recorder

       ## 9. wire up event handlers
       self$events.engine$RegisterHandler('TICK','BacktestEngine.TickEventHandler', self$TickEventHandler)
       self$events.engine$RegisterHandler('BAR','BacktestEngine.BarEventHandler', self$BarEventHandler)
       self$events.engine$RegisterHandler('ORDER','BacktestEngine.OrderEventHandler', self$OrderEventHandler)
       self$events.engine$RegisterHandler('FILL','BacktestEngine.FillEventHandler', self$FillEventHandler)
     },
     #---------------- function TickEventHandler --------------#
     TickEventHandler = function(tick.event) {
       self$current.time <- tick.event$timestamp

       self$performance.manager$UpdatePerformance(self$current.time, self$portfolio.manager, self$data.board)
       self$portfolio.manager$MarktoMarket(self$current.time, tick.event$full.symbol, tick.event$price)
       self$data.board$OnTick(tick.event)
       self$strategy$OnTick(tick.event)
     },
     #-------------- function BarEventHandler -----------------#
     BarEventHandler = function(bar.event) {
       #print(paste('bar event', bar.event$full.symbol, bar.event$start.time, bar.event$adj.close.price))
       self$current.time <- bar.event$BarEndTime()

       self$performance.manager$UpdatePerformance(self$current.time, self$portfolio.manager, self$data.board)
       self$portfolio.manager$MarktoMarket(self$current.time, bar.event$full.symbol, bar.event$adj.close.price)
       self$data.board$OnBar(bar.event)
       self$strategy$OnBar(bar.event)
     },
     #-------------- function OrderEventHandler -----------------#
     OrderEventHandler = function(order.event) {
       self$backtest.brokerage$PlaceOrder(order.event)
     },
     #-------------- function FillEventHandler -----------------#
     FillEventHandler = function(fill.event) {
       self$portfolio.manager$OnFill(fill.event)
       self$performance.manager$OnFill(fill.event)
     },
     #-------------------- function run -------------------------#
     Run = function() {
       # reset
       self$performance.manager$Reset()
       self$data.board$symbols = list()
       self$portfolio.manager$cash <- self$initial.cash
       self$portfolio.manager$positions <- list()
       self$strategy$Reset()

       self$events.engine$Run()

       self$performance.manager$UpdateFinalPerformance(self$current.time, self$portfolio.manager, self$data.board)
       self$performance.manager$SaveResults(self$output.dir)
       self$performance.manager$CreateTearsheet()
     }
  )
)
