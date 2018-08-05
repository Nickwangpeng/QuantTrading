# http://www.sciviews.org/recipes/tcltk/TclTk-notebook-widget/
library(R6)

LiveEngine <- function()
{
  library(tcltk2)
  library(rzmq)
  library(yaml)

  e <- globalenv()
  e$main.window <- tktoplevel()
  tktitle(e$main.window) <- "EliteQuant"

  config.yaml <- yaml.load_file("config.yaml")

  #-------------------------------- Menu --------------------------------------------------#
  e$menu <- tk2menu(e$main.window)           # Create a menu
  tkconfigure(e$main.window, menu = e$menu)  # Add it to the 'e$main.window' window
  e$menuFile <- tk2menu(e$menu, tearoff = FALSE)
  tkadd(e$menuFile, "command", label = "Quit",
        command = function() {tkdestroy(e$main.window)})
  tkadd(e$menu, "cascade", label = "File", menu = e$menuFile)
  #------------------------------ End of Menu-----------------------------------------------#

  #-------------------------------- Layout-----------------------------------------------#
  # see color code in r   http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  e$left <- tkframe(e$main.window, width = 100, height = 50, bg = "white")
  e$right <- tkframe(e$main.window, width = 500, height = 50, bg = "white")
  tkpack(e$left, side='left',fill="both",expand=TRUE)
  tkpack(e$right, side='right',fill='both')

  e$top.left <- tkframe(e$left, width = 100, height = 50, bg = 'white')
  e$bottom.left <- tkframe(e$left, width = 100, height = 300, bg = "white")
  tkpack(e$top.left, side='top',fill="both",expand=TRUE)
  tkpack(e$bottom.left, side='bottom',fill='both')

  e$top.right <- tkframe(e$right, width = 500, height = 50, bg = 'white')
  e$bottom.right <- tkframe(e$right, width = 500, height = 300, bg = "white")
  tkpack(e$top.right, side='top',fill="both",expand=TRUE)
  tkpack(e$bottom.right, side='bottom',fill='both')

  #--------------------------------End of layout -----------------------------------------------#


  #-------------------------------- Top Left -----------------------------------------------#
  market.data.name <- 'marketdata'
  market.table.title <- c('Symbol', 'Bid Size', 'Bid', 'Ask', 'Ask Size', 'Last', 'Last Size')
  e$symbols <- config.yaml$tickers
  .Tcl(paste0("set ", market.data.name, "(", 0, ",", 0,") \"", 0, "\""))
  .Tcl(paste0("unset ", market.data.name))

  for (j in (1:length(market.table.title)))
    .Tcl(paste0("set ", market.data.name, "(", 0, ",", j-1,") \"", market.table.title[j], "\""))

  for (i in 1:length(e$symbols)) {
    .Tcl(paste0("set ", market.data.name, "(", i, ",", 0,") \"", e$symbols[i], "\""))
    for (j in (2:length(market.table.title)))
      .Tcl(paste0("set ", market.data.name, "(", i, ",", j-1,") \"0\""))
  }

  # .TkRoot$env$TclVarCount
  #.Tcl("set marketdata(1,1) \"3\"")   # set
  # tclvalue("marketdata(0,0)")        # get
  e$market.table <- tk2table(e$top.left, variable = market.data.name,
                                           rows = length(e$symbols) + 1, cols = length(market.table.title),
                                           titlerows = 1, titlecols = 1,
                                           maxwidth = 1000, maxheight = 1000,
                                           selectmode = "extended", background = "white",
                                           colwidth = 30, drawmode = "fast",
                                           yscrollcommand = function(...) tkset(yscr,...))
  yscr <- tk2scrollbar(e$top.left, orient = "vertical",
                       command = function(...) tkyview(e$market.table, ...))
  tkpack(e$market.table, side='left', fill = "both", expand = TRUE)
  tkpack(yscr, side='right',fill='both')

  #------------------------------ End of Top Left ------------------------------------------#

  #-------------------------------- Top Right -----------------------------------------------#
  e$queue <- Queue$new()
  sym.name <- tclVar("AAPL STK SMART")
  e$symName <-tk2entry(e$top.right, width = "25", textvariable = sym.name)
  tkgrid(tk2label(e$top.right, text = "Symbol:", justify = "left"),
         e$symName,
         padx = 10, pady = c(15, 5), sticky = "w")

  order.type <- tclVar()
  e$orderType <- tk2combobox(e$top.right, textvariable=order.type)
  tkgrid(tk2label(e$top.right, text = "OrderType:", justify = "left"),
         e$orderType,
         padx = 10, pady = c(15, 5), sticky = "w")
  tkconfigure(e$orderType, values=c("MKT LMT"))

  order.flag <- tclVar()
  e$orderFlag <- tk2combobox(e$top.right, textvariable=order.flag)
  tkgrid(tk2label(e$top.right, text = "OrderFLag:", justify = "left"),
         e$orderFlag,
         padx = 10, pady = c(15, 5), sticky = "w")
  tkconfigure(e$orderFlag, values=c("Open Close CloseToday CloseYesterday"))

  order.price <- tclVar("100")
  e$orderPrice <-tk2entry(e$top.right, width = "25", textvariable = order.price)
  tkgrid(tk2label(e$top.right, text = "Price:", justify = "left"),
         e$orderPrice,
         padx = 10, pady = c(15, 5), sticky = "w")

  order.size <- tclVar("100")
  e$orderSize <-tk2entry(e$top.right, width = "25", textvariable = order.size)
  tkgrid(tk2label(e$top.right, text = "Size:", justify = "left"),
         e$orderSize,
         padx = 10, pady = c(15, 5), sticky = "w")

  onSubmit <- function() {
    symm <- tclvalue(sym.name)
    sizee <- tclvalue(order.size)
    otype <- tclvalue(order.type)
    oflag <- tclvalue(order.flag)

    ofstr = "0"
    if (oflag == "Close") {
      ofstr = "1"
    } else if (oflag == "CloseToday") {
      ofstr = '2'
    } else if (oflag == "CloseYesterday") {
      ofstr = '3'
    }

    if (otype == "MKT") {
      msg <- paste0("o|MKT|", symm, '|', sizee, '|', ofstr)
    }
    else {
      pricee <- tclvalue(order.price)
      msg <- paste0("o|LMT|", symm, '|', sizee, '|', pricee, '|', ofstr)
    }

    e$queue$add(msg)
    #tkmessageBox(message = msg)
    print(msg)
  }

  e$butSubmit <-tk2button(e$top.right, text = "SubmitOrder", width = -6, command = onSubmit)
  tkgrid(e$butSubmit, padx = 10, pady = c(5, 15))
  #tkbind(e$order.size, "<Return>", onSubmit)

  cancel.order.id <- tclVar()
  e$cancelOrderId <-tk2entry(e$top.right, width = "25", textvariable = cancel.order.id)
  tkgrid(tk2label(e$top.right, text = "Order ID:", justify = "left"),
         e$cancelOrderId,
         padx = 10, pady = c(15, 5), sticky = "w")

  onCancel <- function() {
    oid <- tclvalue(cancel.order.id)
    msg <- paste0("c|", oid)
    e$queue$add(msg)
    #tkmessageBox(message = msg)
    print(msg)
  }

  e$butCancel <-tk2button(e$top.right, text = "CancelOrder", width = -6, command = onCancel)
  tkgrid(e$butCancel, padx = 10, pady = c(5, 15))
  #tkbind(e$order.size, "<Return>", onOK)

  tkfocus(e$main.window)
  #------------------------------ End of Top Right ------------------------------------------#

  #-------------------------------- bottom left -----------------------------------------------#
  # Create two tabs
  e$nb <- tk2notebook(e$bottom.left, tabs = c("Message", "Trade"), height = 300)
  tkpack(e$nb, fill = "both", expand = TRUE)

  # Populate these tabs with various widgets
  e$tb1 <- tk2notetab(e$nb, "Message")
  e$scrx <- tk2scrollbar(e$tb1, orient = "horizontal",
                                       command = function(...) tkxview(e$msgw, ...))
  e$scry <- tk2scrollbar(e$tb1, orient = "vertical",
                                       command = function(...) tkyview(e$msgw, ...))
  e$msgw <- tk2text(e$tb1, width = 60, height = 300, wrap = "none",
                                 xscrollcommand = function(...) tkset(e$scrx, ...),
                                 yscrollcommand = function(...) tkset(e$scry, ...))
  tkgrid(e$msgw, e$scry, sticky = "nsew")
  tkgrid.rowconfigure(e$tb1, e$msgw, weight = 1)
  tkgrid.columnconfigure(e$tb1, e$msgw, weight = 1)
  tkgrid(e$scrx, sticky = "ew")

  e$tb2 <- tk2notetab(e$nb, "Trade")
  e$but <- tk2button(e$tb2, text = "EXIT",
                                   command = function() tkdestroy(e$main.window))
  # You can use a different manager than for the notebook
  tkgrid(e$but, padx = 50, pady = 30)

  # Select a tab programmatically
  tk2notetab.select(e$nb, "Message")
  tk2notetab.text(e$nb) # Text of the currently selected tab

  #------------------------------ End of bottom left ------------------------------------------#


  #-------------------------------- bottom right -----------------------------------------------#
  e$strategyw <- tk2text(e$bottom.right)
  tkpack(e$strategyw, fill = "both", expand = TRUE)
  # You must insert text before to disable edition!
  tkinsert(e$strategyw, "end", "Reserved for StrategyManager")
  tkconfigure(e$strategyw, state = "disabled")

  #------------------------------ End of bottom right ------------------------------------------#

  #-------------------------------- Socket --------------------------------------------------#
  if (!(exists('socket1', envir = e)))
  {
    print('connect to server1')
    e$context1 = init.context()
    e$socket1 = init.socket(e$context1, 'ZMQ_PAIR')
    connect.socket(e$socket1, 'tcp://localhost:55558')
  }

  if (!(exists('socket2', envir = e)))
  {
    print('connect to server2')
    e$context2 = init.context()
    e$socket2 = init.socket(e$context2, 'ZMQ_SUB')
    connect.socket(e$socket2, 'tcp://localhost:55559')
    subscribe(e$socket2, '')
  }

  #-------------------------------- emd of socket----------------------------------------------#

  #-------------------------------- Event Loop ------------------------------------------#

  # This function will be called every 0.1 sec
  assign('tryText', "hello", envir=e)
  tclTaskSchedule(100,  LiveEventEngine(), id = "eventengine", redo = TRUE)

  exitProg <- function()
  {
    tclTaskDelete(NULL)    # delete all pending tasks
    #tclTaskDelete("eventengine")
    e <- globalenv()
    if (exists('main.window', envir = e))
    {
      #disconnect.socket(e$socket1, 'tcp://localhost:55558')
      rm(main.window, envir=e)
      rm(menu, envir = e)
      rm(menuFile, envir = e)
      rm(but, envir = e)
      rm(bottom.left, envir=e)
      rm(bottom.right, envir=e)
      rm(butSubmit, envir=e)
      rm(butCancel, envir=e)
      rm(left, envir=e)
      rm(market.table, envir=e)
      print("Thanks for using EliteQuantR")
    }
  }

  tkbind(e$main.window,"<Destroy>", exitProg)

  #-------------------------------- End of Event Loop ------------------------------------------#
}
