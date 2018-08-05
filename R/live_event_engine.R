LiveEventEngine <- function() {
  library(tcltk2)
  library(rzmq)

  curLine <- format(Sys.time(), "%H:%M:%OS2")
  #self$myText <<- paste0(self$myText, curLine, "\n")

  #con <- file("temp.txt", blocking=TRUE)
  #writeLines(self$myText, con)
  #close(con)
  e <- globalenv()

  act <- e$queue$remove()
  if (!is.null(act))
  {
    send.raw.string(e$socket1, act)   # 'o|MKT|SPY STK SMART|-100'
  }

  msg <- receive.socket(e$socket1, unserialize=FALSE, dont.wait=TRUE)
  if (!is.null(msg))
  {
    msg <- rawToChar(msg)
    print(msg)
    #v = as.list(strsplit(msg,'|',fixed = TRUE)[[1]])

    tkinsert(e$msgw, "end", paste0(msg, "\n"))
  }

  msg2 <- receive.socket(e$socket2, unserialize=FALSE, dont.wait=TRUE)
  if (!is.null(msg2))
  {
    msg2 <- rawToChar(msg2)
    v = as.list(strsplit(msg2,'|',fixed = TRUE)[[1]])

    if ((v[[3]] == '0') || ((v[[3]] == '2')) || (v[[3]] == '4'))  # bid/ask/last price
    {
      #v2 = as.list(strsplit(v[[1]],' ',fixed = TRUE)[[1]])
      #idx <- match(v2[[1]], e$symbols)
      idx <- match(v[[1]], e$symbols)
      if (! is.na(idx)) {
        .Tcl(paste0("set marketdata(", idx, ",5) \"", v[[4]], "\""))
      }
    }
  }
}
