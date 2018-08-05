.onLoad <- function(libname = find.package("EliteQuantR"), pkgname = "EliteQuantR") {
  library(R6)
  library(Quandl)
  library(yaml)

  InitEliteQuantR()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('EliteQuantR -- unified R backtest and live trading system\nWebsite: http://www.elitequant.com')
}

InitEliteQuantR <- function() {
  Quandl.api_key("ay68s2CUzKbVuy8GAqxj")
}
