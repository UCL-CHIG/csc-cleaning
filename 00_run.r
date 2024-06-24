
assign(".lib.loc", c(.libPaths(), "path omitted"), envir = environment(.libPaths))

library(RODBC)
library(data.table)

setwd("path omitted")

mode_fun <- function(v) {
  v <- v[!is.na(v)]
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  md <- ux[tab == max(tab)]
  return(md[length(md)]) # last if multimodal
}

source("scripts/01_cin.R")
source("scripts/02_cla.R")

