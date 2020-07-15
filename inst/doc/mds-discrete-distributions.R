## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----loadpkg, message=FALSE---------------------------------------------------
library(dad)

## ----load_data----------------------------------------------------------------
data("dspg")
print(dspg)

## ----mds, results='hide', fig.height=3, fig.width=3.5-------------------------
resultmds <- mdsdd(dspg)

## -----------------------------------------------------------------------------
names(resultmds)

## -----------------------------------------------------------------------------
print(resultmds)

## ----fig.height = 3, fig.width = 4.5------------------------------------------
plot(resultmds, fontsize.points = 1)

## ----fig.height = 4.5, fig.width = 8.8----------------------------------------
interpret(resultmds, nscore = 1)

