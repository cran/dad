## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(dad)
data("roses")
x <- roses[, c("Sha", "Den", "Sym", "rose")]
head(x)

## -----------------------------------------------------------------------------
rosesf <- as.folder(x, groups = "rose")
print(rosesf, max = 9)

## -----------------------------------------------------------------------------
library(dad)
data(roseflowers)
df1 <- roseflowers$variety
df2 <- roseflowers$flower

## -----------------------------------------------------------------------------
fh1 <- folderh(df1, "rose", df2)
print(fh1)

