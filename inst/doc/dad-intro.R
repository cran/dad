## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dad)

## -----------------------------------------------------------------------------
data("roses")
rosesf <- as.folder(roses[,c("Sha", "Den", "Sym", "rose")], groups = "rose")
print(rosesf, max = 9)

## -----------------------------------------------------------------------------
data(roseflowers)
df1 <- roseflowers$variety
df2 <- roseflowers$flower
fh1 <- folderh(df1, "rose", df2)
print(fh1)

