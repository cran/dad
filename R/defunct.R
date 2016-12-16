# Functions for data manipulation
df2folder <- function(...) .Defunct("as.folder", "dad", "In dad >= 2.0, df2folder is replaced by as.folder")
folder2df <- function(...) .Defunct("as.data.frame.folder", "dad", "In dad >= 2.0, folder2df is replaced by the generic function as.data.frame to which is added a method that accepts an object of class folder as an argument.\nSee help(as.data.frame.folder)")
folderh2df <- function(...) .Defunct("as.data.frame.folderh", "dad", "In dad >= 2.0, folderh2df is replaced by the generic function as.data.frame to which is added a method that accepts an object of class folderh as an argument.\nSee help(as.data.frame.folderh)")
folderh2folder <- function(...) .Defunct("as.folder.folderh", "dad", "In dad >= 2.0, folderh2folder is replaced by as.folder.\nSee help(as.folder.folderh)")

append.df2folderh <- function(...) .Defunct("append2folderh", "dad", "In dad >= 2.0, append.df2folderh is replaced by append2folderh")


# Descriptive statistics on folders
meanf <- function(...) .Defunct("mean.folder", "dad", "In dad >= 2.0, meanf is replaced by the generic function mean to which is added a method that accepts an object of class folder as an argument.\nSee help(mean.folder)")
varf <- function(...) .Defunct("var.folder", "dad", "In dad >= 2.0, varf is replaced by var.folder")
corf <- function(...) .Defunct("cor.folder", "dad", "In dad >= 2.0, corf is replaced by cor.folder")
skewnessf <- function(...) .Defunct("skewness.folder", "dad", "In dad >= 2.0, skewnessf is replaced by skewness.folder")
kurtosisf <- function(...) .Defunct("kurtosis.folder", "dad", "In dad >= 2.0, kurtosisf is replaced by kurtosis.folder")


# L2-inner product between probability densities
l2d.gp <- function(...) .Defunct("l2dpar", "dad", "In dad >= 2.0, l2d.gp is replaced by l2dpar")
l2d.gp.u <- function(...) .Defunct("l2dpar", "dad", "In dad >= 2.0, l2d.gp.u is replaced by l2dpar")
l2d.gs <- function(...) .Defunct("l2d", "dad", "In dad >= 2.0, l2d.gs is replaced by l2d")
l2d.gs.u <- function(...) .Defunct("l2d", "dad", "In dad >= 2.0, l2d.gs.u is replaced by l2d")
l2d.kga <- function(...) .Defunct("l2d", "dad", "In dad >= 2.0, l2d.kga is replaced by l2d")
l2d.kga.u <- function(...) .Defunct("l2d", "dad", "In dad >= 2.0, l2d.kga.u is replaced by l2d")
l2d.kgw <- function(...) .Defunct("l2d", "dad", "In dad >= 2.0, l2d.kga is replaced by l2d")
l2d.kgw.u <- function(...) .Defunct("l2d", "dad", "In dad >= 2.0, l2d.kga.u is replaced by l2d")


# L2-distances between probability densities
dist.l2d.gp <- function(...) .Defunct("distl2dpar", "dad", "In dad >= 2.0, dist.l2d.gp is replaced by distl2dpar")
dist.l2d.gp.u <- function(...) .Defunct("distl2dpar", "dad", "In dad >= 2.0, dist.l2d.gp.u is replaced by distl2dpar")
dist.l2d.gs <- function(...) .Defunct("distl2d", "dad", "In dad >= 2.0, dist.l2d.gs is replaced by distl2d")
dist.l2d.gs.u <- function(...) .Defunct("distl2d", "dad", "In dad >= 2.0, dist.l2d.gs.u is replaced by distl2d")
dist.l2d.kga <- function(...) .Defunct("distl2d", "dad", "In dad >= 2.0, dist.l2d.kga is replaced by distl2d")
dist.l2d.kga.u <- function(...) .Defunct("distl2d", "dad", "In dad >= 2.0, dist.l2d.kga.u is replaced by distl2d")
dist.l2d.kgw <- function(...) .Defunct("distl2d", "dad", "In dad >= 2.0, dist.l2d.kgw is replaced by distl2d")
dist.l2d.kgw.u <- function(...) .Defunct("distl2d", "dad", "In dad >= 2.0, dist.l2d.kgw.u is replaced by distl2d")


# Matrix of L2-inner products
mat.ip.l2d.gp <- function(...) .Defunct("matipl2dpar", "dad", "In dad >= 2.0, mat.ip.l2d.gp is replaced by matipl2dpar")
mat.ip.l2d.gp.u <- function(...) .Defunct("matipl2dpar", "dad", "In dad >= 2.0, mat.ip.l2d.gp.u is replaced by matipl2dpar")
mat.ip.l2d.gs <- function(...) .Defunct("matipl2d", "dad", "In dad >= 2.0, mat.ip.l2d.gs is replaced by matipl2d")
mat.ip.l2d.gs.u <- function(...) .Defunct("matipl2d", "dad", "In dad >= 2.0, mat.ip.l2d.gs.u is replaced by matipl2d")
mat.ip.l2d.kga <- function(...) .Defunct("matipl2d", "dad", "In dad >= 2.0, mat.ip.l2d.kga is replaced by matipl2d")
mat.ip.l2d.kga.u <- function(...) .Defunct("matipl2d", "dad", "In dad >= 2.0, mat.ip.l2d.kga.u is replaced by matipl2d")
mat.ip.l2d.kgw <- function(...) .Defunct("matipl2d", "dad", "In dad >= 2.0, mat.ip.l2d.kgw is replaced by matipl2d")
mat.ip.l2d.kgw.u <- function(...) .Defunct("matipl2d", "dad", "In dad >= 2.0, mat.ip.l2d.kgw.u is replaced by matipl2d")

# L2-distance matrix
mat.dist.l2d.gp <- function(...) .Defunct("matdistl2dpar", "dad", "In dad >= 2.0, mat.dist.l2d.gp is replaced by matdistl2dpar")
mat.dist.l2d.gp.u <- function(...) .Defunct("matdistl2dpar", "dad", "In dad >= 2.0, mat.dist.l2d.gp.u is replaced by matdistl2dpar")
mat.dist.l2d.gs <- function(...) .Defunct("matdistl2d", "dad", "In dad >= 2.0, mat.dist.l2d.gs is replaced by matdistl2d")
mat.dist.l2d.gs.u <- function(...) .Defunct("matdistl2d", "dad", "In dad >= 2.0, mat.dist.l2d.gs.u is replaced by matdistl2d")
mat.dist.l2d.kga <- function(...) .Defunct("matdistl2d", "dad", "In dad >= 2.0, mat.dist.l2d.kga is replaced by matdistl2d")
mat.dist.l2d.kga.u <- function(...) .Defunct("matdistl2d", "dad", "In dad >= 2.0, mat.dist.l2d.kga.u is replaced by matdistl2d")
mat.dist.l2d.kgw <- function(...) .Defunct("matdistl2d", "dad", "In dad >= 2.0, mat.dist.l2d.kgw is replaced by matdistl2d")
mat.dist.l2d.kgw.u <- function(...) .Defunct("matdistl2d", "dad", "In dad >= 2.0, mat.dist.l2d.kgw.u is replaced by matdistl2d")
