read.mtg <- function(file, ...) {
  # ARGUMENTS :
  # - file : character. Path of the mtg file containing the data.
  # VALUE :
  # Object of class 'foldermtg': list of the following data frames:
  # - classes, description, feature: the tables 'CLASSES:',
  #   'DESCRIPTION:' and 'FEATURE:' (headers of the MTG file)
  # - a data frame per vertex class: values of the features
  
  
  # Initialisation of the list which will contain the data frames of the foldermtg
  listdf <- list()

  # Reading the MTG file
  # - 'blank.lines.skip = FALSE': not omitting empty rows
  # - 'as.is' = FALSE: character columns are not changed into factors
  mtgcomplete <- read.table(file, blank.lines.skip = FALSE,
      as.is = FALSE)

  # Controls: are there the four tables of a MTG file?
  if (!any(mtgcomplete[, 1] == "CLASSES:"))
    stop(paste(file, "should contain a line 'CLASSES:'"))
  if (!any(mtgcomplete[, 1] == "DESCRIPTION:"))
    stop(paste(file, "should contain a line 'DESCRIPTION:'"))
  if (!any(mtgcomplete[, 1] == "FEATURES:"))
    stop(paste(file, "should contain a line 'FEATURES:'"))
  if (!any(mtgcomplete[, 1] == "MTG:"))
    stop(paste(file, "should contain a line 'MTG:'"))

  # Determination of the first row of the tables 'CLASSES:', 'DESCRIPTION:',
  # 'FEATURES:' and 'MTG:'
  beginclasses <- which(mtgcomplete[, 1] == "CLASSES:") + 1
  begindescription <- which(mtgcomplete[, 1] == "DESCRIPTION:") + 1
  beginfeatures <- which(mtgcomplete[, 1] == "FEATURES:") + 1
  beginmtg <- which(mtgcomplete[, 1] == "MTG:") + 1

  # Determination of the last row of the tables 'CLASSES:', 'DESCRIPTION:'
  # and 'FEATURES:'
  endclasses <- begindescription - 2
  enddescription <- beginfeatures - 2
  endfeatures <- beginmtg - 2


  # Reading the 'CLASSES:' table
  classes1 <- read.table(file, sep = "\t",
      nrows = endclasses - beginclasses, skip = beginclasses - 1,
      header = TRUE, blank.lines.skip = TRUE, strip.white = TRUE)

  # Deleting NA columns
  colnotna <- which(!apply(apply(classes1, 2, is.na), 2, any))
  classes <- classes1[, colnotna]

  listdf$classes <- classes

  # Reading the 'DESCRIPTION:' table
  description1 <- read.table(file, sep = "\t",
      nrows = enddescription - begindescription, skip = begindescription - 1,
      header = TRUE, blank.lines.skip = TRUE, strip.white = TRUE)

  # Deleting NA columns
  colnotna <- which(!apply(apply(description1, 2, is.na), 2, any))
  description <- description1[, colnotna]

  listdf$description <- description

  # Reading the 'FEATURES:' table
  if (endfeatures > beginfeatures) {
    # If "FEATURES:" table is not empty:
    features1 <- read.table(file, sep = "\t",
        nrows = endfeatures - beginfeatures, skip = beginfeatures - 1,
        header = TRUE, blank.lines.skip = FALSE, strip.white = FALSE)[, 1:2]
    # If features1 only contains NA: features1 = data frame with 0 row.
    if (!any(!is.na(features1)))
      features1 <- data.frame(NAME = character(0), TYPE = character(0))
  } else {
    # If "FEATURES:" table is empty: features1 = data frame with 0 row.
    features1 <- data.frame(NAME = character(0), TYPE = character(0))
  }

  # Deleting NA columns
  if (nrow(features1) > 0) {
    colnotna <- which(!apply(apply(features1, 2, is.na), 2, any))
    features <- features1[, colnotna]
  } else
    features <- features1

  listdf$features <- features

  # Reading the 'MTG:' table
  mtg1 <- read.table(file, sep = "\t", skip = beginmtg - 1, stringsAsFactors = TRUE,
      header = TRUE, blank.lines.skip = TRUE, fill = TRUE, strip.white = FALSE)
  
  # Deleting NA columns
  colnotna <- which(apply(!apply(mtg1, 2, is.na), 2, any))
  mtg <- mtg1[, colnotna]

  # Selecting columns of mtg containing the symbols of the vertices (topology)
  if (nrow(features) == 0) {
    coltopo <- 1:ncol(mtg)
    } else{
    coltopo <- 1:(min(which(colnames(mtg) %in% features$NAME)) - 1)
    }
  colnames(mtg)[coltopo] <- paste0("order", coltopo)
  
  # Do the feature column names of the data frame mtg match with
  # features$NAME? (if not, there is a warning)
  colmtg.feat <- (colnames(mtg)[-coltopo] %in% as.character(features$NAME))
  if (any(!colmtg.feat))
    warning("Some of the column names of 'mtg' data frame do not match with features$NAME, because of spaces/special characters\n\t(such caracters are changed into dots during the import from the MTG file)")
  
  
  #-----------------------------------------------------------------
  # Building the data frames containing the topology and tbe coordinates
  #-----------------------------------------------------------------
  
  # Identifiers of vertices ("v01", "v02", ...)
  n <- nrow(mtg)
  Id <- 1:n
  n0 <- nchar(n) - 1
  Id <- paste0("v",
    sapply(sapply(nchar(Id) - 1, function(y){rep("0", n0-y)}), paste, collapse = ""),
    Id)
    
  rownames(mtg) <- Id
  
  # Create the data frame containing the plant topology
  entity <- mtg[, substring(colnames(mtg), 1, 5) == "order"]
  vectentity <- apply(entity, 1, paste, collapse = "")
  rownames(entity) <- Id
  entity <- data.frame(entity, vertex = vectentity, stringsAsFactors = FALSE)
  norder <- ncol(entity)-1
  
  listdf$topology <- entity
  
  # The data frame containing the vertex coordinates (if they exist)
  if (any(c("XX", "YY", "ZZ", "AA", "BB", "CC") %in% colnames(mtg))) {
    jcoord <- which(colnames(mtg) %in% c("XX", "YY", "ZZ", "AA", "BB", "CC"))
  } else {
    jcoord <- numeric(0)
  }
  if (length(jcoord) > 0)
    coordinates <- mtg[, jcoord] else
    coordinates <- data.frame()
    
  listdf$coordinates <- coordinates
  
  
  #-----------------------------------------------------------------
  # Building the data frames containing the features per class
  #-----------------------------------------------------------------
  
  # Check if the types of the columns of mtg data frame (factor/numeric/integer)
  # match with features$TYPE (STRING/REAL/INT)
  feat.in.mtg <- features[features$NAME %in% colnames(mtg), ]
  feat.string <- as.character(feat.in.mtg[feat.in.mtg$TYPE == "STRING", "NAME"])
  warn.string <- which(!apply(mtg[feat.string], 2, is.character))
  if (length(warn.string) > 0)
    warning(paste0("Features: ", paste(warn.string, collapse = ", "), " should be STRING\n but the corresponding columns in the MTG table are not 'character'"))
  feat.num <- as.character(feat.in.mtg[feat.in.mtg$TYPE == "REAL", "NAME"])
  warn.num <- which(!apply(mtg[feat.num], 2, is.numeric))
  if (length(warn.num) > 0)
    warning(paste0("Features: ", paste(warn.num, collapse = ", "), " should be REAL\n but the corresponding columns in the MTG table are not 'numeric'"))
  feat.int <- as.character(feat.in.mtg[feat.in.mtg$TYPE == "INT", "NAME"])
  warn.int <- which(!apply(mtg[feat.int], 2, is.integer))
  if (length(warn.int) > 0)
    warning(paste0("Features: ", paste(warn.int, collapse = ", "), " should be INT\n but the corresponding columns in the MTG table are not 'character'"))

  # Suppression of spaces in character columns of mtg
  supprespace <- function(x) {
    a <- unlist(strsplit(x," "))
    paste(a, collapse="")
  }
  for (feat in feat.string)
    mtg[, feat] <- as.factor(sapply(as.character(mtg[, feat]), supprespace))
  
  # Change all "" into NA
  mtg[mtg == ""] <- NA
  
  vclass <- as.character(classes[classes$SCALE > 0, "SYMBOL"])
  class.miss <- character()
  for (sym in vclass) {
    # Builing the data frame dfsym corresponding to the sym class
    i.sc <- grep(sym, entity$vertex)
    dfsym <- mtg[i.sc, ]
    
    if (nrow(dfsym) == 0) {
      class.miss <- c(class.miss, sym)
#      listdf[[sym]] <- data.frame()
    } else {
      # jomit: NA column indices to be deleted
      if (nrow(dfsym) == 1) {
        jomit <- which(apply(dfsym, 2, is.na) == 1)
      } else
        jomit <- which(apply(apply(dfsym, 2, is.na), 2, min) == 1)
      
      # Appending dfsym data frame to the list of data frames
      jomit <- unique(c(1:norder, jcoord, jomit))
      listdf[[sym]] <- dfsym[-jomit]
    }
    
    # REMARQUE : S'IL N'Y A PAS DE FEATURES NI DE COORDONNEES,
    #            listdf[[sym]] EST UN DATA FRAME DE 0 COLONNES,
    #            (ET AUTANT DE LIGNES QU'IL Y A D'ENTITES DE CETTE CLASSE)
    #            --> EST-CE ENNUYEUX ?
  }
  
  # If some of the classes in classes$SYMBOL are not present in mtg:
  # these classes are removed from classes data.frame:
  if (length(class.miss) > 0) {
    warning(paste0("The class(es) ", paste(class.miss, collapse = ", "), " are absent from the plant topology.\nThey are removed in classes data frame."))
    listdf$classes <- listdf$classes[!(as.character(listdf$classes$SYMBOL) %in% class.miss), ]
  }

  class(listdf) <- "foldermtg"
  
  return(listdf)
}
