plotframes <-
  function(x, y, xlab = NULL, ylab = NULL, font.size=12, layout = NULL)
  { 
    # x:          data frame of the variables which is displayed on the
    #             abscissa.
    # y:          data frame of the variables which are displayed on the
    #             ordinate.
    # font.size:  size of the characters in the strips of the graphs.
    # layout:     numeric vector of length 2 or 3 defining the layout of the
    #             graphical device, as in "xyplot" (package "lattice"). It is
    #             either c(i, j) or c(i, j, k):
    #               - i: number of lines
    #               - j: number of columns
    #               - k: number of pages (if omitted, it is set to as many as is
    #                    required to plot all the panels, see "xyplot")
    #             If omitted: layout=c(3, 3)
    
    # Convert x and y into data frames (if they are vectors or matrices)
    x <- as.data.frame(x, stringsAsFactors = TRUE)
    y <- as.data.frame(y, stringsAsFactors = TRUE)
    
    # Correlations between variables
    correl <- cor(x, y)
    
    # Build a 5-column-data frame:
    #   1) factor: groups (levels of the factor)
    #   2) numeric: stacked x values (variables on the abscissa)
    #   3) factor: names of x variables
    #   4) numeric: stacked y values (variables on the ordinate)
    #   5) factor: names of y variables
    xx <- stack(x)
    xx <- data.frame(rep(rownames(x), ncol(x)), xx, stringsAsFactors = TRUE)
    names(xx) <- c("groupe", "xv", "varx")
    yy <- stack(y)
    yy <- data.frame(rep(rownames(y), ncol(y)), yy, stringsAsFactors = TRUE)
    names(yy) <- c("groupe", "yv", "vary")
    xy <- merge(xx, yy)
    xy$groupe <- as.character(xy$groupe)
    
    # y variables to plot (ordinates)
    levy <- colnames(y)
    # Number of y variables
    nlevy <- length(levy)
    # Number of graphics windows (columns)
    ngraphy <- ceiling(nlevy/9)
    
    # There are max. 3*3 graphs by graphics window.
    # Then: (ngraphy*ngraphy) graphics windows are opened.
    for (nx in colnames(x)) {
      for (iy in 1:ngraphy) {
        jy <- 9*iy-8
        jy <- jy:min(jy+8, nlevy)
        xy.i <- xy[(xy$varx == nx) & (xy$vary %in% levy[jy]), ]
        levels(xy.i$vary) <- paste0(levels(xy.i$vary), " (r = ", round(correl[nx, levels(xy.i$vary)], 2), ")")
        graph.i <- ggplot(xy.i, aes_q(x = quote(xv), y = quote(yv), label = quote(groupe)))
        graph.i <- graph.i + geom_text(fontface = "bold")
        graph.i <- graph.i + facet_wrap(~vary, scales = c("free"), ncol = 3)
        if (is.null(xlab))
          xl <- nx else
            xl <- xlab
        if (is.null(ylab))
          yl <- "" else
            yl <- ylab
        graph.i <- graph.i + labs(x = xl, y = yl, title = nx)
        if (.Device %in% c("null device", "X11", "windows", "quartz", "RStudioGD")) {
          dev.new()
        }
        print(graph.i)
      }
    }
    
    return(invisible(NULL))
  }
