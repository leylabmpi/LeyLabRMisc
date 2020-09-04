#' convert to numeric while avoiding factor conversion issues
#'
#' @param x an interable
#' @return a numeric object
as.Num = function(x){
  as.numeric(as.character(x))
}

#' rowMeans that works inside a dplyr::mutate() call
row_means = function(..., na.rm=TRUE){
  rowMeans(cbind(...), na.rm=na.rm)
}

#' rowSums that works inside a dplyr::mutate() call
row_sums = function(..., na.rm=TRUE){
  rowSums(cbind(...), na.rm=na.rm)
}

#' A simple dataframe summary
#'
#' @param df dataframe object
#' @param n Number of lines to print
#' @return a dataframe object
dfhead = function(df, n=3){
  print(dim(df))
  head(df, n=n)
}

#' Changing number of rows/columns shown when printing a data frame
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#'
#' @param nrows number of rows to print
#' @param ncols number of columns to print
#' @return NULL
df.dims = function(nrows=4, ncols=20){
  options(repr.matrix.max.rows=nrows, repr.matrix.max.cols=ncols)
}

#' Summary for numeric vectors that includes sd and stderr
#'
#' sd = standard deviation
#' stderr = standard error of the mean (sd(x) / sqrt(length(x)))
#'
#' @param x a numeric vector
#' @param label row name label for the output. If NULL, then the label will be the input object label.
#' @param sel_col If "x" is data.table or data.frame, which column to assess?
#' @param rnd number of digits to round sd and stderr to
#' @return a matrix
summary_x = function(x, label=NULL, sel_col=NULL, rnd=3){
  if(is.null(label)){
    label = deparse(substitute(x))
  }
  if(any(c('tidytable', 'data.table') %in% class(x))){
    tryCatch({
      sel_col = ggplot2::enexpr(sel_col)
    })
    if(is.null(sel_col)){
      stop('sel_col cannot be NULL for data.table objects')
    }
    x = tidytable::dt_pull(x, !!sel_col)
  } else if(any(c('data.frame') %in% class(x))){
    tryCatch({
      sel_col = dplyr::enquo(sel_col)
    })
    if(is.null(sel_col)){
      stop('sel_col cannot be NULL for data.frame objects')
    }
    x = dplyr::pull(x, !!sel_col)
  }
  x = as.matrix(summary(x))
  sd_x = sd(x)
  y = matrix(round(sd_x, rnd), dimnames=list('sd', 'V1'))
  z = matrix(round(sd_x / sqrt(length(x)), rnd), dimnames=list('sd_err_of_mean', 'V1'))
  x = t(rbind(rbind(x,y), z))
  row.names(x) = label
  return(x)
}

#' Pretty print number of unique elements in a vector
#'
#' The result will be cat'ed to the screen.
#' tidytable compatable. Maje
#'
#' @param x a vector or data.table. If data.table, sel_col must not be NULL
#' @param label what to call the items in the vector (eg., "samples")
#' @param sel_col If x is data.table or data.frame, which column to assess?
#' @param ret Return the unique values?
#' @returns NULL
unique_n = function(x, label='items', sel_col=NULL, ret=FALSE){
  if(any(c('tidytable', 'data.table') %in% class(x))){
    tryCatch({
      sel_col = ggplot2::enexpr(sel_col)
    })
    if(is.null(sel_col)){
      stop('sel_col cannot be NULL for data.table objects')
    }
    x = tidytable::dt_distinct(x, !!sel_col)
    x = tidytable::dt_pull(x, !!sel_col)
  } else if(any(c('data.frame') %in% class(x))){
    tryCatch({
      sel_col = dplyr::enquo(sel_col)
    })
    if(is.null(sel_col)){
      stop('sel_col cannot be NULL for data.frame objects')
    }
    x = dplyr::pull(x, !!sel_col)
  }
  cat(sprintf('No. of unique %s:', label),
      length(base::unique(x)), '\n')
  if(ret==TRUE){
    return(base::unique(x))
  }
}

#' Determine counts of setdiff, intersect, & union of 2 vectors (or data.tables)
#'
#' The output is printed text of intersect, each-way setdiff, and union.
#' Data.table compatible! Just make sure to provide sel_col_x and/or sel_col_y
#'
#' @param x vector1 or data.table. If data.table, sel_col_x must not be NULL
#' @param y vector2 or data.table. If data.table, sel_col_y must not be NULL
#' @param sel_col_x If x = data.table, which column to assess?
#' @param sel_col_y If y = data.table, which column to assess?
#' @param to_return (depreciated) "counts" = print overlap counts; "diff_x-or-y" = return setdiff; "diff_fuzzy" = return closest matches for those that differ (ordered best to worst)
#' @param diff Alternative to "to_return". "x" or "y" = return setdiff; "int" = intersect, "union" = union
#' @return NULL
#'
overlap = function (x, y, sel_col_x = NULL, sel_col_y = NULL,
                    to_return = c("counts", "diff_x", "diff_y", "diff_fuzzy"),
                    diff = c(NA, "x", "y", "int", "union", "fuzzy")){
  if (any(c("tidytable", "data.table") %in% class(x))) {
    require(tidytable)
    tryCatch({
      sel_col_x = ggplot2::enexpr(sel_col_x)
    })
    if (is.null(sel_col_x)) {
      stop("sel_col_x cannot be NULL for data.table objects")
    }
    x = tidytable::dt_distinct(x, !!sel_col_x)
    x = tidytable::dt_pull(x, !!sel_col_x)
  }
  if (any(c("tidytable", "data.table") %in% class(y))) {
    require(tidytable)
    tryCatch({
      sel_col_y = ggplot2::enexpr(sel_col_y)
    })
    if (is.null(sel_col_y)) {
      stop("sel_col_y cannot be NULL for data.table objects")
    }
    y = tidytable::dt_distinct(y, !!sel_col_y)
    y = tidytable::dt_pull(y, !!sel_col_y)
  }
  if (any(c("data.frame") %in% class(x))) {
    tryCatch({
      sel_col_x = dplyr::enquo(sel_col_x)
    })
    if (is.null(sel_col_x)) {
      stop("sel_col_x cannot be NULL for data.frame objects")
    }
    x = dplyr::pull(x, !!sel_col_x)
  }
  if (any(c("data.frame") %in% class(y))) {
    tryCatch({
      sel_col_y = dplyr::enquo(sel_col_y)
    })
    if (is.null(sel_col_y)) {
      stop("sel_col_y cannot be NULL for data.frame objects")
    }
    y = dplyr::pull(y, !!sel_col_y)
  }
  if (to_return[1] == "counts" & is.na(diff[1])) {
    cat("intersect(x,y):", length(base::intersect(x, y)),
        "\n")
    cat("setdiff(x,y):", length(base::setdiff(x, y)), "\n")
    cat("setdiff(y,x):", length(base::setdiff(y, x)), "\n")
    cat("union(x,y):", length(base::union(x, y)), "\n")
  }
  else if (to_return[1] == "diff_x" | (!is.na(diff[1]) & diff[1] == "x")) {
    return(setdiff(x, y))
  }
  else if (to_return[1] == "diff_y" | (!is.na(diff[1]) & diff[1] == "y")) {
    return(setdiff(y, x))
  }
  else if (to_return[1] == "diff_fuzzy" | (!is.na(diff[1])& diff[1] == 'fuzzy')) {
    x = base::setdiff(x, y)
    y = base::setdiff(y, x)
    d = cbind(expand.grid(x, y), as.vector(adist(x, y)))
    colnames(d) = c("x", "y", "dist")
    d = d[d$dist != 0, ]
    return(d[order(d$dist), ])
  }
  else if (!is.na(diff[1]) & diff[1] == "int") {
    return(intersect(x, y))
  }
  else if (!is.na(diff[1]) & diff[1] == "union") {
    return(union(x, y))
  }
  else {
    stop("\"to_return\" value not recognized")
  }
}

#' Returns the sizes of R objects
#'
#' @param Robj Vector with the names of R objects as characters
#' @return A list with the name of R objects as names and the formatted size of the objects
size_objects <- function(Robj){
    sapply(Robj,function(x){format(object.size(get(x)), units = 'auto')})
}
