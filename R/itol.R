#' create itol symbol file
#'
#' https://itol.embl.de/help.cgi#symbols
#'
#' If a data.frame is provided, the row names must exactly match the tree node labels.
#' If a data.table or tibble is provided, the first column values must exactly match the tree node labels.
#' Columns in the table must include: symbol,size,color,fill,position,(label)
#'
#' @param df A data.frame, tibble, or data.table (see above)
#' @param dataset_label What to label the itol dataset
#' @param out_file Name of the output file
#' @param out_dir Where to write the output
#' @param MAXIMUM_SIZE The max size of the symbols
#' @param COLOR Legend color
#' @return NULL
#' @export
#' @examples
#' # data.frame
#' df = data.frame(symbol = c(3,1), size = c(5,10), color = c('#0000ff', '#00ff00'), fill = c(0,1), position = c(0, 0.5), row.names = LETTERS[1:2])
#' itol_symbol(df, 'test_label')
#' # data.table
#' dt = data.table(taxa = LETTERS[1:2], symbol = c(3,1), size = c(5,10), color = c('#0000ff', '#00ff00'), fill = c(0,1), position = c(0, 0.5))
#' itol_symbol(dt, 'test_label')
itol_symbol = function(df, dataset_label, out_file='itol_symbol.txt', out_dir=getwd(),
                       MAXIMUM_SIZE=50, COLOR="#ff0000"){
  # Output
  if(! is.null(out_dir)){
    out_file = file.path(out_dir, out_file)
  }
  # Input format
  if(length(intersect(c('tidytable', 'data.table', 'tbl'), class(df))) > 0){
    df = as.data.frame(df)
    rownames(df) = df[,1]
    df[,1] = NULL
  }
  cols = c('symbol', 'size', 'color', 'fill', 'position')
  if('label' %in% colnames(df)){ cols = c(cols, 'label') }
  df = df[,cols]
  # Main params
  cat('DATASET_SYMBOL\n', file=out_file)
  cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
  cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
  cat(sprintf('COLOR %s\n', COLOR), file=out_file, append=TRUE)
  cat(sprintf('MAXIMUM_SIZE %s\n', MAXIMUM_SIZE), file=out_file, append=TRUE)

  # Data
  cat('DATA\n', file=out_file, append=TRUE)
  write.table(df, file=out_file, append=TRUE, sep=' ',
              quote=FALSE, row.names=TRUE, col.names=FALSE)
  cat('File written:', out_file, '\n')
}

#' create itol multi-bar file
#'
#' https://itol.embl.de/help.cgi#multibar
#'
#' If a data.frame is provided, the row names must exactly match the tree node labels.
#' If a data.table or tibble is provided, the first column values must exactly match the tree node labels.
#' All other columns are assumed to be values for plotting.
#'
#' @param df A data.frame, tibble, or data.table (see above)
#' @param dataset_label What to label the itol dataset
#' @param out_file Name of the output file
#' @param out_dir Where to write the output
#' @param legend A list that includes shapes, colors, and labels (see \code{\link{itol_colorstrip}})
#' @param WIDTH Bar width
#' @param COLOR Legend color
#' @return NULL
#' @export
#' @examples
#' # data.frame
#' df = data.frame(value1 = c(1,2,3), value2 = c(3,2,1), row.names = LETTERS[1:3])
#' itol_multibar(df, 'test_label')
#' # data.table
#' dt = data.table(taxa = LETTERS[1:3], value1 = c(1,2,3), value2 = c(3,2,1))
#' itol_multibar(dt, 'test_label')
itol_multibar = function(df, dataset_label, out_file='itol_multibar.txt', out_dir=getwd(),
                         legend=NULL, WIDTH=200, COLOR="#ff0000"){
  # Output
  if(! is.null(out_dir)){
    out_file = file.path(out_dir, out_file)
  }
  # Input format
  if(length(intersect(c('tidytable', 'data.table', 'tbl'), class(df))) > 0){
    df = as.data.frame(df)
    rownames(df) = df[,1]
    df[,1] = NULL
  }

  # Main params
  cat('DATASET_MULTIBAR\n', file=out_file)
  cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
  cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
  cat(sprintf('COLOR %s\n', COLOR), file=out_file, append=TRUE)
  cat(sprintf('WIDTH %s\n', WIDTH), file=out_file, append=TRUE)

  # Field labels
  labs = gsub(' ', '_', colnames(df))
  labs = sprintf('FIELD_LABELS %s\n', paste(labs, collapse=' '))
  cat(labs, file=out_file, append=TRUE)

  # Field colors
  if(is.null(legend)){
    cols = rainbow(ncol(df))
  } else {
    cols = legend$colors %>% as.character
  }
  cols = sprintf('FIELD_COLORS %s\n', paste(cols, collapse=' '))
  cat(cols, file=out_file, append=TRUE)

  # legend
  cat(sprintf('LEGEND_TITLE %s\n', dataset_label), file=out_file, append=TRUE)
  if(is.null(legend)){
    shapes = rep(1, colnames(df) %>% length)
    cols = rainbow(length(shapes))
    labs = paste(gsub(' ', '_', colnames(df)), collapse=' ')
  } else {
    stopifnot(all(colnames(legend) %in% c('shapes', 'colors', 'labels')))
    shapes = legend$shapes %>% as.character
    cols = legend$colors %>% as.character
    labs = legend$labels %>% as.character
  }
  cat(sprintf('LEGEND_SHAPES %s\n', paste(shapes, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_COLORS %s\n', paste(cols, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_LABELS %s\n', paste(labs, collapse=' ')), file=out_file, append=TRUE)

  # Data
  cat('DATA\n', file=out_file, append=TRUE)
  write.table(df, file=out_file, append=TRUE, sep=' ',
              quote=FALSE, row.names=TRUE, col.names=FALSE)
  cat('File written:', out_file, '\n')
}

#' create itol boxplot file
#'
#' https://itol.embl.de/help.cgi#boxplot
#'
#' If a data.frame is provided, the row names must exactly match the tree node labels.
#' If a data.table or tibble is provided, the first column values must exactly match the tree node labels.
#' Columns in the table must include: minimum,q1,median,q3,maximum,(extreme_value1),(extreme_value2)
#'
#' @param df A data.frame, tibble, or data.table (see above)
#' @param dataset_label What to label the itol dataset
#' @param out_file Name of the output file
#' @param out_dir Where to write the output
#' @param key_color The color for the legend key
#' @param WIDTH Maximum width
#' @return NULL
#' @export
#' @examples
#' # data.frame
#' df = data.frame(minimum = c(1,2,1), q1 = c(2,3,3), median = c(3,5,4), q3 = c(4,6,5), maximum = c(5,8,6), row.names = LETTERS[1:3])
#' itol_boxplot(df, 'test_label')
#' # data.table
#' dt = data.table(taxa = LETTERS[1:3], minimum = c(1,2,1), q1 = c(2,3,3), median = c(3,5,4), q3 = c(4,6,5), maximum = c(5,8,6))
#' itol_boxplot(dt, 'test_label')
itol_boxplot = function(df, dataset_label, out_file='itol_boxplot.txt',
                        out_dir=getwd(), key_color='#ff0000', WIDTH=200){
  # Output
  if(! is.null(out_dir)){
    out_file = file.path(out_dir, out_file)
  }
  # Input format
  if(length(intersect(c('tidytable', 'data.table', 'tbl'), class(df))) > 0){
    df = as.data.frame(df)
    rownames(df) = df[,1]
    df[,1] = NULL
  }
  cols = c('minimum','q1','median','q3','maximum')
  if('extreme_value1' %in% colnames(df)){ cols = c(cols, 'extreme_value1') }
  if('extreme_value2' %in% colnames(df)){ cols = c(cols, 'extreme_value2') }
  df = df[,cols]

  # Main params
  cat('DATASET_BOXPLOT\n', file=out_file)
  cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
  cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
  cat(sprintf('COLOR %s\n', key_color), file=out_file, append=TRUE)
  cat(sprintf('WIDTH %s\n', WIDTH), file=out_file, append=TRUE)

  # data
  cat('DATA\n', file=out_file, append=TRUE)
  write.table(df, file=out_file, append=TRUE, sep=' ',
              quote=FALSE, row.names=TRUE, col.names=FALSE)
  cat('File written:', out_file, '\n')
}

#' create itol heatmap file
#'
#' https://itol.embl.de/help.cgi#heatmap
#'
#' If a data.frame is provided, the row names must exactly match the tree node labels.
#' If a data.table or tibble is provided, the first column values must exactly match the tree node labels.
#' All other columns are assumed to be values for plotting.
#'
#' @param df A data.frame, tibble, or data.table (see above)
#' @param dataset_label What to label the itol dataset
#' @param out_file Name of the output file
#' @param out_dir Where to write the output
#' @param tree Tree object used for ordering the heatmap columns; if NULL, the dist_method will be used to create the tree
#' @param dist_method vegan::vegdist method for creating the correlation dendrogram
#' @param color_scheme Heatmap color scheme. color = blue-orange-yellow; bw=white-grey-black
#' @return NULL
#' @export
#' @examples
#' # data.frame
#' df = data.frame(sample1 = c(1,2,3), sample2 = c(3,2,1), row.names = LETTERS[1:3])
#' itol_heatmap(df, 'test_label')
#' # data.table
#' dt = data.table(taxa = LETTERS[1:3], sample1 = c(1,2,3), sample2 = c(3,2,1))
#' itol_heatmap(dt, 'test_label')
itol_heatmap = function(df, dataset_label, out_file='itol_heatmap.txt',
                        out_dir=getwd(), tree=NULL, dist_method='bray',
                        color_scheme=c('color', 'bw')){
  # Output
  if(! is.null(out_dir)){
    out_file = file.path(out_dir, out_file)
  }

  # Input format
  if(length(intersect(c('tidytable', 'data.table', 'tbl'), class(df))) > 0){
    df = as.data.frame(df)
    rownames(df) = df[,1]
    df[,1] = NULL
  }

  # Main params
  cat('DATASET_HEATMAP\n', file=out_file)
  cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
  cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
  cat('COLOR #ff0000\n', file=out_file, append=TRUE)

  # Field labels
  labs = gsub(' ', '_', colnames(df))
  labs = sprintf('FIELD_LABELS %s\n', paste(labs, collapse=' '))
  cat(labs, file=out_file, append=TRUE)

  ## colors
  cat('COLOR_NAN #eae8e8\n', file=out_file, append=TRUE)
  cat('USE_MID_COLOR 1\n', file=out_file, append=TRUE)
  if(color_scheme[1] == 'color'){
    cat('COLOR_MIN #0000ff\n', file=out_file, append=TRUE)
    cat('COLOR_MAX #ffff00\n', file=out_file, append=TRUE)
    cat('COLOR_MID #ff8000\n', file=out_file, append=TRUE)
  } else
    if(color_scheme[1] == 'bw'){
      cat('COLOR_MIN #ffffff\n', file=out_file, append=TRUE)
      cat('COLOR_MAX #cccccc\n', file=out_file, append=TRUE)
      cat('COLOR_MID #000000\n', file=out_file, append=TRUE)
    } else {
      stop('color_scheme not recoginized')
    }

  # creating correlation tree
  if(is.null(tree) & !is.null(dist_method)){
    tree = df %>% t %>% vegan::vegdist(method=dist_method) %>% hclust %>% ape::as.phylo()
  }
  if(!is.null(tree)){
    cat('FIELD_TREE ', file=out_file, append=TRUE)
    ape::write.tree(tree, file=out_file, append=TRUE)
  }

  # data
  cat('DATA\n', file=out_file, append=TRUE)
  write.table(df, file=out_file, append=TRUE, sep=' ',
              quote=FALSE, col.names=FALSE)
  cat('File written:', out_file, '\n')
}

#' Create itol colorstrip file
#'
#' https://itol.embl.de/help.cgi#strip
#'
#' If a data.frame is provided, the row names must exactly match the tree node labels. The 1st column with be used for plotting.
#' If a data.table or tibble is provided, the first column values must exactly match the tree node labels. The 2nd column will be used for plotting.
#'
#' Custom Legend: requires a data.frame with the number of rows equaling the number of unique
#' values in the legend.
#' \itemize{
#'   \item "shapes" => numeric (see \href{https://itol.embl.de/help.cgi#dsLeg}{the itol docs})
#'   \item "colors" => hexidecimal (see \href{https://www.color-hex.com/}{this website for examples})
#'   \item "labels" => legend labels
#' }
#'
#' @param df A data.frame, tibble, or data.table (see above)
#' @param dataset_label What to label the itol dataset
#' @param out_file Name of the output file
#' @param out_dir Where to write the output
#' @param legend Custom legend (see the function description)
#' @return NULL
#' @export
#' @examples
#' # data.frame input
#' df = data.frame(values = c('x','x','y'), row.names = LETTERS[1:3])
#' itol_colorstrip(df, 'test_label')
#' # data.table
#' dt = data.table(taxa = LETTERS[1:3], values = c('x','x','y'))
#' itol_colorstrip(dt, 'test_label')
#' # creating a custom legend
#' legend = data.frame(unique(iris$Species),
#' colors = c('#00FF00', '#FFCC33', '#FF0000'),
#' shapes = rep(1, length(unique(iris$Species))))
#' print(legend)
itol_colorstrip = function(df, dataset_label, out_file='itol_colorstrip.txt',
                           out_dir=getwd(), legend=NULL){
  # Output
  if(! is.null(out_dir)){
    out_file = file.path(out_dir, out_file)
  }

  # Input format
  if(length(intersect(c('tidytable', 'data.table', 'tbl'), class(df))) > 0){
    df = as.data.frame(df)
    rownames(df) = df[,1]
    df[,1] = NULL
  }
  df = data.frame(tip = rownames(df),
                  feature = as.character(df[,1]))

  # main options
  cat('DATASET_COLORSTRIP\n', file=out_file)
  cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
  cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
  cat('COLOR #ff0000\n', file=out_file, append=TRUE)

  # legend
  cat(sprintf('LEGEND_TITLE %s\n', dataset_label), file=out_file, append=TRUE)
  if(is.null(legend)){
    shapes = rep(1, df[,2] %>% unique %>% length)
    cols = gsub("#[A-Z0-9]{6}FF$", '', rainbow(length(shapes)))
    labs = paste(gsub(' ', '_', df[,2] %>% unique), collapse=' ')
  } else {
    stopifnot(all(colnames(legend) %in% c('shapes', 'colors', 'labels')))
    shapes = legend$shapes %>% as.character
    cols = legend$colors %>% as.character
    labs = legend$labels %>% as.character
  }
  cat(sprintf('LEGEND_SHAPES %s\n', paste(shapes, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_COLORS %s\n', paste(cols, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_LABELS %s\n', paste(labs, collapse=' ')), file=out_file, append=TRUE)

  # data
  ## adding colors
  if(is.null(legend)){
    cols = data.frame(feature = df[,2] %>% unique,
                      color = cols)
  } else {
    cols = legend[,c('labels', 'colors')]
    colnames(cols) = c('feature', 'color')
  }
  df = df %>%
    inner_join(cols, c('feature')) %>%
    dplyr::select(tip, color, feature) %>%
    as.data.frame
  ## writing
  cat('DATA\n', file=out_file, append=TRUE)
  write.table(df, file=out_file, append=TRUE, sep=' ',
              quote=FALSE, col.names=FALSE, row.names=FALSE)
  cat('File written:', out_file, '\n')
}

#' create itol external shape file
#'
#' https://itol.embl.de/help.cgi#shapes
#'
#' If a data.frame is provided, the row names must exactly match the tree node labels. The 1st column with be used for plotting.
#' If a data.table or tibble is provided, the first column values must exactly match the tree node labels. The 2nd column will be used for plotting.
#'
#' @param df A data.frame, tibble, or data.table (see above)
#' @param dataset_label What to label the itol dataset
#' @param out_file Name of the output file
#' @param out_dir Where to write the output
#' @param legend Specify particular legend (see \code{\link{itol_colorstrip}})
#' @return NULL
#' @export
#' @examples
#' # data.frame input
#' df = data.frame(shape = c(1,1,2), row.names = LETTERS[1:3])
#' itol_externalshape(df, 'test_label')
#' # data.table
#' dt = data.table(taxa = LETTERS[1:3], shape = c(1,1,2))
#' itol_externalshape(dt, 'test_label')
itol_externalshape = function(df, dataset_label, out_file='itol_externalshape.txt',
                              out_dir=getwd(), legend=NULL, WIDTH=200){
  # Output
  if(! is.null(out_dir)){
    out_file = file.path(out_dir, out_file)
  }
  # Input format
  if(length(intersect(c('tidytable', 'data.table', 'tbl'), class(df))) > 0){
    df = as.data.frame(df)
    rownames(df) = df[,1]
    df[,1] = NULL
  }

  # Main params
  cat('DATASET_EXTERNALSHAPE\n', file=out_file)
  cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
  cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
  cat('COLOR #ff0000\n', file=out_file, append=TRUE)
  cat(sprintf('WIDTH %s\n', WIDTH), file=out_file, append=TRUE)

  # Field labels
  labs = gsub(' ', '_', colnames(df))
  labs = sprintf('FIELD_LABELS %s\n', paste(labs, collapse=' '))
  cat(labs, file=out_file, append=TRUE)

  # Field colors
  if(is.null(legend)){
    cols = gsub("#[A-Z0-9]{6}FF$", '', rainbow(ncol(df)))
  } else {
    cols = legend$colors %>% as.character
  }
  cols = sprintf('FIELD_COLORS %s\n', paste(cols, collapse=' '))
  cat(cols, file=out_file, append=TRUE)

  # legend
  cat(sprintf('LEGEND_TITLE %s\n', dataset_label), file=out_file, append=TRUE)
  if(is.null(legend)){
    shapes = rep(1, colnames(df) %>% length)
    cols = gsub("#[A-Z0-9]{6}FF$", "", rainbow(length(shapes)))
    labs = paste(gsub(' ', '_', colnames(df)), collapse=' ')
  } else {
    stopifnot(all(colnames(legend) %in% c('shapes', 'colors', 'labels')))
    shapes = legend$shapes %>% as.character
    cols = legend$colors %>% as.character
    labs = legend$labels %>% as.character
  }
  cat(sprintf('LEGEND_SHAPES %s\n', paste(shapes, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_COLORS %s\n', paste(cols, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_LABELS %s\n', paste(labs, collapse=' ')), file=out_file, append=TRUE)

  # Data
  cat('DATA\n', file=out_file, append=TRUE)
  write.table(df, file=out_file, append=TRUE, sep=' ',
              quote=FALSE, row.names=TRUE, col.names=FALSE)
  cat('File written:', out_file, '\n')
}

#' create itol simple-bar file
#'
#' https://itol.embl.de/help.cgi#bar
#'
#' If a data.frame is provided, the row names must exactly match the tree node labels. The 1st column with be used for plotting.
#' If a data.table or tibble is provided, the first column values must exactly match the tree node labels. The 2nd column will be used for plotting.
#'
#' @param df A data.frame, tibble, or data.table (see above)
#' @param dataset_label What to label the itol dataset
#' @param out_file Name of the output file
#' @param out_dir Where to write the output
#' @param legend Specify particular legend (see \code{\link{itol_colorstrip}})
#' @param WIDTH Bar width
#' @return NULL
#' @export
#' @examples
#' # data.frame input
#' df = data.frame(values = c(1,1,2), row.names = LETTERS[1:3])
#' itol_simplebar(df, 'test_label')
#' # data.table
#' dt = data.table(taxa = LETTERS[1:3], values = c(1,1,2))
#' itol_simplebar(dt, 'test_label')
itol_simplebar = function(df, dataset_label, out_file='itol_simplebar.txt',
                          out_dir=getwd(), legend=NULL, WIDTH=200){
  # Output
  if(! is.null(out_dir)){
    out_file = file.path(out_dir, out_file)
  }
  # Input format
  if(length(intersect(c('tidytable', 'data.table', 'tbl'), class(df))) > 0){
    df = as.data.frame(df)
    rownames(df) = df[,1]
    df[,1] = NULL
  }

  # Main params
  cat('DATASET_SIMPLEBAR\n', file=out_file)
  cat('SEPARATOR SPACE\n', file=out_file, append=TRUE)
  cat(sprintf('DATASET_LABEL %s\n', dataset_label), file=out_file, append=TRUE)
  cat('COLOR #ff0000\n', file=out_file, append=TRUE)
  cat(sprintf('WIDTH %s\n', WIDTH), file=out_file, append=TRUE)

  # Field labels
  labs = gsub(' ', '_', colnames(df))
  labs = sprintf('FIELD_LABELS %s\n', paste(labs, collapse=' '))
  cat(labs, file=out_file, append=TRUE)

  # Field colors
  if(is.null(legend)){
    cols = gsub("#[A-Z0-9]{6}FF$", '', rainbow(ncol(df)))
  } else {
    cols = legend$colors %>% as.character
  }
  cols = sprintf('FIELD_COLORS %s\n', paste(cols, collapse=' '))
  cat(cols, file=out_file, append=TRUE)

  # legend
  cat(sprintf('LEGEND_TITLE %s\n', dataset_label), file=out_file, append=TRUE)
  if(is.null(legend)){
    shapes = rep(1, colnames(df) %>% length)
    cols = gsub("#[A-Z0-9]{6}FF$", '', rainbow(length(shapes)))
    labs = paste(gsub(' ', '_', colnames(df)), collapse=' ')
  } else {
    stopifnot(all(colnames(legend) %in% c('shapes', 'colors', 'labels')))
    shapes = legend$shapes %>% as.character
    cols = legend$colors %>% as.character
    labs = legend$labels %>% as.character
  }
  cat(sprintf('LEGEND_SHAPES %s\n', paste(shapes, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_COLORS %s\n', paste(cols, collapse=' ')),
      file=out_file, append=TRUE)
  cat(sprintf('LEGEND_LABELS %s\n', paste(labs, collapse=' ')), file=out_file, append=TRUE)

  # Data
  cat('DATA\n', file=out_file, append=TRUE)
  write.table(df, file=out_file, append=TRUE, sep=' ',
              quote=FALSE, row.names=TRUE, col.names=FALSE)
  cat('File written:', out_file, '\n')
}
