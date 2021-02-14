#' Global change of plot size options
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#'
#' @param w figure width
#' @param h figure height
#' @param res figure resolution (DPI)
#' @return NULL
#' @export
p.dims = function(w=5, h=5, res=200){
  options(repr.plot.width = w, repr.plot.height = h, repr.plot.res = res)
}

#' plot figure and save the figure grob object to a file at the same time
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#'
#' @param p Plot object (ggplot2, base, etc)
#' @param file File name to write
#' @param path Path to write to
#' @param suffix File name suffix (eg., '.png')
#' @param saveObj Write the Robj to a file?
#' @param saveImg Write the image to a file?
#' @param width Figure width. If NA, uses global options
#' @param height Figure height. If NA, uses global options
#' @return NULL
#' @export
Plot = function(p, file=NULL, path=NULL, suffix='', saveObj=TRUE, saveImg=FALSE, width=NA, height=NA, ...){
  # file path
  if(is.null(path)){
    path = file.path(getwd(), '.figures')
    if(! dir.exists(path)){
      dir.create(path, showWarnings=FALSE)
    }
  }
  # writing figure
  if(saveObj == TRUE){
    fileRDS = file
    if(is.null(fileRDS)){
      fileRDS = paste0(Robj_md5sum(p), suffix, '.RDS')
    }
    fileRDS = file.path(path, fileRDS)

    saveRDS(p, file=fileRDS)
    cat('File written:', fileRDS, '\n')
  }
  # saving image file
  if(saveImg == TRUE){
    if(is.null(file)){
      file = paste0(Robj_md5sum(p), suffix, '.png')
    }
    file = file.path(path, file)

    # width & height
    if(is.na(width)){
      width = options()$repr.plot.width
    }
    if(is.na(height)){
      height = options()$repr.plot.height
    }

    # writting figure
    if(length(class(p)) >= 2 & (class(p)[2] == 'ggplot' | class(p)[2] == 'ggplot2' | class(p)[2] == 'ggmatrix')){
      cat('Class of plot object is "ggplot2" (or "ggmatrix"). Using ggsave()\n', file = stderr())
      ggplot2::ggsave(filename=file, plot=p, width=width, height=height, ...)
    } else {
      cat('Class of plot object is not "ggplot2". Using png()\n', file = stderr())
      png(file=file, width=width, height=height)
      plot(p, ...)
      dev.off()
    }
    cat('File written:', file, '\n')
  }

  # plotting
  if(length(class(p)) >= 2 & class(p)[2] == 'ggmatrix'){
    print(p)
  } else {
    plot(p)
  }
}

#' Extract data from ggplot object
#'
#' The data is written to files
#'
#' @param plot_object A ggplot object
#' @param output_path Where to write the output
#' @return NULL
#' @export
extract_pltdt = function(plot_object, output_path){
  require(ggplot2)
  # Extract data tables
  raw_data = plot_object$data
  figure_data = ggplot_build(plot_object)$data[1]
  # File names
  plot_name = deparse(substitute(plot_object))
  raw_filename = file.path(output_path, paste(plot_name, "rawdata", "tsv", sep = "."))
  figure_filename = file.path(output_path, paste(plot_name, "figuredata", "tsv", sep = "."))
  # Save tables
  write.table(raw_data, raw_filename, sep = "\t", quote = F, row.names = F)
  write.table(figure_data, figure_filename, sep = "\t", quote = F, row.names = F)
}

#' create UUID for figure file name
#' @param full Full length uuid or trimmed to just 24 char?
#' @return character object
#' @export
fig_uuid = function(full=FALSE){
  baseuuid = paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")

  if(full == TRUE){
    id = paste(
      substr(baseuuid,1,8),
      "-",
      substr(baseuuid,9,12),
      "-",
      "4",
      substr(baseuuid,13,15),
      "-",
      sample(c("8","9","a","b"),1),
      substr(baseuuid,16,18),
      "-",
      substr(baseuuid,19,30),
      sep="",
      collapse="")
  } else{
    id = substr(baseuuid,1,24)
  }
  id = paste0('fig-', id)
  return(id)
}

#' getting RColorBrewer entire palette
.get_brewer_palette = function(){
  # color palletes
  qual_col_pals = RColorBrewer::brewer.pal.info
  qual_col_pals$rowname = rownames(qual_col_pals)
  qual_col_pals = subset(qual_col_pals, category == "qual")
  # Create single vector with all colors
  color_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors,
                               qual_col_pals$rowname))
  return(color_vector)
}

#' Great a better coloring scheme for taxon abundance barcharts
#'
#' The default coloring scheme for ggplot2 makes it hard to
#' distinguish among data points in complex bar charts (eg., taxa plots).
#' This function is a wrapper around scale_color_continuous()
#' which changes the color scheme used.
#'
#' @param ... Parameters passed to scale_fill_manual()
#' @param return_hex Return a vector of color hexidecimals instead of a plotting object.
#' @return ScaleContinuous/ggproto object or vector
#' @export
#' @examples
#' ggplot(mpg, aes(fl, hwy, fill=model)) +
#'   geom_bar(stat='identity') +
#'   scale_fill_all()
scale_fill_all = function(..., return_hex = FALSE){
  color_vector = .get_brewer_palette()
  if(return_hex == TRUE){
    return(color_vector)
  } else {
    return(scale_fill_manual(values = color_vector, ...))
  }
}

#' Great a better coloring scheme for taxon abundance barcharts
#'
#' The default coloring scheme for ggplot2 makes it hard to
#' distinguish among data points in complex bar charts (eg., taxa plots).
#' This function is a wrapper around scale_color_continuous()
#' which changes the color scheme used.
#'
#' @param ... Parameters passed to scale_colorl_manual()
#' @param return_hex Return a vector of color hexidecimals instead of a plotting object.
#' @return ScaleContinuous/ggproto object or vector
#' @export
#' @examples
#' ggplot(mpg, aes(cty, hwy, color=class)) +
#'   geom_point() +
#'   scale_color_all()
scale_color_all = function(..., return_hex = FALSE){
  color_vector = .get_brewer_palette()
  if(return_hex == TRUE){
    return(color_vector)
  } else {
    return(scale_color_manual(values = color_vector, ...))
  }
}


