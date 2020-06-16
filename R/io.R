#' list.files with full.names=TRUE & recursive=TRUE
#'
#' @param path a character vector of full path names; the default corresponds to the working directory,
#' @param pattern an optional regular expression. Only file names which match the regular expression will be returned.
#' @param full.names 	a logical value. If TRUE, the directory path is prepended to the file names to give a relative file path. If FALSE, the file names (rather than paths) are returned
#' @param recursive logical. Should the listing recurse into directories?
#' @return A character vector containing the names of the files in the specified directories
list_files = function(path, pattern=NULL, full.names=TRUE, recursive=TRUE, ...){
  list.files(path, pattern, full.names = full.names, recursive = recursive, ...)
}

#' Simple wrapper around data.table::fread
#'
#' @param infile input file name
#' @param cmd command instead of input file (eg., "gunzip -c INFILE")
#' @param sep value delimiter
#' @param check.names format check column names
#' @param ... passed to data.table::fread
#' @return data.table
Fread = function(infile=NULL, cmd=NULL, sep='\t', check.names=TRUE, ...){
  if(is.null(infile) & is.null(cmd)){
    stop('infile and cmd cannot both be NULL')
  } else if(is.null(cmd)){
    return(data.table::fread(infile, sep=sep, check.names=check.names, ...))
  } else if(is.null(infile)){
    return(data.table::fread(cmd=cmd, sep=sep, check.names=check.names, ...))
  }
}

#' splitting path and returning just one item in the vector
#'
#' This is useful for merging tables in which the individual table ID
#' is within the file path.
#'
#' @param file_path File path(s). If vector or list of paths provided, then a list will be returned
#' @param index Which item in the path to return? 1-indexing. If <1, samples selected from the end. "O" will select the file name.
#' @return string if 1 path, else list
path_get_label = function(file_path, index){
  # if multiple input
  if(length(file_path) > 1){
    labs = lapply(as.list(file_path), function(x) path_get_label(x, index=index))
    file_path = setNames(as.list(file_path), labs)
    return(file_path)
  }
  # else just return 1
  file_path = unlist(strsplit(file_path, '/'))
  if(index < 1){
    index = length(file_path) + index
  }
  return(file_path[index])
}
#' convert a vector of file paths into a named list
#'
#'
#' @param files Vector of file paths (eg., by using "list_files()")
#' @param label_index Which item in the path to return? 1-indexing. If <1, samples selected from the end.
#'
#' @examples
#'  files = c('/path/to/project/Sample1/table.txt', '/path/to/project/Sample2/table.txt')
#'  files_to_list(files, -1)
#'  files = c('/path/to/project/Sample1.txt', '/path/to/project/Sample2.txt')
#'  files_to_list(files, 0)
files_to_list = function(files, label_index=-1){
  L = as.list(files)
  names(L) = as.character(sapply(files, path_get_label, index=label_index))
  return(L)
}

#' writing table convience function
#'
#' This is most useful for working with IRkernl in Jupyter notebooks.
#' If a data.table is provided, then fwrite is used; otherwise, write.table is used.
#'
#' @param df data.frame or data.table to write out
#' @param file Output file path
#' @param sep the field separator string. Values within each row of x are separated by this string
#' @param quote a logical value (TRUE or FALSE) or a numeric vector. If TRUE, any character or factor columns will be surrounded by double quotes.
#' @param row.names either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
#' @param ... Passed to write.table (if data.frame) or fwrite (if data.table)
#' @return NULL
write_table = function(df, file, sep="\t", quote=FALSE, row.names=FALSE, ...){
  if('data.table' %in% class(df)){
    data.table::fwrite(df, file=file, sep=sep, quote=quote, row.names=row.names, ...)
  } else {
    write.table(df, file=file, sep=sep, quote=quote, row.names=row.names, ...)
  }
  cat('File written:', file, '\n')
}

#' A helper function for creating a directory (recursively)
#'
#' @param dir path for the new directory (will create recursively)
#' @param quite quite output
#' @return NULL
make_dir = function(dir, quiet=FALSE){
  if(! dir.exists(dir)){
    dir.create(dir, recursive = TRUE, showWarnings=FALSE)
    if(quiet == FALSE){
      cat('Created directory:', dir, '\n')
    }
  } else {
    if(quiet == FALSE){
      cat('Directory already exists:', dir, '\n')
    }
  }
  if (!dir.exists(dir)){
    stop('Could not create directory!')
  }
}

#' Dump an R object as text to a temp file and get the md5sum of the file
#'
#' @param Robj Any R object
#' @return md5sum
Robj_md5sum = function(Robj){
  F = tempfile()
  dput(Robj, file=F)
  as.character(tools::md5sum(c(F)))
}

#' Read the last N lines of a file
#'
#' @param x The file name
#' @param n The last N lines to read
#' @param ... Passed to scan()
#' @return NULL
readLinesTail = function(x, n, ...){
  con = file(x)
  open(con)
  out = scan(con, n, what="char(0)", sep="\n", quiet=TRUE,...)
  while(TRUE){
    tmp = scan(con,1,what="char(0)",sep="\n",quiet=TRUE)
    if(length(tmp)==0) {
      close(con) ; break
    }
    out = c(out[-1],tmp)
  }
  out
}
