#' Elegant negation of %in%
'%!in%' = Negate('%in%')

#' convert to numeric while avoiding factor conversion issues
#'
#' @param x an interable
#' @return a numeric object
as.Num = function(x){
  as.numeric(as.character(x))
}

#' simple dataframe summary
#'
#' @param df dataframe object
#' @param n Number of lines to print
#' @return a dataframe object
dfhead = function(df, n=3){
  df %>% dim %>% print
  df %>% head(n=n)
}

#' global change of plot size options
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#'
#' @param w figure width
#' @param h figure height
#' @param res figure resolution (DPI)
#' @return NULL
dims = function(w=5, h=5, res=300){
  options(repr.plot.width=w, repr.plot.height=h, repr.plot.res = res)
}

#' changing number of rows/columns shown when printing a data frame
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#'
#' @param nrows number of rows to print
#' @param ncols number of columns to print
#' @return NULL
df.dims = function(nrows=4, ncols=20){
  options(repr.matrix.max.rows=nrows, repr.matrix.max.cols=ncols)
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

#' bash job using conda env
#'
#' The conda setup is assumed to be in your ~/.bashrc
#' If print_output == TRUE: the stdout/stderr will be printed instead of returned
#' Else: the stdout/stderr with be returned by the function
#' stderr/stdout is printed unless print_output==FALSE
#'
#' @param cmd The bash command in a string format
#' @param conda_env The conda env to use
#' @param stdout Print the stdout from the command?
#' @param stderr Print the stderr from the command?
#' @param quiet No printing
#' @returns NULL
bash_job = function(cmd, conda_env, stdout=TRUE, stderr=TRUE, print_output=TRUE){
  # cmd : string; commandline job (eg., 'ls -thlc')
  # conda_env : string; conda environment name
  cmd = sprintf('. ~/.bashrc; conda activate %s; %s', conda_env, cmd)
  cmd = sprintf('-c "%s"', cmd)
  ret = system2('bash', cmd, stdout=stdout, stderr=stderr)
  if(print_output==TRUE){
    cat(paste(ret, collapse='\n'))
    return(NULL)
  } else {
    return(ret)
  }
}

#' pretty printing of a text file via cat
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#'
#' @param file_name the name of the file to print
#' @return NULL
cat_file = function(file_name){
  cmd = paste('cat', file_name, collapse=' ')
  system(cmd, intern=TRUE) %>% paste(collapse='\n') %>% cat
}


#' A helper function to send an email via the mail bash cmd
#'
#' @param body The email body
#' @param subject The email subject line
#' @param email The email address
#' @return The output of the system() call
send_email = function(body, subject='R job complete', email='nyoungblut@tuebingen.mpg.de'){
  cmd = sprintf('echo %s | mail -s "%s" "%s"', body, subject, email)
  system(cmd)
}

#' "conda list" in R
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#' @param conda_env The name of the conda env to list
#' @return NULL
condaInfo = function(conda_env){
  cat(paste(bash_job('conda list', conda_env), collapse='\n'))
}

#' snakemake conda info
#'
#' @param config_file The path to the config file
#' @param pipeline_dir The path to the pipeline_directory
#' @param conda_env The conda env that has snakemake installed
#' @return The environment info
snakemakeInfo = function(config_file, pipeline_dir, conda_env){
  snakefile = file.path(pipeline_dir, 'Snakefile')
  cmd = sprintf('snakemake --list-conda-envs --configfile %s --directory %s --snakefile %s -F',
                config_file, pipeline_dir, snakefile)
  envs = bash_job(cmd, conda_env, stderr=TRUE)

  env_list = list()
  for(x in envs){
    if (grepl('\t\\.snakemake/conda/', x)){
      y = gsub('.+\t(\\.snakemake/conda/.+)', '\\1', x)
      z = file.path(pipeline_dir, y)
      cmd = sprintf('conda list -p %s', z)
      cat(cmd, '\n')
      env_list[[z]] = bash_job(cmd, conda_env)
    }
  }
  return(env_list)
}

#' pipeline sessionInfo
#'
#' sessionInfo for LeyLab snakemake pipelines
#' @param pipeline_path The path to the pipeline directory
#' @param head_n The number of lines to print from the readme
#' @return NULL
pipelineInfo = function(pipeline_path, head_n=10){
  # readme
  readme_path = file.path(pipeline_path, 'README.md')
  if(!file.exists(readme_path)){
    cat('Cannot find README.md file in pipeline directory')
    stop()
  }
  cmd = sprintf('head -n %s %s', head_n, readme_path)
  cat(paste(system(cmd, intern=TRUE), collapse='\n'))
  cat('\n\n--- conda envs ---\n')
  # conda envs
  env_path = file.path(pipeline_path, 'bin', 'envs')
  cmd = sprintf('find %s -name "*.yaml" | xargs head -n 1000', env_path)
  cat(paste(system(cmd, intern=TRUE), collapse='\n'))
}

#' create UUID for figure file name
#' @param full Full length uuid or trimmed to just 24 char?
#' @return character object
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

#' Dump an R object as text to a temp file and get the md5sum of the file
#'
#' @param Robj Any R object
#' @return md5sum
Robj_md5sum = function(Robj){
  F = tempfile()
  dput(Robj, file=F)
  as.character(tools::md5sum(c(F)))
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
