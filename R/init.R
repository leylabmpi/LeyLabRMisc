#' Elegant negation of %in%
'%!in%' = Negate('%in%')

#' convert to numeric
as.Num = function(x){
  as.numeric(as.character(x))
}

#' simple dataframe summary
dfhead = function(df, n=3){
  df %>% dim %>% print
  df %>% head(n=n)
}

#' global change of plot size options
dims = function(width=5, height=5, res=300){
  options(repr.plot.width=width, repr.plot.height=height, repr.plot.res = res)
}

#' changing number of rows/columns shown when printing a data frame
df.dims = function(nrows=4, ncols=20){
  options(repr.matrix.max.rows=nrows, repr.matrix.max.cols=ncols)
}

#' make directory
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
  if !dir.exists(dir){
    stop('Could not create directory!')
  }
}



#' bash job using conda env
bash_job = function(cmd, conda_env, stdout=TRUE, stderr=TRUE){
  # cmd : string; commandline job (eg., 'ls -thlc')
  # conda_env : string; conda environment name
  cmd = sprintf('. ~/.bashrc; conda activate %s; %s', conda_env, cmd)
  cmd = sprintf('-c "%s"', cmd)
  system2('bash', cmd, stdout=stdout, stderr=stderr)
}

#' "cat {file}" in R
cat_file = function(file_name){
  cmd = paste('cat', file_name, collapse=' ')
  system(cmd, intern=TRUE) %>% paste(collapse='\n') %>% cat
}


#' send and email
send_email = function(body, subject='R job complete', email='nyoungblut@tuebingen.mpg.de'){
  cmd = sprintf('echo %s | mail -s "%s" "%s"', body, subject, email)
  system(cmd)
}

#' conda list in R
condaInfo = function(conda_env){
  cat(paste(bash_job('conda list', conda_env), collapse='\n'))
}

#' snakemake conda info (sessionInfo)
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
Robj_md5sum = function(Robj){
  F = tempfile()
  dput(Robj, file=F)
  as.character(tools::md5sum(c(F)))
}


#' plot figure and save the figure grob object to a file at the same time
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



# Extract data from ggplot object
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
