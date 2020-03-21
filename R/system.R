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
#' @param email The email address. If NULL, then username used
#' @param email_ext The part after the "at" symbol
#' @return The output of the system() call
send_email = function(body, subject='R job complete', email=NULL, email_ext='tuebingen.mpg.de'){
  if(is.null(email)){
    email = sprintf('%s@%s', Sys.info()['user'], email_ext)
  }
  message(sprintf('Sending email to: %s', email))
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
