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
#' @param print_output Pretty printing of the output to the console?
#' @param return_output Return the bash command output?
#' @param log_file Write stdout to log file (stderr written to log_file.err)
#' @param verbose Write status messages?
#' @param wait Wait for the process to finish?
#' @returns NULL
#' @export
#' @examples
#' # simple
#' bash_job('ls -thlc')
#' # write to log file
#' bash_job('ls -thlc', log_file='log.txt')
#' # use conda env
#' bash_job('conda list', conda_env='base')
bash_job = function(cmd, conda_env=NULL, stdout=TRUE, stderr=TRUE,
                    print_output=TRUE, return_output=FALSE,
                    log_file=NULL, verbose=TRUE, wait=TRUE){
  if(!is.null(conda_env)){
    CMD = sprintf('. ~/.bashrc; conda activate %s;', conda_env)
  } else {
    CMD = ''
  }
  CMD = sprintf('%s %s', CMD, cmd)
  if(!is.null(log_file)){
    stdout = log_file
    stderr = paste0(log_file, '.err')
  }
  if('sys' %in% rownames(installed.packages())){
    require(sys)
    CMD = c('-c', '.', CMD)
    if(verbose == TRUE){
      message(sprintf('bash %s', paste(CMD, collapse=' ')))
    }
    if(wait == TRUE){
      ret = exec_wait('bash', CMD, std_out=stdout, std_err=stderr)
    } else {
      ret = exec_background('bash', CMD, std_out=stdout, std_err=stderr)
    }

  } else {
    CMD = sprintf('-c "%s"', CMD)
    if(verbose == TRUE){
      message(sprintf('bash %s', CMD))
    }
    ret = system2('bash', CMD, stdout=stdout, stderr=stderr, wait=TRUE)
  }
  if(print_output == TRUE){
      cat(paste(ret, collapse='\n'))
  }
  if(return_output == TRUE){
      return(ret)
  }
  return(invisible())
}

#' pretty printing of a text file via cat
#'
#' This is most useful for working with IRkernl in Jupyter notebooks
#'
#' @param file_name the name of the file to print
#' @return NULL
#' @export
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
#' @export
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
#' @export
condaInfo = function(conda_env){
  cat(paste(bash_job('conda list', conda_env), collapse='\n'))
}

#' snakemake conda info
#'
#' @param config_file The path to the config file
#' @param pipeline_dir The path to the pipeline_directory
#' @param conda_env The conda env that has snakemake installed
#' @return The environment info
#' @export
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
#' @export
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
