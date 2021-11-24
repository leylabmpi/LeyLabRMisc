#-- helper functions for running jobs on the cluster --#

#' Set clustermq options
#'
#' These options must be set before running clustermq
#'
#' @param scheduler The clustermq.scheduler option. Use "multicore" for local jobs.
#' @param template The clustermq.template option. It defaults to ~.clustermq.tmpl
#' @return NULL
#' @export
#' @examples
#' clustermq_setup()              # sge job
#' clustermq_setup('multicore')   # local job
clustermq_setup = function(scheduler = c('sge', 'multicore'),
                           template = file.path(Sys.getenv("HOME"), '.clustermq.tmpl')){
  options(
    clustermq.scheduler = scheduler[1],
    clustermq.template = template
  )
}

#' Set a path for clustermq cluster job log files
#'
#' Log files are optional for clustermq. The must be set in the template.
#' This function will create a unique directory within the "base_dir".
#' It will also return a path that you MUST use for the "log_file" parameter
#' in the Q template.
#' Moreover, the function will set the "clustermq.logfile" option to that
#' directory (used by clustermq_get_logs).
#'
#' The function requires the uuid package.
#'
#' @param base_dir The base directory will the logfiles will be located.
#' @return logfile path
#' @export
#' @examples
#' clustermq_setup()
#' tmpl = list(job_mem = '8G', log_file = clustermq_logfile())
#' fx = function(x, y) x * 2 + y
#' Q(fx, x=1:3, const=list(y=10), n_jobs=10, job_size=1, template=tmpl)
clustermq_logfile = function(base_dir = '/ebio/abt3_scratch/'){
  d = file.path(base_dir, Sys.info()[['user']], 'clustermq', uuid::UUIDgenerate())
  dir.create(d, recursive = TRUE, showWarnings=FALSE)
  options(clustermq.logfile = d)
  message(sprintf('Setting logfile dir: %s', d))
  file.path(d, 'clustermq$TASK_ID.log')
}

#' Get/read clustermq cluster job log files
#'
#' If you use "log_file = clustermq_logfile()" in your template,
#' then you can use this function to get the log file paths or
#' directly read the contents of the log files.
#'
#' @param lines The number of lines of each log file to read.
#' If 0, then the log file paths will be returned;
#' if >0 then the first N lines will be printed;
#' if <0 then the last N lines will be printed.
#' @param logfile_dir The base directory containing all of the logfiles.
#' If not provided, then this is obtained by getOption('clustermq.logfile')
#' @return logfile paths or NULL
#' @export
#' @examples
#' clustermq_setup()
#' tmpl = list(job_mem = '8G', log_file = clustermq_logfile())
#' fx = function(x, y) x * 2 + y
#' Q(fx, x=1:3, const=list(y=10), n_jobs=10, job_size=1, template=tmpl)
#' clustermq_get_logs()             # getting log file paths
#' clustermq_get_logs(lines=10)     # reading the first 10 lines
#' clustermq_get_logs(lines=-10)    # reading the list 10 lines
clustermq_get_logs = function(lines=0, logfile_dir=NULL){
  if(is.null(logfile_dir)){
    logfile_dir = getOption('clustermq.logfile')
  }
  log_files = list.files(logfile_dir, '*log', full.names = TRUE, recursive = FALSE)
  if(lines > 0){
    for(F in log_files){
      cat(sprintf('--- File: %s ---\n', F))
      cat(paste(readLines(F, n = lines), collapse='\n'))
      cat('\n')
    }
  } else if(lines < 0){
    for(F in log_files){
      cat(sprintf('--- File: %s ---\n', F))
      cat(paste(readLinesTail(F, n = abs(lines)), collapse='\n'))
      cat('\n')
    }
  } else {
    return(log_files)
  }
}
