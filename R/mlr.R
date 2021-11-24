#-- supplemental functions for the mlr package --#

#' Version of getNestedTuneResultsOptPathDf that actually works
#'
#' For main docs, see ?getNestedTuneResultsOptPathDf
#'
#' @param r The result of resampling of a tuning wrapper
#' @param trafo Should the units of the hyperparameter path be converted to the transformed scale?
#' @return data.frame
#' @export
#' @importFrom data.table rbindlist
mlr_getNestedTuneResultsOptPathDf = function(r, trafo = FALSE) {
  checkmate::assertClass(r, "ResampleResult")
  checkmate::assertList(r$extract)
  lapply(r$extract, checkmate::assertClass, classes = "TuneResult")
  checkmate::assertFlag(trafo)
  ops = BBmisc::extractSubList(r$extract, "opt.path", simplify = FALSE)
  if (trafo) ops = lapply(ops, trafoOptPath)
  op.dfs = lapply(ops, as.data.frame)
  op.dfs = data.table::rbindlist(op.dfs, use.name=TRUE, idcol='iter', fill=TRUE)
  return(op.dfs)
}

#' Custom mlr filter for Boruta
#'
#' A custom mlr filter that uses Boruta to select important features
#' This function registers the "boruta.filter" filter to be used with
#' makeFilterWrapper and other mlr filter functions.
#'
#' \itemize{
#'   \item target str; what is the target variable in the task object (default: 'Class')
#'   \item pValue float; see Boruta docs (default: 0.01)
#'   \item maxRuns int; see Boruta docs (default: 200)
#'   \item hostHistory bool; see Boruta docs (default: FALSE)
#'   \item withTentative bool; keep tentative features (default: TRUE)
#'   \item verbose bool; list features selected? (default: FALSE)
#'   \item mustKeep vector; features that cannot be filtered (default: NULL)
#'   \item threads int; number of threads to use for Boruta (default: 1)
#' }
#' @return Nothing, but "boruta.filter" filter will be registered
#' @export
mlr_boruta_filter = function(){
  mlr::makeFilter(
    name = "boruta.filter",
    desc = "Uses boruta for feature selection",
    pkg = character(0),
    supported.tasks = c("classif", "regr"),
    supported.features = c("numerics", "factors", "ordered"),
    fun = function(task, nselect, decreasing = TRUE, target='Class', pValue=0.01, maxRuns=200,
                   hostHistory=FALSE, withTentative=TRUE, verbose=FALSE, mustKeep=NULL, threads=1) {
      # boruta run
      message('Starting Boruta feature selection')
      feats = getTaskData(task)
      if(!target %in% colnames(feats)){
        stop('Cannot find target "', target, '" in the task object')
      }
      target_vec = feats[,target]
      feats = feats[,setdiff(colnames(feats), target)]
      boruta_res = Boruta::Boruta(x=feats, y=target_vec,
                                  pValue = pValue,
                                  maxRuns = maxRuns,
                                  holdHistory = hostHistory,
                                  num.threads = threads)
      # selected features
      to_keep = Boruta::getSelectedAttributes(boruta_res, withTentative = withTentative)
      if(!is.null(mustKeep)){
        to_keep = union(to_keep, mustKeep)
      }
      if(length(to_keep) == 0){
        warning('Boruta selected 0 features! Using 10 random features')
        to_keep = sample(colnames(feats), 10)
      }
      message('Number of features selected: ', length(to_keep))
      if(verbose){
        message('Features selected: ', paste(to_keep, collapse=', '))
      }
      # feature importance (arbitrary)
      imp = rep(100, length(to_keep))
      names(imp) = to_keep
      return(imp)
    }
  )
}
