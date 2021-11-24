#' adding columns to a data.frame, as needed
#' @return data.frame
..add_columns = function(df, cols){
  for(x in setdiff(cols, colnames(df))){
    df[,x] = NA
  }
  return(df)
}

#' adding columns to a data.frame, as needed
#' @return list
.add_columns = function(L){
  all_cols = unique(unlist(lapply(L, colnames)))
  lapply(L, ..add_columns, cols=all_cols)
}

#' recursive search in a nested list; return flattened list
#' @return list
.rec_search = function(L, Lret=list(), full_path=c()){
  if(is.null(names(L))){
    return(NULL)
  } else {
    for(n in names(L)){
      full_path = c(full_path, n)
      if(class(L[[n]]) == 'list'){
        Lret = .rec_search(L[[n]], Lret, full_path)
      } else {
        key = paste(full_path, collapse='..')
        if(class(L[[n]]) == 'data.frame'){
          for(cn in colnames(L[[n]])){
            K = paste(key, cn, sep='..')
            Lret[[K]] = L[[n]][,cn]
          }
        } else {
          if(!is.null(L[[n]])){
            Lret[[key]] = L[[n]]
          } else {
            Lret[[key]] = NA
          }
        }
        full_path = c()
      }
    }
  }
  return(Lret)
}

#' flatten a nested list; return a list
.flatten_nested = function(response){
  if(is.null(response)){
    return(NULL)
  }
  ret = NULL
  if(!is.null(response$data)){
    ret = .rec_search(response$data)
    if(!is.null(ret)){
      ret = do.call(rbind, ret)
      ret = as.data.frame(t(ret))
    }
  }
  ret = list(data = ret,
             meta = list(pagination = list(page = 1, pages=1)))
  return(ret)
}

#' GET request to MGnify API
#'
#' @param url Query url
#' @param page Page of records to return
#' @param query List passed to httr::GET(query=)
#' @param verbose Verbose output?
#' @return list(status = character, page = numberic, pages = numberic, data = data.frame)
#' @export
#' @examples
#' mgnify_request_get('https://www.ebi.ac.uk/metagenomics/api/v1/samples', query=list(instrument_platform = 'ILLUMINA'))
mgnify_request_get = function(url, page=1, query=list(), verbose=TRUE, ...){
  # setting page
  if(!is.null(query)){
    query$page = page
  }
  # GET
  request = httr::GET(url = url, query = query, ...)
  if(httr::http_type(request) != 'application/json') {
    warning('API did not return json', call. = FALSE)
    return(NULL)
  }
  # response
  response = httr::content(request, as = "text", encoding = "UTF-8")
  if(is.null(response) || request$status_code != 200){
    httr::warn_for_status(request)
    return(NULL)
  }
  # to data.frame
  response = jsonlite::fromJSON(response, flatten = TRUE)
  if(verbose == TRUE && page == 1 && !is.null(names(response))){
    message('Total pages: ', response$meta$pagination$pages)
  }
  if(class(response$data) == 'list'){
    response = .flatten_nested(response)
  }
  ## checks
  if(is.null(response$data) || class(response$data) != 'data.frame' || nrow(response$data) == 0){
    if(verbose == TRUE){
      if(is.null(response$data)){
        warning('NULL data object returned')
      } else
        if(class(response$data) != 'data.frame' && nrow(response$data) == 0){
          warning('Returned data.frame is empty')
        }
    }
    return(list(status = request$status_code,
                page = response$meta$pagination$page,
                pages = response$meta$pagination$pages,
                data = NULL))
  }
  colnames(response$data) = gsub('[-.]', '_', colnames(response$data))
  return(list(status = request$status_code,
              page = response$meta$pagination$page,
              pages = response$meta$pagination$pages,
              data = response$data))
}

#' GET request from the ENA
#'
#' @param url ENA API url
#' @param max_pages Max number of pages to return. If NULL, all pages returned.
#' @param query Query list passed to httr::GET
#' @param verbose Verbose output?
#' @param cache_file File name to cache (checkpoint) the results. Useful for big queries in case the job is interuppted.
#' @param cache_break Write cache file every N pages.
#' @param use_cache Read the cache file, if it exists?
#' @param ... Parameters passed to httr::GET
#' @return data.frame
#' @export
#' @examples
#' mgnify_request('https://www.ebi.ac.uk/metagenomics/api/v1/biomes', max_pages = 3)
mgnify_request = function(url, max_pages=NULL, query=list(), verbose=TRUE,
                       cache_file='mgnify_request.RDS', cache_break=10,
                       use_cache=TRUE, ...){
  # init
  responses = list()
  call_status = 200
  page = 1
  # cache
  if(use_cache == TRUE && file.exists(cache_file)){
    if(verbose == TRUE){
      message('use_cache==TRUE; Reading cache file: ', cache_file)
    }
    responses = readRDS(cache_file)
    page = length(responses) -1
    if(page < 1){
      page = 1
    }
  }
  # iterating over pages
  while(call_status == 200){
    if(verbose == TRUE){
      message('GET: Page', page)
    }
    ret = mgnify_request_get(url, page=page, query=query, verbose=verbose, ...)
    if(is.null(ret) | is.null(ret$data) | is.null(ret$page)){
      warning('NULL response; Ending GET loop')
      break
    }
    call_status = ret$status
    responses[[ret$page]] = ret$data
    # pages
    if(is.null(max_pages) || max_pages > ret$pages){
      max_pages = ret$pages
    }
    if(ret$page >= max_pages){
      break
    }
    # cache
    if(!is.null(cache_file) && page %% cache_break == 0){
      saveRDS(responses, cache_file)
      if(verbose == TRUE){
        message('Cache file written: ', cache_file)
      }
    }
    # iter
    page = ret$page + 1
  }
  # cache
  if(file.exists(cache_file)){
    file.remove(cache_file)
    if(verbose == TRUE){
      message('Cache file removed: ', cache_file)
    }
  }
  # binding data.frames
  if(is.null(responses) || length(responses) == 0){
    return(NULL)
  }
  responses = .add_columns(responses)
  responses = tryCatch(
    {
      rbindlist(responses, use.names=TRUE)
    },
    error = function(cond) {
      do.call(rbind, responses)
    })
  return(responses)
}

#' Get MGnify info via the API
#'
#' MGnify API: https://www.ebi.ac.uk/metagenomics/api/v1/
#'
#' This function can query any "section" of the API (eg., "studies" or "samples").
#'
#' Main filtering options are listed in the function (eg., lineage).
#' More queries can be provides as a list via the query parameter.
#' Note that not all filtering options for work each section.
#' To see all filtering options for each section, click the "Filters" button at https://www.ebi.ac.uk/metagenomics/api/v1/samples
#'
#' To prevent accidental big queries, only 1 page of results is returned by default (max_pages).
#' @param accession Study accession (primary or secondary). If provided, just info returned for that study.
#' @param section Section of the API to query.
#' @param search General keyword search to filter records.
#' @param lineage Filter by lineage (eg., "root:Host-associated:Human").
#' @param instrutment_platform Sequencing instrument platform (eg., "ILLUMINA").
#' @param instrutment_model Sequencing instrument model (eg., "HiSeq" or "MiSeq").
#' @param query List of additional queries provided to httr::GET.
#' @param max_pages The maximum number of pages of records to return.
#' @param cache_file File name to cache (checkpoint) the results. Useful for big queries in case the job is interuppted.
#' @param cache_break Write cache file every N pages.
#' @param use_cache Read the cache file, if it exists?
#' @param base_url MGnify API base url
#' @param ... Parameters passed to httr::GET
#' @return data.frame
#' @export
#' @examples
#' mgnify_get()
#' mgnify_get(search='soil')
#' mgnify_get(section='biomes')
#' mgnify_get(section='experiment-types')
#' mgnify_get(accession = 'ERP009004', section='studies')
#' mgnify_get(lineage='root:Host-associated', instrument_platform = 'ILLUMINA',
#'             instrument_model = 'HiSeq', max_pages=8)
#' mgnify_get(accession = 'SRS2472313')
mgnify_get = function(
  accession = NULL,  # accession
  section=c('samples', 'studies', 'analyses', 'biomes', 'experiment-types'),
  search = NULL,  # 'soil'
  lineage = NULL,  # 'root:Host-associated:Human'
  instrument_platform = NULL,  # ILLUMINA
  instrument_model = NULL, # HiSeq OR MiSeq
  query=list(),
  max_pages = 1,
  base_url = 'https://www.ebi.ac.uk/metagenomics/api/v1/',
  cache_file='mgnify_request.RDS',
  cache_break = 10,
  use_cache=TRUE,
  ...
){
  url = paste(gsub('/$', '', base_url), section[1], sep='/')
  if(is.null(section)){
    return(NULL)
  } else {
    section = section[1]
  }
  # query
  ## search
  if(!is.null(search)){
    query$search = search[1]
  }
  ## lineage
  if(!is.null(lineage)){
    if(any(section %in% c('biomes', 'experiment-types'))){
      message('"lineage" ignored for section: ', section[1])
    }
    query$lineage = lineage[1]
  }
  ## instrument-model
  if(!is.null(instrument_model)){
    if(any(section %in% c('studies', 'biomes', 'experiment-types'))){
      message('"instrument_model" ignored for section: ', section[1])
    }
    query$instrument_model = instrument_model[1]
  }
  ## instrument-platform
  if(!is.null(instrument_platform)){
    if(any(section %in% c('studies', 'biomes', 'experiment-types'))){
      message('"instrument_platform" ignored for section: ', section[1])
    }
    query$instrument_platform = instrument_platform[1]
  }
  # accession
  if(!is.null(accession) && any(section %in% c('studies', 'samples', 'analyses'))){
    url = paste(url, accession[1], sep='/')
    if(section %in% c('studies')){
      url = paste(url, 'samples', sep='/')
    }
  }
  # request
  mgnify_request(url, query=query,
              max_pages=max_pages,
              cache_file=cache_file,
              cache_break=cache_break,
              use_cache=use_cache,
              ...)
}
