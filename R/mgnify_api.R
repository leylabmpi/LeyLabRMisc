# adding columns to a data.frame, as needed
..add_columns = function(df, cols){
  for(x in setdiff(cols, colnames(df))){
    df[,x] = NA
  }
  return(df)
}

# adding columns to a data.frame, as needed
.add_columns = function(L){
  all_cols = unique(unlist(lapply(L, colnames)))
  lapply(L, ..add_columns, cols=all_cols)
}

# recursive search in a nested list; return flattened list
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

# # replicate vector values in list to match longest vector in the list
# .replicate = function(L){
#   max_len = max(unlist(lapply(L, length)))
#   for(n in names(L)){
#     if(length(L[[n]]) < max_len){
#       L[[n]] = rep(L[[n]], max_len)[1:max_len]
#     }
#   }
#   if(! all(unlist(lapply(L, length)) == max_len)){
#     stop('could not create vectors of same length')
#   }
#   return(L)
# }
# #df = ena_get_studies()

# flatten a nested list; return a list
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
  require(httr)
  require(RCurl)
  require(jsonlite)
  # setting page
  if(!is.null(query)){
    query$page = page
  }
  # GET
  request = GET(url = url, query = query, ...)
  if(http_type(request) != 'application/json') {
    warning('API did not return json', call. = FALSE)
    return(NULL)
  }
  # response
  response = content(request, as = "text", encoding = "UTF-8")
  if(is.null(response) || request$status_code != 200){
    warn_for_status(request)
    return(NULL)
  }
  # to data.frame
  response = fromJSON(response, flatten = TRUE)
  if(verbose == TRUE && page == 1 && !is.null(names(response))){
    message('Total pages:', response$meta$pagination$pages)
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
#' x = mgnify_request('https://www.ebi.ac.uk/metagenomics/api/v1/biomes', max_pages = 2)
mgnify_request = function(url, max_pages=NULL, query=list(), verbose=TRUE,
                       cache_file='mgnify_request.RDS', cache_break=10,
                       use_cache=TRUE, ...){
  # init
  responses = list()
  call_status = 200
  page = 1
  # cache
  if(use_cache == TRUE && file.exists(cache_file)){
    message('use_cache==TRUE; Reading cache file: ', cache_file)
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
    if(is.null(max_pages) | max_pages > ret$pages){
      max_pages = ret$pages
    }
    if(ret$page >= max_pages){
      break
    }
    # cache
    if(!is.null(cache_file) && page %% cache_break == 0){
      saveRDS(responses, cache_file)
      message('Cache file written: ', cache_file)
    }
    # iter
    page = ret$page + 1
  }
  # cache
  if(file.exists(cache_file)){
    file.remove(cache_file)
    message('Cache file removed: ', cache_file)
  }
  # binding data.frames
  if(is.null(responses) | length(responses) == 0){
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
mgnify_get= function(
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

# # selecting samples
# sample_info = mgnify_get(lineage='root:Host-associated',
#                          instrument_platform = 'ILLUMINA',
#                          instrument_model = 'HiSeq',
#                          section='samples',
#                          max_pages=20)
# ## filtering by studies
# sample_info = sample_info %>%
#   mutate(study_accession = sapply(relationships_studies_data,
#                                   function(x) x[1,'id'])) %>%
#   group_by(study_accession) %>%
#   mutate(n_samples=n(), .groups='drop') %>%
#   filter(n_samples >= 10)
# ## getting read info
# sample_info$attributes_accession %>%
#   head(n=5) %>%
#   lapply(ena_get_filereport) %>%
#   do.call(rbind, .)



# print('----\n')
# #
# #' Get experiment-type info
# #'
# #' If no query info provided, all experiment types are listed
# #'
# #' @param max_pages Max number of pages to return
# #' @param base_url ENA API base url
# #' @param ... Parameters passed to httr::GET
# ena_get_experiment_types = function(
#   max_pages = NULL,
#   base_url = 'https://www.ebi.ac.uk/metagenomics/api/v1/',
#   ...){
#   url = paste0(base_url, 'experiment-types')
#   df = mgnify_request(url, max_pages=max_pages, query=list(), ...)
#   return(df)
# }
# df = ena_get_experiment_types(max_pages=2)
#
# #' Get a table of all biome info from the ENA
# #'
# #' @param base_url ENA API base url
# #' @examples
# #' require(ggplot2)
# #' ggplot(ena_get_biomes(),
# #'       aes(attributes_biome_name, attributes_samples_count)) +
# #'   geom_bar(stat='identity') +
# #'   theme(axis.text.x = element_text(angle=45, hjust=1))
# ena_get_biomes = function(
#   base_url = 'https://www.ebi.ac.uk/metagenomics/api/v1/',
#   experiment_type = NULL, #c('metagenomic')
#   max_pages = 1,
#   ...
# ){
#   url = paste0(base_url, 'biomes')
#   if(!is.null(experiment_type)){
#     url = paste0(url, '/root/samples')
#     query$experiment_type = paste(experiment_type, collapse=',')
#   }
#   mgnify_request(url, query=query, max_pages=max_pages, ...)
# }
# #df = ena_get_biomes(max_pages = 2)
# #df = ena_get_biomes(experiment_type = 'metagenomic', max_pages=2)
# #df = ena_get_biomes(experiment_type = c('amplicon', 'metagenomic'), max_pages=2)
#
#
# #' Get ENA study info
# #'
# #' Query for a set of studies or provide a study accession to
# #' get specific info on that study.
# #'
# #' @param accession Study accession (primary or secondary). If provided, just info returned for that study.
# #' @param lineage Filter by lineage (eg., "root:Host-associated:Human")
# #' @param base_url ENA API base url
# #' @param ... Parameters passed to httr::GET
# #' @examples
# #' ena_get_studies(accession = 'ERP009004')$attributes_biosample
# ena_get_studies = function(
#   base_url = 'https://www.ebi.ac.uk/metagenomics/api/v1/',
#   max_pages = 1,
#   accession = NULL,  # study accession
#   lineage = NULL,  # 'root:Host-associated:Human'
#   search = NULL,
#   study_fields = c('accession', 'bioproject', 'study_name', 'biomes',
#                    'samples', 'samples_count'),
#   sample_fields = c('accession', 'longitude', 'latitude'),
#   query=list(),
#   ...
# ){
#   url = paste0(base_url, 'studies')
#   if(!is.null(lineage)){
#     query$lineage = lineage[1]
#   }
#   if(!is.null(search)){
#     query$search = search[1]
#   }
#
#   if(!is.null(accession)){
#     url = paste0(url, '/', accession, '/samples')
#     #url = paste0(url, '/', accession,
#     #'?include=samples&fields[studies]=',
#     #paste(study_fields, collapse=','),
#     #'&fields[samples]=',
#     #paste(sample_fields, collapse=','))
#   }
#   ena_request(url, query=query, max_pages=max_pages, ...)
# }
# nrow(ena_get_studies())
# nrow(ena_get_studies(instrument_platform='NONE'))
# nrow(ena_get_studies(lineage='root:Host-associated:Human'
#                      #df = ena_get_studies()
#                      #df = ena_get_studies(search = 'soil')
#                      #df = ena_get_studies(accession = 'ERP009004')
#                      #df = ena_get_studies(accession = 'MGYS00005358')
#                      # df = ena_get_studies(lineage = 'root:Host-associated:Human',
#                      #                      max_pages=2)
#
#                      #' Get ENA sample info
#                      #'
#                      #' Query for a set of studies or provide a study accession to
#                      #' get specific info on that study.
#                      #'
#                      #' @param accession Sample accession). If provided, just info returned for that study.
#                      #' @param lineage Filter by lineage (eg., "root:Host-associated:Human")
#                      #' @param instrument_platform
#                      #' @param base_url ENA API base url
#                      #' @param ... Parameters passed to httr::GET
#                      #' @examples
#                      #' ena_get_studies(accession = 'ERP009004')$attributes_biosample
#                      ena_get_samples = function(
#                        accession = NULL,  # study accession
#                        lineage = NULL,  # 'root:Host-associated:Human'
#                        instrument_platform = NULL,  # ILLUMINA
#                        search = NULL,
#                        #study_fields = c('accession', 'bioproject', 'study_name', 'biomes',
#                        #                 'samples', 'samples_count'),
#                        #sample_fields = c('accession', 'longitude', 'latitude'),
#                        query=list(),
#                        base_url = 'https://www.ebi.ac.uk/metagenomics/api/v1/',
#                        max_pages = 1,
#                        ...
#                      ){
#                        url = paste0(base_url, 'samples')
#                        if(!is.null(lineage)){
#                          query$lineage = lineage[1]
#                        }
#                        if(!is.null(instrument_platform)){
#                          query$instrument_platform = instrument_platform[1]
#                        }
#                        if(!is.null(search)){
#                          query$search = search[1]
#                        }
#                        if(!is.null(accession)){
#                          url = paste0(url, '/', accession)
#                        }
#                        ena_request(url, query=query, max_pages=max_pages, ...)
#                      }
#                      df = ena_get_samples(search = 'soil')
#                      #df = ena_get_samples(accession="ERS627828")
#
#
#
#                      #' Get analyses
#                      #'
#                      #' @param base_url ENA API base url
#                      #' @param ... Parameters passed to httr::GET
#                      ena_get_analyses = function(
#                        base_url = 'https://www.ebi.ac.uk/metagenomics/api/v1/',
#                        max_pages = 1,
#                        #accession = NULL,
#                        instrument_platform = NULL,
#                        instrument_model = NULL,
#                        lineage = NULL,  # 'root:Host-associated:Human'
#                        experiment_type = NULL, #c('metagenomic')
#                        search = NULL,
#                        query=list(),
#                        ...
#                      ){
#                        url = paste0(base_url, 'analyses')
#                        if(!is.null(lineage)){
#                          query$lineage = paste(lineage, collapse=',')
#                        }
#                        if(!is.null(instrument_platform)){
#                          query$instrument_platform = paste(instrument_platform, collapse=',')
#                        }
#                        if(!is.null(instrument_model)){
#                          query$instrument_model = paste(instrument_model, collapse='%20')
#                        }
#                        if(!is.null(experiment_type)){
#                          query$experiment_type = paste(experiment_type, collapse=',')
#                        }
#                        if(!is.null(search)){
#                          query$search = paste(search, collapse=',')
#                        }
#                        ena_request(url, query=query, max_pages=max_pages, ...)
#                      }
#                      df = ena_get_analyses(max_pages=2,
#                                            lineage = 'root:Host-associated',
#                                            experiment_type = 'metagenomic',
#                                            instrument_platform = 'ILLUMINA',
#                                            instrument_model = c('Illumina HiSeq'))
#
#                      # df = ena_get_analyses(max_pages=2,
#                      #                       experiment_type = 'amplicon',
#                      #                       instrument_platform = 'ILLUMINA',
#                      #                       instrument_model = 'Illumina MiSeq')
