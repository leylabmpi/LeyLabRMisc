#' Get file reports via ENA Portal API
#'
#' Works at least with sample and run accessions.
#'
#' ENA Portal API: https://www.ebi.ac.uk/ena/portal/api/
#'
#' @param accession Sample/run accession
#' @param fields Fields to return. Use ena_get_search_fields() to list all possible fields.
#' @param base_url ENA API base url
#' @param ... Parameters passed to httr::GET
#' @return data.frame
#' @export
#' @examples
#' df = ena_get_filereport('ERR479486')
#' df = ena_get_filereport('SRS2472312')
ena_get_filereport = function(
  accession,
  fields = c('accession', 'run_accession',
             'nominal_length', 'read_count',
             'base_count', 'library_selection',
             'environment_biome', 'environment_feature',
             'environment_material', 'instrument_model',
             'instrument_platform',
             'library_name', 'library_strategy',
             'sample_accession', 'sample_collection',
             'sampling_platform', 'sequencing_method',
              'project_name'),
  base_url = 'https://www.ebi.ac.uk/ena/portal/api/',
  ...
){
  url = paste0(base_url, 'filereport')
  url = paste0(url, '?accession=', accession,
               '&result=read_run&fields=',
               paste(fields, collapse=','))
  request = httr::GET(url = url, ...)
  response = httr::content(request, as = "text", encoding = "UTF-8")
  read.delim(text=response, sep='\t')
}


#' Get possible search fields
#'
#' ENA Portal API: https://www.ebi.ac.uk/ena/portal/api/
#'
#' @param section Section to query (eg., read_run)
#' @param base_url ENA API base url
#' @param ... Parameters passed to httr::GET
#' @return data.frame
#' @export
#' @examples
#' df = ena_get_search_fields()
#' df = ena_get_search_fields('sample')
ena_get_search_fields = function(
  section = c('read_run', 'study', 'sample'),
  base_url = 'https://www.ebi.ac.uk/ena/portal/api/',
  ...
){
  url = paste(gsub('/$', '', base_url), '/searchFields?result=', section[1], sep='')
  request = httr::GET(url = url, ...)
  response = httr::content(request, as = "text", encoding = "UTF-8")
  read.delim(text=response, sep='\t')
}




