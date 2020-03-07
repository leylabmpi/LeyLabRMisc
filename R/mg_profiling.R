#' function for reading in bracken taxonomy files
#'
#' Either columns ending in "_num" or "_frac" will be removed (just raw or fractional abundances).
#' The table will be converted to long form (sample ~ abundance).
#' Taxonomy will be split into separate levels
#' @param F Path to bracken table file
#' @param to_remove Remove either columns ending in "_num" or "_frac"
#' @return tibble
read_bracken = function(F, to_remove='_num'){
  require(data.table)
  require(dplyr)
  require(tidyr)
  if (! to_remove %in% c('_num', '_frac')){
    stop('to_remove must be "_num" or "_frac"')
  }
  to_keep = ifelse(to_remove == '_num', '_frac', '_num')
  tax_levs = c('Domain', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species')
  df = data.table::fread(F, sep='\t') %>%
    as_tibble %>%
    dplyr::select(-taxIDs, -ends_with('_num')) %>%
    dplyr::mutate(taxonomy = gsub(';[pcofgs]__', ';', taxonomy),
           taxonomy = gsub('^d__', '', taxonomy)) %>%
    tidyr::separate(taxonomy, tax_levs, sep=';') %>%
    tidyr::gather(Sample, Abundance, ends_with(to_keep)) %>%
    dplyr::mutate(Sample = gsub(paste0(to_keep, '$'), '', Sample))

  return(df)
}
