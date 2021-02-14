#' Function for reading in a bracken taxonomy table
#'
#' The table will be converted to long form (sample ~ abundance).
#' Only "_frac" or "_num" columns will be kept (see "keep_frac").
#' Taxonomy will be split into separate levels (see "tax_levs").
#' tidytable (w/ data.table) used to speed the process up.
#'
#' @param infile Path to bracken table file
#' @param nrows Number of table rows to read. If Inf, all lines will be read.
#' @param keep_frac If TRUE, keep all columns ending in "_frac"; otherwise, keep "_num" columns.
#' @param tax_levs Taxonomic levels to separate the taxonomy column into.
#' @param ... Params passed to fread()
#' @return data.table
#' @export
read_bracken = function(infile, nrows=Inf, keep_frac=TRUE,
                        tax_levs = c('Domain', 'Phylum', 'Class', 'Order',
                                     'Family', 'Genus', 'Species'),
                        nThread = 4, ...){
  if(keep_frac){
    to_rm = '_num'
    to_keep = '_frac'
  } else {
    to_rm = '_frac'
    to_keep = '_num'
  }
  dt = data.table::fread(infile, sep='\t', nrows=nrows, check.names=TRUE,
                         nThread=nThread, ...) %>%
    tidytable::select.(-taxIDs, -ends_with(!!to_rm)) %>%
    tidytable::mutate.(taxonomy = gsub(';[pcofgs]__', ';', taxonomy),
                       taxonomy = gsub('^d__', '', taxonomy)) %>%
    tidytable::separate.(taxonomy, tax_levs, sep=';') %>%
    tidytable::pivot_longer.(cols=ends_with(!!to_keep),
                               names_to='Sample',
                               values_to='Abundance') %>%
    tidytable::mutate.(Sample = gsub('(_frac|_num)$', '', Sample))

  return(dt)
}



