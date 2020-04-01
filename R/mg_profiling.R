#' Function for reading in a bracken taxonomy table
#'
#' The table will be converted to long form (sample ~ abundance).
#' Only "_frac" or "_num" columns will be kept (see "keep_frac").
#' Taxonomy will be split into separate levels (see "tax_levs").
#' tidytable (w/ data.table) used to speed the process up.
#'
#' @param infile Path to bracken table file
#' @param is_gzip Is the table file gzip'ed?
#' @param n_lines Number of lines to read. If < 1, all lines will be read.
#' @param keep_frac If TRUE, keep all columns ending in "_frac"; otherwise, keep "_num" columns.
#' @param tax_levs Taxonomic levels to separate the taxonomy column into.
#' @return data.table
read_bracken = function(infile, is_gzip=FALSE, n_lines=0, keep_frac=TRUE,
                        tax_levs = c('Domain', 'Phylum', 'Class', 'Order',
                                     'Family', 'Genus', 'Species')){
  if(n_lines > 0){
    n_lines = glue::glue('| head -n {n}', n=n_lines)
  } else {
    n_lines = ''
  }
  if(is_gzip){
    cmd = glue::glue('gunzip -c {file} {n_lines}',
                     file=infile, n_lines=n_lines)
  } else {
    cmd = glue::glue('cat {file} {n_lines}',
                     file=infile, n_lines=n_lines)
  }
  if(keep_frac){
    to_rm = '_num'
    to_keep = '_frac'
  } else {
    to_rm = '_frac'
    to_keep = '_num'
  }
  dt = data.table::fread(cmd=cmd, sep='\t', check.names=TRUE) %>%
    tidytable::dt_select(-taxIDs, -dt_ends_with(!!to_rm)) %>%
    tidytable::dt_mutate(taxonomy = gsub(';[pcofgs]__', ';', taxonomy),
                         taxonomy = gsub('^d__', '', taxonomy)) %>%
    tidytable::dt_separate(taxonomy, tax_levs, sep=';') %>%
    tidytable::dt_pivot_longer(cols=dt_ends_with(!!to_keep),
                               names_to='Sample',
                               values_to='Abundance') %>%
    tidytable::dt_mutate(Sample = gsub('(_frac|_num)$', '', Sample))

  return(dt)
}
