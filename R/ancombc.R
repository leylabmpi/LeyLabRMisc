#' tidy ancom-bc output
.ancombc_tidy = function(var, L){
  L[[var]] %>%
    mutate(feature = rownames(.)) %>%
    pivot_longer(cols=c(-feature), names_to='covariate', values_to='value') %>%
    mutate(variable = var)

}

#' Tidy ANCOM-BC output
#'
#' Create a tidy table of ANCOM-BC output
#'
#' @param ancombc_out output object from the ancombc() function
#' @return a tibble of tidy data
#' @export
ancombc_tidy = function(ancombc_out){
  vars = ancombc_out$res %>% names
  vars %>%
    lapply(.ancombc_tidy, L=ancombc_out$res) %>%
    do.call(rbind, .) %>%
    pivot_wider(names_from=variable, values_from=value)
}

#' Get unbiased abundances from ANCOM-BC output
#'
#' See https://bioconductor.org/packages/release/bioc/vignettes/ANCOMBC/inst/doc/ANCOMBC.html
#' for more info on unbiased abundances.
#'
#' @param ancombc_out output object from the ancombc() function
#' @param phyloseq_ojb phyloseq object used as input for ancombc() function
#' @return a data.frame of abundances
#' @export
ancombc_unbiased_abundances = function(ancombc_out, phyloseq_obj){
  samp_frac = ancombc_out$samp_frac
  samp_frac[is.na(samp_frac)] = 0
  log_obs_abn = log(otu_table(phyloseq_obj) + 1)
  log_obs_abn_adj = t(t(log_obs_abn) - samp_frac)
  return(log_obs_abn_adj)
}
