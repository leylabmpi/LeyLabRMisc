#' Convert a sub-object of a phyloseq object to a dataframe
#'
#' A helper function for converting OTU, taxonomy, and metadata to dataframes
#'
#' @param physeq_obj The phyloseq object
#' @param physeq_func Which object do you want ('otu_table', 'tax_table', or 'sample_data')
#' @param long Do you want the table in "long" format ("gathered")
#' @return A tibble
phyloseq2df = function(physeq_obj, physeq_func, long=FALSE){
  require(dplyr)
  tbl = physeq_obj %>%
    physeq_func %>%
    as.data.frame(stringsAsFactors = FALSE)

  func_str = as.character(substitute(physeq_func))

  if(func_str == 'otu_table' || func_str == 'tax_table'){
    tbl$OTU = rownames(tbl)
  } else
    if(func_str == 'sample_data'){
      tbl$Sample = rownames(tbl)
    }
  rownames(tbl) = 1:nrow(tbl)

  if(func_str == 'otu_table' && long == TRUE){
    tbl = tbl %>%
      gather(Sample, Count, -OTU)
  } else if(func_str == 'tax_table' && long == TRUE){
    tbl = tbl %>%
      gather(Tax_level, Tax_name, -OTU)
  } else if(func_str == 'sample_data' && long == TRUE){
    tbl = tbl %>%
      gather(Metadata_key, Metadata_value, -Sample)
  }

  return(suppressWarnings(tbl %>% as_tibble))
}

#' phyloseq::estimate_richness, but includes Faith's PD
#'
#' See physeq::estimate richness for full details
#'
#' @param physeq Phyloseq object
#' @param split Splitting the OTU table
#' @param measures Which diversity measures (Faith's PD = "FaithPD)
#' @return Dataframe
estimate_richness_phy = function (physeq, split = TRUE, measures = NULL){
  #' can calculate faith's PD (using Picante, "FaithPD")
  if (!any(otu_table(physeq) == 1)) {
    warning("The data you have provided does not have\n",
            "any singletons. This is highly suspicious. Results of richness\n",
            "estimates (for example) are probably unreliable, or wrong, if you have already\n",
            "trimmed low-abundance taxa from the data.\n", "\n",
            "We recommended that you find the un-trimmed data and retry.")
  }
  if (!split) {
    OTU <- taxa_sums(physeq)
  }
  else if (split) {
    OTU <- as(otu_table(physeq), "matrix")
    if (taxa_are_rows(physeq)) {
      OTU <- t(OTU)
    }
  }
  renamevec = c("Observed", "Chao1", "ACE", "Shannon", "Simpson",
                "InvSimpson", "Fisher")
  names(renamevec) <- c("S.obs", "S.chao1", "S.ACE", "shannon",
                        "simpson", "invsimpson", "fisher")
  if (is.null(measures)) {
    measures = as.character(renamevec)
  }
  if (any(measures %in% names(renamevec))) {
    measures[measures %in% names(renamevec)] <- renamevec[names(renamevec) %in%
                                                            measures]
  }
  if (!any(measures %in% renamevec)) {
    stop("None of the `measures` you provided are supported. Try default `NULL` instead.")
  }
  outlist = vector("list")
  estimRmeas = c("Chao1", "Observed", "ACE")
  if (any(estimRmeas %in% measures)) {
    outlist <- c(outlist, list(t(data.frame(vegan::estimateR(OTU)))))
  }
  if ("Shannon" %in% measures) {
    outlist <- c(outlist, list(shannon = vegan::diversity(OTU, index = "shannon")))
  }
  if ("Simpson" %in% measures) {
    outlist <- c(outlist, list(simpson = vegan::diversity(OTU, index = "simpson")))
  }
  if ("InvSimpson" %in% measures) {
    outlist <- c(outlist, list(invsimpson = vegan::diversity(OTU,
                                                             index = "invsimpson")))
  }
  if ("Fisher" %in% measures) {
    fisher = tryCatch(fisher.alpha(OTU, se = TRUE), warning = function(w) {
      warning("phyloseq::estimate_richness: Warning in fisher.alpha(). See `?fisher.fit` or ?`fisher.alpha`. Treat fisher results with caution")
      suppressWarnings(fisher.alpha(OTU, se = TRUE)[, c("alpha",
                                                        "se")])
    })
    if (!is.null(dim(fisher))) {
      colnames(fisher)[1:2] <- c("Fisher", "se.fisher")
      outlist <- c(outlist, list(fisher))
    }
    else {
      outlist <- c(outlist, Fisher = list(fisher))
    }
  }

  if( "FaithPD" %in% measures){
    outlist <- c(outlist, list(FaithPD = t(picante::pd(samp = OTU, tree = phy_tree(physeq),
                                                       include.root = TRUE))[1,] ))
  }

  out = do.call("cbind", outlist)
  namechange = intersect(colnames(out), names(renamevec))
  colnames(out)[colnames(out) %in% namechange] <- renamevec[namechange]
  colkeep = sapply(paste0("(se\\.){0,}", measures), grep, colnames(out),
                   ignore.case = TRUE)
  out = out[, sort(unique(unlist(colkeep))), drop = FALSE]
  out <- as.data.frame(out)
  return(out)
}


#' Helper Function for rarefaction analysis
#'
#' @param psdata phyloseq object
#' @param measures Which diversity measures
#' @param depth The sampling depth
#' @return molten alpha diversity object
.estimate_rarified_richness <- function(psdata, measures, depth) {
  if(max(sample_sums(psdata)) < depth) return()
  psdata <- prune_samples(sample_sums(psdata) >= depth, psdata)

  rarified_psdata <- rarefy_even_depth(psdata, depth, verbose = FALSE)

  alpha_diversity <- estimate_richness_phy(rarified_psdata, measures = measures)

  # as.matrix forces the use of melt.array, which includes the Sample names (rownames)
  molten_alpha_diversity <- reshape2::melt(as.matrix(alpha_diversity),
                                           varnames = c('Sample', 'Measure'),
                                           value.name = 'Alpha_diversity')

  return(molten_alpha_diversity)
}

#' Function for rarefaction analysis
#'
#' Running estimate_richness_phy() at multiple subsampling depths
#'
#' @param psdata phyloseq object
#' @param measures Which diversity measures (see vegan package)
#' @param depths Which sequencing depths? Example: c(10, 100, 1000)
#' @return A dataframe
calculate_rarefaction_curves <- function(psdata, measures, depths, parallel=FALSE) {
  names(depths) <- depths # this enables automatic addition of the Depth to the output by ldply
  rarefaction_curve_data <- plyr::ldply(depths, .estimate_rarified_richness, psdata = psdata,
                                        measures = measures, .id = 'Depth',
                                        .progress = ifelse(interactive(), 'text', 'none'),
                                        .parallel = parallel)

  # convert Depth from factor to numeric
  rarefaction_curve_data$Depth <- as.numeric(levels(rarefaction_curve_data$Depth))[rarefaction_curve_data$Depth]

  return(rarefaction_curve_data)
}
