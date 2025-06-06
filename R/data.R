#' Long-term stem inventory data from tropical rain forest plots in Australia.
#'
#' A dataset containing the locations of thousands of trees in Queensland, Australia.
#' @format A data frame with 109675 rows and 17 variables:
#' \describe{
#'   \item{epNumber}{CSIRO experimental plot number}
#'   \item{stemNumber}{Alphanumeric character assigned to each stem}
#'   \item{family}{Taxonomic family}
#'   \item{genus}{Taxonomic genus}
#'   \item{taxon}{Full taxonomic name}
#'   \item{taxonAuth}{Full taxonomic name with author}
#'   \item{ALA_APNI_Taxon_LSID}{Life Science Identifier (LSID) sourced from the Australian Plant Name Index}
#'   \item{ALA_APNI_Taxon_URI}{Australian Plant Name Index URI}
#'   \item{wetTropicsEndemic}{Endemic to the Wet Tropic Bioregion of Australia}
#'   \item{coordinates_x_metres}{Stem position along the 100 m axis}
#'   \item{coordinates_y_metres}{Stem position along the 50 m axis}
#'   \item{year}{Year of census}
#'   \item{dbh_centimetres}{Diameter of the stem (cm)}
#'   \item{comment}{Comment on the health of the stem or status of the measurement}
#'   \item{establishmentHeight_metres}{Stem height at establishment}
#'   \item{height_metres}{Height of any stem (m) estimated post-establishment}
#'   \item{status}{Dead or Alive at the census shown in variable 'year'}
#' }
#'
#' @source \url{https://data.csiro.au/dap/SupportingAttachment?collectionId=23294&fileId=2029}
"raw_qld_trees"
