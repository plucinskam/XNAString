#' Default XNAString dictionary
#'
#' A dataset containing default internal XNAString dictionary
#' with HELM to string translation.
#'
#' @format A data.table with 20 rows and 3 variables:
#' \describe{
#'   \item{HELM}{HELM sequence coding monomer}
#'   \item{type}{if element is coding base, sugar, backbone}
#'   \item{symbol}{single string translation of HELM}
#' }
#'
#' @docType data
#'
#' @usage data(xna_dictionary)
#' @source RMR internal bioinformatics database (Mimir)
"xna_dictionary"


#' Default XNAString complementarity dictionary
#'
#' A dataset containing default internal XNAString dictionary
#' with base complemetary.
#'
#' @format A data.table with 6 rows and 3 variables:
#' \describe{
#'   \item{base}{base symbol}
#'   \item{target}{complementary base}
#'   \item{compl_target}{complementary target}
#' }
#'
#' @docType data
#'
#' @usage data(complementary_bases)
#' @source RMR internal bioinformatics database (Mimir)
"complementary_bases"
