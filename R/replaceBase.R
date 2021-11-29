#' Fucntion for replacing bases in XNAString object
#'
#' replaceBase function replace one base for another
#' in positions with specified sugar
#'
#' @param xnastring_obj XNAString object
#' @param base_to_replace string of bases that should be replaced
#' @param base_replacement string of bases that will be introduced
#' @param sugar string of sugars for which base will be replaced
#'
#' @importFrom stringr str_locate_all str_sub
#' @return XNADtring object
#' @include xnaStringClass.R
#'
#' @export
replaceBase <-
    function(xnastring_obj,
             base_to_replace,
             base_replacement,
             sugar) {
        nchar(base_to_replace) == nchar(base_replacement) ||
            stop("Length of base_to_replace must be equal to base_replacement")
        
        nchar(base_to_replace) == nchar(sugar) ||
            stop("Length of sugar must be equal to base_to_replace")
        
        base_pos <-
            stringr::str_locate_all(xnastring_obj@base, base_to_replace)[[1]]
        sugar_pos <-
            stringr::str_locate_all(xnastring_obj@sugar, sugar)[[1]]
        
        for (i in seq(1:dim(base_pos)[1])) {
            if (all(base_pos[i, ] %in% sugar_pos)) {
                stringr::str_sub(xnastring_obj@base, base_pos[i, ][1], base_pos[i, ][2]) <-
                    base_replacement
            }
        }
        return(xnastring_obj)
        
    }
