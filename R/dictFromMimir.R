#' Reformat mimir table to XNA dictionary standards
#' @param table data.table or data.frame (must incluse "HELM", "TS_BASE_SEQ", "TS_SUGAR_SEQ" and "TS_BACKBONE_SEQ" columns)
#' @param base.col character (base column name)
#' @param sugar.col character (sugar column name)
#' @param backbone.col character (backbone column name)
#' 
#' @return data.table (written in the xna_dictionary format)
#' 
#' @examples
#' dt <- data.table::data.table(HELM = c("([PPG])", "[fR]", "[srP]"), 
#'                  TS_BASE_SEQ = c("F", NA, NA), 
#'                  TS_SUGAR_SEQ = c(NA, NA, 'F'), 
#'                  TS_BACKBONE_SEQ = c(NA, 'S', NA))
#' mimir2XnaDict(dt, 'TS_BASE_SEQ', 'TS_SUGAR_SEQ', 'TS_BACKBONE_SEQ')     
#' 
#' @importFrom data.table data.table setnames as.data.table melt  
#'        
#' @export
mimir2XnaDict <- function(table, base.col, sugar.col, backbone.col){
  stopifnot(c("HELM", base.col, sugar.col, backbone.col) %in% colnames(table))
  
  dt <- data.table::as.data.table(eval(parse(text = paste("table[, c('HELM','", base.col,"','", sugar.col,"','", backbone.col, "')]", sep=''))))
  setnames(dt, old = c(base.col, sugar.col, backbone.col), new = c('base', 'sugar', 'backbone'))
  dt <- data.table::melt(data = dt, id.vars = "HELM", measure.vars = c('base', 'sugar', 'backbone'), variable.name = "type", value.name = "symbol")
  dt <-dt[!(is.na(dt$symbol)), ]
  dt$type <- as.character(dt$type)
  
  return(dt)
}
