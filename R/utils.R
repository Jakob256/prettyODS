#' get hex representation of a color
#'
#' @param colors todo
#'
#' @returns todo
#'
#' @examples prettyODS:::.col2hex(c("red","darkblue","#ff3300",NA))
.col2hex <- function(colors){
  rgb=grDevices::col2rgb(colors)
  hex=sprintf("#%02X%02X%02X", rgb[1,], rgb[2,], rgb[3,])
  hex[is.na(colors)]=NA
  return(hex)
}



# we want 1.65 to be rounded to 1.7 with digits=1. However
#   round(1.65,1)=1.6
#   sprintf("%.1f",1.65)="1.6"
#   .round2character(1.65,1)="1.7"
# hence this unusual form:

#' Title
#'
#' @param x todo
#' @param digits todo
#'
#' @returns todo
#'
#' @examples 1+1
.round2character <- function(x, digits){
  sprintf(paste0("%.", digits, "f"), x*(1+2^(-48)))
}
