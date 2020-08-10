#' Fragility Index ggplot2 theme
#'
#' @param ... 
#'
#' @importFrom ggplot2 `%+replace%` theme_bw theme element_blank
#'
#' @return
#' @export
#'
#' @examples
theme_fi <- function(...) {
  
  pct <- theme_bw(base_family = "sans", base_size = 11) %+replace% 
    theme(legend.background = element_blank(),
          legend.key = element_blank(), 
          panel.background = element_blank(),
          panel.border = element_blank(), 
          strip.background = element_blank(),
          plot.background = element_blank(), 
          axis.line = element_blank(),
          panel.grid = element_blank())
  
}