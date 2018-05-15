#' Function for formatting numeric labels
#'
#' @param x numeric
#' @param prepend character
#' @param append character
#' @param as_factor logical
#' @param round numeric passed to \code{round()}
#' @param signif numeric passed to \code{signif()}
#' @return y
#' @export
#' @import dplyr
#' @examples
#' ez_labels(10^(0:10))
#' ez_labels(2000, append = " apples")
#' ez_labels(0:10, append = " apples", as_factor = TRUE)
#' ez_labels(c(0, 0.1, 0.01, 0.001, 0.0001))
ez_labels = function(x,
                     prepend = "",
                     append = "",
                     as_factor = FALSE,
                     round = Inf,
                     signif = Inf){
  unit = data.frame(order = 0:8,
                    unit = c("", "k", "m", "b", "t", "qd", "qn", "sx", "sp"),
                    stringsAsFactors = FALSE)

  # browser()
  df = data_frame(x = x) %>%
    mutate(round_x = signif(round(x, round), signif),
           sign = sign(x),
           absx = abs(round_x),
           tens = floor(log10(absx)),
           order = floor(tens / 3)) %>%
    left_join(unit, "order") %>%
    mutate(label = ifelse(is.na(unit),
                          ifelse(tens < -2 & is.finite(tens),
                                 paste0(absx / 10 ^ tens, "\u00D710\u207B",
                                       superscript(abs(tens))),
                                 as.character(absx)),
                          paste0(absx / 10 ^ (3 * order), unit)),
           label = ifelse(is.na(label), "", paste0(prepend, label, append)),
           label = ifelse(sign > -1 | is.na(sign), label, paste0("-", label)))
  if (as_factor) {
    df = mutate(df, label = factor(label, unique(label[order(x)])))
  }
  df[["label"]]
}

superscript = function(x) {
  x = ifelse(is.finite(x), x, NA_real_)
  unicode = c("\u2070", "\u00B9", "\u00B2", "\u00B3", "\u2074",
              "\u2075", "\u2076", "\u2077", "\u2078", "\u2079")
  sapply(strsplit(as.character(x), ""),
         function(x){
           paste(unicode[as.numeric(x) + 1], collapse = "")
         })

}

