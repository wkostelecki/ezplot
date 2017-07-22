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
ez_labels = function(x,
                     prepend = "",
                     append = "",
                     as_factor = FALSE,
                     round = Inf,
                     signif = Inf){
  unit = data.frame(order = 0:8,
                    unit = c("", "k", "m", "b", "t", "qd", "qn", "sx", "sp"),
                    stringsAsFactors = FALSE)
  df = data_frame(x = x) %>%
    mutate(round_x = signif(round(x, round), signif),
           sign = sign(x),
           absx = abs(round_x),
           order = floor(log10(absx) / 3)) %>%
    left_join(unit, "order") %>%
    mutate(label = ifelse(is.na(unit),
                          as.character(absx),
                          paste0(absx / 10 ^ (3 * order), unit)),
           label = ifelse(is.na(label), "", paste0(prepend, label, append)),
           label = ifelse(sign > -1 | is.na(sign), label, paste0("-", label)))
  if (as_factor) {
    df = mutate(df, label = factor(label, unique(label[order(x)])))
  }
  df[["label"]]
}

