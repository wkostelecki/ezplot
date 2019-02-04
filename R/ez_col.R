#' Color palette interpolation
#'
#' @param n number of colours
#' @param palette palette to interpolate from
#'
#' @return rgb
#' @export
#'
#' @importFrom utils tail
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' ez_col(15)
#' ez_col(2, c("blue", "red"))
#' ez_col(3, c("blue", "red"))
ez_col = function(n = 50, palette = NULL){
  if (is.null(palette)){
    palette = c("dodgerblue4",
                "olivedrab3",
                "mediumorchid4",
                "tomato2",
                "darkgoldenrod1",
                "forestgreen",
                "steelblue2",
                "brown3")
  }
  len_p = length(palette)
  if (n <= length(palette)){
    return(palette[1:n])
  }
  freq = c(rep(floor((n - 1) / (len_p - 1)), len_p - 1), 1) +
    c(rep(1, (n - 1) %% (len_p - 1)), rep(0, len_p - (n - 1) %% (len_p - 1)))
  palette = c(palette, tail(palette, 1))
  unlist(lapply(1:len_p,
                function(x) {
                  colorRampPalette(palette[x + c(0:1)])(freq[x] + 1)[1:freq[x]]
                }))

}

#' text_contrast
#'
#' @param x Vector of colours.
#'
#' @return Vector indicating whether black or white should be used for text
#'   overlayed on x.
#' @export
#'
#' @importFrom grDevices col2rgb
#'
#' @examples
#' text_contrast("#000000")
#' text_contrast("black")
text_contrast = function(x){
  ifelse(apply(col2rgb(x), 2, mean) >= 128, "#000000", "#FFFFFF")
}


#' ez_jet
#' @param n Number of colours to return.
#' @param palette Vector of colours.
#' @export
ez_jet = function(n = 100,
                  palette = c("dodgerblue4",
                              "steelblue2",
                              "olivedrab3",
                              "darkgoldenrod1",
                              "brown")){
  stopifnot(n >= 5)
  colorRampPalette(colors = palette)(n)
}
