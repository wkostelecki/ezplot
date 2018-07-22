#' save_png
#'
#' @param g A ggplot or ezplot object.
#' @param file A path.
#' @param width Width of output image.
#' @param height Height or output image.
#' @param res Resolution of output image.
#' @param ... Further arguments to pass to png
#' @param vp Viewport created with grid::viewport
#'
#' @importFrom grDevices png dev.off
#' @export
#'
#' @return NULL
save_png = function (g, file, width, height, res, ..., vp = NULL){
  png(file, width = width, height = height, res = res, ...)
  on.exit(dev.off())
  print(g, vp = vp)
  invisible(NULL)
}


#' @export
ez_png = function (g,
                   file,
                   width = 1200,
                   height = 600,
                   res = 72,
                   resx = 1,
                   ...,
                   vp = NULL,
                   dir.create = FALSE,
                   check = TRUE){

  file = normalizePath(file, winslash = "/", mustWork = FALSE)
  dir_path = dirname(file)

  if (!dir.exists(dir_path) & dir.create) {
    dir.create(dir_path, recursive = TRUE)
  }

  save_png(g,
           file,
           width * resx,
           height * resx,
           res * resx,
           ...,
           vp = vp)

  if (check) open_file(file)

  invisible(NULL)

}

open_file = function(file){
  file = normalizePath(file, winslash = "/", mustWork = TRUE)
  system(sprintf("open \"%s\"", file))
  invisible(NULL)
}


