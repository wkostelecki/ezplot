#' save_png
#' @description Saves ggplot or ezplot objects to png.
#' @param g A ggplot or ezplot object.
#' @param file A png file path.
#' @param width Width of output image.
#' @param height Height or output image.
#' @param res Resolution of output image.
#' @param ... Further arguments to pass to \code{png()}.
#' @param vp A viewport object created with grid::viewport.
#' @importFrom grDevices png dev.off
#' @export
#'
#' @return NULL
save_png = function (g, file, width, height, res, ..., vp = NULL) {
  png(file, width = width, height = height, res = res, ...)
  on.exit(dev.off())
  print(g, vp = vp)
  invisible(NULL)
}



#' ez_png
#' @description Saves ggplot or ezplot objects to png (with useful defaults).
#' @inheritParams save_png
#' @param width Image width (in pixels). Default is 1200.
#' @param height Image height (in pixels). Default is 600.
#' @param res Resolution (PPI) of output image. Default is 72.
#' @param resx Resolution multiplier. Default is 1.
#' @param dir.create Logical. If \code{TRUE}, creates the directory to save
#'   into. Default is \code{FALSE}.
#' @param check Logical. If \code{TRUE}, opens png file after saving. Default is \code{TRUE}.
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
                   check = TRUE) {

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

  if (check & Sys.info()["sysname"] == "Windows") open_file(file)

  invisible(NULL)

}

open_file = function(file) {
  file = normalizePath(file, winslash = "/", mustWork = TRUE)
  system(sprintf("open \"%s\"", file))
  invisible(NULL)
}


