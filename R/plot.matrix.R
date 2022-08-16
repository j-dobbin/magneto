"plot.matrix" <-
function (x, ...) {
  plot(magneto:::newPixmapRGB(x, x, x), ...)
}

"newPixmapRGB" <-
function(red=NULL, green=NULL, blue=NULL) {
  pmap.data <- array(data = c(red, green, blue), dim = c(dim(red)[1], dim(red)[2], 3))
  pmap <- pixmap::pixmapRGB(pmap.data, nrow=dim(red)[1], ncol=dim(red)[2],
       bbox=NULL, bbcent=FALSE, cellres=NULL)

  return(pmap)
}

