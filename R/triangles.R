get_triangles <- function(x){
  x2 <- flip_edges(x, unclass(x$from) > unclass(x$to))
  x2[,{
      x2[from %in% .SD$to & to %in% .SD$to]
    },by=.(out=from)]
}
