get_triangles <- function(x){
  edges <- flip_edges(x, unclass(x$from) > unclass(x$to))
  setDT(edges)
  x_tri <-
    edges[,{
      tri <- edges[ from %in% .SD$to & to %in% .SD$to
             ,.(b = from, c = to, bc=weight)
             ]

      i <- match(tri$b, .SD$to)
      tri$ab <- .SD$weight[i]

      i <- match(tri$c, .SD$to)
      tri$ac <- .SD$weight[i]
      tri[, curl := ab + bc - ac]

      },by=.(a=from)]
  x_tri
}

get_gamma <- function(x){
  x[, id := .I]
  n <- nrow(x)
  m <- list(
    x[x, .(i = id, j = i.id, x = 1), on=.(a,b), nomatch=NULL, by=.EACHI],
    x[x, .(i = id, j = i.id, x = 1), on=.(b,c), nomatch=NULL, by=.EACHI],
    x[x, .(i = id, j = i.id, x = 1), on=.(a,c), nomatch=NULL, by=.EACHI],
    x[x, .(i = id, j = i.id, x = 1), on=c(a="b", b="c"), nomatch=NULL, by=.EACHI],
    x[x, .(i = id, j = i.id, x = -1), on=c(a="b", c="c"), nomatch=NULL, by=.EACHI],
    x[x, .(i = id, j = i.id, x = -1), on=c(a="a", b="c"), nomatch=NULL, by=.EACHI]
    #x_tri[x_tri, .(i = id, j = i.id, x = 1), on=c(b="a", c="b"), nomatch=NULL],
  )
  m <- rbindlist(m)
  m <- m[i != j,]
  # and flip (is symmetric)
  m <- m[, .(i = c(i,j, seq_len(n)),j = c(j,i, seq_len(n)),x = c(x,x,rep(3,n)))]
  print(m)
  S <- Matrix::sparseMatrix(i = m$i, j = m$j, x = m$x, use.last.ij = T)
  print(S)
  x$gamma <- solve_svd(S, x$curl)
  x
}

get_w_s <- function(tri, edges){
  d1s <- rbindlist(list(
    tri[edges, .(from, to, w_s = curl) ,on=c(a="from", b="to"), nomatch=NULL],
    tri[edges, .(from, to, w_s = -curl) ,on=c(a="from", c="to"), nomatch=NULL],
    tri[edges, .(from, to, w_s = curl) ,on=c(b="from", c="to"), nomatch=NULL]
  ))
  d1s[,.(w_s = sum(w_s)), by = .(from, to)]
}



