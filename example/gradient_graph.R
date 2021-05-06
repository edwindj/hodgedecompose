library(data.table)
el <- fread(
"from,to,weight
2,1,1
1,8,4
3,2,2
2,6,8
3,4,3
3,5,6
3,6,10
3,8,7
4,5,3
4,6,7
5,6,4
7,6,1
8,7,2
")

el2 <- fread(
  "from,to,weight
2,1,1
1,8,4.2
3,2,2
2,6,8.1
3,4,3.1
3,5,5.9
3,6,9.8
3,8,7.1
4,5,3.1
4,6,6.9
5,6,4.1
7,6,1
8,7,2
")

el_traffic <- fread(
"from,to,weight
1,3,2
2,10,5
3,4,20
3,5,20
3,6,20
3,10,20
7,10,20
8,10,20
9,10,20
")

el
x <- el
library(Matrix)

solve_svd <- function(A, b, tol = .Machine$double.eps){
  # could also use sparsesvd!

  A_inv <- with(svd(A), {
    is_pos <- d > max(tol * d[1L], 0)
    # so that 1/d is zero
    d[!is_pos] <- Inf
    # alternatively, may be faster, drop all !is_pos columns
    v %*% ((1/d) * t(u))
  })
  as.numeric(A_inv %*% b)
}

solve_qr <- function(A, b, tol = .Machine$double.eps){
  as.numeric(
    solve( qr(A, tol = tol)
         , b
         )
  )
}

flip_edges <- function(x, flip){
  for (n in colnames(x)[-c(1:2)]){
    x[[n]][flip] <- -x[[n]][flip]
  }
  #x$weight[flip] <- -x$weight[flip]

  from_flip <- x$to[flip]
  x$to[flip] <- x$from[flip]
  x$from[flip] <- from_flip
  x
}

# assumes edgelist with from,to,weight
gradient_graph_edgelist <- function( x
                                   , tol = .Machine$double.eps
                                   , method=c("qr", "svd")
                                   ){
  method <- match.arg(method)

  # node id to factor...
  if (is.factor(x$from)){
    x$from <- as.character(x$from)
  }
  if (is.factor(x$to)){
    x$to <- as.character(x$to)
  }
  # above code is needed to make sure, concatenating is going well.
  f <- factor(c(x$from, x$to))
  l <- levels(f)

  # recoding node ids
  x$from <- factor(x$from, levels = l)
  x$to <- factor(x$to, levels = l)

  # remove self links
  x <- x[x$from != x$to,]

  # normalize el: always from < to (by negating weight).
  x <- flip_edges(x, unclass(x$from) > unclass(x$to))

  # take the net flow
  x <- aggregate(weight ~ from + to, data = x, sum)
  # x

  v <- data.frame(id = l)

  # get divergence
  v$div <- tapply(x$weight, x$from, sum, default = 0) -
           tapply(x$weight, x$to,   sum, default = 0)

  v$div <- as.numeric(v$div)

  # calculate Graph Laplacian (unweighted, undirected)
  L <- sparseMatrix(i = c(x$from, x$to), j = c(x$to, x$from), x = -1)
  diag(L) <- -rowSums(L)
  L
  v$pot <-
    switch( method
          , qr  = solve_qr(L, v$div, tol = tol)
          , svd = solve_svd(L, v$div, tol = tol)
          )

  # potential is translation invariant, so make lowest potential 0.
  v$pot <- v$pot - min(v$pot)

  # is this ok? just the potential difference? Shouldn't this be a weighted version?
  e_gradient <- within(x, w_g <- v$pot[from] - v$pot[to])

  # denormalize gradient
  e_gradient <- flip_edges(e_gradient, e_gradient$weight < 0)

  #e_gradient

  list( v          = v
      , e_gradient = e_gradient
      )
}

library(igraph)
df_g <- gradient_graph_edgelist(el)
g <- graph_from_data_frame( df_g$e_gradient
                          , vertices = df_g$v
                          )

coords <- cbind(runif(vcount(g)), V(g)$pot)
coords <- cbind(0, V(g)$pot)
fixed <- logical(vcount(g))
fixed[c( which.max(V(g)$pot)
       , which.min(V(g)$pot)
       )] <- TRUE

layout <- layout_with_drl(g
                         , weights = NA
                         , seed = coords
                         , use.seed=TRUE
                         , fixed = fixed
                         )

layout[,2] <- rank(V(g)$pot)

plot(g, layout = layout)


l_sg <- layout_with_sugiyama(g, weights = NA)
plot(g, layout= l_sg$layout)



write_graph(g, "g.dot", format="dot")
# dot g.dot -O -Tpng


gradient_graph_edgelist(el2)


gradient_graph_edgelist(el_traffic)

