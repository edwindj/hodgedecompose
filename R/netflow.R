#' @import data.table
net_base_flow <- function(x){
  x <- as.data.table(x)
  el <- normalize_edges(x)
  el2 <- el[,.(nett = sum(weight), base = min(abs(weight))), by=.(from,to)]
  list( nett   = el2[nett != 0, .(from, to, weight = nett, weight_rel = nett/base)]
      , base   = el2[base != 0, .(from, to, weight = base)]
      )
}

#edgelist <- nl_migration
# a <- net_base_flow(nl_migration)
# View(a$nett)
