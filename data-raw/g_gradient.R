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
" -> csv

g_gradient <- read.csv(text = csv)
usethis::use_data(g_gradient, overwrite = TRUE)
