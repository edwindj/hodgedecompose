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
" -> csv

g_nongradient <- read.csv(text = csv, strip.white = TRUE)
usethis::use_data(g_nongradient, overwrite = TRUE)
