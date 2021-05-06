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
" -> csv

g_traffic <- read.csv(text = csv)
usethis::use_data(g_traffic, overwrite = TRUE)
