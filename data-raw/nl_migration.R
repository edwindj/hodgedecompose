# load data

# remove non-ascii
nl_migration[,c("from", "to")] <- lapply(nl_migration[,c("from", "to")], stringi::stri_trans_general, "latin-ascii")
usethis::use_data(nl_migration, overwrite = TRUE)
