#try to automate MCMCglmm object reading
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Fiona Soper, Dr - Sofia AusStoich Project/purple cont only")

#it works immediately? 
ln_leaf_N_chain1 <- readRDS("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Fiona Soper, Dr - Sofia AusStoich Project/purple cont only/ln_leaf_N/ln_leaf_N_chain1.RDS")

ln_NP_ratio_chain1 <- readRDS("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Fiona Soper, Dr - Sofia AusStoich Project/purple cont only/ln_NP_ratio/ln_NP_ratio_chain1.RDS")

ln_NP_ratio_chain1 <- readRDS("ln_NP_ratio/ln_NP_ratio_chain1.RDS")
test <- readRDS("ln_leaf_N/ln_leaf_N_chain1.RDS") #works 
#csv reading also works
file.exists("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Fiona Soper, Dr - Sofia AusStoich Project/purple cont only/ln_NP_ratio/ln_NP_ratio_chain2.RDS")
#gives true
list.files("ln_NP_ratio") #RDS does show up

con <- file("ln_NP_ratio/ln_NP_ratio_chain1.RDS", "rb")
readRDS(con)
close(con)
con <- gzfile("ln_NP_ratio/ln_NP_ratio_chain1.RDS")
readRDS(con)

gzcon <- gzfile("~/Documents/temp chains/ln_NP_ratio_chain1.RDS", "rb")
readBin(gzcon, raw(), n = 100)
close(gzcon)

readRDS(
  gzfile("~/Documents/temp chains/ln_NP_ratio_chain1.RDS", "rb")
)

saveRDS(
  object,
  "~/Documents/temp chains/ln_NP_ratio_chain1.RDS",
  compress = FALSE,
  version = 2
)

#did this do smth weird so that now i cant read it in anymore?
dir.create(tempdir())
