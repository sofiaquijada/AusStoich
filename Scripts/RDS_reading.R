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
