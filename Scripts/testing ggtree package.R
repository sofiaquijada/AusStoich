library(ggimage)
library(ggtree)
devtools::install_github("YuLab-SMU/TDbook")
library(TDbook)

# load `tree_boots`, `df_tip_data`, and `df_inode_data` from 'TDbook'
p <- ggtree(tree_boots) %<+% df_tip_data + xlim(-.1, 4)
p2 <- p + geom_tiplab(offset = .6, hjust = .5) +
  geom_tippoint(aes(shape = trophic_habit, color = trophic_habit, 
                    size = mass_in_kg)) + 
  theme(legend.position = "right") + 
  scale_size_continuous(range = c(3, 10))

p2 %<+% df_inode_data + 
  geom_label(aes(label = vernacularName.y, fill = posterior)) + 
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(3, "YlGnBu"))

p <- ggtree(tree_boots) + geom_tiplab() + xlim(-.1, 4)


#example of merging tbl_tree object to external data
d <- tibble(label = paste0('t', 1:4),
            trait = rnorm(4))
y <- full_join(x, d, by = 'label')

library(ggtree)
library(TDbook)

#example of coloring tree branches by continuous or discrete trait
#includes legend with color gradient for that trait
ggtree(beast_tree, aes(color=rate)) +
  scale_color_continuous(low='darkgreen', high='red') +
  theme(legend.position="right")


#example of plotting tree with data
## load `tree_nwk`, `df_info`, `df_alleles`, and `df_bar_data` from 'TDbook'
tree <- tree_nwk
snps <- df_alleles
snps_strainCols <- snps[1,] 
snps<-snps[-1,] # drop strain names
colnames(snps) <- snps_strainCols

gapChar <- "?"
snp <- t(snps)
lsnp <- apply(snp, 1, function(x) {
  x != snp[1,] & x != gapChar & snp[1,] != gapChar
})
lsnp <- as.data.frame(lsnp)
lsnp$pos <- as.numeric(rownames(lsnp))
lsnp <- tidyr::gather(lsnp, name, value, -pos)
snp_data <- lsnp[lsnp$value, c("name", "pos")]

## visualize the tree 
p <- ggtree(tree) 

## attach the sampling information data set 
## and add symbols colored by location
p <- p %<+% df_info + geom_tippoint(aes(color=location))

## visualize SNP and Trait data using dot and bar charts,
## and align them based on tree structure
p + geom_facet(panel = "SNP", data = snp_data, geom = geom_point, 
               mapping=aes(x = pos, color = location), shape = '|') +
  geom_facet(panel = "Trait", data = df_bar_data, geom = geom_col, 
             aes(x = dummy_bar_value, color = location, 
                 fill = location), orientation = 'y', width = .6) +
  theme_tree2(legend.position=c(.05, .85))

