#fab only analyses


fab_tree_tib

ggplot(data = fab_tree_tib) + geom_histogram(mapping = aes(x = avg_leaf_N))


#-- Linkage of data to phylo object with treeio
#need info df with column "label" then trait data as columns
names(avg_fabdata)[1] <- "label"
attempt_fab_tree <- full_join(as.treedata(fab_tree),
                        avg_fabdata, by = "label")
attempt_fab_tree  #tree object with nutrient data associated
View(as.tibble(attempt_fab_tree))
get.data(attempt_fab_tree) #to extract data from phylo

ggtree(attempt_fab_tree, aes(color = avg_leaf_N), layout = "circular") +
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  geom_tiplab(size = 0.5) #can manually set to black if needed

tree_tib = "fab_data_tib"
if (tree_tib== "fab_data_tib") {
  cut = 189
  tree_tib = fab_tree_tib
  tree = fab_tree
}

trait <- "avg_leaf_N"
trait_data <- extract_trait_values(tree_tib, "label", trait, cut)
lambda <- phylosig(tree, trait_data, method = "lambda", test = TRUE)

phylosig(fab_tree, x = fab_tree_tib$ln_leaf_N,
                    method = "lambda", test = TRUE)

#MCMCglmm
library(ape)
library(tidyverse)
library(MCMCglmm)
library(coda)
library(ggtree)
library(tictoc)
library(dplyr)
library(ggplot2)
library(corrplot)

#data import -------------------------------------------------------------------

#set working directory as needed
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")

#--read in trait data
aus_data <- read_csv("Data/aus_data.csv")

#scale continuous predictors for comparable estimates
cont_predictors <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

cat_predictors <- c("myc_type", "woodiness", "putative_BNF", "reclass_life_history")

predictors <- c(cont_predictors, cat_predictors)

aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#--read in tree data
ausdata_tree <- read.tree("Inputs/Trees/ausdata.tre")

#sanity check, inverseA requires ultrametric trees
is.ultrametric(ausdata_tree) #TRUE

#look at node labels
table(ausdata_tree$node.label)
#remove internal node labels ""
ausdata_tree$node.label <- NULL 

#inverted phylogenetic covariance matrix
phyloinv_complete <- inverseA(ausdata_tree, nodes ="TIPS",scale=TRUE) #tree with some uncertain nodes
#error: phylogeny needs to be rooted: if i do it only on fab data..

#--prep for MCMCglmm
#get column for phylo tip labels, and another for species
aus_data$phylo <- aus_data$species_binom

#prune aus_data to include exclusively species in tree of choice
#necessary step for tree with complete resolution
fab_data <- aus_data %>% 
  filter(species_binom %in% fab_tree$tip.label)
#for uncertain tree, aus_data object as is will suffice

#ensure columns are factors
fab_data$phylo <- factor(fab_data$phylo)
fab_data$species_binom <- factor(fab_data$species_binom)


#family column causes issues
fab_data <- fab_data%>%
  rename(fam = family)
fab_data$family <- NULL


#MCMCglmm requires dataframe
#or else will say: some levels of phylo do not have a row entry in ginverse
all(levels(fab_data$phylo) %in% rownames(phyloinv_complete$Ainv)) #TRUE

fab_data<- as.data.frame(fab_data)

#MCMCglmm can't have any NAs in fixed predictors
fab_data <- fab_data %>%
  filter(!is.na(myc_type) & !is.na(woodiness))



#variable selection ------------------------------------------------------------
env <- as.data.frame(fab_data[predictors])
env <- env %>% mutate(across(all_of(cat_predictors), as.character))

#compute VIF for continuous predictors
diag(solve(cor(env[cont_predictors])))

#plot correlated variables
corrplot(cor(fab_data[cont_predictors]))

#Highly colinear: AET-PPT, AET-temp_seasonality. Remove AET
env$AET <- NULL
cont_predictors <- cont_predictors[cont_predictors != "AET"]
diag(solve(cor(env[cont_predictors])))
#all VIFs below or marginally above 10 once AET removed
fab_data$AET <- NULL


#model matrix for colinearity between categorical and continuous variables


#trait distributions to check normality assumption
ggplot(data = fab_data) +
  geom_histogram(mapping = aes(x = leaf_C_per_dry_mass)) +
  theme_minimal()

#MCMCglmm-----------=-----------------------------------------------------------


#inverse wishart prior for phylogeny
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 1), #for phylo
    G2 = list(V = 1, nu = 1)  #for species
  ),
  R = list(V = 1, nu = 1) #residual 
)


Nnitt = 10000
Nburnin = 1000
Nthin = 20


#specify path and trait choice
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")
filepath <- "Results/fab"
response <- "ln_leaf_P" #next do leaf P!!!


#following loop will run three chains for desired trait + outputs & diagnostics
for (i in 1:3) {
  
  model_formula <- as.formula(
    paste0(response, " ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 + ",
           "CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + ",
           "precipitation_seasonality + temp_seasonality + ",
           "reclass_life_history")
  )
  
  chain <- MCMCglmm(
    model_formula,
    random = ~ phylo + species_binom,
    family = "gaussian",
    ginverse = list(phylo = phyloinv_complete$Ainv),
    prior = prior_phylo,
    data = fab_data,
    nitt = Nnitt, burnin = Nburnin, thin = Nthin
  )
  
  #save text summary for ease of access
  sink(file.path(filepath, paste0(response, "_chain", i, "_summary.txt")))
  print(summary(chain))
  sink()
  
  #save chain diagnostics
  sink(file.path(filepath, paste0(response, "_chain", i, "_heidel.txt")))
  print(heidel.diag(chain$Sol))
  print(heidel.diag(chain$VCV))
  sink()
  
  #save variance partitioning
  sink(file.path(filepath, paste0(response, "_chain", i, "_varpart.txt")))
  print(get_info(chain))
  sink()
  
  #save chain solutions and model
  saveRDS(chain, file = file.path(filepath, paste0(response, "_chain", i, ".RDS")))
  write.csv(chain$Sol, file = file.path(filepath, paste0(response, "_chain", i, "_Sol.csv")))
  write.csv(chain$VCV, file = file.path(filepath, paste0(response, "_chain", i, "_VCV.csv")))
  
  #store chain
  assign(paste0("chain", i), chain)
  
  count = 0 + i
  
  #once 3 chains have run, create combined chain object
  #save gelman.diag + HPD intervals
  if (count == 3){
    combined_chains_sol <- mcmc.list(chain1$Sol, chain2$Sol, chain3$Sol)
    combined_chains_VCV <- mcmc.list(chain1$VCV, chain2$VCV, chain3$VCV)
    
    sink(file.path(filepath, paste0(response, "_gelman.diag_sol.txt")))
    print(gelman.diag(combined_chains_sol))
    sink()
    
    sink(file.path(filepath, paste0(response, "_gelman_diag_VCV.txt")))
    print(gelman.diag(combined_chains_VCV))
    sink()
    
    sink(file.path(filepath, paste0(response, "HPD_interval_sol.txt")))
    print(HPDinterval(combined_chains_sol))
    sink()
    
    sink(file.path(filepath, paste0(response, "HPD_interval_VCV.txt")))
    print(HPDinterval(combined_chains_VCV))
    sink()
  }
}



