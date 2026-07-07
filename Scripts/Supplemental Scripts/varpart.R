library(ape)
library(ggplot2)
library(vegan)
library(nlme)
library(car)
library(adespatial)
library(usdm)
library(corrplot)
library(dplyr)

#Borcard & Legendre method

#check data completeness
#how many rows for complete NP, CN, CP?

complete_data <- aus_data %>% 
  drop_na() #1298 rows
#there can be NAs in CP, but not in NP

# 1.Ordinary linear regression y ~ explanatory variables
#a) variable selection procedure: ordistep() from vegan
#                                 forward.sel() from adespatial
#b) colinearity. vif()
#trait of interest: ratios 


#validate normality

#untransformed ratios kind of right skewed
ggplot(data = aus_data, mapping = aes(x = NP_ratio)) +
  geom_histogram() +
  theme_minimal() +
  ggplot(data = aus_data, mapping = aes(x = CN_ratio)) +
  geom_histogram() +
  theme_minimal() +
  ggplot(data = aus_data, mapping = aes(x = CP_ratio)) +
  geom_histogram() +
  theme_minimal()

#logged ratios
ggplot(data = aus_data, mapping = aes(x = ln_NP_ratio)) +
  geom_histogram() +
  theme_minimal() +
  ggplot(data = aus_data, mapping = aes(x = ln_CN_ratio)) +
  geom_histogram() +
  theme_minimal() +
  ggplot(data = aus_data, mapping = aes(x = ln_CP_ratio)) +
  geom_histogram() +
  theme_minimal()
#better


#---------------------- OLS

#scale predictors
predictor_columns <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                       "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                       "precipitation_seasonality", "temp_seasonality")
data <- aus_data
data[predictor_columns] <- scale(aus_data[predictor_columns])

corrplot(cor(data[predictor_columns]))

categorical_columns <- c("woodiness", "reclass_life_history", "putative_BNF",
                         "myc_type")

set.seed(22) #to make sure R2 and p value are the same every time

#this includes categories!
a <- lm(ln_NP_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                      CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                      precipitation_seasonality + temp_seasonality +
          woodiness + reclass_life_history + putative_BNF+ myc_type,
        data = data) 
summary(a)
vif(a) #same values as the other ones for env variables, so it does work

#NP ratio ------------------------------------------

complete_lnNP <- lm(ln_NP_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                  CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                  precipitation_seasonality + temp_seasonality, data = data)
summary(complete_lnNP) #R2 = 0.175
car::vif(complete_lnNP)
barplot(car::vif(complete_lnNP), main = "Variance Inflation Factor (VIF)")
#unsig: temp_seasonality

#try forward selection----

#select df with just trait of interest, then remove NAs
trait_env <- data %>% select("ln_NP_ratio", predictor_columns) 
trait_env <- na.omit(trait_env)

Y_NP <- trait_env["ln_NP_ratio"]
X_NP <- trait_env[predictor_columns]
#remove predictors who are not significant in global model
X_NP$temp_seasonality <- NULL

vif(as.data.frame(X_NP))
vifstep(as.data.frame(X_NP), th = 10, method = "pearson")
#remove predictors with vif over threshold
remaining <- vifstep(as.data.frame(X_NP),
                     th = 10, method = "pearson")@results$Variables
X_NP <- X_NP[,remaining] #keep remaining


forward.sel(Y = Y_NP, X = X_NP,
            adjR2thresh = summary(complete_lnNP)$adj.r.squared)
remaining <- forward.sel(Y = Y_NP,
        X = X_NP,adjR2thresh = summary(complete_lnNP)$adj.r.squared)$variables
#remove predictors who don't significantly contribute to explained variance
X_NP <- X_NP[,remaining]

NP_rda <- rda(Y_NP, X_NP) #A + B
summary(NP_rda)
RsquareAdj(NP_rda)$adj.r.squared #0.15

paste("y ~", paste(predictors, collapse = " + "))

#should be the same R2
check <- lm(as.formula(paste("ln_NP_ratio ~", paste(names(X_NP), collapse = " + ")))
                       ,data = data) 
summary(check)#0.15, all good

#Y and X must have same number of rows
NP_phylo_rda <- rda(Y_NP, ausdata_PCs) 
#PCs = one row per species
#from ausdataPCs, prune them st. only the ones included 
#only one value per species.....noooo

#NP_phylo_lm <-  lm(ln_NP_ratio ~ ausdata_PC columns, data = data)


#CN ratio ---------------------------------------------
complete_lnCN <- lm(ln_CN_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality, data = data)
summary(complete_lnCN) #R2 = 0.1207
plot(complete_lnCN)
car::vif(complete_lnCN)
barplot(car::vif(complete_lnCN, main = "Variance Inflation Factor (VIF)"))
#unsig: SN, precip_seasonality and temp_seasonality

trait_env <- data %>% select("ln_CN_ratio", predictor_columns) 
trait_env <- na.omit(trait_env)

Y_CN <- trait_env["ln_CN_ratio"]
X_CN <- trait_env[predictor_columns]
#remove predictors who are not significant in global model
X_CN$SN_total_0_30 <- NULL
X_CN$precipitation_seasonality <- NULL
X_CN$temp_seasonality <- NULL

vif(as.data.frame(X_CN))
vifstep(as.data.frame(X_CN), th = 10, method = "pearson")
#remove predictors with vif over threshold
remaining <- vifstep(as.data.frame(X_CN),
                     th = 10, method = "pearson")@results$Variables
X_CN <- X_CN[,remaining] #keep remaining


forward.sel(Y = Y_CN, X = X_CN,
            adjR2thresh = summary(complete_lnCN)$adj.r.squared)
remaining <- forward.sel(Y = Y_CN,
                         X = X_CN,adjR2thresh = summary(complete_lnCN)$adj.r.squared)$variables
#remove predictors who don't significantly contribute to explained variance
X_CN <- X_CN[,remaining]

CN_rda <- rda(Y_CN, X_CN) #A + B
summary(CN_rda)
RsquareAdj(CN_rda)$adj.r.squared #0.11



#CP ratio ---------------------------------------------
complete_lnCP <- lm(ln_CP_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality, data = data)
summary(complete_lnCP) #R2 = 0.4143
plot(complete_lnCP)
#variance inflation factor
car::vif(complete_lnCP)
barplot(car::vif(complete_lnCP), main = "Variance Inflation Factor (VIF)")
#unsig: SN, MAT, AET

trait_env <- data %>% select("ln_CP_ratio", predictor_columns) 
trait_env <- na.omit(trait_env)

Y_CP <- trait_env["ln_CP_ratio"]
X_CP <- trait_env[predictor_columns]
#remove predictors who are not significant in global model
X_CP$SN_total_0_30 <- NULL
X_CP$MAT <- NULL
X_CP$AET <- NULL

vif(as.data.frame(X_CP))
vifstep(as.data.frame(X_CP), th = 10, method = "pearson")
#remove predictors with vif over threshold
remaining <- vifstep(as.data.frame(X_CP),
                     th = 10, method = "pearson")@results$Variables
X_CP <- X_CP[,remaining] #keep remaining


forward.sel(Y = Y_CP, X = X_CP,
            adjR2thresh = summary(complete_lnCP)$adj.r.squared)
remaining <- forward.sel(Y = Y_CP,
                         X = X_CP,adjR2thresh = summary(complete_lnCP)$adj.r.squared)$variables
#remove predictors who don't significantly contribute to explained variance
X_CP <- X_CP[,remaining]

CP_rda <- rda(Y_CP, X_CP) #A + B
summary(CP_rda)
RsquareAdj(CP_rda)$adj.r.squared #0.40 is high 



#----------------------------
ausdata_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))
d <- as.dist(cophenetic(ausdata_tree))
ausdata_PCs <- ape::pcoa(d)$vectors

#Code from amine for Y~ phylo PCs 
cophenetic(rcoal(20)) #rcoal generates random trees
tr <- rcoal(20)
D <- as.dist(cophenetic(tr)) #d as pairwise distances

pcoa(D) #PCs of distance matrix 

#extract eigenvectors
pcoa(D)$vectors
plot(pcoa(D)$vectors[,1])
plot(tr)

 
library(ape)
#
tr <- rcoal(20)
D <- as.dist(cophenetic(tr))
U <- pcoa(D)$vectors
#
k <- 1
plot(tr)
points(rep(4, 20), 1:20, cex=abs(U[,k])/2, pch=ifelse(U[,k] < 0, 1, 16))
plot(dist(U), as.dist(cophenetic(tr)))


tree <- rcoal(20)

#-------------------code that simulates lambda
plot_lambda <- function(tr, lam)
{
  V <- vcv(tr)
  d <- diag(V)
  V <- lam*V
  diag(V) <- d
  tree_lambda <- vcv2phylo(V)
  plot(tree_lambda)
}

plot_lambda(tree, 1)
plot_lambda(tree, 0.5)
plot_lambda(tree, 0.0001)
#how likely trait is to evolve under BM
#felsestein


plot_lambda(ausdata_tree, 0.4, show.tip.label = FALSE)
plot(ausdata_tree)
tree <- rcoal(20)

plot_lambda <- function(tr, lam, ...)
{
  V <- vcv(tr)
  d <- diag(V)
  V <- lam*V
  diag(V) <- d
  tree_lambda <- vcv2phylo(V)
  plot(tree_lambda, ...) #promise argument!!!!!!
}
#promise argument: to include arguments from whatever function 
#without having to specify them
#here used so that show.tip.label works

plot_lambda(ausdata_tree, 0.4, show.tip.label = FALSE)
plot_lambda(tree, 0.5)