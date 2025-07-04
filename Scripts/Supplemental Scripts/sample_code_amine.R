#walk through of likelihood ratio testing

#example of binomial distribution
#imagine fieldwork, plant sampling  in an area with wilt outbreak
#out of 50 plants, 42 have the disease

# Write a small function to calculate the probability of a specific outcome
likelihood_fun <- function(k, n, p)
{
  # Calculate the likelihood using the formula
  lik <- choose(n, k) * (p^k) * ((1.0 - p)^(n - k))
  
  # Return the result
  return(lik)
}


#---------------------Variance Partitioning Example

# Load the vegan library
library(vegan)
# Load the mite dataset
data(mite) #35 species, 70 sites
data(mite.env) #env variables for each of the 70 sites 
data(mite.xy) #geographical coordinates of 70 sites


# Create tables and consider a Hellinger transformation
Y <- decostand(mite, method="hellinger")#this takes square root of site proportions
                    #Y = species abundance matrix
X <- mite.env[,1:2] #X = env
W <- mite.xy        #W = spatial (geographical coordinates)

View(cbind(X,W))

#multivariate extension of lm
#if Y had a single variable, same result as lm()
rda_xw <- rda(Y, cbind(X, W)) #A + B + C
#cbind here just combines X and Y
rda_x <- rda(Y, X)            #A + B
rda_w <- rda(Y, W)            #B + C


# Compute the adjusted R2 values
abc_frac <- RsquareAdj(rda_xw)$adj.r.squared
ab_frac <- RsquareAdj(rda_x)$adj.r.squared
bc_frac <- RsquareAdj(rda_w)$adj.r.squared

# Compute the adjusted R2 values
a_frac <- abc_frac - bc_frac
c_frac <- abc_frac - ab_frac
b_frac <- ab_frac - a_frac
d_frac <- 1.0 - abc_frac
#
c(a_frac, c_frac, b_frac, d_frac) #manually

# Compute the adjusted R2 values
varpart(Y, X, W)$part$indfract[,3] #gives the same!
#stuff after $is just to clarify it for me... without it
#varpart summary is very complicated

# Plot the fractions of variation
plot(varpart(Y, X, W))

#in my case, replace spatial variables by phylogenetic PCS
#i.e. W = phylo PCs




#-- MCMC tutorial
# For riwish function, you need
library(MCMCpack)
# For the covariance ellipses, you need
library(ellipse)

# Parameters for the Inverse Wishart distribution

#we will use inverse wishart prior for the phylogeny!!


# nu (v): degrees of freedom, needs to be > dim(Psi) or R will throw a fit
# for weak prior, v will be similar to number of species
# Psi (S): Scale matrix, can be anything so long as it is positive semidefinite
# positive semidefinite = eigenvalues non negative (0 or positive)
# for weak prior, psi will be similar to identity matrix (i.e. star tree)
nu <- 5 + 0.001
Psi <- matrix(c(1, 0.01, 0.01, 1), nrow=2, ncol=2)

# Sample 100 covariance matrices from the parametrized Inverse Wishart distribution
n_samples <- 100
samples <- replicate(n_samples, riwish(nu, Psi), simplify = FALSE)

# Begin with empty plot
plot(NA,
     xlim=c(-4,4),
     ylim=c(-4,4),
     xlab="X",
     ylab="Y",
     main="Covariance ellipses from Inverse Wishart samples",
     asp=1)
# Add the first 20 ellipses (you can change this value, so long as it remains
# <= n_samples)
for(i in 1:20){
  lines(ellipse(samples[[i]],
                centre=c(0,0),
                level=0.95),
        col="blue",
        lwd=0.75,
        lty=1)
}
# Add grid for ... prettiness
grid()

#mess with parameters: covariance ellipses 
#can be big and different each time they're plotted 

#sample for generating matrices with every possible combination
expand.grid()
#this will have x from 1 to 3 with each associated to 20 and 21
#6 x 2 matrix
expand.grid(x = 1:3, y = 20:21)
#this will come in handy, as opposed to having multiple scripts


#lost code where we looked at phylogenies with generated data
#according to lambda
#didnt find :(


#biogeom tree niche hypothesis paper
model <- MCMCglmm(Root_Scores ~ 1, random=~phylogeny+species,
                  data= dfasc, pedigree=tree.dfa, nodes='TIPS',
                  family="gaussian", verbose=T, prior=random_intercepts_prior(2),
                  nitt=Nnitt, burnin=Nburnin, thin=Nthin) 
#same prior - they defined their own random_intercepts_prior(2)

#to show inva and solve(vcv) do the same thing
invA <- as.matrix(inverseA(pedigree = ausdata_all_pos_sp_tree, nodes = "TIPS", scale = TRUE)$Ainv)
invVCV <- solve(vcv(phy = ausdata_all_pos_sp_tree))

cor(c(invA), c(invVCV))
plot(c(invA), c(invVCV))
abline(h = 0, v = 0, lty = 2)