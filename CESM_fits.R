#######################################################################
##########################   GRIDWORLD  ###############################

# Takes CESM model predictions (where all 'Other' is zero), brings in Other from PChoice,
# and fits to actual data where people did often cite 'Other'

library(tidyverse)
library(ggplot2)
rm(list=ls())

# Load model predictions generated in 'ecesm_model_preds.R'
load('s_vals.rdata', verbose = T) # loads mps, a df of 1216 obs of 8 vars
# load pChoice world setup 
load('64_world_setup.rdata', verbose = T) 
# Load data from behavioural experiment -- FOR COGSCI23 SUBMISSION THIS STEP REMOVED FOR ID PRIVACY

# Give the 1:64 rows of pChoice an explicit number 
pChoice <- pChoice %>% mutate(ix = 1:n()) %>% dplyr::select(ix, everything())

#Reorder columns: Preference  Knowledge  Character  Start (Other) [ Choice  Path] 
pChoice<-pChoice[,c(1, 3,2,4,5,7,6, 9,8,10:17)]
mps<-mps[,c(1, 3,2,4,5, 6,7,8)]

# Bring tag over from pChoice to mps
mps2 <- merge(x=mps, y=pChoice[ , c("ix", "tag")], by="ix", all.y = TRUE)
# Then reorder to be neat
mps2 <- mps2 %>% dplyr::select(ix, tag, everything())

# Save each s_val subset of mps2 as its own 64*9 dataframe 
subsets <- split(mps2, mps2$s_val)

# Softmax function 
softmax <- function(x, t)
{
  exp(x/t)/sum(exp(x/t))
}

# Function to output NLL: the most important bit
cesmOther_mod <- function(par, dat, mp) 
{
  tau <- exp(par[1])
  eta <- exp(par[2])
  out <- rep(NA, length = nrow(dat)) 
  for (i in 1:nrow(dat))
  {
    tag <- dat[i,2] 
    case <- mp[mp$tag==tag,]
    CES <- case[3:6] # Get the model predictions of the four existing causes 
    p_outcome <- case[[9]] 
    Other <- (1-p_outcome)*eta # For the 'Flat Other' model here is just eta, meaning a flat constant tendency to say Other
    CESOther <- cbind(CES,Other) # Now we have 5 causes
    these_mp <- softmax(CESOther, tau) # Normalise our 5 causes (but these only live here. The subset dfs still have NAs in Other)
    out[i] <- mean(these_mp[dat[i,5:9]==1]) # Gives c.0.1-0.3, as rows tend to have 1s in 1-3 columns
  }
  
  print(-sum(log(out)))
  
  -sum(log(out), na.rm=T) 
}

# Intermediary step to help set up df to store params
dfnames <- c('s005', 's01', 's015', 's02', 's025', 's03', 's035', 's04', 's045', 's05', 's055', 's06', 's065', 's07', 's075', 's08', 's085', 's09', 's095')

# df to store params
params <- data.frame(ij = 1:19, dfname = dfnames, tau = NA, eta = NA, nll = NA)

# Optimise parameters and put them in the df
for (k in 1:19)
{
  opt <- optim(par = c(1,1), fn = cesmOther_mod, dat = XXX, mp = subsets[[k]])
  p <- exp(opt$par)
  params[k,3] <- p[[1]]
  params[k,4] <- p[[2]]
  params[k,5] <- opt$value
}
