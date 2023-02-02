#######################################################################
##########################   GRIDWORLD  ###############################
# Implementation of Quillien and Lucas 2022 CESM

# Takes the 64 combinations of gridworld factors
# and runs counterfactual model on them, with a range of s params 0.05-0.95, saving model predictions
# To then fit to actual data, go to script 'CESM_fits.R' 

library(doParallel)
library(foreach)

rm(list=ls())

cl <- makeCluster(10)
registerDoParallel(cl)
print(getDoParWorkers())

#COUNTERFACTUAL EFFECT SIZE MODEL
out <- foreach(s=1:19) %dopar%
  {
    library(tidyverse)
    load('64_world_setup', verbose = T) 
    
    causes<-c('Knowledge','Preference','Character','Start')
    s_vals<-seq(0.05, 0.95, 0.05)
    

    
    
    N_cf <- 1000 #How many counterfactual samples to draw. Can reduce this for speed
    
    #A place to keep the model predictions
    mp<-data.frame(ix = 1:64, Knowledge = NA, Preference = NA, Character = NA, Start = NA, 
                   Other = NA, s_val = s_vals[s], p_outcome = NA)
    
    #Loop through cases
    for (c_ix in 1:64)
    {
      #The current case
      case<-pChoice[c_ix,]
      mp[c_ix, 8] <- case$p_action # p_action is the probability of the outcome from behavioural Experiment 1
      
      #Generate a sample of N counterfactuals
      cfs<-data.frame(Knowledge = rep(NA, N_cf),
                      Preference = rep(NA, N_cf),
                      Character = rep(NA, N_cf),
                      Start = rep(NA, N_cf),
                      
                      Path = rep(NA, N_cf),
                      Choice = rep(NA, N_cf),
                      
                      Match = rep(NA, N_cf))
      for (i in 1:N_cf)
      {
        #Counterfactual case setup (the causes only)
        #They should be biased toward the actual world,
        
        cf_cs<-as.numeric(case[1:4])
        #So start with the actual world and flip them with probability 1-s
        flip<-runif(4)>s_vals[s] # Low stability makes it more likely to flip 
        cf_cs[flip]<- 3-as.numeric(cf_cs[flip]) #Switches 1s to 2s and 2s to 1s for the flipped cases, to fit with R factors
        
        #Pull out the corresponding four outcomes (to see their probabilities)
        cf_cases<-pChoice %>% filter(as.numeric(Knowledge)==cf_cs[1],
                                     as.numeric(Preference)==cf_cs[2],
                                     as.numeric(Character)==cf_cs[3],
                                     as.numeric(Start)==cf_cs[4])
        #Sample one outcome according to its probability
        cf_out_ix<-sample(x=1:4, size = 1, p=cf_cases$p_action)
        
        cf<-cf_cases[cf_out_ix,1:6]
        #And check if it (exactly) matches the true outcome
        cf$Match = cf$Choice==case$Choice & cf$Path==case$Path
        #Add it to the collection
        cfs[i,]<-cf
      }
      #Now calculate how much the effect depends (directly) on each cause across these counterfactual worlds
      cor_sizes<-rep(NA, 4)
      for (cause in 1:4)
      {
        #Across the counterfactuals how much more common is the outcome with vs without each factor
        cor_sizes[cause]<-cor(cfs[[causes[cause]]], cfs$Match) * (c(-1,1)[as.numeric(case[[causes[cause]]])])
      }
      
      mp[c_ix,2:5]<-cor_sizes
      
      cat(c_ix, cor_sizes, '\n')
    }
    

    save(file=paste0('s_vals_partial', s, '.rdata'), mp)
    mp
  }   


rm(list=ls())
load('s_vals_partial1.rdata', verbose = T)
mps<-mp
for (s in 2:19)
{
  load(paste0('s_vals_partial', s, '.rdata'), verbose = T)
  mps<-rbind(mps, mp)
}
head(mps, 19)
tail(mps,19)
save(file='s_vals.rdata', mps)

