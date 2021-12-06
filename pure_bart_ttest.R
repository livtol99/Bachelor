library(R2jags)
library(dplyr)
# setwd("C:/Users/au199986/OneDrive - Aarhus Universitet/Courses/E21Theses/Liv")
getwd()

citation("R2jags")

citation()
dat <- read.csv("readytorun.csv")

dat %>%  group_by(Study_Group) %>% summarise(count = n_distinct(ID))


# #The data set included in the analysis contains:
# # 27 crack users 
# # 18 Healthy adolescents
# # 20 Healthy adults


# -----------------------------------------
ncards <- 32
ntrials <- max(dat$CCT_Round_Tria_Num)


# set probability vectors - to copy into subject matrices according to dat$CCT_Round_LossNum variable
p_win1 <- (seq(32,1,-1)-1)/ #number of winners left  #number of winners left (-1, because at the first choice in the 1 win condition, there are only 31 winners left)
  seq(32,1,-1) # number of cards left

p_win3 <- (seq(32,1,-1)-3)/ #number of winners left
  seq(32,1,-1) # number of cards left

p_win3[31:32] <- 0 # because the last cards two cards cannot be picked. The third last can be picked
# assuming all others were wins, but if it is picked, the trial is ended

#------------------------------------------------------------------------
# ---------- make data matrices for CONTROL group --------------------------
#------------------------------------------------------------------------

# - find control subjects
cont.subID <- unique(dat$ID[dat$Study_Group=="Health Adults"])
cont.nsubs <- length(cont.subID)

# - make empty data arrays to populate
cont.p <- array(0,c(ncards,ntrials,cont.nsubs))
cont.d <- array(0,c(ncards,ntrials,cont.nsubs))

cont.v_loss <- array(0,c(ntrials,cont.nsubs))
cont.v_win <- array(0,c(ntrials,cont.nsubs))

cont.nchoices <- array(0,c(ntrials,cont.nsubs))

# - fill data arrays -------------------------
for (s in 1:cont.nsubs) {
  
  # make data matrices for one subject
  sub_dat <- dat[dat$ID==cont.subID[s] & dat$CCT_Task_Type == "Hot1",]
  
  #- fill probability arrays, dependent on number of loss cards
  for (t in 1:ntrials) {
    
    if (sub_dat$CCT_Round_LossNum[t] == 1)
    {cont.p[,t,s] <- p_win1}
    else 
    {cont.p[,t,s] <- p_win3}
    
  }  
  #-----------------------------------------------------------
  
  # make decision array - will be a ragged array
  d.temp <- t(as.matrix(sub_dat[,17:(17+31)]))
  colnames(d.temp) <- NULL
  rownames(d.temp) <- NULL
  
  # code all non-choices as NA - change from 0
  d.temp[d.temp==0] <- NA
  # code all "opt-in" card flips as 2 - for model specification
  d.temp <- d.temp - 1
  
  # identify bust trials - which trials did the subject actively choose a loss card - no opt-out 
  bust <- sub_dat$CCT_Score==(sub_dat$CCT_NumCardChosen-1)*sub_dat$CCT_Round_WinValue-sub_dat$CCT_Round_LossValue
  
  # find and replace "opt-out" choices - cases where subject decided to choose no more cards
  # code as 1 - for model specification
  for (t in 1:ntrials) {
    
    if (bust[t] == FALSE) {
      d.temp[sub_dat$CCT_NumCardChosen[t],t] = 1
    }
    
  }
  
  cont.d[,,s] <- d.temp
  
  cont.v_loss[,s] <- sub_dat$CCT_Round_LossValue
  cont.v_win[,s] <- sub_dat$CCT_Round_WinValue
  
  cont.nchoices[,s] <- sub_dat$CCT_NumCardChosen
  
}


#------------------------------------------------------------------------
# ---------- make data matrices for TEEN group --------------------------
#------------------------------------------------------------------------

# - find teen subjects
teen.subID <- unique(dat$ID[dat$Study_Group=="Health Adolescents"])
teen.nsubs <- length(teen.subID)

# - make empty data arrays to populate
teen.p <- array(0,c(ncards,ntrials,teen.nsubs))
teen.d <- array(0,c(ncards,ntrials,teen.nsubs))

teen.v_loss <- array(0,c(ntrials,teen.nsubs))
teen.v_win <- array(0,c(ntrials,teen.nsubs))

teen.nchoices <- array(0,c(ntrials,teen.nsubs))

# - fill data arrays -------------------------
for (s in 1:teen.nsubs) {
  
  # make data matrices for one subject
  sub_dat <- dat[dat$ID==teen.subID[s] & dat$CCT_Task_Type == "Hot1",]
  
  #- fill probability arrays, dependent on number of loss cards
  for (t in 1:ntrials) {
    
    if (sub_dat$CCT_Round_LossNum[t] == 1)
    {teen.p[,t,s] <- p_win1}
    else 
    {teen.p[,t,s] <- p_win3}
    
  }  
  #-----------------------------------------------------------
  
  # make decision array - will be a ragged array
  d.temp <- t(as.matrix(sub_dat[,17:(17+31)]))
  colnames(d.temp) <- NULL
  rownames(d.temp) <- NULL
  
  # code all non-choices as NA - change from 0
  d.temp[d.temp==0] <- NA
  # code all "opt-in" card flips as 2 - for model specification
  d.temp <- d.temp-1
  
  # identify bust trials - which trials did the subject actively choose a loss card - no opt-out 
  bust <- sub_dat$CCT_Score==(sub_dat$CCT_NumCardChosen-1)*sub_dat$CCT_Round_WinValue-sub_dat$CCT_Round_LossValue
  
  # find and replace "opt-out" choices - cases where subject decided to choose no more cards
  # code as 1 - for model specification
  for (t in 1:ntrials) {
    
    if (bust[t] == FALSE) {
      d.temp[sub_dat$CCT_NumCardChosen[t],t] = 1
    }
    
  }
  
  teen.d[,,s] <- d.temp
  
  teen.v_loss[,s] <- sub_dat$CCT_Round_LossValue
  teen.v_win[,s] <- sub_dat$CCT_Round_WinValue
  
  teen.nchoices[,s] <- sub_dat$CCT_NumCardChosen
  
}

#------------------------------------------------------------------------
# ---------- Apply t.test: TEENS VERSUS CONTROLS ---------------------
#------------------------------------------------------------------------

A.p <- 1-cont.p[1,,]
A.d <- cont.d
A.v_loss <- cont.v_loss
A.v_win <- cont.v_win
A.nchoices <- cont.nchoices
A.nsubs <- cont.nsubs

B.p <- 1-teen.p[1,,]
B.d <- teen.d
B.v_loss <- teen.v_loss
B.v_win <- teen.v_win
B.nchoices <- teen.nchoices
B.nsubs <- teen.nsubs

B.nchoices[10,16] <- 31 # edge correction - can't make 32 choices

data <- list("ntrials","A.nchoices","A.nsubs","A.p","A.d",
             "B.nchoices","B.nsubs","B.p","B.d") #data inputted into jags

params <- c("A.mu_tau_log","A.mu_g_ref_log","A.mu_tau","A.mu_g_ref",
            "B.mu_tau_log","B.mu_g_ref_log","B.mu_tau","B.mu_g_ref",
            "mu_tau_log","mu_g_ref_log","delta_tau","delta_g_ref") #parameters we'll track in jags

teenvscontrols.ttest.samples <- jags(data, inits=NULL, params,
                           model.file ="pure_bart_ttest.txt",
                           n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)

#Looking at the output of the t-test


teenvscontrols.ttest.samples

traceplot(teenvscontrols.ttest.samples)

# do density plots plot(density (samples)) plot prior together 
# plot (density(rnorm(1000, 0, 1)))
#traceplots ion appendix
#rhat how well did the chains mix?
# report rhgat
pacman::p_load(mcmcr)


plot(as.mcmc(teenvscontrols.ttest.samples))
#------------------------------------------------------------------------
# ---------- make data matrices for CRACK group --------------------------
#------------------------------------------------------------------------

# - find crack users
crac.subID <- unique(dat$ID[dat$Study_Group=="Crack Users"])
crac.nsubs <- length(crac.subID)

# - make empty data arrays to populate
crac.p <- array(0,c(ncards,ntrials,crac.nsubs))
crac.d <- array(0,c(ncards,ntrials,crac.nsubs))

crac.v_loss <- array(0,c(ntrials,crac.nsubs))
crac.v_win <- array(0,c(ntrials,crac.nsubs))

crac.nchoices <- array(0,c(ntrials,crac.nsubs))

# - fill data arrays -------------------------
for (s in 1:crac.nsubs) {
  
  # make data matrices for one subject
  sub_dat <- dat[dat$ID==crac.subID[s] & dat$CCT_Task_Type == "Hot1",]
  
  #- fill probability arrays, dependent on number of loss cards
  for (t in 1:ntrials) {
    
    if (sub_dat$CCT_Round_LossNum[t] == 1)
    {crac.p[,t,s] <- p_win1}
    else 
    {crac.p[,t,s] <- p_win3}
    
  }  
  #-----------------------------------------------------------
  
  # make decision array - will be a ragged array
  d.temp <- t(as.matrix(sub_dat[,17:(17+31)]))
  colnames(d.temp) <- NULL
  rownames(d.temp) <- NULL
  
  # code all non-choices as NA - change from 0
  d.temp[d.temp==0] <- NA
  # code all "opt-in" card flips as 2 - for model specification
  d.temp <- d.temp -1
  
  # identify bust trials - which trials did the subject actively choose a loss card - no opt-out 
  bust <- sub_dat$CCT_Score==(sub_dat$CCT_NumCardChosen-1)*sub_dat$CCT_Round_WinValue-sub_dat$CCT_Round_LossValue
  
  # find and replace "opt-out" choices - cases where subject decided to choose no more cards
  # code as 1 - for model specification
  for (t in 1:ntrials) {
    
    if (bust[t] == FALSE) {
      d.temp[sub_dat$CCT_NumCardChosen[t],t] = 1
    }
    
  }
  
  crac.d[,,s] <- d.temp
  
  crac.v_loss[,s] <- sub_dat$CCT_Round_LossValue
  crac.v_win[,s] <- sub_dat$CCT_Round_WinValue
  
  crac.nchoices[,s] <- sub_dat$CCT_NumCardChosen
  
}

#------------------------------------------------------------------------
# ---------- Apply t.test to crack group ---------------------
#------------------------------------------------------------------------

B.p <- 1-crac.p[1,,]
B.d <- crac.d
B.v_loss <- crac.v_loss
B.v_win <- crac.v_win
B.nchoices <- crac.nchoices
B.nsubs <- crac.nsubs

#B.nchoices[10,16] <- 31 # edge correction - can't make 32 choices

data <- list("ntrials","A.nchoices","A.nsubs","A.p","A.d",
             "B.nchoices","B.nsubs","B.p","B.d") #data inputted into jags

params <- c("A.mu_tau_log","A.mu_g_ref_log","A.mu_tau","A.mu_g_ref",
            "B.mu_tau_log","B.mu_g_ref_log","B.mu_tau","B.mu_g_ref",
            "mu_tau_log","mu_g_ref_log","delta_tau","delta_g_ref") #parameters we'll track in jags

crac.ttest.samples <- jags(data, inits=NULL, params,
                           model.file ="pure_bart_ttest.txt",
                           n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)


