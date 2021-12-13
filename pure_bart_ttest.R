library(R2jags)
library(dplyr)

pacman::p_load(bayesplot, MCMCvis)

# setwd("C:/Users/au199986/OneDrive - Aarhus Universitet/Courses/E21Theses/Liv")
getwd()

#citation("R2jags")

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
  sub_dat <- dat[dat$ID==cont.subID[s] & dat$CCT_Task_Type == "Hot2",]
  
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
  sub_dat <- dat[dat$ID==teen.subID[s] & dat$CCT_Task_Type == "Hot2",]
  
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
  sub_dat <- dat[dat$ID==crac.subID[s] & dat$CCT_Task_Type == "Hot2",]
  
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


########################################################################
#------------------- All results together ------------------------------
########################################################################


####-----------T.tests-----------------####
#------  Controls vs. teens --------

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

#B.nchoices[10,16] <- 31 # edge correction - can't make 32 choices

data <- list("ntrials","A.nchoices","A.nsubs","A.p","A.d",
             "B.nchoices","B.nsubs","B.p","B.d") #data inputted into jags

params <- c("A.mu_tau","A.mu_g_ref",
            "B.mu_tau","B.mu_g_ref",
            "mu_tau_log","mu_g_ref_log","delta_tau","delta_g_ref") #parameters we'll track in jags

controlsvsteens.ttest.samples <- jags(data, inits=NULL, params,
                                     model.file ="pure_bart_ttest.txt",
                                     n.chains=4, n.iter=5000, n.burnin=1000, n.thin=1)


## ---- Controls vs. crack --------###

A.p <- 1-cont.p[1,,]
A.d <- cont.d
A.v_loss <- cont.v_loss
A.v_win <- cont.v_win
A.nchoices <- cont.nchoices
A.nsubs <- cont.nsubs


B.p <- 1-crac.p[1,,]
B.d <- crac.d
B.v_loss <- crac.v_loss
B.v_win <- crac.v_win
B.nchoices <- crac.nchoices
B.nsubs <- crac.nsubs

#B.nchoices[10,16] <- 31 # edge correction - can't make 32 choices

data <- list("ntrials","A.nchoices","A.nsubs","A.p","A.d",
             "B.nchoices","B.nsubs","B.p","B.d") #data inputted into jags

params <- c("A.mu_tau","A.mu_g_ref",
            "B.mu_tau","B.mu_g_ref",
            "mu_tau_log","mu_g_ref_log","delta_tau","delta_g_ref") #parameters we'll track in jags

controlsvscrack.ttest.samples <- jags(data, inits=NULL, params,
                                     model.file ="pure_bart_ttest.txt",
                                     n.chains=4, n.iter=5000, n.burnin=1000, n.thin=1)



## ---------Teens vs. Crack---------###
A.p <- 1-teen.p[1,,]
A.d <- teen.d
A.v_loss <- teen.v_loss
A.v_win <- teen.v_win
A.nchoices <- teen.nchoices
A.nsubs <- teen.nsubs

B.p <- 1-crac.p[1,,]
B.d <- crac.d
B.v_loss <- crac.v_loss
B.v_win <- crac.v_win
B.nchoices <- crac.nchoices
B.nsubs <- crac.nsubs


#B.nchoices[10,16] <- 31 # edge correction - can't make 32 choices

data <- list("ntrials","A.nchoices","A.nsubs","A.p","A.d",
             "B.nchoices","B.nsubs","B.p","B.d") #data inputted into jags

params <- c("A.mu_tau","A.mu_g_ref",
            "B.mu_tau","B.mu_g_ref",
            "mu_tau_log","mu_g_ref_log","delta_tau","delta_g_ref") #parameters we'll track in jags

teensvscrack.ttest.samples <- jags(data, inits=NULL, params,
                                  model.file ="pure_bart_ttest.txt",
                                  n.chains=4, n.iter=5000, n.burnin=1000, n.thin=1)



##################################################################3
###----------- Output of the t-tests ---------------####
######################################################################

controlsvsteens.ttest.samples

controlsvscrack.ttest.samples

teensvscrack.ttest.samples



#### Credible intervals#####3
MCMCplot(controlsvsteens.ttest.samples,
         params = c("delta_g_ref", "delta_tau"),
         labels = c(expression(paste(delta,gamma)), expression(paste(delta, Tau))),
         ci = c(.01, 95),
         ref_ovl = TRUE,
         rank = TRUE,
         main = " Controls vs. Adolescents",
         guide_axis = F,
         sz_ax = 1,
         xlab = "Estimate Value",
         sz_ax_txt = 1,
         sz_tick_txt = 1,
         sz_labels = 1.1,
         xlim = c(-3,3))

MCMCplot(controlsvscrack.ttest.samples,
         params = c("delta_g_ref", "delta_tau"),
         labels = c(expression(paste(delta,gamma)), expression(paste(delta, Tau))),
         ci = c(.01, 95),
         col = c('blue', 'blue'),
         rank = TRUE,
         main = " Controls vs. Crack Users",
         guide_axis = F,
         sz_ax = 1,
         xlab = "Estimate Value",
         sz_ax_txt = 1,
         sz_tick_txt = 1,
         sz_labels = 1.1,
         xlim = c(-3,3))

MCMCplot(teensvscrack.ttest.samples,
         params = c("delta_g_ref", "delta_tau"),
         labels = c(expression(paste(delta,gamma)), expression(paste(delta, Tau))),
         ci = c(.01, 95),
         ref_ovl = TRUE,
         rank = TRUE,
         main = "Adolescents vs. Crack Users",
         guide_axis = F,
         sz_ax = 1,
         xlab = "Estimate Value",
         sz_ax_txt = 1,
         sz_tick_txt = 1,
         sz_labels = 1.1,
         xlim = c(-3,3))



controlsvscrack.ttest.samples

############
# PRIOR PLOT MUST BE FIXED HERE!
# Density plots with prior and posterior together
prior <- dnorm(0,0,1)

#lines(density(controlsvsteens.ttest.samples$BUGSoutput$sims.list$delta_tau), col = 'blue') #posterior


MCMCtrace(controlsvsteens.ttest.samples,        # Controls and teens, delta gamma
          params = c("delta_g_ref"),
          exact = TRUE,
          ISB = FALSE,
          type = 'density',
          priors = prior,
          pdf = FALSE,
          Rhat = TRUE)

MCMCtrace(controlsvsteens.ttest.samples,        # Controls and teens, delta tau
          params = c("delta_tau"),
          exact = TRUE,
          ISB = FALSE,
          type = 'density',
          priors = prior,
          pdf = FALSE,
          Rhat = TRUE)


    
MCMCtrace(controlsvscrack.ttest.samples,        # Controls and crack users, delta gamma
          params = c("delta_g_ref"),
          exact = TRUE,
          ISB = FALSE,
          type = 'density',
          priors = prior,
          pdf = FALSE,
          Rhat = TRUE)


MCMCtrace(controlsvscrack.ttest.samples,        # Controls and crack users, delta tau
          params = c("delta_tau"),
          exact = TRUE,
          ISB = FALSE,
          type = 'density',
          priors = prior,
          pdf = FALSE,
          Rhat = TRUE)

MCMCtrace(teensvscrack.ttest.samples,           # Teens vs crack, delta gamma
          params = c("delta_g_ref"),
          exact = TRUE,
          ISB = FALSE,
          type = 'density',
          priors = prior,
          pdf = FALSE,
          Rhat = TRUE)

MCMCtrace(teensvscrack.ttest.samples,           # Teens vs crack, delta tau
          params = c("delta_tau"),
          exact = TRUE,
          ISB = FALSE,
          type = 'density',
          priors = prior,
          pdf = FALSE,
          Rhat = TRUE)



# MCMC table

MCMCsummary(controlsvscrack.ttest.samples, round = 2,
            params = c("A.mu_g_ref", "A.mu_tau", "B.mu_g_ref", "B.mu_tau", "delta_g_ref", "delta_tau"),
            n.eff = F)


##### Bayes Factor for delta
pacman::p_load(logspline)

prior <- dnorm(0,0,1) #prior for delta for both tau and Gamma

#fit.posterior <- logspline(samples$BUGSoutput$sims.list$parameter) #update this with your posterior for delta

#posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0 using a logspline fit

#BF <- prior/posterior # the ratio of the prior to the posterior at the value for zero – how confident you are that there is an effect
# If BF is below 1, we believe the null hypothesis more.
# Above 3 is worth talking about
# Below 0.3 or 1/3, then we believe the null hypothesis more


## Controls vs. teens
# delta gamma = 1.041 (Not worth more than a bare mention)
fit.posterior <- logspline(controlsvsteens.ttest.samples$BUGSoutput$sims.list$delta_g_ref)
posterior <-  dlogspline(0, fit.posterior)
BF <- prior/posterior


# delta tau = 4.811 (Substansial evidence for there being a difference, H1. Worth talking about)
fit.posterior <- logspline(controlsvsteens.ttest.samples$BUGSoutput$sims.list$delta_tau)
posterior <-  dlogspline(0, fit.posterior)
BF <- prior/posterior


## Controls vs. crack users
# delta gamma = 12733.76 (Decisive evidence for H1)
fit.posterior <- logspline(controlsvscrack.ttest.samples$BUGSoutput$sims.list$delta_g_ref)
posterior <-  dlogspline(0, fit.posterior)
BF <- prior/posterior

# Delta tau = 4.499 (Susbtantial evidence for)
fit.posterior <- logspline(controlsvscrack.ttest.samples$BUGSoutput$sims.list$delta_tau)
posterior <-  dlogspline(0, fit.posterior)
BF <- prior/posterior

## Teens vs. Crack users

# delta gamma = (0.897, more in favour of H0)
fit.posterior <- logspline(teensvscrack.ttest.samples$BUGSoutput$sims.list$delta_g_ref)
posterior <-  dlogspline(0, fit.posterior)
BF <- prior/posterior

# delta tau = (0.455, more in favour of H0)
fit.posterior <- logspline(teensvscrack.ttest.samples$BUGSoutput$sims.list$delta_tau)
posterior <-  dlogspline(0, fit.posterior)
BF <- prior/posterior







#### Trace plots - furry caterpillar and posterior
#MCMCsummary(controlsvsteens.ttest.samples, round = 3)

MCMCtrace(controlsvsteens.ttest.samples,     # controls and teens
          ISB = FALSE, 
          exact = TRUE,
          Rhat = TRUE)

MCMCtrace(controlsvscrack.ttest.samples,  # controls and crack users
          ISB = FALSE, 
          exact = TRUE,
          Rhat = TRUE)

MCMCtrace(teensvscrack.ttest.samples,    # teens and crack users
          ISB = TRUE, 
          exact = TRUE,
          Rhat = TRUE)



# Density and convergence plots for delta
MCMCtrace(controlsvsteens.ttest.samples,
          params = c("delta_g_ref", "delta_tau"),
          ISB = FALSE,
          exact = TRUE,
          Rhat = TRUE,
          pdf = FALSE)

MCMCtrace(controlsvscrack.ttest.samples,
          params = c("delta_g_ref", "delta_tau"),
          ISB = FALSE,
          exact = TRUE,
          Rhat = TRUE,
          pdf = FALSE)

MCMCtrace(teensvscrack.ttest.samples,
          params = c("delta_g_ref", "delta_tau"),
          ISB = FALSE,
          exact = TRUE,
          Rhat = TRUE,
          pdf = FALSE)

#Converting mean values to linear apce
teens_mu_gamma<- exp(teensvscrack.ttest.samples$BUGSoutput$sims.list$A.mu_g_ref)
teens_mu_tau <- exp(teensvscrack.ttest.samples$BUGSoutput$sims.list$A.mu_tau)

crack_mu_gamma <- exp(controlsvscrack.ttest.samples$BUGSoutput$sims.list$B.mu_g_ref)
crack_mu_tau <- exp(controlsvscrack.ttest.samples$BUGSoutput$sims.list$B.mu_tau)

control_mu_gamma <- exp(controlsvscrack.ttest.samples$BUGSoutput$sims.list$A.mu_g_ref)
control_mu_tau <- exp(controlsvscrack.ttest.samples$BUGSoutput$sims.list$A.mu_tau)


crack_mu_gamma <- exp(teensvscrack.ttest.samples$BUGSoutput$sims.list$B.mu_g_ref)
median(crack_mu_gamma)   # Median gamma crack 8.07
median(control_mu_gamma) # Median control gamma 1.877

median(crack_mu_tau)     # Median tau crack 1.042
median(control_mu_tau)   # Median control tau 1.1267



# Median gamma crack 8.07
# Median control gamma 1.877

# Median tau crack 1.042
# Median control tau 1.1267

median(teens_mu_gamma)   # Median gamma teens 3.7837
median(teens_mu_tau)     # Median tau teens 1.035

