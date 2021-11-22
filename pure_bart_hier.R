library(R2jags)
library(dplyr)

dat <- read.csv("CCT_long.csv")
unique(dat$Gender)

#Males should be filtered out
dat <- dat[dat$Gender == "Female",]
unique(dat$Gender)

# # The hot2 condition should be filtered out
# dat <- dat[dat$CCT_Task_Type == "Hot1",] 
# unique(dat$CCT_Task_Type)

#Summarizing the number of participants in each group, after removing males
dat %>%  group_by(Study_Group) %>% summarise(count = n_distinct(ID))

#There are: 
# 30 crack users # subject number 43 lacks in the demo, and subject 69 may be troublesome. Possibly remove these two
# 26 Healthy adolescents
# 21 Healthy adults

# Looking at the demographics data set to match with the long df
demo <- read.csv("demographics.csv")
demo <- demo[demo$Gender == "2",]
demo %>% group_by(Grupo) %>% summarise(count = n_distinct(N_Protocol))

# There is an issue with subject number 69. Coded as group 3, but as 0 in the "crack" column. 

# Removed in the demo set are:
# 1, adults: 47
# 2, adolescents: 1, 5, 72, 77, 80, 90, 100, 102
# 3, crack: 42, 69

#Removing excluded participants
dat<-dat[!(dat$ID=="47"),]
dat<-dat[!(dat$ID=="1"),]
dat<-dat[!(dat$ID=="5"),]
dat<-dat[!(dat$ID=="72"),]
dat<-dat[!(dat$ID=="77"),]
dat<-dat[!(dat$ID=="80"),]
dat<-dat[!(dat$ID=="90"),]
dat<-dat[!(dat$ID=="100"),]
dat<-dat[!(dat$ID=="102"),]
dat<-dat[!(dat$ID=="42"),]
dat<-dat[!(dat$ID=="69"),]
dat<-dat[!(dat$ID=="43"),]


unique(dat$ID)


dat %>%  group_by(Study_Group) %>% summarise(count = n_distinct(ID))

#There are now: 
# 27 crack users 
# 18 Healthy adolescents
# 20 Healthy adults
# ------------------------------------

ncards <- 32
ntrials <- max(dat$CCT_Round_Tria_Num)

# set probability vectors - to copy into subject matrices according to dat$CCT_Round_LossNum variable
p_win1 <- (seq(32,1,-1)-1)/ #number of winners left
  seq(32,1,-1) # number of cards left     

p_win3 <- (seq(32,1,-1)-3)/ #number of winners left
  seq(32,1,-1) # number of cards left

p_win3[31:32] <- 0

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

p <- 1-cont.p
d <- cont.d
v_loss <- cont.v_loss
v_win <- cont.v_win
nchoices <- cont.nchoices
nsubs <- cont.nsubs

#data <- list("ntrials","nchoices","nsubs","p","v_loss","v_win","d") #data inputted into jags

data <- list("ntrials", "nchoices", "nsubs", "p", "d")

params <- c("mu_tau_log","mu_g_ref_log",
            "mu_tau","mu_g_ref") #parameters we'll track in jags

cont.samples <- jags(data, inits=NULL, params,
                     model.file ="pure_bart_hier.txt",
                     n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)

plot(density(cont.samples$BUGSoutput$sims.list$mu_tau))
traceplot(cont.samples)

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
# ---------- Apply hierarchical model to teen group ---------------------
#------------------------------------------------------------------------

p <- 1-teen.p
d <- teen.d
v_loss <- teen.v_loss
v_win <- teen.v_win
nchoices <- teen.nchoices
nsubs <- teen.nsubs

nchoices[10,16] <- 31 # edge correction - can't make 32 choices

# data <- list("ntrials","nchoices","nsubs","p","v_loss","v_win","d") #data inputted into jags
data <- list("ntrials","nchoices","nsubs","p","d")

params <- c("mu_rho_probit","mu_lambda_log","mu_tau_log","mu_g_ref_log",
            "mu_rho","mu_lambda","mu_tau","mu_g_ref") #parameters we'll track in jags

params <- c("mu_tau_log","mu_g_ref_log",
            "mu_tau","mu_g_ref") #parameters we'll track in jags

teen.samples <- jags(data, inits=NULL, params,
                     model.file ="pure_bart_hier.txt",
                     n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)


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
# ---------- Apply hierarchical model to crack group --------------------
#------------------------------------------------------------------------

p <- 1-crac.p
d <- crac.d
v_loss <- crac.v_loss
v_win <- crac.v_win
nchoices <- crac.nchoices
nsubs <- crac.nsubs

nchoices[10,16] <- 31 # edge correction - can't make 32 choices

# data <- list("ntrials","nchoices","nsubs","p","v_loss","v_win","d") #data inputted into jags
data <- list("ntrials","nchoices","nsubs","p","d")

params <- c("mu_tau_log","mu_g_ref_log",
            "mu_tau","mu_g_ref") #parameters we'll track in jags

crac.samples <- jags(data, inits=NULL, params,
                     model.file ="pure_bart_hier.txt",
                     n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)





crac.samples
