# dat <- read.csv("CCT_long.csv")
# 
# unique(dat$Gender)
# 
# #Males should be filtered out
# dat <- dat[dat$Gender == "Female",]
# unique(dat$Gender)
# 
# # # The hot2 condition should be filtered out
# # dat <- dat[dat$CCT_Task_Type == "Hot1",] 
# # unique(dat$CCT_Task_Type)
# 
# #Summarizing the number of participants in each group, after removing males
# dat %>%  group_by(Study_Group) %>% summarise(count = n_distinct(ID))
# 
# #There are: 
# # 30 crack users # subject number 43 lacks in the demo, and subject 69 may be troublesome. Possibly remove these two
# # 26 Healthy adolescents
# # 21 Healthy adults
# 
# # Looking at the demographics data set to match with the long df
# # demo <- read.csv("demographics.csv")
# # demo <- demo[demo$Gender == "2",]
# # demo %>% group_by(Grupo) %>% summarise(count = n_distinct(N_Protocol))
# 
# # There is an issue with subject number 69. Coded as group 3, but as 0 in the "crack" column. 
# 
# # Removed in the demo set are:
# # 1, adults: 47
# # 2, adolescents: 1, 5, 72, 77, 80, 90, 100, 102
# # 3, crack: 42, 69
# 
# #Removing excluded participants
# dat<-dat[!(dat$ID=="47"),]
# dat<-dat[!(dat$ID=="1"),]
# dat<-dat[!(dat$ID=="5"),]
# dat<-dat[!(dat$ID=="72"),]
# dat<-dat[!(dat$ID=="77"),]
# dat<-dat[!(dat$ID=="80"),]
# dat<-dat[!(dat$ID=="90"),]
# dat<-dat[!(dat$ID=="100"),]
# dat<-dat[!(dat$ID=="102"),]
# dat<-dat[!(dat$ID=="42"),]
# dat<-dat[!(dat$ID=="69"),]
# dat<-dat[!(dat$ID=="43"),]
# 
# 
# #unique(dat$ID)
# 
# 
# dat %>%  group_by(Study_Group) %>% summarise(count = n_distinct(ID))
# 
# #There are now: 
# # 27 crack users 
# # 18 Healthy adolescents
# # 20 Healthy adults
# 
# Writing new data frame to csv so the above steps don't have to be completed every time
# write.csv(dat,"readytorun.csv", row.names = FALSE)


dat <- read.csv("readytorun.csv")
dat %>%  group_by(Study_Group) %>% summarise(count = n_distinct(ID))

unique(dat$CCT_Task_Type) # Both conditions are included, so be aware of specifying this when running the model
