
tot_trials=15*15+39

library(ggplot2)
library(dplyr)
# ---- Load the Data ----
setwd("../data_rec")
files = dir(pattern = "*.txt")
files = files[file.size(files)!=0] # throw away empty files
data = do.call(rbind, lapply(files, read.table))
setwd("../code")


# ---- Structure the data ----
data = subset(data,select=1:8)
colnames(data) = c("phase","block","trial","itemtype","category","token","resp","accuracy")
data$itemtype = factor(data$itemtype, labels=c("old","proto","newmed","foil"))
cond_fileidx = as.integer(!grepl("nrep",files))+1
data$cond = factor(rep(cond_fileidx,each=tot_trials),labels=c("nrep","rep"))
data$phase = factor(data$phase,labels=c("train","test"))
N_total = length(files)
data$ID = rep(1:N_total,each = tot_trials)
data$oldprop = 2 - data$resp

#------summarize the data-------
rec_train_data = data[data$phase=="train",] %>%
                 Rmisc::summarySE(measurevar = "accuracy", groupvars = c("ID","block", "cond"))

rec_test_data = data[data$phase=="test",] %>%
  Rmisc::summarySE(measurevar = "oldprop", groupvars = c("ID","itemtype", "cond"))

#------check outliers-------
## outlier check
train_subject_data = data[data$phase == "train" & data$block %in% 8:15,] %>%
                     Rmisc::summarySE(measurevar = "accuracy", groupvars = c("cond","ID"))
train_subject_data$sub_name = files[train_subject_data$ID]
test_subject_data = data[data$phase == "test",] %>%
                    group_by(cond,ID) %>%
                    summarize(accuracy = mean(oldprop[itemtype == "old"]) - mean(oldprop[itemtype == "foil"]))
test_subject_data$sub_name = files[test_subject_data$ID]

ggplot(aes(x = accuracy,fill = cond), data=train_subject_data) +
  geom_histogram(color = "black",bins = 15) +
  ggtitle("Learning, Expt 1") +
  xlab("Accuracy") + 
  ylab ("Frequency") +
  scale_y_continuous(breaks=seq(1,35,4))  +
  theme(panel.grid.minor = element_blank()) +
  facet_grid(. ~ cond) +
  theme_bw() 

ggplot(aes(x = accuracy,fill = cond), data=test_subject_data) +
  geom_histogram(color = "black",bins = 15) +
  ggtitle("Recognition, Expt 1") +
  xlab("Accuracy") + 
  ylab ("Frequency") +
  scale_y_continuous(breaks=seq(1,40,4)) +
  facet_grid(. ~ cond) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) 

#identity outliers
check_outlier = function(train_data,test_data){
  attach(train_data)
  mean_rep_train = mean(accuracy[cond =="rep"]);mean_nrep_train = mean(accuracy[cond =="nrep"])
  sd_rep_train = sd(accuracy[cond =="rep"]);sd_nrep_train = sd(accuracy[cond =="nrep"])
  
  filter_rep1 = cond == "rep" & accuracy < (mean_rep_train - 2.5*sd_rep_train)
  train_rep = data.frame(accuracy = accuracy[filter_rep1], ID = ID[filter_rep1])
  train_rep$subject = gsub("poly","",sub_name[filter_rep1],ignore.case = TRUE) %>%
    gsub(".txt","",.,ignore.case = TRUE)
  
  filter_nrep1 = cond == "nrep" & accuracy < (mean_nrep_train - 2.5*sd_nrep_train)
  train_nrep = data.frame(accuracy = accuracy[filter_nrep1], ID = ID[filter_nrep1])
  train_nrep$subject = gsub("poly","",sub_name[filter_nrep1],ignore.case = TRUE) %>%
    gsub(".txt","",.,ignore.case = TRUE)
  detach(train_data)
  
  attach(test_data)
  mean_rep_test = mean(accuracy[cond =="rep"]);mean_nrep_test = mean(accuracy[cond =="nrep"])
  sd_rep_test = sd(accuracy[cond =="rep"]);sd_nrep_test = sd(accuracy[cond =="nrep"])
  
  filter_rep2 = cond == "rep" & accuracy < (mean_rep_test - 2.5*sd_rep_test)
  test_rep = data.frame(acuracy = accuracy[filter_rep2],ID = ID[filter_rep2])
  test_rep$subject = gsub("poly","",sub_name[filter_rep2],ignore.case = TRUE) %>%
    gsub(".txt","",.,ignore.case = TRUE)
  
  filter_nrep2 = cond == "nrep" & accuracy < (mean_nrep_test - 2.5*sd_nrep_test)
  test_nrep = data.frame(accuracy = accuracy[filter_nrep2],ID = ID[filter_nrep2])
  test_nrep$subject = gsub("poly","",sub_name[filter_nrep2],ignore.case = TRUE) %>%
    gsub(".txt","",.,ignore.case = TRUE)
  
  detach(test_data)
  
  means = c(mean_rep_train, mean_rep_test, mean_nrep_train, mean_nrep_test)
  sds = c(sd_rep_train, sd_rep_test, sd_nrep_train, sd_nrep_test)
  outlier = list(train_rep = train_rep, test_rep = test_rep,
                 train_nrep = train_nrep, test_nrep = test_nrep,
                 mean = means,sd = sds)
  return(outlier)
}


outliers = check_outlier(train_subject_data,test_subject_data)
print(outliers)




#------write data-------
## remove subjects 132(rep33),161(rep62),167(rep68),112 (rep13), 118(rep19), 184(rep83), 195(rep95)
## 9 (nrep12), 21(nrep24), 36(nrep38), 71(nrep72), 97(nrep96)
rec_test_data_full = rec_test_data
rec_train_data_full = rec_train_data
rec_test_data = rec_test_data[!rec_test_data$ID %in% c(132,161,167,112,118,184,195,9,21,36,71,97),]
rec_train_data = rec_train_data[!rec_train_data$ID %in% c(132,161,167,112,118,184,195,9,21,36,71,97),]



N_rep_all = with(rec_train_data_full, length(unique(ID[cond =="rep"])))
N_nrep_all = with(rec_train_data_full, length(unique(ID[cond =="nrep"])))
N_rep = with(rec_train_data, length(unique(ID[cond =="rep"])))
N_nrep = with(rec_train_data, length(unique(ID[cond =="nrep"])))
print(c(N_rep_all,N_nrep_all,N_rep, N_nrep))


write.table(rec_train_data,file = "rec_train_data.txt")
write.table(rec_test_data,file = "rec_test_data.txt")
write.table(rec_train_data_full,file = "rec_train_data_full.txt")
write.table(rec_test_data_full,file = "rec_test_data_full.txt")



