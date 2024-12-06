
tot_trials=15*15+63

library(ggplot2)
library(dplyr)

# ---- Load the Data ----
setwd("../data_class")
files = dir(pattern = "*.txt")
files = files[file.size(files)!=0] # throw away empty files
data = do.call(rbind, lapply(files, read.table))
setwd("../code")


# ---- Structure the data ----
data = subset(data,select=1:8)
colnames(data) = c("phase","block","trial","itemtype","category","token","resp","accuracy")
data$itemtype = factor(data$itemtype, labels=c("old","proto","newlow","newmed","newhigh"))
cond_fileidx = as.integer(!grepl("nrep",files))+1
data$cond = factor(rep(cond_fileidx,each=tot_trials),labels=c("nrep","rep"))
data$phase = factor(data$phase,labels=c("train","test"))
data$ID = rep(1:length(files),each = tot_trials)

#------summarize the data-------
class_test_data = data[data$phase=="test",]
class_test_data = Rmisc::summarySE(class_test_data, measurevar = "accuracy", groupvars = c("ID","itemtype", "cond"))
class_test_data$sub_name = files[class_test_data$ID]



class_train_data = data[data$phase=="train",]
class_train_data = Rmisc::summarySE(class_train_data, measurevar = "accuracy", groupvars = c("ID","block", "cond"))
class_train_data$sub_name = files[class_train_data$ID]

#------check outliers-------
## outlier check
train_subject_data = class_train_data[class_train_data$block %in% 8:15,] %>%
                     Rmisc::summarySE(measurevar = "accuracy", groupvars = c("cond","ID"))
train_subject_data$sub_name = files[train_subject_data$ID]

test_subject_data = class_test_data %>%
                    Rmisc::summarySE(measurevar = "accuracy", groupvars = c("cond","ID"))
test_subject_data$sub_name = files[test_subject_data$ID]

ggplot(aes(x = accuracy,fill = cond), data=train_subject_data) +
  geom_histogram(color = "black",bins = 15) +
  ggtitle("Learning, Expt 2") +
  xlab("Accuracy") + 
  ylab ("Frequency") +
  scale_y_continuous(breaks= 1:13)  +
  theme(panel.grid.minor = element_blank()) +
  facet_grid(. ~ cond) +
  theme_bw() 

ggplot(aes(x = accuracy,fill = cond), data=test_subject_data) +
  geom_histogram(color = "black",bins = 15) +
  ggtitle("Classification, Expt 2") +
  xlab("Accuracy") + 
  ylab ("Frequency") +
  scale_y_continuous(breaks=1:15) +
  facet_grid(. ~ cond) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) 

#identify outliers
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



#-----write data-------
## remove subjects 57(rep22),59(rep25),65(rep32),57 (rep22), 73(rep46)
## 8 (nrep16), 33 (nrep42)
class_test_data_full = class_test_data
class_train_data_full = class_train_data
class_test_data = class_test_data[!class_test_data$ID %in% c(8,33,57,59,65,57,73),]
class_train_data = class_train_data[!class_train_data$ID %in% c(8,33,57,59,65,57,73),]



N_rep_all = with(class_train_data_full, length(unique(ID[cond =="rep"])))
N_nrep_all = with(class_train_data_full, length(unique(ID[cond =="nrep"])))
N_rep = with(class_train_data, length(unique(ID[cond =="rep"])))
N_nrep = with(class_train_data, length(unique(ID[cond =="nrep"])))
print(c(N_rep_all,N_nrep_all,N_rep, N_nrep))


write.table(class_train_data,file = "class_train_data.txt")
write.table(class_test_data,file = "class_test_data.txt")
write.table(class_train_data_full,file = "class_train_data_full.txt")
write.table(class_test_data_full,file = "class_test_data_full.txt")


