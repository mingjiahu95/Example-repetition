tot_trials=15*15+39

library(ggplot2)
library(dplyr)
# ---- Load the Data ----
setwd("../../data_rec")
files = dir(pattern = "*.txt")
files = files[file.size(files)!=0] # throw away empty files
data = do.call(rbind, lapply(files, read.table))
setwd("../code/data analyses")


# ---- Structure the data ----
data = subset(data,select=1:8)
colnames(data) = c("phase","block","trial","itemtype","category","token","resp","accuracy")
data$itemtype = factor(data$itemtype, labels=c("old","proto","newmed","foil"))
cond_fileidx = as.integer(!grepl("nrep",files))+1
data$cond = factor(rep(cond_fileidx,each=tot_trials),labels=c("nrep","rep"))
data$phase = factor(data$phase,labels=c("train","test"))
N_total = length(files)
data$ID = rep(1:N_total,each = tot_trials)
data$oldresp = 2 - data$resp

#------subset the data-------
oldresp_train_block = filter(data,!ID %in% c(132,161,167,112,118,184,195,9,21,36,71,97)) %>%
                      filter(cond == "nrep" & phase == "test" & itemtype == "old" ) %>%
                      select(ID, token, oldresp) 
oldresp_train_block$block = floor((oldresp_train_block$token - 1)/5) + 1
oldprop_train_block = group_by(oldresp_train_block,block) %>%
                      summarise(oldprop = mean(oldresp))

#------plot the data-------
p= ggplot(aes(y=oldprop,x=block),data=oldprop_train_block) +
  geom_line(size = 1) +
  geom_point(size = 2,color = "red") +
  ggtitle("Recognition across training blocks") +
  xlab("Training Blocks") + 
  ylab ("Proportion Old") +
  scale_x_continuous(breaks = 1:15) +
  ylim(0,1) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor=element_blank()) 

plot(p)

ggsave(paste0("../../figure/no_outliers/recognition across blocks",".jpg"), 
       plot = p, device = "jpg",
       width = 8, height = 6, units = "in")







#------statistical analyses-------
library(car)
library(rstatix)
## run ANOVA
res.aov <- anova_test(oldresp ~ block + Error(ID|block),
                      data= oldresp_train_block,
                      type=3, effect.size = "pes",detailed=T)
get_anova_table(res.aov,correction = "auto")

                      