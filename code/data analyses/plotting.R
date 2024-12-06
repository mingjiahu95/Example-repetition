#-----------load the data-------------
library(dplyr)
library(ggplot2)
library(openxlsx)
library(gridExtra)
source("utils.R")

class_learning_plot = read.table("class_train_data.txt") %>%
                      summarySEwithin2(measurevar = "accuracy",
                                             betweenvars = "cond", withinvars = "block", idvar = "ID")
class_learning_plot$block = as.numeric(class_learning_plot$block)
class_transfer_plot = read.table("class_test_data.txt") %>%
                      summarySEwithin2(measurevar = "accuracy", 
                                       betweenvars = "cond", withinvars = "itemtype", idvar = "ID")
rec_learning_plot = read.table("rec_train_data.txt") %>%
                    summarySEwithin2(measurevar = "accuracy", 
                                    betweenvars = "cond", withinvars = "block", idvar = "ID")
rec_learning_plot$block = as.numeric(rec_learning_plot$block)
rec_transfer_plot = read.table("rec_test_data.txt") %>%
                    summarySEwithin2(measurevar = "oldprop", 
                                             betweenvars = "cond", withinvars = "itemtype", idvar = "ID")


# #single gamma fit
# class_transfer_pred = data.frame(cond = rep(c("REP","NREP"),each = 5),
#                                  itemtype = rep(c("Old","Proto","Low","Med","High"),times = 2),
#                                  pred_accuracy = c(.981,.937,.916,.859,.745,
#                                                    .914,.960,.946,.896,.782))
# 
# rec_transfer_pred = data.frame(cond = rep(c("REP","NREP"),each = 4),
#                                  itemtype = rep(c("Old","New","Proto","Foil"),times = 2),
#                                  pred_oldprop = c(.854,.358,.746,.105,
#                                                    .679,.615,.917,.206)) 

#double gamma fit
class_transfer_pred = data.frame(cond = rep(c("REP","NREP"),each = 5),
                                 itemtype = rep(c("Old","Proto","Low","Med","High"),times = 2),
                                 pred_accuracy = c(0.957,0.916,0.905,0.854,0.760,
                                                   0.883,0.918,0.909,0.865,0.778))

rec_transfer_pred = data.frame(cond = rep(c("REP","NREP"),each = 4),
                                 itemtype = rep(c("Old","New","Proto","Foil"),times = 2),
                                 pred_oldprop = c(0.852,0.350,0.767,0.074,
                                                  0.691,0.624,0.941,0.156))

#save summarized data into excel files
# write.xlsx(class_learning_plot,
#            file = "../data_summarized/no_outliers/Expt2_learning.xlsx")
# write.xlsx(class_transfer_plot,
#            file = "../data_summarized/no_outliers/Expt2_transfer.xlsx")
# write.xlsx(rec_learning_plot,
#            file = "../data_summarized/no_outliers/Expt1_learning.xlsx")
# write.xlsx(rec_transfer_plot,
#            file = "../data_summarized/no_outliers/Expt1_transfer.xlsx")
# 
# write.xlsx(class_learning_plot,
#            file = "../data_summarized/with_outliers/Expt2_learning.xlsx")
# write.xlsx(class_transfer_plot,
#            file = "../data_summarized/with_outliers/Expt2_transfer.xlsx")
# write.xlsx(rec_learning_plot,
#            file = "../data_summarized/with_outliers/Expt1_learning.xlsx")
# write.xlsx(rec_transfer_plot,
#            file = "../data_summarized/with_outliers/Expt1_transfer.xlsx")



#-----------Classification experiment----------
## Fig 3: REP vs. NREP learning
plot_data = class_learning_plot
levels(plot_data$cond) = c("NREP","REP")
plot_data$cond = factor(plot_data$cond,levels = c("REP","NREP"))

p3= ggplot(aes(y=accuracy,x=block,shape=cond,linetype=cond,color=cond),data=plot_data) +
  geom_line(size = 1) +
  geom_point(size = 3, color = "black") +
  ggtitle("Learning, Experiment 2") +
  xlab("Blocks") + 
  ylab ("Proportion Correct") +
  scale_x_continuous(breaks = 1:15) +
  scale_linetype_manual(values = c("solid","dashed")) + 
  scale_shape_discrete(solid = F) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor=element_blank(), legend.title = element_blank()) 
  
plot(p3)

ggsave(paste0("../figure/no_outliers/Expt2 learning",'.png'), device = "png",
       width = 8, height = 6, units = "in")
# ggsave(paste0("../figure/with_outliers/Expt2 learning",'.png'), device = "png",
#        width = 8, height = 6, units = "in")

# Fig 4: transfer
# select and rename portion of the dataset
# plot_data = class_transfer_plot
# levels(plot_data$itemtype) = c("High","Low","Med","Old","Proto")
# plot_data$itemtype = factor(plot_data$itemtype, levels = c("Old","Proto","Low","Med","High"))
# levels(plot_data$cond) = c("NREP","REP")
# plot_data$cond = factor(plot_data$cond,levels = c("REP","NREP"))
# # merge in the pred_accuracy
# plot_data = merge(plot_data,class_transfer_pred,by = c("cond","itemtype"))
# 
# 
# p4 = ggplot(aes(y=accuracy, x=itemtype, fill=cond), data=plot_data) +
#   geom_bar(stat="identity", color = "black", position=position_dodge(.9),width=.8)+
#   geom_errorbar(aes(ymin=accuracy, ymax=accuracy+se),
#                 size=.5,width=.2,position=position_dodge(.9)) +
#   geom_point(aes(y=pred_accuracy),size = 1.5,position=position_dodge(.9), show.legend=FALSE) +
#   ggtitle("B. Classification, Experiment 2") +
#   xlab("Transfer Item") +
#   ylab ("Proportion Correct") +
#   theme_bw(base_size = 18) +
#   theme(legend.title = element_blank())
# 
# plot(p4)

# ggsave(paste0("../figure/no_outliers/Expt2 transfer",'.png'), device = "png",
#        width = 6, height = 7, units = "in")
# ggsave(paste0("../figure/with_outliers/Expt2 transfer",'.png'), device = "png",
#        width = 6, height = 7, units = "in")

# Fig 4-1: transfer by distortion
# select and rename portion of the dataset
levels(class_transfer_plot$itemtype) = c("High","Low","Med","Old","Proto") # rename
levels(class_transfer_plot$cond) = c("NREP","REP")
class_transfer_plot$cond = factor(class_transfer_plot$cond,levels = c("REP","NREP"))
plot_data = merge(class_transfer_plot,class_transfer_pred,by = c("cond","itemtype"))

plot_data1 = plot_data[plot_data$itemtype != "Old",]
plot_data1$itemtype = factor(plot_data1$itemtype,levels = c("Proto","Low","Med","High")) # reorder



p_1 = ggplot(aes(y=accuracy, x=itemtype, fill=cond), data=plot_data1) +
  geom_bar(stat="identity", color = "black", position=position_dodge(.9),width=.8)+
  geom_errorbar(aes(ymin=accuracy, ymax=accuracy+se),
                size=.5,width=.2,position=position_dodge(.9)) +
  geom_point(aes(y=pred_accuracy),size = 1.5,position=position_dodge(.9), show.legend=FALSE) +
  ggtitle("B1. Classification, Experiment 2") +
  xlab("Transfer Item") +
  ylab ("Proportion Correct") +
  scale_fill_discrete(name = "Condition") +
  theme_bw(base_size = 18)



## Fig 4-2: transfer by old, new and prototype
# select and rename portion of the dataset
plot_data2 = plot_data[plot_data$itemtype %in% c("Old","Med","Proto"),]
plot_data2$itemtype = factor(plot_data2$itemtype,levels = c("Old","Med","Proto")) # reorder
levels(plot_data2$itemtype) = c("Old","New","Proto")


p_2 = ggplot(aes(y=accuracy, x=cond, fill=itemtype), data=plot_data2) +
  geom_bar(stat="identity", color = "black", width = .8, position=position_dodge(.9))+
  geom_errorbar(aes(ymin=accuracy, ymax=accuracy+se),
                size=.5,width=.2,position=position_dodge(.9)) +
  geom_point(aes(y=pred_accuracy),size = 1.5,position=position_dodge(.9), show.legend=FALSE) +
  ggtitle("B2. Classification, Experiment 2") +
  xlab("Learning Condition") +
  ylab ("Proportion Correct") +
  scale_fill_manual(name = "Item Type",
                    values = c("Old" = "deepskyblue1","New" = "limegreen","Proto" = "magenta")) +
  theme_bw(base_size = 18)

library(grid)
p4 = grid.arrange(p_1, p_2, nrow = 1,
                 top = textGrob("Classification, Experiment 2",gp = gpar(fontsize = 18),just = "center"))

plot(p4)
# 
ggsave(paste0("../figure/no_outliers/Expt2 transfer_combined",'.png'), plot = p4, device = "png",
       width = 12, height = 7, units = "in")
# ggsave(paste0("../figure/with_outliers/Expt2 transfer_combined",'.png'), device = "png",
#        width = 12, height = 7, units = "in")

#-----------Recognition experiment----------
## Fig 1: REP vs. NREP learning
plot_data = rec_learning_plot
levels(plot_data$cond) = c("NREP","REP")
plot_data$cond = factor(plot_data$cond,levels = c("REP","NREP"))


p1 = ggplot(aes(y=accuracy,x=block,shape=cond,linetype=cond,color=cond),data=plot_data) +
  geom_line(size = 1) +
  geom_point(size = 3, color = "black") +
  ggtitle("Learning, Experiment 1") +
  xlab("Blocks") + 
  ylab ("Proportion Correct") +
  scale_x_continuous(breaks = 1:15) +
  scale_linetype_manual(values = c("solid","dashed")) + 
  scale_shape_discrete(solid = F) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor=element_blank(),legend.title = element_blank()) 

plot(p1)

ggsave(paste0("../figure/no_outliers/Expt1 learning",'.png'), device = "png",
       width = 8, height = 6, units = "in")
# ggsave(paste0("../figure/with_outliers/Expt1 learning",'.png'), device = "png", 
#        width = 8, height = 6, units = "in")

## Fig 2: transfer 
# select and rename portion of the dataset
plot_data = rec_transfer_plot
levels(plot_data$itemtype) # check the labels of item type
levels(plot_data$itemtype) = c("Foil","New","Old","Proto") # rename
plot_data$itemtype = factor(plot_data$itemtype,levels = c("Old","New","Proto","Foil")) # reorder
levels(plot_data$itemtype) # double check
levels(plot_data$cond) = c("NREP","REP")
plot_data$cond = factor(plot_data$cond,levels = c("REP","NREP"))
# merge in the pred_oldprop
plot_data = merge(plot_data,rec_transfer_pred,by = c("cond","itemtype"))

p2 = ggplot(aes(y=oldprop, x=cond, fill=itemtype), data=plot_data) +
  geom_bar(stat="identity", color = "black", width = .8, position=position_dodge(.9))+
  geom_errorbar(aes(ymin=oldprop, ymax=oldprop+se), 
                size=.5,width=.2, position=position_dodge(.9)) +
  geom_point(aes(y = pred_oldprop),size = 1.5, position=position_dodge(.9), show.legend=FALSE) +
  ggtitle("A. Recognition, Experiment 1") +
  xlab("Learning Condition") + 
  ylab ("Proportion Old") +
  ylim(0.0,1.0) +
  scale_fill_manual(name = "Item Type",
                    values = c("Old" = "deepskyblue1","New" = "limegreen","Proto" = "magenta","Foil" = "red")) +
  theme_bw(base_size = 18) 

plot(p2)

ggsave(paste0("../figure/no_outliers/Expt1 transfer",'.png'), device = "png",
       width = 6, height = 7, units = "in")
# ggsave(paste0("../figure/with_outliers/Expt1 transfer",'.png'), device = "png", 
#        width = 6, height = 7, units = "in")

#-----------Transfer prediction plots----------
p = grid.arrange(p2, p_1, p_2, nrow = 3)
ggsave(paste0("../figure/no_outliers/transfer prediction",'.png'), plot = p, device = "png",
       width = 6, height = 9, units = "in")