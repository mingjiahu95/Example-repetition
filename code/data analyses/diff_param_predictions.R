library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)


#------------Classification experiment-------------
##construct data frames
index = factor(rep(rep(1:8,each = 2*5),times = 1))
cond = factor(rep(rep(c("REP","NREP"),each = 5),times = 8),
              levels = c("REP","NREP"))
itemtype = factor(rep(c("Old","Proto","Low","Med","High"),times = 8),
                  levels = c("Old","Proto","Low","Med","High"))
prop_correct = c(0.957, 0.916, 0.905, 0.853, 0.760, 0.884, 0.919, 0.910, 0.863, 0.778,
             0.925, 0.916, 0.911, 0.884, 0.828, 0.890, 0.916, 0.912, 0.886, 0.834,
             0.907, 0.801, 0.787, 0.725, 0.640, 0.771, 0.804, 0.792, 0.738, 0.662,
             0.980, 0.964, 0.959, 0.925, 0.847, 0.944, 0.966, 0.961, 0.932, 0.861,
             0.953, 0.943, 0.938, 0.910, 0.848, 0.919, 0.944, 0.939, 0.914, 0.856,
             0.966, 0.877, 0.860, 0.784, 0.679, 0.862, 0.885, 0.870, 0.805, 0.708,
             0.927, 0.888, 0.878, 0.830, 0.748, 0.851, 0.890, 0.881, 0.838, 0.762,
             0.972, 0.932, 0.921, 0.865, 0.767, 0.906, 0.936, 0.927, 0.878, 0.788)

class_data = data.frame(Index = index, Condition = cond, Itemtype = itemtype, 
                      Prop_correct = prop_correct)
panel_names = list()
panel_names[[1]] = "Best-Fitting Parameters"
panel_names[[2]] = expression(paste(italic("within"), "-low, ", italic("c"), "-low" ))
panel_names[[3]] = expression(paste(italic("between"), "-low"))
panel_names[[4]] = expression(paste(italic("between"), "-high"))
panel_names[[5]] = expression(paste(italic("within"), "-low"))
panel_names[[6]] = expression(paste(italic("within"), "-high"))
panel_names[[7]] = expression(paste(italic("c"), "-low"))
panel_names[[8]] = expression(paste(italic("c"), "-high"))

#panel_names = c("best-fitting parameters","within-low, c-low","between-low","between-high","within-low","within-high","c-low","c-high")


##plotting
for (i in 1:8){
  p <- ggplot(data = class_data[class_data$Index == i,], aes(y=Prop_correct, fill=Condition, x=Itemtype)) +
    geom_bar(stat="identity", color = "black", width = .8, position=position_dodge(.9)) +
    xlab("Item Type") + 
    ylab ("Proportion Correct") +
    ggtitle(panel_names[[i]]) +
    coord_cartesian(ylim=c(0.5,1.0)) + 
    #ylim(0.5,1.0) +
    theme_bw(base_size = 15) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0))
  
  assign(paste("plot", i, sep = ""),p)
}

## combine plots
p_class = ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,
                    nrow = 4,ncol = 2,
                    common.legend = TRUE,legend = "right")
plot(p_class)

## save plot
ggsave(paste0("../../figure/review1/pred_class_8params",'.png'), plot = p_class, device = "png",
       width = 6*2, height = 6*2, units = "in")


#------------Recognition experiment-------------
##construct data frames
index = factor(rep(rep(1:8,each = 2*4),times = 1))
cond = factor(rep(rep(c("REP","NREP"),each = 4),times = 8),
              levels = c("REP","NREP"))
itemtype = factor(rep(c("Old","Med","Proto","Foil"),times = 8),
                  levels = c("Old","Med","Proto","Foil"))
prop_old = c(0.853, 0.352, 0.767, 0.072, 0.691, 0.623, 0.941, 0.154,
                 0.782, 0.584, 0.832, 0.101, 0.751, 0.736, 0.914, 0.154,
                 0.864, 0.412, 0.811, 0.184, 0.737, 0.678, 0.954, 0.352,
                 0.847, 0.324, 0.749, 0.029, 0.666, 0.594, 0.936, 0.063,
                 0.831, 0.553, 0.853, 0.072, 0.736, 0.712, 0.929, 0.108,
                 0.820, 0.158, 0.523, 0.045, 0.458, 0.342, 0.837, 0.102,
                 0.884, 0.541, 0.874, 0.152, 0.829, 0.795, 0.970, 0.292,
                 0.832, 0.228, 0.641, 0.038, 0.559, 0.462, 0.897, 0.085)
rec_data = data.frame(Index = index, Condition = cond, Itemtype = itemtype, 
                        Prop_old = prop_old)

##plotting
for (i in 1:8){
  p <- ggplot(data = rec_data[rec_data$Index == i,], aes(y=Prop_old, x=Condition, fill=Itemtype)) +
    geom_bar(stat="identity", color = "black", width = .8, position=position_dodge(.9)) +
    scale_fill_manual(name = "Item Type",
                      values = c("Old" = "deepskyblue1","Med" = "limegreen","Proto" = "magenta","Foil" = "red")) +
    xlab("Learning Condition") + 
    ylab ("Proportion Old") +
    ggtitle(panel_names[[i]]) +
    ylim(0.0,1.0) +
    theme_bw(base_size = 15) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0))
  
  assign(paste("plot", i, sep = ""),p)
}

## combine plots
p_rec = ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,
                    nrow = 4,ncol = 2,
                    common.legend = TRUE,legend = "right")
## save plot
ggsave(paste0("../../figure/review1/pred_rec_8params",'.png'), plot = p_rec, device = "png",
       width = 6*2, height = 6*2, units = "in")