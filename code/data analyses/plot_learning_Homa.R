library(dplyr)
library(ggplot2)
source("utils.R")

# predicted learning: REP vs. NREP
cond = factor(rep(rep(c("REP","NREP"),each = 20),times = 3),
              levels = c("REP","NREP"))
back = factor(rep(c(0.50,1.00,2.00),each = 2*20),
              levels = c(0.50,1.00,2.00))
block = rep(1:20,times = 2*3)
learning_data = data.frame(Condition = cond, Block = block, Background = back,
                           pred_accuracy = c(0.459, 0.811, 0.891, 0.921, 0.937, 0.946, 0.952, 0.956, 0.958, 0.961, 0.962, 0.964, 0.965, 0.966, 0.967, 0.967, 0.968, 0.968, 0.969, 0.969,
                                             0.459, 0.655, 0.746, 0.793, 0.821, 0.842, 0.858, 0.869, 0.876, 0.882, 0.887, 0.890, 0.896, 0.899, 0.902, 0.905, 0.907, 0.907, 0.908, 0.909,
                                             0.408, 0.693, 0.805, 0.860, 0.890, 0.909, 0.922, 0.931, 0.937, 0.942, 0.946, 0.950, 0.952, 0.954, 0.956, 0.958, 0.959, 0.960, 0.961, 0.962,
                                             0.408, 0.552, 0.641, 0.699, 0.738, 0.768, 0.791, 0.809, 0.822, 0.833, 0.842, 0.849, 0.858, 0.863, 0.869, 0.874, 0.878, 0.880, 0.883, 0.885,
                                             0.374, 0.568, 0.683, 0.755, 0.802, 0.835, 0.858, 0.876, 0.890, 0.900, 0.909, 0.916, 0.922, 0.927, 0.931, 0.935, 0.938, 0.940, 0.943, 0.945,
                                             0.374, 0.466, 0.536, 0.589, 0.630, 0.665, 0.693, 0.717, 0.736, 0.752, 0.766, 0.777, 0.790, 0.798, 0.808, 0.816, 0.823, 0.827, 0.832, 0.838
                                             ))

learning_data = learning_data[(20*4+1):(20*6),]

p = ggplot(aes(y=pred_accuracy, x=Block, shape = Condition,linetype = Condition, color = Condition), data=learning_data) +
  #shape=Condition
  geom_line(size = 1) +
  geom_point(size = 3,color = "black") +
  ggtitle("Learning Prediction") +
  xlab("Block") +
  ylab ("Proportion Correct") +
  scale_x_continuous(breaks = 1:20) +
  scale_linetype_manual(values = c("solid","dashed")) + 
  scale_shape_discrete(solid = F) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_blank(),panel.grid.minor = element_blank())

plot(p)

ggsave(paste0("../../figure/review1/homa_learning",'.png'), plot = p, device = "png",
       width = 6, height = 4, units = "in")
