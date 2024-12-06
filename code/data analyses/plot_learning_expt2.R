library(ggplot2)
# predicted and observed learning Expt 2 (decreasing back)
cond = factor(rep(c("REP","NREP"),each = 15),
              levels = c("REP","NREP"))
block = rep(1:15,times = 2)

learning_pred = data.frame(Condition = cond, Block = block,
                           Accuracy = c(0.470,0.681,0.749,0.793,0.825,0.850,0.870,0.886,0.900,0.912,0.922,0.930,0.935,0.937,0.939,
                                        0.472,0.617,0.668,0.698,0.720,0.736,0.749,0.758,0.765,0.774,0.777,0.785,0.789,0.798,0.804))
learning_obs = data.frame(Condition = cond, Block = block,
                           Accuracy = c(0.532,0.696,0.752,0.774,0.831,0.832,0.880,0.884,0.889,0.911,0.928,0.949,0.930,0.921,0.935,
                                        0.498,0.591,0.653,0.673,0.702,0.747,0.745,0.767,0.762,0.745,0.779,0.772,0.791,0.820,0.821))


p = ggplot(data = NULL,aes(y=Accuracy, x=Block)) +
  geom_line(data=learning_pred,aes(linetype = Condition,color = Condition),size = 1) +
  geom_point(data=learning_obs,aes(shape = Condition),color = "black", size = 3) +
  xlab("Block") +
  ylab ("Proportion Correct") +
  scale_x_continuous(breaks = 1:15) +
  scale_linetype_manual(values = c("solid","dashed")) + 
  scale_shape_discrete(solid = F) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_blank(),panel.grid.minor = element_blank())

plot(p)

ggsave(paste0("../../figure/no_outliers/expt2_learning_predicted",'.png'), plot = p, device = "png",
       width = 6, height = 4, units = "in")

# predicted and observed learning Expt 2 (constant back)
cond = factor(rep(c("REP","NREP"),each = 15),
              levels = c("REP","NREP"))
block = rep(1:15,times = 2)

learning_pred = data.frame(Condition = cond, Block = block,
                           Accuracy = c(0.482,0.682,0.747,0.790,0.821,0.846,0.866,0.883,0.896,0.907,0.917,0.925,0.931,0.933,0.935,
                                        0.484,0.627,0.676,0.706,0.728,0.744,0.756,0.763,0.769,0.776,0.777,0.782,0.781,0.787,0.790))
learning_obs = data.frame(Condition = cond, Block = block,
                          Accuracy = c(0.532,0.696,0.752,0.774,0.831,0.832,0.880,0.884,0.889,0.911,0.928,0.949,0.930,0.921,0.935,
                                       0.498,0.591,0.653,0.673,0.702,0.747,0.745,0.767,0.762,0.745,0.779,0.772,0.791,0.820,0.821))


p = ggplot(data = NULL,aes(y=Accuracy, x=Block)) +
  geom_line(data=learning_pred,aes(linetype = Condition, color = Condition), size = 1) +
  geom_point(data=learning_obs,aes(shape = Condition),color = "black", size = 3) +
  xlab("Block") +
  ylab ("Proportion Correct") +
  scale_x_continuous(breaks = 1:15) +
  scale_linetype_manual(values = c("solid","dashed")) + 
  scale_shape_discrete(solid = F) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_blank(),panel.grid.minor = element_blank())

plot(p)

ggsave(paste0("../../figure/no_outliers/expt2_learning_predicted_constBack",'.png'), plot = p, device = "png",
       width = 6, height = 4, units = "in")
