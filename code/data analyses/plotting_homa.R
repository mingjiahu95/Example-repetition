expt1_data = data.frame(cond = rep(c("REP","NREP"),each = 4),
                                 itemtype = rep(c("Proto","Low","Med","High"),times = 2),
                                 pred_accuracy = c(.964,.957,.917,.832,
                                                   .971,.965,.933,.854),
                                 obs_accuracy = c(.980,.980,.920,.840,
                                                  .990,.970,.960,.890))

expt2_data = data.frame(cond = rep(c("REP","NREP"),each = 3),
                               itemtype = rep(c("Old","New","Foil"),times = 2),
                               pred_oldprop = c(.887,.628,.153,
                                                .808,.784,.225),
                               obs_oldprop = c(.900,.600,.200,
                                               .780,.800,.240))

expt3_data = data.frame(cond = rep(c("REP","NREP"),each = 3),
                        itemtype = rep(c("Old","New","Proto"),times = 2),
                        pred_oldprop = c(.860,.584,.864,
                                         .789,.764,.941),
                        obs_oldprop = c(.860,.580,.870,
                                        .780,.780,.920))

library(ggplot2)
# expt 1
expt1_data$cond = factor(expt1_data$cond,levels = c("REP","NREP"))
expt1_data$itemtype = factor(expt1_data$itemtype,levels = c("Proto","Low","Med","High"))
p1 = ggplot(aes(y=obs_accuracy, x=itemtype, fill=cond), data=expt1_data) +
  geom_bar(stat="identity", color = "black", position=position_dodge(.9),width=.8)+
  geom_point(aes(y=pred_accuracy),size = 1,position=position_dodge(.9), show.legend=FALSE) +
  ggtitle("A.Classification (Homa et al., Exp. 1)") +
  xlab("Transfer Item") +
  ylab ("Proportion Correct") +
  ylim(0,1) +
  theme_bw(base_size = 8) +
  theme(legend.title = element_blank())

plot(p1)

# expt 2
expt2_data$cond = factor(expt2_data$cond,levels = c("REP","NREP"))
expt2_data$itemtype = factor(expt2_data$itemtype,levels = c("Old","New","Foil"))
p2 = ggplot(aes(y=obs_oldprop, x=cond, fill=itemtype), data=expt2_data) +
  geom_bar(stat="identity", color = "black", position=position_dodge(.9),width=.8)+
  geom_point(aes(y=pred_oldprop),size = 1,position=position_dodge(.9), show.legend=FALSE) +
  ggtitle("B.Recognition (Homa et al., Exp. 2)") +
  xlab("Learning condition") +
  ylab ("Proportion Old") +
  ylim(0,1) +
  scale_fill_manual(values = c("deepskyblue1","limegreen","red")) +
  theme_bw(base_size = 8) +
  theme(legend.title = element_blank())

plot(p2)

# expt 3
expt3_data$cond = factor(expt3_data$cond,levels = c("REP","NREP"))
expt3_data$itemtype = factor(expt3_data$itemtype,levels = c("Old","New","Proto"))
p3 = ggplot(aes(y=obs_oldprop, x=cond, fill=itemtype), data=expt3_data) +
  geom_bar(stat="identity", color = "black", position=position_dodge(.9),width=.8)+
  geom_point(aes(y=pred_oldprop),size = 1,position=position_dodge(.9), show.legend=FALSE) +
  ggtitle("C.Recognition (Homa et al., Exp. 3)") +
  xlab("Learning Condition") +
  ylab ("Proportion Old") +
  ylim(0,1) +
  scale_fill_manual(values = c("deepskyblue1","limegreen","magenta")) +
  theme_bw(base_size = 8) +
  theme(legend.title = element_blank())

plot(p3)

library(gridExtra)
p = grid.arrange(p1, p2, p3, nrow = 2)


ggsave(paste0("../../figure/homa/transfer",'.png'), plot = p, device = "png",
       width = 7, height = 4, units = "in")


# predicted learning: REP vs. NREP
cond = factor(rep(rep(c("REP","NREP"),each = 20),times = 4),
              levels = c("REP","NREP"))
back = factor(rep(c(0.50,1.00,2.00,5.00),each = 2*20),
              levels = c(0.50,1.00,2.00,5.00))
block = rep(1:20,times = 2*4)
learning_data = data.frame(Condition = cond, Block = block, Background = back,
                           pred_accuracy = c(0.448, 0.696, 0.752, 0.776, 0.789, 0.797, 0.803, 0.807, 0.811, 0.813, 0.815, 0.817, 0.818, 0.819, 0.820, 0.821, 0.822, 0.823, 0.823, 0.824,
                                             0.449, 0.600, 0.646, 0.667, 0.680, 0.689, 0.694, 0.699, 0.702, 0.705, 0.707, 0.708, 0.711, 0.712, 0.713, 0.714, 0.715, 0.715, 0.716, 0.717,
                                             0.410, 0.627, 0.701, 0.737, 0.758, 0.771, 0.781, 0.788, 0.793, 0.798, 0.801, 0.804, 0.806, 0.808, 0.810, 0.812, 0.813, 0.814, 0.815, 0.816,
                                             0.410, 0.540, 0.596, 0.627, 0.646, 0.660, 0.669, 0.677, 0.682, 0.687, 0.690, 0.693, 0.696, 0.698, 0.700, 0.702, 0.704, 0.705, 0.706, 0.707,
                                             0.379, 0.543, 0.626, 0.673, 0.704, 0.725, 0.740, 0.752, 0.761, 0.768, 0.774, 0.779, 0.784, 0.787, 0.790, 0.793, 0.796, 0.798, 0.800, 0.802,
                                             0.379, 0.475, 0.531, 0.568, 0.594, 0.612, 0.626, 0.638, 0.646, 0.654, 0.660, 0.665, 0.670, 0.674, 0.677, 0.680, 0.683, 0.685, 0.687, 0.689,
                                             0.354, 0.445, 0.510, 0.559, 0.596, 0.625, 0.648, 0.666, 0.682, 0.695, 0.706, 0.715, 0.723, 0.730, 0.737, 0.742, 0.747, 0.752, 0.756, 0.760,
                                             0.354, 0.406, 0.446, 0.478, 0.504, 0.525, 0.543, 0.558, 0.570, 0.582, 0.592, 0.600, 0.608, 0.614, 0.620, 0.626, 0.631, 0.635, 0.639, 0.643))

learning_data = learning_data[(20*4+1):(20*6),]

p = ggplot(aes(y=pred_accuracy, x=Block, shape=Condition,linetype = Condition, color = Condition), data=learning_data) +
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

ggsave(paste0("../figure/homa/learning",'.png'), plot = p, device = "png",
       width = 6, height = 4, units = "in")





