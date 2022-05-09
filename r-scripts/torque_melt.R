
torque<-setNames(data.frame(
  qpcR:::cbind.na(data1$PartCount,
                  abs(d1$Torque_1),
                  abs(d2$Torque_1),
                  abs(d3$Torque_1),
                  abs(d4$Torque_1))),
  c("PartCount", "Tool1", "Tool2", "Tool3", "Tool4"))

torque[torque== "NA"] <-''

tplot<-reshape2::melt(torque[,c("PartCount", "Tool1", "Tool2", "Tool3", "Tool4")], id.vars=1)
ggplot(tplot, aes(PartCount, y=value)) + geom_line(aes(color = variable)) + theme_minimal()

dataset_clus1<-setNames(data.frame(cbind(PartCount1, abs(torque$Tool1), clus_1$cluster)), c("PartCount", "Torque", "Cluster"))
gg1<-
  ggplot(dataset_clus1, aes(PartCount, Torque)) +
  geom_point(aes(color = as.factor(Cluster)), size=2.5) +
  geom_line(color="grey") +
  theme_bw() +
  scale_y_continuous(limits = c(1.75, 3.5))+
  theme(legend.position = "none")
 # scale_color_discrete(name =expression("Cluster"))

dataset_clus2<-setNames(data.frame(cbind(PartCount2, abs(torque$Tool2), clus_2$cluster)), c("PartCount", "Torque", "Cluster"))
dataset_clus2<-dataset_clus2[1:36,]
gg2<-
  ggplot(dataset_clus2, aes(PartCount, Torque)) +
  geom_point(aes(color = as.factor(Cluster)), size=2.5) +
  geom_line(color="grey") +
  theme_bw()+
  scale_y_continuous(limits = c(1.75, 3.5))+
  labs(title="Cluster Identification on Torque Values for Gundrill") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face="bold"))+
  scale_color_discrete(name =expression("Cluster"))

dataset_clus3<-setNames(data.frame(cbind(PartCount1, abs(torque$Tool3), clus_3$cluster)), c("PartCount", "Torque", "Cluster"))
gg3<-
  ggplot(dataset_clus3, aes(PartCount, Torque)) +
  geom_point(aes(color = as.factor(Cluster)), size=2.5) +
  geom_line(color="grey") +
  theme_bw() +
  scale_y_continuous(limits = c(1.75, 3.5))+
  theme(legend.position = "none")
 # scale_color_discrete(name =expression("Cluster"))

dataset_clus4<-setNames(data.frame(cbind(PartCount2, abs(torque$Tool4), clus_4$cluster)), c("PartCount", "Torque", "Cluster"))
dataset_clus4<-dataset_clus4[1:36,]
gg4<-
  ggplot(dataset_clus4, aes(PartCount, Torque)) +
  geom_point(aes(color = as.factor(Cluster)), size=2.5) +
  geom_line(color="grey") +
  theme_bw() +
  scale_y_continuous(limits = c(1.75, 3.5))+
  theme(legend.position = "none")
  #scale_color_discrete(name =expression("Cluster"))

plot_row<-plot_grid(
  gg1,
  gg2 + theme(legend.position="none"),
  gg3,
  gg4,
  align = 'vh',
  hjust = -1,
  nrow = 2,
  labels = c('1', '2', '3', '4'))
plot_row

legend <- get_legend(gg2 + theme(legend.box.margin = margin(0, 0, 0, 12)))
grid<-plot_grid(plot_row, legend, rel_widths = c(3,.2))

title <- get_title(gg2)

title <- ggdraw() +
  draw_label("Mean Torque Values of 4 Tool Datasets Superimposed with Respective Clusters",
    fontface = 'bold',x = 0.02, hjust = 0, size = 18)

plot_grid(title, grid, rel_heights = c(0.1, 1), nrow=2)


