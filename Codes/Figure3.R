
p1<-orchard_plot(object = meta7, 
                 mod = "Stage", 
                 group = "site_id", trunk.size = 10,
                 xlab = "Effect size (Hedges' G)") + 
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0),  # 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none")  


model_results <- data.frame(
  term = c("Early", "Late"),
  estimate = c(0.08, -0.05),
  se = c(0.02, 0.02),
  ci.lb = c(0.03, -0.09),
  ci.ub = c(0.12, -0.005))

p2<- ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, size=1.2) +
  xlab("Effect size (Hedges' G)") +
  #ylab("Model Terms") +
  geom_vline(xintercept = 0, linetype="dashed", size=1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),  # 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none") 
p1+p2



meta7.1$taxa <- factor(meta7.1$taxa, 
                       levels = c("Abundance", "Richness", "FDis_eco", "FDis_bio","FEve_eco","FEve_bio" ))

p3<- orchard_plot(object = meta7.1, trunk.size = 10,
                  mod = "taxa", 
                  group = "site_id", 
                  xlab = "Effect size (Hedges' G)")+ 
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0),  # 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none")  +
  scale_color_manual(values = c("purple2", "steelblue", "darkolivegreen4", "gold2", "salmon", "deeppink2")) +
  scale_fill_manual(values = c("purple2", "steelblue", "darkolivegreen4", "gold2", "salmon", "deeppink2")) 



model_results <- data.frame(
  term = c("Abundance", "FDispersion (biological)", "FDispersion (ecological)", "FEvennes (biological)", "FEvennes (ecological)", 
           "Richness", "Abundance:late", "FDispersion (biological):late", 
           "FDispersion (ecological):late", "FEvennes (biological):late", "FEvennes (ecological):late", 
           "Richness:late"),
  estimate = c(0.0609, 0.2027, 0.1996, -0.1607, -0.1372, 0.4888, -0.0968, -0.2135, 
               -0.2436, -0.1584, -0.1511, -0.2522),
  se = c(0.0234, 0.0334, 0.0332, 0.0332, 0.0332, 0.0349, 0.0138, 0.0401, 
         0.0398, 0.0402, 0.0403, 0.0409),
  ci.lb = c(0.0151, 0.1373, 0.1345, -0.2259, -0.2022, 0.4203, -0.1238, -0.2921, 
            -0.3216, -0.2372, -0.2301, -0.3323),
  ci.ub = c(0.1068, 0.2682, 0.2648, -0.0956, -0.0721, 0.5573, -0.0698, -0.1349, 
            -0.1656, -0.0796, -0.0721, -0.1721))

model_results$term <- factor(model_results$term, levels = c(
  "FEvennes (biological):late","FEvennes (biological)",
  "FEvennes (ecological):late", "FEvennes (ecological)",
  "FDispersion (biological):late",  "FDispersion (biological)", 
  "FDispersion (ecological):late",  "FDispersion (ecological)",
  "Richness:late","Richness", "Abundance:late","Abundance" ))


p4 <- ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, size=1.2) +
  xlab("Effect size (Hedges' G)") +
  #ylab("Model Terms") +
  geom_vline(xintercept = 0, linetype="dashed", size=1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),  # 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none") 

plot_layout <- (p1 | p2) /
  (p3 | p4) /
  (p5 | p6)

cowplot::plot_grid(
  #p0.1, p0,
  # p1, p2, 
  p3, p4, 
  p5, p6, 
  align = 'hv',
  ncol = 2, 
  labels = NA)

ggsave(filename = "my_plot.svg", 
       plot = last_plot(), 
       device = "svg", width = 15, height = 12, units = "in")
