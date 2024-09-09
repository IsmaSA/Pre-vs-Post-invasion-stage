
p1<-orchard_plot(object = meta_7, 
             mod = "Stage", 
             group = "site_id", trunk.size = 12,
             xlab = "Effect size (Hedges' G)") + 
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0),  # 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none")  

# meta_7.1
meta_7.1$taxa <- factor(meta_7.1$taxa, 
      levels = c("Abundance", "Richness", "FDis_eco", "FDis_bio","FEve_eco","FEve_bio" ))

p2<- orchard_plot(object = meta_7.1, trunk.size = 12,
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
  term = c("Intercept", "FDispersion (biological)", "FDispersion (ecological)", "FEvennes (biological)", "FEvennes (ecological)", 
           "Richness", "Abundance:late", "FDispersion (biological):late", 
           "FDispersion (ecological):late", "FEvennes (biological):late", "FEvennes (ecological):late", 
           "Richness:late"),
  estimate = c(0.0609, 0.1461, 0.1414, -0.2220, -0.1979, 0.4233, -0.0387, 0.0376, 
               -0.0041, -0.0652, -0.0067, 0.0486),
  ci.lb = c(0.0187, 0.0945, 0.0902, -0.2733, -0.2490, 0.3677, -0.0658, -0.0410, 
            -0.0821, -0.1440, -0.0857, -0.0316),
  ci.ub = c(0.1032, 0.1978, 0.1926, -0.1708, -0.1468, 0.4788, -0.0116, 0.1163, 
            0.0739, 0.0136, 0.0723, 0.1287))

model_results$term <- factor(model_results$term, levels = c(
  "FEvennes (biological):late","FEvennes (biological)",
  "FEvennes (ecological):late", "FEvennes (ecological)",
  "FDispersion (biological):late",  "FDispersion (biological)", 
  "FDispersion (ecological):late",  "FDispersion (ecological)",
  "Richness:late","Richness", "Abundance:late","Intercept" ))


p3 <- ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, size=1.2) +
  xlab("Effect Size") +
  ylab("Model Terms") +
  geom_vline(xintercept = 0, linetype="dashed", size=1.2) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),  # 
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none") 

p1 + p2 + p3
