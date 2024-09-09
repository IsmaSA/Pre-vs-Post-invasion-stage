
summary(meta7.3)

p5<- orchard_plot(object = meta7.3, 
                  mod = "Taxo_Func", 
                  group = "site_id", 
                  xlab = "Effect size (Hedges' G)", trunk.size = 10) + 
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),   
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values = c("#D2691E", "orchid")) +
  scale_fill_manual(values = c("#D2691E", "orchid"))


model_results <- data.frame(
  term = c('Functional', 'Taxonomic', 'Functional:late','Taxonomic:late'),
  estimate = c(0.0252, 0.0905, -0.190, -0.109),
  se = c(0.0247, 0.0147, 0.0204, 0.0134),
  zval = c(1.1025, 4.8134, -0.4413, -1.9392),
  pval = c(0.2703, '<.0001', 0.6590, 0.0525),
  ci.lb = c(-0.0241, 0.0518, -0.2391, -0.1321),
  ci.ub = c(0.0755, 0.1493, -0.150, -0.0799),
  signif = c('', '***', '', '.')
)
model_results$term <- factor(model_results$term, levels = c(
  "Functional:late","Functional", "Taxonomic:late","Taxonomic"))

p6<-ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, size=1.2) +
  xlab("Effect size (Hedges' G)") +
  ylab("") + geom_vline(xintercept = 0, linetype="dashed", size=1.2)  + 
  theme_bw()+  theme(axis.text.x = element_text(size = 12, color = "black"),
                     axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),   
                     axis.line.x = element_line(color = "black"),
                     axis.line.y = element_line(color = "black"),
                     axis.title = element_text(size = 14),
                     panel.grid.major.y = element_blank(), 
                     legend.position = "none")

p3 + p4
