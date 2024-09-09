p3<- orchard_plot(object = meta_7.3, 
             mod = "Taxo_Func", 
             group = "site_id", 
             xlab = "Effect size (Hedges' G)", trunk.size = 12) + 
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
  term = c('Intercept', 'Taxonomical', 'Functional:late','Taxonomical:late'),
  estimate = c(0.0272, 0.0705, -0.0090, -0.0259),
  se = c(0.0247, 0.0147, 0.0204, 0.0134),
  zval = c(1.1025, 4.8134, -0.4413, -1.9392),
  pval = c(0.2703, '<.0001', 0.6590, 0.0525),
  ci.lb = c(-0.0211, 0.0418, -0.0491, -0.0521),
  ci.ub = c(0.0755, 0.0993, 0.0310, 0.0003),
  signif = c('', '***', '', '.')
)
model_results$term <- factor(model_results$term, levels = c(
  "Functional:late","Taxonomical:late","Taxonomical","Intercept"))

p4<-ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, size=1.2) +
  xlab("Effect Size") +
  ylab("Model terms") + geom_vline(xintercept = 0, linetype="dashed", size=1.2)  + 
  theme_bw()+  theme(axis.text.x = element_text(size = 12, color = "black"),
                     axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),   
                     axis.line.x = element_line(color = "black"),
                     axis.line.y = element_line(color = "black"),
                     axis.title = element_text(size = 14),
                     panel.grid.major.y = element_blank(), 
                     legend.position = "none")

p3 + p4

