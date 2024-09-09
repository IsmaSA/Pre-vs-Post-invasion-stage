
p1<- orchard_plot(object = meta7.2, trunk.size = 10,
                  mod = "sp_community", 
                  group = "site_id", 
                  xlab = "Effect size (Hedges' G)") + 
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),   
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none") +
  scale_color_manual(values = c("purple1", "turquoise")) +
  scale_fill_manual(values = c("purple1", "turquoise")) 


model_results <- data.frame(
  term = c("Community", "Species", "Community:late", "Species:late"),
  estimate = c(0.1107, 0.0653, -0.1863, -0.1057),
  se = c(0.0251, 0.0237, 0.0184, 0.0138),
  zval = c(4.4189, 2.7567, -10.1122, -7.6582),
  pval = c(0.00001, 0.0058, 0.00001, 0.00001),
  ci.lb = c(0.0616, 0.0189, -0.2224, -0.1327),
  ci.ub = c(0.1599, 0.1118, -0.1502, -0.0786)
)
model_results$term <- factor(model_results$term, levels = c(
  "Community:late","Community", "Species:late","Species"))

p2<-ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, size=1.5) +
  xlab("Effect Size") +
  ylab("Model terms") + geom_vline(xintercept = 0, linetype="dashed", size=1.2)  + 
  theme_bw()+   theme(axis.text.x = element_text(size = 12, color = "black"),
                      axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color="black"),  
                      axis.line.x = element_line(color = "black"),
                      axis.line.y = element_line(color = "black"),
                      axis.title = element_text(size = 14),
                      panel.grid.major.y = element_blank(), 
                      legend.position = "none")

p1 + p2
