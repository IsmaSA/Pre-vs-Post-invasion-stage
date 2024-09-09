
p1<- orchard_plot(object = meta_7.2, trunk.size = 12,
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
  term = c("Intercept", "Species", "Community:late", "Species:late"),
  estimate = c(0.1119, -0.0460, 0.0211, -0.0498),
  se = c(0.0236, 0.0139, 0.0184, 0.0138),
  ci.lb = c(0.0657, -0.0732, -0.0150, -0.0769),
  ci.ub = c(0.1582, -0.0188, 0.0572, -0.0227)
)
model_results$term <- factor(model_results$term, levels = c(
  "Community:late","Species:late","Species","Intercept"))

p2<-ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, size=1.5) +
  xlab("Effect Size") +
  ylab("Model terms") + geom_vline(xintercept = 0, linetype="dashed", size=1.2)  + 
  theme_bw()+ theme(axis.text.x = element_text(size = 12, color = "black"),
                    axis.text.y = element_text(size = 12, vjust = 0.5, angle = 0, color = "black"),   
                    axis.line.x = element_line(color = "black"),
                    axis.line.y = element_line(color = "black"),
                    axis.title = element_text(size = 14),
                    panel.grid.major.y = element_blank(), 
                    legend.position = "none")

p1 + p2
