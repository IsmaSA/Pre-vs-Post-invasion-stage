
data_summary <- data1 %>% mutate(taxa2 =paste0(taxa,"_", site_id)) %>% 
  group_by(taxa2) %>%
  summarise(
    G_Hedges_mean = mean(G_Hedges),
    se_G_mean = sqrt(mean(variance_G)), # Standard error of the mean effect size
    lower_ci = G_Hedges_mean - 1.96 * se_G_mean,
    upper_ci = G_Hedges_mean + 1.96 * se_G_mean,
    .groups = 'drop' # This option removes the grouping structure afterwards
  )

overall_effect <- data.frame(
  taxa = "Overall",
  G_Hedges_mean = 0.030,
  lower_ci = -0.01, 
  upper_ci = 0.07)

nudge <- 0.5  
overall_y <- -1  

p0<- ggplot(data = data_summary, aes(x = G_Hedges_mean, y = reorder(taxa2, -G_Hedges_mean))) + 
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci),
                 height = 0, size = 0.5, color = "grey", 
                 position = position_nudge(y = nudge)) + 
  geom_point(size = 0.8, color = "darkgreen", 
             position = position_nudge(y = nudge)) +
  geom_vline(xintercept = 0, linetype = 2, size =1) +
  geom_hline(yintercept = nudge - 1, color = "black", linetype = 5, size = 1) + 
  #annotate("text", x = -4.2, y = overall_y, label= "Overall estimate", size = 4, adj = "right", fontface = "bold") +
  scale_y_discrete(expand = c(0.025, 0.01)) +
  xlab("Effect size (Hedges' G)") +
  ylab("Study case") +
  scale_x_continuous(breaks=c(-8,-6,-4,-2,0,2,4,6,8))+
  # Insert overall estimate
  geom_errorbarh(aes(xmin = overall_effect$lower_ci, xmax = overall_effect$upper_ci, y = overall_y), 
                 color = "grey") +
  geom_point(data = overall_effect, aes(x = G_Hedges_mean, y = overall_y), size = 3, color = "forestgreen") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none" )

# lets stry the orchard plot: 
install.packages("pacman")
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, emmeans)

devtools::install_github("daniel1noble/orchaRd", force = TRUE)
library(orchaRd)
orchaRd::orchard_plot

p0.1 <- orchaRd::orchard_plot(meta6, mod = "1", group = "site_id", 
                            xlab = "Standardised mean difference", 
                            transfm = "none", trunk.size = 10, alpha = 0.2)

p0.1 <- p0.1 + 
  scale_color_manual(values = "forestgreen", aesthetics = c("color", "fill")) + # Change point colors and fill
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        legend.position = "none"
  )+
  ylab("Effect size (Hedges' G)")

p2 + p1
