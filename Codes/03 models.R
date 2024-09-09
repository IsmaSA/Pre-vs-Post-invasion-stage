


meta_6 <- metafor::rma.mv(G_Hedges,  variance_G, 
                          random = ~ 1 | Country/River/site_id,
                          data = data1, method = "REML")

summary(meta_6)




  
data1$Stage <- as.factor(data1$Stage)
data1$Stage <- relevel(data1$Stage, ref = "early")
meta_7 <- metafor::rma.mv(G_Hedges,  variance_G, 
                          random = ~ 1 | Country/River/site_id,
                          mods = ~ Stage - 1,
                          data = data1, method = "REML")
summary(meta_7)






data11 <- data1 %>%
  mutate(taxa = ifelse(taxa %in% c( "FEve_eco","FEve_bio", "FDis_eco","FDis_bio",
                                   "Richness"), taxa, "Abundance"))

meta_7.1 <- metafor::rma.mv(G_Hedges,  variance_G, 
                          random = ~ 1 | Country/River/site_id,
                          mods = ~ taxa+ Stage:taxa,
                          data = data11, method = "REML")
summary(meta_7.1)






meta_7.2 <- metafor::rma.mv(G_Hedges, variance_G, 
                          random = ~ 1 | Country/River/site_id,
                          mods = ~ sp_community + Stage:sp_community,
                          data = data11, method = "REML")
summary(meta_7.2)





meta_7.3 <- metafor::rma.mv(G_Hedges, variance_G, 
                            random = ~ 1 | Country/River/site_id,
                            mods = ~   Taxo_Func + Stage:Taxo_Func,
                            data = data11, method = "REML")
summary(meta_7.3)

