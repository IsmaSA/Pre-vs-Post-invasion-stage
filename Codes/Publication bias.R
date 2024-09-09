meta <- c(meta6, meta7,meta7.1, meta7.2, meta7.3)

# Egger test:
for (i in 1:lenght(meta)){
  resid <- rstandard(meta6)
  eggers <- regtest(x = resid$resid, sei =sqrt(data1$variance_G), model = "lm") 
  print(eggers)
}



# Funnel plot:
biases <- list(meta_7, meta_7.1, meta_7.2, meta_7.3)
par(mar=c(4, 4, 2, 2))

p1<- funnel(meta6, xlab = "Effect Size", ylab = "Standard Error",back = "white")
p2<- funnel(meta_7, xlab = "Effect Size", ylab = "Standard Error",back = "white")
p3<- funnel(meta_7.1, xlab = "Effect Size", ylab = "Standard Error",back = "white")
p4<- funnel(meta_7.2, xlab = "Effect Size", ylab = "Standard Error",back = "white")
p5<- funnel(meta_7.3, xlab = "Effect Size", ylab = "Standard Error",back = "white")

p2+ p3+
  p4+p5
