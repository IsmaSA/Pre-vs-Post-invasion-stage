meta <- c(meta6, meta7,meta7.1, meta7.2, meta7.3)

# Egger test:
for (i in 1:lenght(meta)){
  resid <- rstandard(meta6)
  eggers <- regtest(x = resid$resid, sei =sqrt(data1$variance_G), model = "lm") 
  print(eggers)
}
