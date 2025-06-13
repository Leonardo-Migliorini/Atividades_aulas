source("UCHARMA_gen.R")
source("UCHARMA_fit.R")

set.seed(10)
serie <- simu.ucharma(100, phi = 0.5, theta = 1, sigma = 0.5)
summary(serie)
plot(serie)

ucharma.fit(serie, ar = 1, ma = 1)
