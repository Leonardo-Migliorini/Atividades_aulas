library(ggplot2)

load("Dados//Models_Data.RData")
dados_1 <- Models_Data
dados <- Models_Data |> 
  dplyr::mutate(
    GHGC = log(GHGC)
  ) |>
  dplyr::select(
    -c(1,6)
  )

attach(dados)

shapiro.test(GHGC)
summary(GHGC)

cor_dados <- cor(dados) 

pdf(file = "Corrplot.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(corrplot::corrplot(cor_dados, 
                   type = "upper", 
                   method = "color", 
                   col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"), # Color palette
                   addCoef.col = "black", # Add correlation coefficients
                   tl.col = "black", # Text label color
                   tl.srt = 45, # Text label rotation
                   number.cex = 0.7, # Size of the correlation coefficient numbers
                   cl.cex = 0.7, # Size of the color legend text
                   mar = c(0,0,1,0))) # Title margin
dev.off()



pdf(file = "Histograma.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(data = dados, aes(x = GHGC)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "log(GHGC)",
       y = "Frequência") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank()))
dev.off()
