load("Dados//Models_Data.RData")
dados_1 <- Models_Data
dados <- Models_Data |> 
  dplyr::mutate(
    GHGC = log(GHGC)
  ) |>
  dplyr::select(
    -c(1)
  )

shapiro.test(CO2)

cor_dados <- cor(dados) 
corrplot::corrplot(cor_dados, 
                   type = "upper", 
                   method = "color", 
                   col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"), # Color palette
                   addCoef.col = "black", # Add correlation coefficients
                   tl.col = "black", # Text label color
                   tl.srt = 45, # Text label rotation
                   number.cex = 0.7, # Size of the correlation coefficient numbers
                   cl.cex = 0.7, # Size of the color legend text
                   title = "Gráfico de correlação das variáveis", 
                   mar = c(0,0,1,0)) # Title margin
