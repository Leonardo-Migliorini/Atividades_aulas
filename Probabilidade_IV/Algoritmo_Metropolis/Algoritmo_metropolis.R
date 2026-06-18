# ============================================================
# Implementação do Algoritmo de Metrópolis para o Problema do Caixeiro Viajante (PCV)
# Aplicação do algoritmo às Capitais Brasileiras
# Comparações com os métodos do Vizinho Mais Próximo e Vizinho Mais Distante
# ============================================================

# ============================================================
# Importando dados das capitais brasileiras e fixando uma seed para reprodutibilidade.
# ============================================================

set.seed(15)

# Origem dos dados: https://github.com/kelvins/municipios-brasileiros

# Filtrando as 27 capitais:
capitais <- readr::read_csv(
  "C:\\Users\\leona\\Documents\\GitHub\\Atividades_aulas\\Probabilidade_IV\\Algoritmo_Metropolis\\municipios.csv"
) |>
  dplyr::filter(capital == 1) |>
  dplyr::select(nome, latitude, longitude)

# ===========================================================
# Cálculo da matriz de distâncias entre as capitais
# ===========================================================

# Temos duas métricas para calcular as distâncias:
# - Distância Euclidiana: trata latitudes e longitudes como coordenadas planas.
# - Distância geodésica: considera a curvatura da Terra, usando a fórmula de Haversine.

# A fórmula de haversine é uma equação matemática amplamente utilizada na navegação e geolocalização para calcular a distância de "círculo máximo" (a menor distância linear) entre dois pontos na superfície de uma esfera, usando suas latitudes e longitudes.

# Função para calcular a distância geodésica usando a fórmula de haversine
haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # raio médio da Terra em km
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180 # são as latitudes dos pontos 1 e 2 (em radianos)
  dphi <- (lat2 - lat1) * pi / 180 # é a diferença entre as latitudes (em radianos)
  dlam <- (lon2 - lon1) * pi / 180 # é a diferença entre as longitudes (em radianos)
  a <- asin(sqrt(sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2)) # é a fórmula de haversine para calcular a distância angular entre os dois pontos
  2 * R * a # retorna a distância geodésica entre os dois pontos, em quilômetros
}

# Função para calcular a distância euclidiana
euclidiana <- function(lat1, lon1, lat2, lon2) {
  sqrt((lat2 - lat1)^2 + (lon2 - lon1)^2)
}

n <- nrow(capitais)
# matrizes de distâncias
MD_euclid <- matrix(0, n, n)
MD_geo <- matrix(0, n, n)

# Calculando as entradas das matrizes de distâncias
for (i in 1:n) {
  for (j in 1:n) {
    # calculando a distância geodésica
    MD_geo[i, j] <- haversine(
      capitais$latitude[i],
      capitais$longitude[i],
      capitais$latitude[j],
      capitais$longitude[j]
    )
    # calculando a distância euclidiana
    MD_euclid[i, j] <- euclidiana(
      capitais$latitude[i],
      capitais$longitude[i],
      capitais$latitude[j],
      capitais$longitude[j]
    )
  }
}

# Função para calcular a distância total da rota, (incluindo o retorno à cidade de origem)
custo_rota <- function(rota, dm = MD_geo) {
  sum(dm[cbind(rota, c(rota[-1], rota[1]))])
}

# ===========================================================
# Implementação do Algoritmo de Metrópolis
# ===========================================================

metropolis <- function(dm, nomes = NULL, T, print_int) {
  # Inicialização do algoritmo
  n <- nrow(dm) # número de cidades
  seq_inicial <- sample(n)
  rota_atual <- c(seq_inicial, seq_inicial[1]) # rota inicial escolhida aleatoriamente
  custo_atual <- custo_rota(rota_atual, dm) # custo da rota
  melhor_rota <- rota_atual # melhor rota encontrada até o momento
  melhor_custo <- custo_atual # custo da melhor rota encontrada até o momento

  # Armazena histótico de custos
  historico_custos <- custo_atual

  # Função para formatar os prints periódicos do algoritmo com os nomes das cidades
  formatar_rota <- function(rota) {
    paste(nomes[rota], collapse = " → ")
  }

  arquivo_log <- "melhores_rotas.txt"

  cat(
    "Historico de melhores solucoes\n\n",
    file = arquivo_log
  )

  # Armazena a quantidade de iterações realizadas pelo algoritmo
  iter <- 0L

  # withCallingHandlers é utilizada para capturar a interrupção do loop e permitir que o algoritmo imprima o resultado final
  resultado <- withCallingHandlers(
    tryCatch(
      {
        while (TRUE) {
          iter <- iter + 1L # atualiza o contador de iterações

          # Troca duas cidades de posição na rota atual para gerar uma nova rota candidata
          pos <- sample(n, 2)
          nova_rota <- rota_atual
          nova_rota[pos[1]] <- rota_atual[pos[2]]
          nova_rota[pos[2]] <- rota_atual[pos[1]]
          nova_rota[length(nova_rota)] <- nova_rota[1] # garante que a rota seja um ciclo, retornando à cidade de origem
          novo_custo <- custo_rota(nova_rota, dm)

          # Perturbação: 2-opt swap
          # pos <- sort(sample(n, 2))
          # nova_rota <- rota_atual
          # nova_rota[pos[1]:pos[2]] <- rev(nova_rota[pos[1]:pos[2]])
          # novo_custo <- custo_rota(nova_rota, dm)

          # Critério de Metrópolis
          prob_aceitar <- min(exp((custo_atual - novo_custo) / T), 1)
          if (runif(1) < prob_aceitar) {
            rota_atual <- nova_rota
            custo_atual <- novo_custo
            historico_custos <- c(historico_custos, custo_atual)
          }

          # Atualiza melhor a melhor rota encontrada até o momento
          if (custo_atual < melhor_custo) {
            melhor_rota <- rota_atual
            melhor_custo <- custo_atual

            cat(
              sprintf(
                "Iteracao: %d\nCusto: %.2f\nRota: %s\n\n",
                iter,
                melhor_custo,
                formatar_rota(melhor_rota)
              ),
              file = arquivo_log,
              append = TRUE
            )
          }

          # Impressão periódica da rota atual e da melhor rota encontrada até o momento
          if (iter %% print_int == 0L) {
            cat(sprintf("\n--- Iteração %d ---\n", iter))
            cat(sprintf(
              "  Atual  (%.0f km): %s\n",
              custo_atual,
              formatar_rota(rota_atual)
            ))
            cat(sprintf(
              "  Melhor (%.0f km): %s\n",
              melhor_custo,
              formatar_rota(melhor_rota)
            ))
          }
        }
      },
      interrupt = function(e) NULL
    )
  )

  # Print final após interrupção do loop
  cat(sprintf("\n\n=== Loop encerrado na iteração %d ===\n", iter))
  cat(sprintf("  Melhor distância: %.0f km\n", melhor_custo))
  cat(sprintf("  Melhor rota: %s\n", formatar_rota(melhor_rota)))

  # Lista de resultados
  list(
    rota = melhor_rota,
    rota_nomes = formatar_rota(melhor_rota),
    custo = melhor_custo,
    historico_custos = historico_custos,
    iteracoes = iter
  )
}

res_metro <- metropolis(
  MD_geo, # ou MD_euclid
  nomes = capitais$nome,
  T = 100,
  print_int = 50000
)

# Melhor resultado já encontrado, 13971km
# ===========================================================
# Algoritmos determinísticos
# ===========================================================
library(TSP)

# MD_euclid ou MD_geo podem ser usados como matriz de distâncias.
tsp_obj <- TSP(MD_geo)

# Método do vizinho mais próximo
res_nn <- solve_TSP(tsp_obj, method = "nearest_insertion")
# Método do vizinho mais distante
res_fd <- solve_TSP(tsp_obj, method = "farthest_insertion")

# Extrai vetor de índices e custo total
rota_nn <- as.integer(res_nn)
custo_nn <- tour_length(res_nn)

rota_fd <- as.integer(res_fd)
custo_fd <- tour_length(res_fd)

# ===========================================================
# Comparação entre os métodos
# ===========================================================

# Printado os resultados dos métodos no console
cat("Algoritmo de Metrópolis", res_metro$custo)
cat("Vizinho mais Próximo:", custo_nn)
cat("Vizinho mais Distante:", custo_fd)

# Print da melhor rota encontrada pelo Metrópolis
seq_rota <- res_metro$rota
cat(paste(capitais$nome[seq_rota], collapse = " → "), "\n")

# Plotando a melhor rota encontrada por cada método
plot_rota_geo <- function(cap, rota, titulo, cor_linha, lwd = 1.5) {
  rota_f <- c(rota, rota[1])
  plot(
    cap$longitude,
    cap$latitude,
    pch = 19,
    cex = 0.9,
    col = "gray20",
    xlab = "Longitude",
    ylab = "Latitude",
    main = titulo,
    asp = 1.2
  )
  lines(cap$longitude[rota_f], cap$latitude[rota_f], col = cor_linha, lwd = lwd)
  text(
    cap$longitude,
    cap$latitude,
    labels = cap$nome,
    pos = 3,
    cex = 1.1,
    col = "black"
  )
}

# Mapas das rotas
par(mfrow = c(1, 3), mar = c(3, 3, 3, 1))
plot_rota_geo(
  capitais,
  res_metro$rota,
  sprintf("Algoritmo de Metrópolis\n%.0f km", res_metro$custo),
  "#2166ac"
)
plot_rota_geo(
  capitais,
  rota_nn,
  sprintf("Vizinho mais Próximo\n%.0f km", custo_nn),
  "#d73027"
)
plot_rota_geo(
  capitais,
  rota_fd,
  sprintf("Vizinho mais Distante\n%.0f km", custo_fd),
  "#4dac26"
)

# Convergência do Algoritmo de Metrópolis
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
plot(
  # res_metro$historico_custos[1:1000],
  res_metro$historico_custos,
  type = "l",
  col = "#2166ac",
  lwd = 1.5,
  xlab = "Rotas aceitas",
  ylab = paste0("Distância total (Km)"),
  main = "Convergência do Algoritmo de Metrópolis"
)

# ===========================================================
# Visualização geográfica das rotas usando o pacote geobr
# ===========================================================

library(geobr)
library(sf)

brasil <- read_country(year = 2020, showProgress = FALSE)

plot_rota_mapa <- function(cap, rota, titulo, cor_linha, lwd = 1.5) {
  rota_f <- c(rota, rota[1])

  plot(
    st_geometry(brasil),
    col = "#f0f0f0",
    border = "gray60",
    main = titulo,
    xlim = c(-75, -28),
    ylim = c(-35, 6)
  )

  lines(cap$longitude[rota_f], cap$latitude[rota_f], col = cor_linha, lwd = lwd)
  points(cap$longitude, cap$latitude, pch = 19, cex = 0.9, col = "gray20")
  text(
    cap$longitude,
    cap$latitude,
    labels = cap$nome,
    pos = 3,
    cex = 1,
    col = "black"
  )
}

par(mfrow = c(1, 3), mar = c(3, 3, 3, 1))
plot_rota_mapa(
  capitais,
  res_metro$rota,
  sprintf("Algoritmo de Metrópolis\n%.0f km", res_metro$custo),
  "#2166ac"
)
plot_rota_mapa(
  capitais,
  rota_nn,
  sprintf("Vizinho mais Próximo\n%.0f km", custo_nn),
  "#d73027"
)
plot_rota_mapa(
  capitais,
  rota_fd,
  sprintf("Vizinho mais Distante\n%.0f km", custo_fd),
  "#4dac26"
)
