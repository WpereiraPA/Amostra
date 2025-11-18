calc_tamanho_amostra_interativa <- function() {
  # Solicita os parâmetros ao usuário via console
  conf.level <- as.numeric(readline(prompt="Digite o nível de confiança (ex: 0.95 para
  95%): "))
  s <- as.numeric(readline(prompt="Digite o desvio padrão populacional: "))
  E <- as.numeric(readline(prompt="Digite o erro máximo permitido (E): "))

  # Calcula o valor crítico Z a partir do nível de confiança
  alpha <- 1 - conf.level
  Z <- qnorm(1 - alpha / 2)

  # Calcula o tamanho da amostra
  N <- ((Z * s) / E)^2
  # Exibe o resultado
  cat("Tamanho calculado foi N =", format(round(N, 1), nsmall = 1), "\n")

  return(N)
}


