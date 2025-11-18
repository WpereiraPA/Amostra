calc_tamanho_amostra_interativa <- function() {
  # Solicita os parâmetros ao usuário via console
  conf.level <- as.numeric(readline(prompt = "Digite o nível de confiança (ex: 0.95 para 95%): "))
  s <- as.numeric(readline(prompt = "Digite o desvio padrão: "))
  E <- as.numeric(readline(prompt = "Digite o erro máximo permitido (E): "))
  n <- as.numeric(readline(prompt = "Digite o tamanho da amostra (n): "))

  # Calcula o valor crítico t a partir do nível de confiança e tamanho da amostra
  alpha <- 1 - conf.level
  gl <- n - 1  # graus de liberdade
  t <- qt(1 - alpha / 2, df = gl)

  # Calcula o tamanho da amostra
  N <- ((t * s) / E)^2

  # Exibe o resultado
  cat("Graus de liberdade (gl) =", gl, "\n")
  cat("Valor crítico t =", round(t, 4), "\n")
  cat("Tamanho calculado foi N =", format(round(N, 1), nsmall = 1), "\n")

  return(N)
}
