teste_t_interativo <- function() {

  cat("========================================\n")
  cat("  TESTE T PAREADO COM INTERPRETAÇÃO\n")
  cat("========================================\n\n")

  # Entrada da primeira variável

  cat("Digite os valores da primeira variável (Variável A, Medida 1)\n")
  cat("Separe os valores por vírgula (ex: 4.05, 3.69, 4.10)\n")
  entrada1 <- readline(prompt = "> ")
  var1 <- as.numeric(strsplit(entrada1, ",")[[1]])

  cat("\nDigite os valores da segunda variável (Variável B, Medida 2)\n")
  cat("Separe os valores por vírgula (ex: 3.77, 3.85, 4.07)\n")
  entrada2 <- readline(prompt = "> ")
  var2 <- as.numeric(strsplit(entrada2, ",")[[1]])

  # Validação
  if (length(var1) != length(var2)) {
    cat("\nERRO: As duas variáveis devem ter o mesmo número de valores!\n")
    return(NULL)
  }
  # Executar teste t pareado
  resultado <- t.test(var1, var2, paired = TRUE)

  # Gerar interpretação
  if (resultado$p.value >= 0.05) {
    interpretacao <- paste(
      "Aceita-se a hipótese nula de igualdade entre as médias (p =",
      round(resultado$p.value, 4),
      ", t =", round(resultado$statistic, 4), ").",
      "Não há diferença estatística significativa."
    )
  } else {
    interpretacao <- paste(
      "Rejeita-se a hipótese nula. Existe diferença estatística significativa entre as médias (p =",
      round(resultado$p.value, 4), ", t =", round(resultado$statistic, 4), ")."
    )
  }
  # Exibir resultado
  cat("\n========================================\n")
  cat("RESULTADO DO TESTE T PAREADO\n")
  cat("========================================\n\n")
  cat("Estatística t:", round(resultado$statistic, 4), "\n")
  cat("Graus de liberdade:", resultado$parameter, "\n")
  cat("P-valor:", round(resultado$p.value, 4), "\n")
  cat("Diferença média:", round(resultado$estimate, 4), "\n\n")
  cat("Interpretação:\n", interpretacao, "\n\n")

  # Salvar no Environment
  assign("interpretacao", interpretacao, envir = .GlobalEnv)
  assign("resultado_teste", resultado, envir = .GlobalEnv)

  cat("Objetos salvos no Environment: 'interpretacao' e 'resultado_teste'\n")

  return(invisible(resultado))
}

# ====================================================
# EXECUTAR A FUNÇÃO
# ====================================================

teste_t_interativo()
