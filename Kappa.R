install.packages(c("irr", "psych"))
library(irr)
library(psych)

### ============================================================
### ESTRUTURA DA SUA BASE DE DADOS (EXEMPLO)
### ============================================================

# Linha = 1 lâmina
# Coluna = um avaliador
# Exemplo:
df <- data.frame(
  patologista1 = c(0, 1, 1, 2, 3, 2, 1),
  patologista2 = c(0, 1, 2, 2, 3, 2, 1),
  patologista3 = c(0, 1, 1, 2, 3, 1, 1)
)

### ============================================================
### 1) COHEN’S KAPPA (para dois avaliadores)
### ============================================================

# Cohen kappa entre patologista 1 e 2
cohen_12 <- kappa2(df[, c("patologista1", "patologista2")])
cohen_12

# Se quiser comparar 1 vs 3:
cohen_13 <- kappa2(df[, c("patologista1", "patologista3")])
cohen_13


### ============================================================
### 2) WEIGHTED KAPPA (variáveis ordinais)
### Pesos: "equal", "squared" ou "linear"
### ============================================================

# Weighted kappa 1 vs 2 (pesos quadráticos — mais comum)
weighted_12 <- kappa2(df[, c("patologista1", "patologista2")], weight = "squared")
weighted_12

# Pesos lineares:
weighted_12_linear <- kappa2(df[, c("patologista1", "patologista2")], weight = "linear")
weighted_12_linear


### ============================================================
### 3) FLEISS’ KAPPA (≥ 3 avaliadores)
### ============================================================

# Fleiss kappa para TODOS os avaliadores
fleiss <- kappam.fleiss(df)
fleiss


### ============================================================
### 4) FUNÇÃO
### ============================================================

calcular_kappas <- function(data, pesos = "squared") {
  
  # Resultados armazenados
  resultados <- list()
  
  # Cohen Kappa (todas as combinações de pares)
  avaliadores <- colnames(data)
  pares <- combn(avaliadores, 2, simplify = FALSE)
  
  resultados$cohen <- lapply(pares, function(p) {
    list(
      par = p,
      kappa = irr::kappa2(data[, p])
    )
  })
  
  # Weighted Kappa (todas as combinações de pares)
  resultados$weighted <- lapply(pares, function(p) {
    list(
      par = p,
      kappa = irr::kappa2(data[, p], weight = pesos)
    )
  })
  
  # Fleiss Kappa (todos os avaliadores juntos)
  resultados$fleiss <- psych::kappam.fleiss(data)
  
  return(resultados)
}

### ============================================================
### 5) RESULTADOS
### ============================================================

resultado_final <- calcular_kappas(df)
resultado_final$cohen
resultado_final$weighted
resultado_final$fleiss
