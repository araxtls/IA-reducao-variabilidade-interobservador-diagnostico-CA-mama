# PARÂMETROS GERAIS: intervalo de confiança, variancia e poder estatístico
alpha <- 0.05
power <- 0.90

z_alpha <- qnorm(1 - alpha/2)
z_beta  <- qnorm(power)

var_kappa0 <- 0.04
var_kappa1 <- 0.04

# FUNÇÃO PARA CALCULAR O N
calc_n <- function(kappa0, kappa1, var0, var1){
  delta <- kappa1 - kappa0
  n <- ((z_alpha + z_beta)^2 * (var0 + var1)) / (delta^2)
  return(ceiling(n))
}

# DIVIDINDO O CALCULO DO N DE ACORDO COM OS CENÁRIOS DE TIPO TUMORAL
# 1. Tumores pouco diferenciados / alta marcação (possuem um kappa alto)
kappa0_high <- 0.70
kappa1_high <- 0.85
N_high <- calc_n(kappa0_high, kappa1_high, var_kappa0, var_kappa1)

# 2) Tumores com diferenciação intermediária (possuem um kappa moderado)
kappa0_mid <- 0.50
kappa1_mid <- 0.65
N_mid <- calc_n(kappa0_mid, var_kappa0, kappa1_mid, var_kappa1)

# 3) Tumores bem diferenciados / baixa marcação (possuem um kappa baixo)
kappa0_low <- 0.30
kappa1_low <- 0.45
N_low <- calc_n(kappa0_low, kappa1_low, var_kappa0, var_kappa1)

# RESULTADO FINAL
data.frame(
  Cenario = c("Alta concordância", "Concordância moderada", "Baixa concordância"),
  Kappa_Sem_IA = c(kappa0_high, kappa0_mid, kappa0_low),
  Kappa_Com_IA = c(kappa1_high, kappa1_mid, kappa1_low),
  N_Estimado = c(N_high, N_mid, N_low)
)

N = cat("O tamanho mínimo da amostra em questão é de", sum(N_high, N_mid, N_low), "lâminas")
