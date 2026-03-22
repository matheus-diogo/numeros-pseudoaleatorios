# Método da Transformada Inversa ###############################################################

## Garantir reprodutibilidade

set.seed(2026)

## Gerar 30.000 números entre 0 e 1 com distribuição uniforme pelo método Mersenne-Twister
U1 <- runif(30000); U2 <- runif(30000); U3 <- runif(30000); U4 <- runif(30000)

## Gerar 30.000 números a partir de uma distribuição pelo Método da Transformada Inverva

### Para distribuição Logística
mu <- 0; sigma <- 1; X1 <- qlogis(U1, mu, sigma)

### Para distribuição Exponencial
lambda.exp <- 1; X2 <- qexp(U2, lambda.exp)

### Para distribuição de Weibull
k <- 10; lambda.weibull <- 2; X3 <- qweibull(U3, k, lambda.weibull)

### Para distribuição Uniforme
a <- 5; b <- 10; X4 <- qunif(U4, a, b)

# Validação dos Resultados #####################################################################

### Histograma e Gráfico Q-Q ###################################################################

#### Considere do todos os percentis no Gráfico Q-Q
p <- seq(0.01,0.99,0.01)

#### Para distribuiçao Logística
hist(
  x = X1,
  ylim = c(0,dlogis(mu, mu, sigma)),
  probability = TRUE,
  main = "Distribuição de X1 e Curva Logística",
  xlab = "Números gerados",
  ylab = "Densidade",
  col = "orange",
  border = "orange4"
); curve(dlogis(x, mu, sigma), lwd = 2, add = TRUE)
qqplot(
  x = qlogis(p, mu, sigma),
  y = quantile(X1, probs = p),
  main = "Comparativo com Distribuição Logística",
  xlab = "Quantis teóricos",
  ylab = "Quantis amostrais",
  col = "orange3"
); abline(0,1)

#### Para distribuição Expoencial
hist(
  x = X2,
  ylim = c(0,lambda.exp),
  probability = TRUE,
  main = "Distribuição de X2 e Curva Exponencial",
  xlab = "Números gerados",
  ylab = "Densidade",
  col = "orange",
  border = "orange4"
); curve(dexp(x, lambda.exp), lwd = 2, add = TRUE)
qqplot(
  x = qexp(p, lambda.exp),
  y = quantile(X2, probs = p),
  main = "Comparativo com Distribuição Exponencial",
  xlab = "Quantis teóricos",
  ylab = "Quantis amostrais",
  col = "orange3"
); abline(0,1)

#### Para distribuição de Weibull
hist(
  x = X3,
  probability = TRUE,
  main = "Distribuição de X3 e Curva de Weibull",
  xlab = "Números gerados",
  ylab = "Densidade",
  col = "orange",
  border = "orange4"
); curve(dweibull(x, k, lambda.weibull), lwd = 2, add = TRUE)
qqplot(
  x = qweibull(p, k, lambda.weibull),
  y = quantile(X3, probs = p),
  main = "Comparativo com Distribuição de Weibull",
  xlab = "Quantis teóricos",
  ylab = "Quantis amostrais",
  col = "orange3"
); abline(0,1)

#### Para distribuição Uniforme
hist(
  x = X4,
  probability = TRUE,
  main = "Distribuição de X4 e Curva Uniforme",
  xlab = "Números gerados",
  ylab = "Densidade",
  col = "orange",
  border = "orange4"
); curve(dunif(x, a, b), lwd = 2, add = TRUE)
qqplot(
  x = qunif(p, a, b),
  y = quantile(X4, probs = p),
  main = "Comparativo com Distribuição Uniforme",
  xlab = "Quantis teóricos",
  ylab = "Quantis amostrais",
  col = "orange3"
); abline(0,1)

### Teste de Kolmogorov-Smirnov (KS) e Teste Anderson-Darling (AD) #############################

#### Considere que a hipótese nula é que as funções de distribução acumulada das distribuições
#### empíricas e teóricas são iguais e que o nível de significância é de 5%
alpha <- 0.05

#### Definir função para teste de KS e AD
library(goftest); library(magrittr)
teste.ks.ad <- function(amostra, teórica, ...) {
  p.valor.ks <- ks.test(amostra, teórica, ...) %>% .$p.value
  p.valor.ad <- ad.test(amostra, teórica, ...) %>% .$p.value
  p.valores = c(p.valor.ks, p.valor.ad)
  return(data.frame(
    teste = c("KS","AD"),
    p.valor = p.valores,
    decisão = c(ifelse(p.valores<alpha,"Rejeitar Ho","Não rejeitar Ho"))
  ))
}

#### Para distribuição Logística
teste.ks.ad(X1, "plogis", mu, sigma)

#### Para distribuição Exponencial
teste.ks.ad(X2, "pexp", lambda.exp)

#### Para distribuição de Weibull
teste.ks.ad(X3, "pweibull", k, lambda.weibull)

#### Para distribuição Uniforme
teste.ks.ad(X4, "punif", a, b)

# Comparação com o Método de Aceitação-Rejeição ################################################

