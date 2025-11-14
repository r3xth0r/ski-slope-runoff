# Kling-Gupta Efficiency
kge <- function(truth, response) {
  r <- cor(truth, response)
  beta <- mean(response) / mean(truth)
  alpha <- sd(response) / sd(truth)
  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}
