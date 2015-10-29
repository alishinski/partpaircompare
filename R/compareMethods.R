#' optimal pooled t-test
#'
#' @param x A vector of data, including missing values
#' @param y A vector of data, including missing values
#' @param data The data frame you got these from
#' @return The result of the optimal pooled t-test: adjusted t-test, degrees
#' of freedom and p-value
#' @details The
#' @examples optPairTtest(x,y)
optPairTtest(x,y) <-
  D_bar <- mean(x) - mean(y)
  SD_D_sqr <- var(x - y)

  # is this optimal speed-wise?
  data <- data.frame(x,y)
  unpaired_data <- data[is.na(x) | is.na(y),]

  S_0U_sqr <- var(unpaired_data$x)
  S_1U_sqr <- var(unpaired_data$y)

  n <- nrow(data)
  n0 <- length(!is.na(unpaired_data$x))
  n1 <- length(!is.na(unpaired_data$y))

  S1_sqr <- SD_D_sqr / n
  S2_sqr <- (S_0U_sqr / n0) + (S_1U_sqr / n1)

  omega <- (sqrt(S1_sqr) ^ -2) + (sqrt(S2_sqr) ^ -2)

  omega1 <- (sqrt(S1_sqr) ^ -2) / omega
  omega2 <- (sqrt(S2_sqr) ^ -2) / omega

  mu_hat1 <- D_bar
  mu_hat2 <- mean(unpaired_data$x) - mean(unpaired_data$x)

  mu_hat <- (omega1 * mu_hat1) + (omega2 * mu_hat2)

  S_mu_hat_sqr <- (1 + (4 * omega1 * (1 - omega1)/df1) + (4 * omega2 * (1 - omega2)/df2)) / omega

  toPool <- mu_hat / sqrt(S_mu_hat)

  df1 <- n - 1
  df2 <- (((S_0U_sqr / n0) + (S_1U_sqr / n1)) ^ 2 / (((S_0U_sqr ^ 2 / n0) / (n0 - 1)) + ((S_1U_sqr ^ 2 / n1) / (n1 - 1))))

  df = 1 / ((omega1 ^ 2 / df1) + (omega2 ^ 2 / df2))

  p-value <- pt(toPool, df = df)

  results <- list(toPool, df, p-value)

  invisible(results)

  print(output)
