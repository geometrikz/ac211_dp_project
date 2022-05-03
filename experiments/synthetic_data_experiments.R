library(ggplot2)

genData <- function(N, eps)
{
  Y_err <- 2
  corr <- 0
  
  # Draw the true X's
  Z1_mean <- 7
  Z2_mean <- 9
  Z3_mean <- 3
  Z1 <- rpois(N, Z1_mean)
  Z2 <- Z1*corr + rpois(N, Z2_mean)
  Z3 <- rpois(N, Z3_mean)
  
  b0 <- 10
  b1 <- -8
  b2 <- -3
  b3 <- 9
  
  Y_res <- rnorm(N, mean = 0, sd = Y_err)
  Y <- b0 + b1*Z1 + b2*Z2 + b3*Z3 + Y_res
  
  original_data <- data.frame(Y = Y, Z1 = Z1, Z2 = Z2, Z3 = Z3)
  
  s1 <- sd(Z1)/eps
  s2 <- sd(Z2)/eps
  s3 <- sd(Z3)/eps
  X1 <- Z1 + ExtDist::rLaplace(N, 0, s1)
  X2 <- Z2 + ExtDist::rLaplace(N, 0, s2)
  X3 <- Z3 + ExtDist::rLaplace(N, 0, s3)
  
  
  sy = sd(Y)/eps
  
  Y2 = Y + ExtDist::rLaplace(N, 0, b = sy)
  
  data_error <- data.frame(Y = Y2, X1 = X1, X2 = X2, X3 = X3)
  err_vec <- data.frame(Y = sy, X1 = s1, X2 = s2, X3 = s3)
  data_error <- as.data.frame(rbind(err_vec, data_error))
  
  return(list(private_data = original_data, dp_data = data_error))
  
}

genData2 <- function(N, eps)
{
  
  pi0 <- 0.4
  p <- 0.2
  r <- 20
  n <- N
  
  ind <- sample(c(0, 1), n, prob = c(pi0, 1 - pi0), replace = TRUE)
  Z1 <- ind*rnbinom(n, mu = p*r/(1-p), size = r)
  s1 <- sd(Z1)/eps
  X1 <- Z1 + ExtDist::rLaplace(n, 0, b = s1)
  
  
  b0 <- 2
  b1 <- 8
  
  
  Y_res <- sapply(1:length(Z1), FUN = function(i) rnorm(1, mean = 0, sd = sqrt(10 + 2*Z1[i])))
  Y <- b0 + b1*Z1 + Y_res
  
  original_data <- data.frame(Y = Y, Z1 = Z1)
  s2 = sd(Y)/eps
  
  Y2 = Y + ExtDist::rLaplace(n, 0, b = s2)
  # Y2 = Y
  data_error <- data.frame(Y = Y2, X1 = X1)
  err_vec <- data.frame(Y = s2, X1 = s1)
  data_error <- as.data.frame(rbind(err_vec, data_error))
  
  return(list(private_data = original_data, dp_data = data_error))
  
}

library(data.table)
library(magrittr)

nsims = 100
epsilons = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 2.0, 4.0, 8.0, 16.0, 32)
# 10^(seq(30)/12-1.1)
b0 = b1 = b2 = b3 = numeric(nsims*length(epsilons))
lm_df = data.table(b0, b1, b2, b3, epsilon=rep(epsilons, each=nsims))
lm_df_dp = data.table(b0, b1, b2, b3, epsilon=rep(epsilons, each=nsims))

count = as.integer(0)
for(k in 1:length(epsilons)){
  for(i in 1:nsims){
    count = count + as.integer(1)
    eps = epsilons[k]
    df = genData(10000, eps=eps)
    lm_p = lm(Y ~ Z1 + Z2 + Z3, data=df$private_data)
    lm_dp = lm(Y ~ X1 + X2 + X3, data=df$dp_data[2:nrow(df$dp_data),])
    set(lm_df, count, names(lm_df), as.list(c(lm_p$coefficients, eps)))
    set(lm_df_dp, count, names(lm_df_dp), as.list(c(lm_dp$coefficients, eps)))
  }
}
lm_dfdp_viz = merge(melt(lm_df_dp[,lapply(.SD, function(x) mean(x)), by=epsilon], id='epsilon', value.name='mean'), 
                    melt(lm_df_dp[,lapply(.SD, function(x) sd(x)), by=epsilon], id='epsilon', value.name='sd'))


p0 = lm_dfdp_viz[variable == 'b0'] %>%
  ggplot(aes(
    x = epsilon,
    y = mean,
    ymin = mean - 2 * sd,
    ymax = mean + 2 * sd
  )) +
  geom_hline(yintercept = 10,
             color = '#37A2FF',
             size = 1) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_x_log10() + ylab('b0') +
  scale_y_continuous(trans = ggallin::pseudolog10_trans) +
  theme_linedraw()
p0
# ggsave('b0.png', p0, width = 5, height = 3.5)

p1 = lm_dfdp_viz[variable == 'b1'] %>% ggplot(aes(
  x = epsilon,
  y = mean,
  ymin = mean - 2 * sd,
  ymax = mean + 2 * sd
)) +
  geom_hline(yintercept = -8,
             color = '#37A2FF',
             size = 1) +
  geom_line(color='black') +
  geom_errorbar(width=0, color='black') +
  geom_point(color='black') +
  scale_x_log10() + ylab('b1') +
  scale_y_continuous(trans = ggallin::pseudolog10_trans) +
  theme_linedraw()
p1

# ggsave('b1.png', p1, width = 5, height = 3.5)

p2 = lm_dfdp_viz[variable == 'b2'] %>% ggplot(aes(
  x = epsilon,
  y = mean,
  ymin = mean - 2 * sd,
  ymax = mean + 2 * sd
)) +
  geom_hline(yintercept = -3,
             color = '#37A2FF',
             size = 1) +
  geom_line() +
  geom_errorbar(width=0) +
  geom_point() +
  geom_hline(yintercept = 0,
             color = '#A51C30',
             size = 0.75,
             linetype='dotted') +
  scale_x_log10() + ylab('b2') +
  # annotate(
  #   "text", label = "reversed coefficient",
  #   x = 5.5, y = 0.1, size = 3, colour = '#A51C30'
  # ) +
  # annotate(
  #   "text", label = "true coefficient",
  #   x = 0.175, y = -2.8, size = 3, colour = '#37A2FF'
  # ) +
  scale_y_continuous(trans = ggallin::pseudolog10_trans) +
  theme_linedraw()

p2

# ggsave('b2.png', p2, width = 5, height = 3.5)

p3 = lm_dfdp_viz[variable == 'b3'] %>% ggplot(aes(
  x = epsilon,
  y = mean,
  ymin = mean - 2 * sd,
  ymax = mean + 2 * sd
)) +
  geom_hline(yintercept = 9,
             color = '#37A2FF',
             size = 1) +
  geom_line() +
  geom_errorbar(width=0) +
  geom_point() +
  scale_x_log10() + ylab('b3') +
  scale_y_continuous(trans = ggallin::pseudolog10_trans) +
  theme_linedraw()


# ggsave('b3.png', p3, width = 5, height = 3.5)

gridExtra::grid.arrange(p0, p1, p2, p3)


nsims = 100
epsilons = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 2.0, 4.0, 8.0, 16.0, 32)
b0 = b1 = b2 = b3 = numeric(nsims*length(epsilons))
lm_df2 = data.table(b0, b1, epsilon=rep(epsilons, each=nsims))
lm_df2dp = data.table(b0, b1, epsilon=rep(epsilons, each=nsims))

count = as.integer(0)
for(k in 1:length(epsilons)){
  for(i in 1:nsims){
    count = count + as.integer(1)
    eps = epsilons[k]
    nrows=10000
    df2 = genData2(nrows, eps=eps)
    lm_p = lm(Y ~ Z1, data=df2$private_data)
    lm_dp = lm(Y ~ X1, data=df2$dp_data[2:nrows,])
    set(lm_df2, count, names(lm_df2), as.list(c(lm_p$coefficients, eps)))
    set(lm_df2dp, count, names(lm_df2dp), as.list(c(lm_dp$coefficients, eps)))
  }
}

lm_dfdp_viz = merge(melt(lm_df2dp[,lapply(.SD, function(x) mean(x)), by=epsilon], id='epsilon', value.name='mean'), 
                    melt(lm_df2dp[,lapply(.SD, function(x) sd(x)), by=epsilon], id='epsilon', value.name='sd'))

lm_dfdp_viz[variable=='b0'] %>% ggplot(aes(x=epsilon, y=mean, ymin=mean-2*sd, ymax=mean+2*sd)) + 
  geom_line() + 
  geom_errorbar() +
  geom_hline(yintercept=2, color='red') +
  scale_x_log10()

lm_dfdp_viz[variable=='b1'] %>% ggplot(aes(x=epsilon, y=mean, ymin=mean-2*sd, ymax=mean+2*sd)) + 
  geom_line() + 
  geom_errorbar() +
  geom_hline(yintercept=8, color='red') +
  scale_x_log10()
