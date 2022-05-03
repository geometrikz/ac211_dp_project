library(ggplot2)
library(data.table)
library(magrittr)

nsims = 1000
epsilons = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 2.0, 4.0, 8.0, 16.0, 32)
epsilons = exp(seq(from = log(0.01), to = log(32), length.out = 32))

df = fread('aggregated.csv')
df_counts = fread('group_statistics.csv')

lm_true = lm(income ~ . + sex, data=df)
summary(lm_true)

generate_dpdf = function(df, df_counts, epsilon){
  df_dp = df
  n = nrow(df)
  df_dp$age = df_dp$age + ExtDist::rLaplace(n, 0, n*(df_dp$age)*1/(df_counts$married*epsilon))
  df_dp$educ = df_dp$educ + ExtDist::rLaplace(n, 0, n*(df_dp$educ)*1/(df_counts$married*epsilon))
  df_dp$sex = df_dp$sex + ExtDist::rLaplace(n, 0, n*1/(df_counts$sex*epsilon))
  df_dp$latino = df_dp$latino + ExtDist::rLaplace(n, 0, n*1/(df_counts$latino*epsilon))
  df_dp$asian = df_dp$asian + ExtDist::rLaplace(n, 0, n*1/(df_counts$asian*epsilon))
  df_dp$black = df_dp$black + ExtDist::rLaplace(n, 0, n*1/(df_counts$black*epsilon))
  df_dp$married = df_dp$married + ExtDist::rLaplace(n, 0, n*1/(df_counts$married*epsilon))
  df_dp$income = df_dp$income + ExtDist::rLaplace(n, 0, n*(df_dp$income)/(df_counts$married*epsilon))
  return(df_dp)
}                            

generate_dpdf(df, df_counts, 1.0)                          
                                

b0 = b1 = b2 = b3 = b4 = b5 = b6 = b7 = numeric(nsims*length(epsilons))
lm_df = data.table(b0, b1, b2, b3, b4,b5,b6,b7, epsilon=rep(epsilons, each=nsims))
# lm_df2dp = data.table(b0, b1, epsilon=rep(epsilons, each=nsims))

count = as.integer(0)
for(k in 1:length(epsilons)){
  for(i in 1:nsims){
    count = count + as.integer(1)
    eps = epsilons[k]
    df_dp = generate_dpdf(df, df_counts, eps) 
    # lm_dp = lm(log(income) ~ age + educ + asian + black + married, data=df_dp)
    lm_dp = lm(income ~ ., data=df_dp)
    # set(lm_df, count, names(lm_df), as.list(c(lm_p$coefficients, eps)))
    set(lm_df, count, names(lm_df), as.list(c(lm_dp$coefficients, eps)))
  }
}

lm_df_dp = lm_df
lm_dfdp_viz = merge(melt(lm_df_dp[,lapply(.SD, function(x) mean(x)), by=epsilon], id='epsilon', value.name='mean'), 
                    melt(lm_df_dp[,lapply(.SD, function(x) sd(x)), by=epsilon], id='epsilon', value.name='sd'))

# fwrite(lm_dfdp_viz, 'viz_df.csv')

varlist = names(lm_df)[1:8]
plot_list = list()

var_names = c('intercept', names(df))
i=0
for(var in varlist){
  i = i+1
  plot_list[[i]] = lm_dfdp_viz[variable == var] %>%
    ggplot(aes(
      x = epsilon,
      y = mean,
      ymin = mean - 2 * sd/sqrt(nsims),
      ymax = mean + 2 * sd/sqrt(nsims)
    )) +
    geom_hline(yintercept = lm_true$coefficients[i],
               color = '#37A2FF',
               size = 1) +
    geom_line() +
    geom_point() +
    # geom_errorbar(width = 0) +
    scale_x_log10() + ylab(var_names[i]) +
    scale_y_continuous(trans = ggallin::pseudolog10_trans) +
    theme_linedraw()
  # ggsave(paste0(var,'.png'), p0)
}
library(gridExtra)
library(ggplot2)
do.call(grid.arrange,plot_list)




