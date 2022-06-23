rm(list = ls() )
library(tidyverse)
library(lme4)
library(emmeans)

n_G1 <- 16
n_G2 <- 8
n_Fa <- 4
n_t <- 30
n_reps <- 10
n_ID <- n_G1*n_reps

plots <- expand.grid(G1 = 1:n_G1, G2 = 1:n_G2, rep = 1:n_reps)

# G1 > G2 > ID
plots <- 
  plots %>% 
  mutate( G2 = factor(as.numeric(factor(paste( G1, G2))))) %>% 
  mutate( ID = factor(as.numeric(factor(paste( G2, rep )))))

plots <- 
  plots %>% left_join(
  plots %>% 
  distinct( G1) %>% 
  mutate( Fa = factor( sort(sample(1:n_Fa, n(), replace = T) )) ) 
)

plots %>% 
  group_by(Fa) %>% 
  summarise( n() , n_distinct(G1), n_distinct(G2), n_distinct(ID))

df <- expand.grid(ID = plots$ID, t = c(1:n_t)/10) %>% 
  arrange( ID, t ) %>% 
  left_join(plots )

nsizes <- df %>% 
  summarise( n_ID = n_distinct(ID), 
             n_G1 = n_distinct(G1), 
             n_G2 = n_distinct(G2), n_Fa = n_distinct(Fa))

df <- df %>% 
  ungroup() %>% 
  mutate( Fa_int = runif(nsizes$n_Fa, -1, 1)[Fa]) %>% 
  mutate( Fa_trend = runif(nsizes$n_Fa, -0.1, 0.1)[Fa]) %>% 
  mutate( G1_int = rnorm(nsizes$n_G1, 0, 1)[G1]) %>% 
  mutate( G1_trend = rnorm(nsizes$n_G1, 0, 0.01)[G1]) %>% 
  mutate( G2_int = rnorm(nsizes$n_G2, 0, 1)[G2]) %>% 
  mutate( G2_trend = rnorm(nsizes$n_G2, 0, 0.04)[G2])  %>% 
  mutate( ID_int = rnorm(nsizes$n_ID, 0, 1)[ID]) %>% 
  mutate( ID_trend = rnorm(nsizes$n_ID, 0, 0.02)[ID]) %>% 
  arrange( ID ) %>% 
  mutate( TotalIntcpt = Fa_int + G1_int + G2_int + ID_int , 
          TotalTrend = Fa_trend + G1_trend + G2_trend + ID_trend )

all_pars <- df %>% 
  select(-t) %>% 
  distinct()

sigma <- 0.1 
n <- nrow(df)

nrow(df)
n_distinct(df$ID)
sample_ids <- sample(1:1280, 200)

df_small <- df %>% 
  ungroup() %>% 
  #filter( ID %in% sample_ids) %>% 
  arrange(ID, t) %>% 
  mutate( y = TotalIntcpt + TotalTrend*t + rnorm( n(), 0, sd = sigma)) %>% 
  mutate( y2 = scale(y))

dfPooled <- df_small %>% 
  group_by(G1, G2, Fa, t) %>% 
  summarise(y_avg = mean(y)) %>% 
  ungroup()  %>% 
  mutate( y_avg2 =  scale(y_avg))

mPooled <- lmer(data = dfPooled, formula = y_avg ~ Fa*t + (t|G1) + (t|G2) )
mIndivid <- lmer(data = df_small, formula = y ~ Fa*t + (t|G1) + (t|G2) + (t|ID))

emtrends(mPooled, ~ Fa, "t")
emtrends(mIndivid, ~ Fa, "t")

all_pars %>% 
  distinct( Fa, Fa_int, Fa_trend) %>% 
  arrange( Fa )

pooled_blups <- ranef(mPooled)
individual_blups <- ranef(mIndivid)


CompareG1 <- all_pars %>%
  #filter( ID %in% sample_ids) %>% 
  select( starts_with('G1')) %>% 
  arrange( G1) %>% 
  distinct() %>% 
  bind_cols(pooled_blups$G1) %>% 
  bind_cols(individual_blups$G1) 

CompareG2 <- all_pars %>% 
  #filter( ID %in% sample_ids) %>% 
  select( starts_with('G2')) %>% 
  arrange( G2) %>% 
  distinct()  %>% 
  bind_cols(pooled_blups$G2) %>% 
  bind_cols(individual_blups$G2)

names( CompareG2  ) <- c('G2', 'G2_int', 'G2_trend', 'Pooled_Int', 'Pooled_Trend', 'Individ_Int', 'Individ_Trend')

CompareG2 %>% 
  ggplot() + geom_point( aes( x = G2_int, y = Pooled_Int )) + 
  geom_abline( aes( intercept = 0 , slope = 1))

CompareG2 %>% 
  ggplot() + geom_point( aes( x = G2_int, y = Individ_Int )) + 
  geom_abline( aes( intercept = 0 , slope = 1))

CompareG2 %>% 
  ggplot() + geom_point( aes( x = G2_trend, y = Pooled_Trend )) + 
  geom_abline( aes( intercept = 0 , slope = 1))

CompareG2 %>% 
  ggplot() + geom_point( aes( x = G2_trend, y = Individ_Trend )) + 
  geom_abline( aes( intercept = 0 , slope = 1))

df_small$yhat <- predict( mPooled , newdata =  df_small)

separate_fits <- df_small %>% 
  mutate( resid = y - yhat) %>% 
  group_by(G1, G2, Fa, ID ) %>% 
  summarise( intercept1 = coef( lm( y ~ t ))[1], 
             trend1 = coef(lm(y~ t))[2], 
             intercept2 = coef(lm( resid ~ t))[1], 
             trend2 = coef(lm(resid~t))[2])

separate_fits %>% 
  group_by( G2 ) %>% 
  summarise( 
    avg_resid_trend = mean(trend2), sd_resid_trend = sd(trend2), var_resid_trend = var(trend2), 
    avg_trend = mean(trend1), sd_trend1 = sd(trend1), var_trend1 = var(trend1), n = n())

separate_fits %>%
  group_by( G1, G2, Fa) %>% 
  summarise(n = n() , avg_trend = mean(trend2), sd_trend = sd(trend2), var_trend = var(trend2)) %>%
  ungroup() %>% 
  summarise( n(), avg_trend = mean(avg_trend), avg_var_trend = mean(var_trend), avg_sd_trend = sqrt(mean(var_trend)), mean_sd_trend = mean(sd_trend))

summary(mIndivid)
summary(mPooled)

VarCorr(mPooled)
( as.data.frame( VarCorr(mPooled) ) [7, ]$vcov )


sd( rnorm(100, 0, sd = 0.1) )
