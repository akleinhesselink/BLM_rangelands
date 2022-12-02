rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(optimx)
library(dfoptim)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

load('data/analysis_data/cover01.rda')
attach(cover0.01)

control_lmer$optCtrl$eval.max <- 1e9
control_lmer$optCtrl$iter.max <- 1e9

trend_formula <- formula(log_value ~ t * ecoregion +
                           (t | ecoregion:OFFICE ) + 
                           (t | uname) 
)

trend_formula2 <- update(trend_formula, . ~ . - t:ecoregion)
trend_formula3 <- update(trend_formula2, . ~ . - ecoregion)
trend_formula4 <- update(trend_formula2, . ~ . - t)

# Now run with all data 
m_afg_ML <- lmer(data = AFG, 
                 trend_formula, 
                 control = control_lmer, REML = F)

summary(m_afg_ML)

m_afg2 <- lmer(data = AFG, trend_formula2, control_lmer, REML = F)
m_afg3 <- lmer(data = AFG, trend_formula3, control_lmer, REML = F)
m_afg4 <- lmer(data = AFG, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_afg_ML, m_afg2, m_afg3, m_afg4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')

kableExtra::save_kable(table, file = 'output/tables/afg_model_comparison_table.01.md')

# Refit with REML
m_afg <- lmer(data = AFG, 
              trend_formula, 
              control = control_lmer)

ecoregion_effects <- get_ecoregion_trends( m_afg) 

random_effects <- get_blm_random_effects( m_afg)

AFG_trends <- blm_trend_summary( AFG, ecoregion_effects, random_effects) 

# Fit Tree  cover --------
m_tree_ML <- lmer(data = TRE, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_tree2 <- lmer(data = TRE, trend_formula2, control_lmer, REML = F)
m_tree3 <- lmer(data = TRE, trend_formula3, control_lmer, REML = F)
m_tree4 <- lmer(data = TRE, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_tree_ML, m_tree2, m_tree3, m_tree4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')

kableExtra::save_kable(table, file = 'output/tables/tree_model_comparison_table.01.md')

m_tree <- lmer(data = TRE, trend_formula, control = control_lmer)
ecoregion_effects <- get_ecoregion_trends(m_tree ) 
random_effects <- get_blm_random_effects( m_tree)
tree_trends <- blm_trend_summary( TRE, ecoregion_effects , random_effects)


# ---------- Output 
saveRDS(m_afg, file = 'output/AFG_cover_trend_model.01.rds')
saveRDS(m_tree, file = 'output/TREE_cover_trend_model.01.rds')
#saveRDS(m_woody, file = 'output/WOODY_cover_trend_model.rds')

save(m_afg_ML, m_afg2, m_afg3, m_afg4, file = 'output/all_annual_cover_models.01.rda')
save(m_tree_ML, m_tree2, m_tree3, m_tree4, file = 'output/all_tree_cover_models.01.rda')

detach(cover0.01 )
rm( cover0.01 ) 

load('data/analysis_data/prod01.rda')
attach(prod0.01)

afg_attributes <- attributes( afgNPP$value2)
pfg_attributes <- attributes( pfgNPP$value2)

# test back-transformation 
stopifnot( 
  all.equal( back_transform(afgNPP$value2[1:10], afg_attributes), 
             afgNPP$value[1:10] ) ) 


# AFG TRENDS ------------------------------------------- # 
# AFG AGB TRENDS
m_afg_NPP_ML <- lmer(data = afgNPP, 
                     trend_formula, 
                     control = control_lmer, REML = F)

m_afg_NPP2 <- lmer(data = afgNPP, trend_formula2, control_lmer, REML = F)
m_afg_NPP3 <- lmer(data = afgNPP, trend_formula3, control_lmer, REML = F)
m_afg_NPP4 <- lmer(data = afgNPP, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_afg_NPP_ML, m_afg_NPP2, m_afg_NPP3, m_afg_NPP4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')

kableExtra::save_kable(table, file = 'output/tables/afg_NPP_model_comparison_table.01.md')

m_afg_NPP <- lmer(formula = trend_formula, 
                  data = afgNPP, 
                  control = control_lmer)

afg_fixed <- get_ecoregion_trends(m_afg_NPP)
afg_random <- get_blm_random_effects(m_afg_NPP)
afg_NPP_trends <- blm_trend_summary( afgNPP, afg_fixed, afg_random)

stopifnot( all(complete.cases(afg_NPP_trends)))

# SAVE AGB models and output 
# Herbaceous AGB Trends AFG + PFG = Total Herbaceous 

saveRDS(m_afg_NPP, file = 'output/AFG_NPP_trend_model.01.rds')

write_csv(afg_NPP_trends, file = 'output/AFG_NPP_group_trends.01.csv')

save(m_afg_NPP_ML, m_afg_NPP2, m_afg_NPP3, m_afg_NPP4, file = 'output/all_annual_prod_models.01.rda')

rm( prod0.01, m_afg_NPP)
