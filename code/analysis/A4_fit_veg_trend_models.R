rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(optimx)
library(dfoptim)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

load('data/analysis_data/cover.rda')
attach(cover)

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

kableExtra::save_kable(table, file = 'output/tables/afg_model_comparison_table.md')

# Refit with REML
m_afg <- lmer(data = AFG, 
               trend_formula, 
               control = control_lmer)

ecoregion_effects <- get_ecoregion_trends( m_afg) 
random_effects <- get_blm_random_effects( m_afg)

AFG_trends <- blm_trend_summary( AFG, ecoregion_effects, random_effects) 

# Fit perennial cover data 
m_pfg_ML <- lmer(data = PFG, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_pfg2 <- lmer(data = PFG, trend_formula2, control_lmer, REML = F)
m_pfg3 <- lmer(data = PFG, trend_formula3, control_lmer, REML = F)
m_pfg4 <- lmer(data = PFG, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_pfg_ML, m_pfg2, m_pfg3, m_pfg4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/pfg_model_comparison_table.md')

# Refit with REML
m_pfg <- lmer(data = PFG, trend_formula, control = control_lmer)

ecoregion_effects <- get_ecoregion_trends(m_pfg ) 
random_effects <- get_blm_random_effects( m_pfg)

PFG_trends <- blm_trend_summary( PFG, ecoregion_effects , random_effects)

# Fit Bare Ground cover --------
m_bare_ML <- lmer(data = BGR, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_bare2 <- lmer(data = BGR, trend_formula2, control_lmer, REML = F)
m_bare3 <- lmer(data = BGR, trend_formula3, control_lmer, REML = F)
m_bare4 <- lmer(data = BGR, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_bare_ML, m_bare2, m_bare3, m_bare4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')

kableExtra::save_kable(table, file = 'output/tables/bare_model_comparison_table.md')

m_bare <- lmer(data = BGR, trend_formula, control = control_lmer)
ecoregion_effects <- get_ecoregion_trends(m_bare ) 
random_effects <- get_blm_random_effects( m_bare)
bare_trends <- blm_trend_summary( BGR, ecoregion_effects , random_effects)

# Fit Shrub  cover --------
m_shrub_ML <- lmer(data = SHR, 
                  trend_formula, 
                  control = control_lmer, REML = F)

m_shrub2 <- lmer(data = SHR, trend_formula2, control_lmer, REML = F)
m_shrub3 <- lmer(data = SHR, trend_formula3, control_lmer, REML = F)
m_shrub4 <- lmer(data = SHR, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_shrub_ML, m_shrub2, m_shrub3, m_shrub4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/shrub_model_comparison_table.md')


m_shrub <- lmer(data = SHR, trend_formula, control = control_lmer)
ecoregion_effects <- get_ecoregion_trends(m_shrub ) 
random_effects <- get_blm_random_effects( m_shrub)

shrub_trends <- blm_trend_summary( SHR, ecoregion_effects , random_effects)

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
kableExtra::save_kable(table, file = 'output/tables/tree_model_comparison_table.md')



m_tree <- lmer(data = TRE, trend_formula, control = control_lmer)
ecoregion_effects <- get_ecoregion_trends(m_tree ) 
random_effects <- get_blm_random_effects( m_tree)
tree_trends <- blm_trend_summary( TRE, ecoregion_effects , random_effects)

# Fit Woody Cover ------------------------------------------------ # 
# m_woody_ML <- lmer(data = WOODY, 
#                    trend_formula, 
#                    control = control_lmer, REML = F)
# 
# m_woody2 <- lmer(data = WOODY, trend_formula2, control_lmer, REML = F)
# m_woody3 <- lmer(data = WOODY, trend_formula3, control_lmer, REML = F)
# m_woody4 <- lmer(data = WOODY, trend_formula4, control_lmer, REML = F)
# 
# table <- MuMIn::model.sel(m_woody_ML, m_woody2, m_woody3, m_woody4) %>% 
#   data.frame() %>% 
#   kableExtra::kable(digits = 2, format = 'pipe')
# kableExtra::save_kable(table, file = 'output/tables/woody_model_comparison_table.md')
# 
# m_woody <- lmer(data = WOODY, trend_formula, control = control_lmer)
# 
# ecoregion_effects <- get_ecoregion_trends(m_woody ) 
# random_effects <- get_blm_random_effects( m_woody)
# woody_trends <- blm_trend_summary( WOODY, ecoregion_effects , random_effects)
# 

# ---------- Output 
saveRDS(m_afg, file = 'output/AFG_cover_trend_model.rds')
saveRDS(m_pfg, file = 'output/PFG_cover_trend_model.rds')
saveRDS(m_bare, file = 'output/BG_cover_trend_model.rds')
saveRDS(m_tree, file = 'output/TREE_cover_trend_model.rds')
saveRDS(m_shrub, file = 'output/SHR_cover_trend_model.rds')
#saveRDS(m_woody, file = 'output/WOODY_cover_trend_model.rds')

save(m_afg_ML, m_afg2, m_afg3, m_afg4, file = 'output/all_annual_cover_models.rda')
save(m_pfg_ML, m_pfg2, m_pfg3, m_pfg4, file = 'output/all_perennial_cover_models.rda')
save(m_shrub_ML, m_shrub2, m_shrub3, m_shrub4, file = 'output/all_shrub_cover_models.rda')
save(m_bare_ML, m_bare2, m_bare3, m_bare4, file = 'output/all_bare_cover_models.rda')
save(m_tree_ML, m_tree2, m_tree3, m_tree4, file = 'output/all_tree_cover_models.rda')
#save(m_woody_ML, m_woody2, m_woody3, m_woody4, file = 'output/all_woody_cover_models.rda')

write_csv(AFG_trends, file = 'output/AFG_cover_group_trends.csv')
write_csv(PFG_trends, file = 'output/PFG_cover_group_trends.csv')
write_csv(bare_trends, file = 'output/BG_cover_group_trends.csv')
write_csv(tree_trends, file = 'output/TREE_cover_group_trends.csv')
write_csv(shrub_trends, file = 'output/SHR_cover_group_trends.csv')
#write_csv(woody_trends, file = 'output/WOODY_cover_group_trends.csv')

detach(cover )
rm( cover ) 

load('data/analysis_data/prod.rda')
attach(prod)

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
kableExtra::save_kable(table, file = 'output/tables/afg_NPP_model_comparison_table.md')


m_afg_NPP <- lmer(formula = trend_formula, 
                  data = afgNPP, 
                  control = control_lmer)

afg_fixed <- get_ecoregion_trends(m_afg_NPP)
afg_random <- get_blm_random_effects(m_afg_NPP)
afg_NPP_trends <- blm_trend_summary( afgNPP, afg_fixed, afg_random)

stopifnot( all(complete.cases(afg_NPP_trends)))

# PFG AGB Trends 
m_pfg_NPP_ML <- lmer(data = pfgNPP, 
                     trend_formula, 
                     control = control_lmer, REML = F)

m_pfg_NPP2 <- lmer(data = pfgNPP, trend_formula2, control_lmer, REML = F)
m_pfg_NPP3 <- lmer(data = pfgNPP, trend_formula3, control_lmer, REML = F)
m_pfg_NPP4 <- lmer(data = pfgNPP, trend_formula4, control_lmer, REML = F)

table <- MuMIn::model.sel(m_pfg_NPP_ML, m_pfg_NPP2, m_pfg_NPP3, m_pfg_NPP4) %>% 
  data.frame() %>% 
  kableExtra::kable(digits = 2, format = 'pipe')
kableExtra::save_kable(table, file = 'output/tables/pfg_NPP_model_comparison_table.md')

#
m_pfg_NPP <- lmer(formula = trend_formula, 
                  data = pfgNPP, 
                  control = control_lmer)

pfg_fixed <- get_ecoregion_trends(m_pfg_NPP)
pfg_random <- get_blm_random_effects(m_pfg_NPP)
pfg_NPP_trends <- blm_trend_summary( pfgNPP, pfg_fixed, pfg_random)

# SAVE AGB models and output 
# Herbaceous AGB Trends AFG + PFG = Total Herbaceous 

saveRDS(m_afg_NPP, file = 'output/AFG_NPP_trend_model.rds')
saveRDS(m_pfg_NPP, file = 'output/PFG_NPP_trend_model.rds')
# saveRDS(m_herbaceous_NPP, file = 'output/HERB_NPP_trend_model.rds')

write_csv(afg_NPP_trends, file = 'output/AFG_NPP_group_trends.csv')
write_csv(pfg_NPP_trends, file = 'output/PFG_NPP_group_trends.csv')
# write_csv(herbaceous_trends, file = 'output/HERB_NPP_group_trends.csv')

save(m_afg_NPP_ML, m_afg_NPP2, m_afg_NPP3, m_afg_NPP4, file = 'output/all_annual_prod_models.rda')
save(m_pfg_NPP_ML, m_pfg_NPP2, m_pfg_NPP3, m_pfg_NPP4, file = 'output/all_perennial_prod_models.rda')
# save(m_herbaceous_NPP_ML, m_herbaceous_NPP2, m_herbaceous_NPP3, m_herbaceous_NPP4, file = 'output/all_herbaceous_prod_models.rda')

rm( prod, m_afg_NPP, m_pfg_NPP)
