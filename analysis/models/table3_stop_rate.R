#' OLS Regressions of Precinct Stop Rates (logged) by 
#' Precinct-level Racial Composition and  Social and Crime Conditions, 
#' 2017 - 2023 (b, SE, p)

#' Table 3 shows a series of regressions to assess whether local racial 
#' composition influences officer allocations. Models 1 and 2 show that local 
#' racial composition is a significant predictor of officer allocations, even 
#' after controlling for other nonracial factors including crime, demography, 
#' and social conditions. 
#' 
#' Model 3 includes local crime rates in the past year. 
#' The effect size for the racial composition variables declines for % Black 
#' but remains positive and significant. The effect for the Latinx population 
#' becomes negative and significant, a reflection perhaps of the diversity of 
#' neighborhood contexts and social stratification of Latinx people in New York 
#' City. 
#' 
#' When we control for borough effects in Model 4, the effect for % Black 
#' increases substantially, and the effect for % Hispanic is no longer 
#' significant but is now positive, suggesting that stop rates may increase as 
#' the share of the Hispanic population increases by precinct.149 
#' 
#' Model 5 includes crime, social and economic covariates for the precinct, and 
#' borough-fixed effects. Including the crime rate reduces the significance of 
#' the % Black coefficient, and the % Latinx coefficient remains unchanged. 
#' Overall, allocations of officers are responsive to violent crime rates, 
#' yet the percentage size of the Black population in a precinct remains a 
#' significant predictor of officer allocations at the p<.10 level.

# using pct_year_ols from ~/analysis/test-hitrate.qmd

library(fixest) # feols() function for OLS regression with 
                # fixed-effects and clustering.

# Model 1: racial composition only
m1 <- feols(log_stops ~ pct_black + pct_hisp,
            data = pct_year_ols, 
            cluster = ~ pct) # cluster the SEs by precinct to account for 
                             # within-precinct correlation over time.

# Model 2: add nonracial demographic & social controls
m2 <- feols(log_stops ~ pct_black + pct_hisp +
              pop_density + pct_public_housing + pct_18_24,
            data = pct_year_ols,
            cluster = ~ pct)

# Model 3: add prior-year violent crime
m3 <- feols(log_stops ~ pct_black + pct_hisp +
              lag_log_violent_rate +
              pop_density + pct_public_housing + pct_18_24,
            data = pct_year_ols,
            cluster = ~ pct)

# Model 4: borough fixed effects
m4 <- feols(log_stops ~ pct_black + pct_hisp +
              lag_log_violent_rate +
              pop_density + pct_public_housing + pct_18_24 |
              BoroName,
            data = pct_year_ols,
            cluster = ~ pct)

# Model 5: full model with crime, social/economic controls, and FE
m5 <- feols(log_stops ~ pct_black + pct_hisp +
              lag_log_violent_rate + lag_log_nonviolent_rate +
              pop_density + pct_public_housing + pct_18_24 |
              BoroName,
            data = pct_year_ols,
            cluster = ~ pct)

etable(
  m1, m2, m3, m4, m5,
  
  dict = c(
    pct_black = "% Black",
    pct_hisp = "% Hispanic",
    lag_log_violent_rate = "Lag Violent Crime (log)",
    lag_log_nonviolent_rate = "Lag Nonviolent Crime (log)",
    pop_density = "Population Density",
    pct_public_housing = "% Public Housing",
    pct_18_24 = "% Age 18–24"
  ),
  
  headers = c(
    "Base Model \n (Race Only)",
    "Model 1 + \n Covariates",
    "Model 2 + \n Lag'd Crime Rate",
    "Model 3 + \n Boro Fixed Effects",
    "Model 4 + \n Change in Violent \n Crime Rate"
  ),
  
  se.below = TRUE,
  digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2,
  
  notes = "Clustered standard errors by precinct in parentheses. * p < .10, ** p < .05, *** p < .01.",
  view = TRUE
)
