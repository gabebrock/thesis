#' Table 5 shows the results for arrests, summons, and arrests or summons. 
#' 
#' To test for precinct effects of local crime and social conditions, we 
#' estimated the models first as OLS regressions without precinct-fixed effects, 
#' and then we reestimated the models including precinct fixed effects. 
#' 
#' We use the following OLS equation, illustrated here with Sanction as 
#' the dependent variable and several controls for suspect characteristics, 
#' event (suspected crime) characteristics, local crime rates, and 
#' social conditions: 
#' 
#' Black, Black Hispanic, and White Hispanic are dummy variables indicating 
#' that a person of that race was stopped. 
#' 
#' Model (2) is the same as model (1) except it incorporates additional terms 
#' that associate the RS factors with the three race dummy variables. 
#' 
#' The third set of models in Table 5 (in columns 7–9) are two-step arrest models. 
#' In this formulation, arrests are a two-stage process: the officer must first 
#' choose whether or not the person deserves some sort of sanction. 
#' Then, the officer must choose to arrest the person or issue a summons. 
#' Accordingly, we estimate arrest as a choice conditional on the decision to 
#' sanction. 
#' 
#' We first use a logistic model to estimate the probability of sanction and 
#' add the predicted probability of sanction to the estimation of which members 
#' of the sanction group are arrested.

# --- Data preparation ----

# Using `sqf_ols` from ~/analysis/test-hitrate.qmd

# load libraries
library(fixest)

# check for missing values in variables used in models
colSums(is.na(sqf_ols))

# impute zeros for NA values for `sqf_ols$female` 
# (assuming cop would have indicated a female detainee)
sqf_ols$female[is.na(sqf_ols$female)] <- 0

# --- Model 1: basic OLS with fixed effects (without interactions) ----

model1 <- feols(
  sanction ~ Black + Hisp + age + female + 
    RS_furtive + RS_crimloc + RS_casing + RS_other +
    RS_drug + RS_suspobj + RS_appear + RS_violent | 
    pct + year + pct:year + off_cat_broad, # fixed effects
  data = sqf_ols,
  cluster = ~ pct
)

summary(model1)

# --- Model 2: OLS with (race and suspicion factors) interactions ----

#' (A + B + C)*(X + Y + Z) in fixest() expands to main effects plus all 
#' interactions between suspicion factors and race dummies automatically.

model2 <- feols(
  sanction ~ (RS_furtive + RS_crimloc + RS_casing + RS_other +
                RS_drug + RS_suspobj + RS_appear + RS_violent)*
    (Black + Hisp) + age + female |
    pct + year + pct:year + off_cat_broad, # fixed effects
  data = sqf_ols,
  cluster = ~ pct
)

summary(model2)

# --- Model 3: Two-stage model for arrest conditional on sanction ----

#' Modeling officer decision AFTER a stop is made (re: similarly-situated persons):
#' Stage 1: Should this person receive any sanction at all?
#' Stage 2: If a sanction is imposed, should it be arrest rather than a summons?

# Step 1: Estimate the probability of sanction (logit)
sanction_model <- feglm(
  sanction ~ Black + Hisp + age + female + 
    RS_furtive + RS_crimloc + RS_casing + RS_other +
    RS_drug + RS_suspobj + RS_appear + RS_violent |
    pct + year + pct:year + off_cat_broad,
  data = sqf_ols,
  cluster = ~ pct,
  family = binomial("logit")
)

# Append predicted probability of sanction to SQF data frame
sqf_ols$prob_sanction <- NA_real_
idx <- sanction_model$obs_selection[[1]]
sqf_ols$prob_sanction[idx] <- predict(sanction_model, type = "response") # P(Sanction = 1 | X)

  # Sanity check
  sum(is.na(sqf_ols$prob_sanction)) # NAs should equal dropped obs in Step 1
  summary(sqf_ols$prob_sanction)

    #' `prob_sanction`:
    #' The officer’s predicted propensity to sanction this person, based on 
    #' observable characteristics, suspicion factors, and fixed effects.

# Step 2: Model decision to arrest conditional on sanction
arrest_model <- feols(
  arrest ~ Black + Hisp + age + female + prob_sanction |
    pct + year + pct:year + off_cat_broad,
  data = subset(sqf_ols, sanction == 1), # estimation of which members of the sanction group are arrested
  cluster = ~ pct
)

    #' `arrest_model`:
    #' Given two people who look equally likely to deserve some punishment, 
    #' are officers more likely to arrest one group than another instead of issuing a summons?

summary(arrest_model)

    #' Conditioning `arrest_model` on `prob_sanction` serves two primary functions.
    #' 
    #' First, it controls for selection bias. Due to the increased burden to issue
    #' a sanction, officers are more likely to sanction people who appear to be
    #' more suspicious or threatening, factors which also affect the likelihood of 
    #' arrest. Without `prob_sanction`, race coefficients could pick up 
    #' co-variations in suspicion as Black suspects could be more likely to be 
    #' arrested because they were more likely to be sanctioned in the first place.
    #' 
    #' Second, it separates severity choice from sanction choice. Race coefficients 
    #' are interpreted as racial differences in arrest vs summons, holding constant:
    #' observable characteristics, crime type, precinct/year conditions, and 
    #' the predicted likelihood that a sanction was warranted at all
