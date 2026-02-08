---
title: "OLS Model Specification"
author: "Gabriel Brock"
date: "`r Sys.Date()`"
output: pdf
---

# OLS Model Specification

I want to use a series of Ordinary Least Squares (OLS) regressions to test for racial bias in two dimensions:

1. Stop rates
2. Stop outcomes (arrests, summones, frisks, searches, force, and "hit rates")

Although many outcomes are binary (e.g., arrest = 0/1), I use OLS “linear probability models” for transparency and interpretability.

## OLS models of stop rates

Unit of analysis: precinct–year

Dependent variable:
- Logged stop rate per 10,000 residents

Independent variables:
- Precinct racial composition (% Black, % Hispanic)
- Crime rates (violent and non-violent complaints, logged)
- Socioeconomic controls (population density, % 18-24, % public housing)
- Year and borough fixed effects

Model structure:
- Begins with race-only models
- Sequentially adds:
    - Social and demographic covariates
    - Lagged crime rates
    - Borough fixed effects
    - Precinct fixed effects

Why OLS here?
Stops are treated as a continuous rate, and logging reduces skew and leverage from high-stop precincts.

Purpose:
To test whether racial composition predicts stop rates after controlling for crime.

## OLS models of stop outcomes (“hit rates”)

Unit of analysis: individual stop

Dependent variables (binary, modeled with OLS):
- Arrest
- Summons
- Arrest vs. summons (conditional on sanction)
- Frisk
- Search
- Seizure of contraband
- Weapons found
- Use of force

Key independent variables:
- Suspect race (Black, Black Hispanic, White Hispanic; White is reference)
- Age, gender
- Suspected crime
- “Reasonable suspicion” (RS) factors
- Interactions between race and RS factors (in later models)

Model progression:
1. OLS (1): No fixed effects
2. FE (2): Precinct and year fixed effects
3. FE (3): Precinct-specific time trends (precinct × year FE)

Interpretation:
Because OLS is used with binary outcomes, coefficients are interpreted as percentage-point changes in probability.

## Two-stage arrest models (hybrid approach)

For arrests, I recognize that officers make two decisions:
1. Whether to sanction at all
2. Whether the sanction is an arrest or a summons

Stage 1:
- Logistic model predicting any sanction

Stage 2:
- OLS model predicting arrest conditional on sanction
- Includes the predicted probability of sanction as a regressor

Purpose:
To separate racial bias in who gets sanctioned from bias in how harshly they are sanctioned.

# Model Component Implementation

## Stop Rate Models

- `ols-stops.R`: Basic stop rate models with precinct racial composition, crime rates, socioeconomic controls, and borough/year fixed effects
- `ols-alloc-lag.R`: Advanced allocation models with lagged crime rates, dynamic specifications, and administrative period comparisons

## Stop Outcome Models
- `ols-sanctions.R`: Models predicting any sanction (arrest or summons) with race, demographics, RS factors, and fixed effects
- `ols-sanctions-cond.R`: Conditional models predicting arrest vs summons given a sanction occurred