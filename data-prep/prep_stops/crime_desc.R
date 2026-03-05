
off_cat_broad <- c("crime_Violent", "crime_Weapons", "crime_Property", "crime_Drug",
                   "crime_Trespass", "crime_QualityOfLife", "crime_Other")

#' if sqf_all$SUSPECTED_CRIME_DESCRIPTION only includes "FEL"/"FELONY"/etc. or 
#' "MISD"/"MISDEMEANOR"/etc. or "VIO"/"VIOLATION"/etc., any maybe a number but
#' no other text describing the offense
#' then we can recode to new set of binary variables for `felony`, `misdemeanor`, and `violation`/

#' if sqf_all$SUSPECTED_CRIME_DESCRIPTION includes more detailed text 
#' describing the offense, then we can use regex to search for keywords in 
#' the text and recode to new variable `off_cat_broad` with 7 broad crime categories
#' `crime_Violent`, `crime_Weapons`, `crime_Property`, `crime_Drug`, `crime_Trespass`, 
#' `crime_QualityOfLife`, and `crime_Other`


# --- inspect raw values ----
crime_desc_freq <- sqf_all %>%
  dplyr::count(SUSPECTED_CRIME_DESCRIPTION, sort = TRUE)


# --- severity: felony / misdemeanor / violation ----
#' Records where the description is only a severity code (no offense detail).
#' These appear mainly in modern data.
sqf_all <- sqf_all %>%
  dplyr::mutate(
    offense_severity = dplyr::case_when(
      # felony — exact codes and common misspellings / truncations
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex("^(FEL|FELONY|FELON|FELONE|FELONEY|FELOMY|FEOLNY|FELONG|FELOONY|FELONLY|FELNOY|FELOYN|FELONT|FELONU|FELLONY|FELONY|FLONY|FFELONY|FRLONY|FEONY|FELQ|FELONYQ|FELONY|PL.?FEL|PL.?FELONY|F|FE|FEK|FEL\\w*|EFL|REL|DEL|GEL|EL|FL)$",
                       ignore_case = TRUE))                                               ~ "felony",
      # misdemeanor — exact codes and common misspellings / truncations
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex("^(MISD|MISDEMEANOR|MIS|M|MIDS|MSID|MSD|MID|MIAD|MIAS|MIISD|MMISD|MISSD|MISDC|MISDD|MISDQ|MISEMEANOR|MISDMEANOR|MIDEMEANOR|MISDEMEANER|MISDEMENOR|MISDEAMENOR|MISDEMENAOR|MISDEAMOR|MISDEAMEANOR|MISDEMNAOR|MISDEMNOR|MISDEMANOR|MISDEMEANER|MISEDEMEANOR|MISDEMEANER|MISDERMEANOR|MISDEMENAOR|MISDEAMNOR|MISDD|MIDSEMEANOR|MESD|NISD|MED|PL.?MIS|PL.?MISD)$",
                       ignore_case = TRUE))                                               ~ "misdemeanor",
      # violation
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex("^(VIO|VIOLATION|VIOL|VOP|VOOP)$",
                       ignore_case = TRUE))                                               ~ "violation",
      TRUE ~ NA_character_
    )
  )


# --- broad crime category: off_cat_broad ----
#' Single categorical variable with 7 levels, assigned by priority order.
#' Records with only a severity code (offense_severity not NA) get NA here.
sqf_all <- sqf_all %>%
  dplyr::mutate(
    off_cat_broad = dplyr::case_when(
      # ---- Violent ----
      # includes assault misspellings (ASS, ASSUALT, ASSLT, ASLT, etc.),
      # robbery variants (ROB, ROBB, ROOBERY), shooting, terrorism, forcible touching,
      # stalking, riot, reckless endangerment, human trafficking
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex(paste0(
          "assault|assau?l?t\\w*|\\bass\\w*lt|\\b(aslt|asslt|asault|asaault|assaul|assaut|assautl|assalut|asssault|assualt|assult)\\b|",
          "robbery|\\brobb?\\b|roob\\w+|rpbb\\w+|",
          "murder|homicide|rape|sex.?abuse|forcible.?touch|sodomy|",
          "kidnap|strangul|manslaughter|extortion|mena[cn]ing|coercion|weapon.?use|",
          "shooting|shots.?fired|",
          "terror(ism|oism|ism)?|490\\.10|",
          "stalking|riot|unlawful.?imprison|human.?traffick|",
          "gang.?ass|reckless.?endang|reck.?end|gang.?rob"
        ), ignore_case = TRUE))                                                           ~ "crime_Violent",

      # ---- Weapons ----
      # CPW / C.P.W. / CWP variants, USFM (unlawful sale firearms),
      # CPFI / C.P.F.I (criminal possession forged instrument excluded — see Property)
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex(paste0(
          "weapon|firearm|gun|knife|pistol|rifle|ammo|",
          "illegal.?poss.*weapon|criminal.?poss.*weapon|crim.?poss.?w[ep]|",
          "C\\.?P\\.?W\\.?|\\bCWP\\b|\\bCPW\\d*\\b|\\bUFSM\\b|\\bUSFM\\b"
        ), ignore_case = TRUE))                                                           ~ "crime_Weapons",

      # ---- Property ----
      # burglary misspellings (BURG, BUGLARY, BURLGARY, BRUGLARY, BUR, BRUG),
      # grand larceny variants (GL, G/L, G.L., GLA, GLFA, GRAN LARC, etc.),
      # petit larceny variants (PET LARC, P LARC, PETIT, etc.),
      # auto stripping, UUV (unauthorized use vehicle),
      # CPSP / C.P.S.P., CPFI (forged instrument), trademark counterfeiting
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex(paste0(
          "burglary|burg\\w*|\\bbur\\b|\\bbrug\\b|buglary|burlgary|bruglary|",
          "larceny|\\blarc\\b|theft|stolen|shopli|arson|",
          "grand.?lar|gran.?lar|gr.?lar|gr.?larc|",
          "G\\.?L\\.?A\\.?|\\bGLA\\b|\\bGLFA\\b|\\bGL\\b|G[./]L|",
          "GL.?(from|auto|veh|from.?auto|from.?veh)|",
          "gl.?from|larc.?from|larc.?auto|larc.?veh|",
          "petit|pet.?lar|p\\.?\\s*larc|p.?larc|pettit.?lar|",
          "auto.?strip|auto.?strp|auto.?brk|car.?break|",
          "fraud|forgery|forged.?inst|",
          "vandal|criminal.?mischief|crim.?misc?h?|crim.?mis[^d]|crim.?misch|",
          "possess.*stolen|cpsp|c\\.?p\\.?s\\.?p|csps|",
          "cpfi|c\\.?p\\.?f\\.?i|",
          "trademark|counterfeit|",
          "uuv|u\\.?u\\.?v|unauthorized.?use.?(of.?)?veh|unauth.?use",
          "|criminal.?tamper|crim.?tamp"
        ), ignore_case = TRUE))                                                           ~ "crime_Property",

      # ---- Drug ----
      # CSCS / C.S.C.S (criminal sale controlled sub),
      # CSM / C.S.M (criminal sale marijuana),
      # CPCS / C.P.C.S (criminal poss controlled sub),
      # CPM / C.P.M (criminal poss marijuana),
      # narco sale, NY Penal Law drug sections (220.xx, 221.xx)
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex(paste0(
          "drug|narcotic|marijuana|marihuana|cannabis|heroin|cocaine|crack|",
          "controlled.?sub|crim.?(poss|sale).?cont|criminal.?(poss|sale).?cont|",
          "C\\.?S\\.?C\\.?S\\.?\\d*|\\bCSCS\\w*\\b|",
          "C\\.?S\\.?M\\.?|\\bCSM\\b|",
          "C\\.?P\\.?C\\.?S\\.?\\d*|\\bCPCS\\d*\\b|",
          "C\\.?P\\.?M\\.?\\d*|\\bCPM\\d*\\b|\\bCPMS\\b|",
          "narc(o|otics?|o.?sale|o.?sales|.?sales?)?\\b|",
          "sale.?drug|possess.?drug|criminal.?sale|crim.?sale|",
          "\\b22[01]\\.\\d{2}\\b"
        ), ignore_case = TRUE))                                                           ~ "crime_Drug",

      # ---- Trespass ----
      # TOS / T.O.S (trespass on subway), CT / C/T / C.T.,
      # all CRIM TRES / CRIM TRESS / CRIMTRES variants
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex(paste0(
          "trespass|tres+pas+|\\btres+\\b|\\btresp\\b|",
          "crim\\.?\\s*tre[sp]|crim\\s*-?tres|crimtres|",
          "criminal.?tre[sp]|cr\\s*tres|crm\\s*tr|",
          "\\bT\\.?O\\.?S\\.?\\b|",
          "\\bC[./]?T\\.?\\b"
        ), ignore_case = TRUE))                                                           ~ "crime_Trespass",

      # ---- Quality of Life ----
      # prostitution variants, graffiti misspellings, swipes/fare card (165.16),
      # public lewdness, disorderly conduct (DISCON), gambling, unlawful assembly,
      # jostling, fireworks
      stringr::str_detect(SUSPECTED_CRIME_DESCRIPTION,
        stringr::regex(paste0(
          "disorderly|dis\\s*con\\b|loiter|",
          "public.?intox|open.?container|urinate|panhandl|noise|",
          "harass+ment|aggravated.?harass|",
          "graf+it+[ie]?|graf+[it]+|making.?graf|",
          "graffiti|graffitti|grafitti|grafiti|graffit|graffti|",
          "fare.?evad|turnstile|swipe|selling.?swipe|165\\.16|",
          "public.?lewd|pub.?lewd|",
          "prostit\\w+|\\bpros\\b|\\bprost\\b|loit.?for.?pros|patron\\w+.?prost|promot\\w+.?prost|",
          "promot\\w+.?gambl|gambl|",
          "unlawful.?assemb|",
          "jostl|",
          "firework|poss.?firework"
        ), ignore_case = TRUE))                                                           ~ "crime_QualityOfLife",

      !is.na(SUSPECTED_CRIME_DESCRIPTION) & is.na(offense_severity)                      ~ "crime_Other",
      TRUE                                                                                ~ NA_character_
    )
  )

# --- check frequency of off_cat_broad ----
off_cat_freq <- sqf_all %>%
  dplyr::count(off_cat_broad, sort = TRUE) %>%
  dplyr::mutate(pct = round(n / sum(n) * 100, 2))

# top raw descriptions falling into crime_Other — for refining regex
crime_other_freq <- sqf_all %>%
  dplyr::filter(off_cat_broad == "crime_Other") %>%
  dplyr::count(SUSPECTED_CRIME_DESCRIPTION, sort = TRUE)
