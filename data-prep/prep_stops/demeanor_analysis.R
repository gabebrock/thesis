# --- sentiment analysis: DEMEANOR_OF_PERSON_STOPPED ----
#' Officer-written free-text field describing suspect demeanor at time of stop.
#' Goal: classify each entry as positive / negative / neutral using tidytext,
#' then attach a demeanor sentiment score back to sqf_all.


# --- frequency of raw entries ----
#' Inspect most common demeanor descriptions before tokenizing
demeanor_freq <- sqf_all %>%
  dplyr::count(DEMEANOR_OF_PERSON_STOPPED, sort = TRUE)


# --- tokenize ----
demeanor_tokens <- sqf_all %>%
  dplyr::select(STOP_FRISK_ID, YEAR2, SUSPECT_RACE_DESCRIPTION, DEMEANOR_OF_PERSON_STOPPED) %>%
  tidyr::drop_na(DEMEANOR_OF_PERSON_STOPPED) %>%
  tidytext::unnest_tokens(word, DEMEANOR_OF_PERSON_STOPPED) %>%
  dplyr::anti_join(tidytext::stop_words, by = "word")


# --- sentiment scoring (AFINN: -5 to +5) ----
afinn <- tidytext::get_sentiments("afinn")

demeanor_sentiment <- demeanor_tokens %>%
  dplyr::inner_join(afinn, by = "word") %>%
  dplyr::group_by(STOP_FRISK_ID) %>%
  dplyr::summarise(
    demeanor_score    = sum(value),
    demeanor_n_words  = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    demeanor_valence = dplyr::case_when(
      demeanor_score > 0  ~ "positive",
      demeanor_score < 0  ~ "negative",
      demeanor_score == 0 ~ "neutral"
    )
  )


# --- join score back to sqf_all ----
sqf_all <- sqf_all %>%
  dplyr::left_join(demeanor_sentiment, by = "STOP_FRISK_ID")
#' Stops with no scoreable words get NA demeanor_score / demeanor_valence


# --- summary: valence by race and year ----
demeanor_by_race_year <- sqf_all %>%
  tidyr::drop_na(demeanor_valence, SUSPECT_RACE_DESCRIPTION, YEAR2) %>%
  dplyr::count(YEAR2, SUSPECT_RACE_DESCRIPTION, demeanor_valence) %>%
  tidyr::pivot_wider(names_from = demeanor_valence, values_from = n, values_fill = 0) %>%
  dplyr::arrange(YEAR2, SUSPECT_RACE_DESCRIPTION)


# --- mean demeanor score by race ----
demeanor_mean_race <- sqf_all %>%
  tidyr::drop_na(demeanor_score, SUSPECT_RACE_DESCRIPTION) %>%
  dplyr::group_by(SUSPECT_RACE_DESCRIPTION) %>%
  dplyr::summarise(
    mean_score = mean(demeanor_score),
    median_score = median(demeanor_score),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(mean_score)
