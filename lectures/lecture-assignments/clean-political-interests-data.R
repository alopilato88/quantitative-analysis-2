library(tibble)
library(dplyr)
library(readr)
library(tidyr)

data_wvs <- readr::read_csv("data/WVS_Cross-National_Wave_7_csv_v6_0.csv")

# Q223: 5: Other, 
#       -2: No Answer
#       -1: Don't Know
#       840001: Rep.
#       840002: Dem.
#       840004: Lib.
#       840006: Green Party
data_politics <- 
  data_wvs |> 
  dplyr::filter(
    B_COUNTRY_ALPHA == "USA",
    Q221 != 4,
    Q222 != 4
  ) |>
  dplyr::select(
    A_WAVE, A_YEAR, B_COUNTRY_ALPHA, Q199_POLITICAL_INTEREST = Q199,
    Q200_DISCUSS_POLS_FRIENDS = Q200, Q209_SIGN_PETITION = Q209, Q210_BOYCOTT = Q210,
    Q211_PEACEFUL_DEMONSTRATE = Q211, Q212_STRIKE = Q212, Q213_DONATE = Q213, 
    Q214_CONTACT_GOVT = Q214, Q215_ENCOURAGE_OTHER_ACTION = Q215, 
    Q216_ENCOURAGE_VOTE = Q216, Q221_VOTE_LOCAL = Q221, Q222_VOTE_NATIONAL = Q222,
    Q223_POL_PARTY = Q223, Q240_POL_VIEW = Q240, Q252_SAT_POL_SYSTEM = Q252, 
    Q254_NATION_PRIDE = Q254, Q269_CITIZEN = Q269, Q260_SEX = Q260, 
    Q261_BIRTH_YEAR = Q261, Q262_AGE = Q262, Q263_IMMIGRANT = Q263, 
    Q275_EDUCATION = Q275, Q279_EMPLOYED = Q279, Q287_SES = Q287, Q288_INCOME = Q288
  ) |>
  dplyr::mutate(
    Q199_POLITICAL_INTEREST = dplyr::if_else(Q199_POLITICAL_INTEREST < 0, NA_real_, 
                                             Q199_POLITICAL_INTEREST), 
    Q200_DISCUSS_POLS_FRIENDS = dplyr::if_else(Q200_DISCUSS_POLS_FRIENDS < 0, NA_real_,
                                               Q200_DISCUSS_POLS_FRIENDS),
    dplyr::across(Q209_SIGN_PETITION:Q216_ENCOURAGE_VOTE, 
                  ~ dplyr::case_when(.x == 1 ~ "Have Done", .x == 2  ~ "Might Do", 
                                     .x == 3 ~ "Would Never Do", TRUE ~ NA_character_)),
    dplyr::across(Q221_VOTE_LOCAL:Q222_VOTE_NATIONAL, ~ dplyr::case_when(
      .x %in% 1:2 ~ "Yes", .x == 3 ~ "No", TRUE ~ NA_character_
    )), 
    Q223_POL_PARTY = dplyr::case_when(
      Q223_POL_PARTY == 5 ~ "Other",
      Q223_POL_PARTY == 840001 ~ "R",
      Q223_POL_PARTY == 840002 ~ "D",
      Q223_POL_PARTY == 840004 ~ "L",
      Q223_POL_PARTY == 840006 ~ "GP",
      TRUE ~ NA_character_
    ),
    dplyr::across(Q240_POL_VIEW:Q254_NATION_PRIDE, ~ dplyr::if_else(.x < 0, NA_real_, .x)),
    Q269_CITIZEN = dplyr::case_when(
      Q269_CITIZEN == 1 ~ "Y",
      Q269_CITIZEN == 2 ~ "N",
      TRUE ~ NA_character_
    ), 
    Q260_SEX = dplyr::case_when(
      Q260_SEX == 1 ~ "M",
      Q260_SEX == 2 ~ "F",
      TRUE ~ NA_character_
    ),
    Q263_IMMIGRANT = dplyr::case_when(
      Q263_IMMIGRANT == 1 ~ "N",
      Q263_IMMIGRANT == 2 ~ "Y",
      TRUE ~ NA_character_
    ),
    Q275_EDUCATION = dplyr::if_else(Q275_EDUCATION < 0, NA_real_, Q275_EDUCATION),
    Q279_EMPLOYED = dplyr::case_when(
      Q279_EMPLOYED %in% 1:3 ~ "Employed",
      Q279_EMPLOYED == 4 ~ "Retired",
      Q279_EMPLOYED == 5 ~ "Housewife",
      Q279_EMPLOYED == 6 ~ "Student",
      Q279_EMPLOYED == 7 ~ "Unemployed",
      Q279_EMPLOYED == 8 ~ "Other",
      TRUE ~ NA_character_
    ),
    dplyr::across(Q287_SES:Q288_INCOME, ~ dplyr::if_else(.x < 0, NA_real_, .x))
  ) 
