## Script for the creation of a Params table based on a mix between our parameters
## (SpParamsMED) and HydraTRY db

library(readr)
library(dplyr)
library(purrr)
library(medfate)
library(tpl)

# hydraTRY
hydratry <- read_delim('data-raw/HydraTRY.csv', delim = '\t') %>%
  select(-row)
# SpParams
data('SpParamsMED')
# new spparams object
paramsmed <- as_tibble(SpParamsMED) %>%
  mutate(Name = as.character(Name))
paramsmed[paramsmed$Name == 'Quercus humilis',"Name"] <- 'Quercus pubescens'

# get the names by the tpl for both db
names_test_paramsmed <- tpl.get(paramsmed$Name)
names_test_hydratry <- tpl.get(hydratry$Species)

# Correct the names in both db
paramsmed_names <- map2_chr(
  names_test_paramsmed$name, as.character(names_test_paramsmed$original.search),
  function(x,y) {
    if (is.na(x)) {
      res <- y
    } else {
      res <- x
    }
  }
)

hydratry_names <- map2_chr(
  names_test_hydratry$name, as.character(names_test_hydratry$original.search),
  function(x,y) {
    if (is.na(x)) {
      res <- y
    } else {
      res <- x
    }
  }
)

# get the coincidences
coincidences <- paramsmed_names[which(paramsmed_names %in% hydratry_names)]

# prepare hydraTRY
hydratry %>%
  filter(Species %in% coincidences) %>%
  rename(VCstem_d = P50,
         xylem_kmax = Ks,
         WoodDens = WD,
         Zmean = Rd,
         Name = Species) %>%
  mutate(Al2As = 10000/Hv,
         LeafDuration = LL/12,
         Hmax = Hmax*100,
         Vmax298_ln = (1.993 + (2.555*log(Narea*10)) - (0.372*log(SLA/1000)) + (0.422*log(Narea*10)*log(SLA/1000))),
         Vmax298 = exp(Vmax298_ln),
         Jmax_ln = 1.197 + (0.847*Vmax298_ln),
         Jmax = exp(Jmax_ln)) %>%
  select(Name, Vmax298, Jmax, VCstem_d, PItlp, xylem_kmax, Al2As,
         # WUE,
         SLA, LeafDuration, WoodDens,
         Narea, Hmax, Zmean) -> hydratry_processed

# prepare paramsmed and join hydratry
paramsmed %>%
  filter(Name %in% coincidences) -> paramsmed_processed

paramsmed_processed %>%
  select(-Vmax298, -VCstem_d, -xylem_kmax, -Al2As,
         # -WUE,
         -SLA, -LeafDuration, -WoodDens, -Hmax) %>%
  left_join(hydratry_processed, by = 'Name') %>%
  mutate(SpIndex = 0:(n()-1)) -> newParams

newParams %>%
  select(sort(names(newParams))) -> newParams

paramsmed_processed %>%
  select(sort(names(paramsmed_processed))) -> paramsmed_processed

# fill NAs
newParams <- map2_df(
  newParams[, -c(10,15,16,34)], paramsmed_processed,
  function(x,y) {
    x[is.na(x)] <- y[is.na(x)]
    # print(x[is.na(x)])
    return(x)
  }
) %>%
  mutate(
    Jmax = newParams$Jmax,
    Narea = newParams$Narea,
    PItlp = newParams$PItlp,
    Zmean = newParams$Zmean,
  ) %>%
  select(names(paramsmed), Jmax, Narea, PItlp, Zmean)


write_csv(newParams, 'data-raw/newParams.csv')

devtools::use_data(newParams, overwrite = TRUE)
