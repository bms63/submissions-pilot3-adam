library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(haven)

dm <- read_xpt("sdtm/dm.xpt")
sv <- read_xpt("sdtm/sv.xpt")
vs <- read_xpt("sdtm/vs.xpt")
sc <- read_xpt("sdtm/sc.xpt")
mh <- read_xpt("sdtm/mh.xpt")
ds <- read_xpt("sdtm/ds.xpt")
qs <- read_xpt("sdtm/qs.xpt")
ex <- read_xpt("sdtm/ex.xpt")

dm <- convert_blanks_to_na(dm)
sv <- convert_blanks_to_na(sv)
vs <- convert_blanks_to_na(vs)
sc <- convert_blanks_to_na(sc)
mh <- convert_blanks_to_na(mh)
ds <- convert_blanks_to_na(ds)
qs <- convert_blanks_to_na(qs)
ex <- convert_blanks_to_na(ex)


  
sv_svt <- sv %>% 
  derive_vars_dt(dtc = SVSTDTC, new_vars_prefix = 'SVST')

ex_ext <- ex %>%
  derive_vars_dt(dtc = EXENDTC, new_vars_prefix = 'EXEN',
                 date_imputation = 'last')

ds_dst <- ds %>%
  derive_vars_dt(dtc = DSSTDTC, new_vars_prefix = 'DSST')
  
qs_eff <- qs %>% 
  filter(VISITNUM > 3,
         QSTESTCD %in% c('ACTOT', 'CIBIC')) %>%
  select(STUDYID, USUBJID, QSTESTCD) %>%
  distinct() 

adsl <- dm %>%
  mutate(TRT01P = ARM, TRT01A = TRT01P,
         TRT01PN = case_when(TRT01P == 'Placebo' ~ 0,
                             TRT01P == 'Xanomeline Low Dose' ~ 54,
                             TRT01P == 'Xanomeline High Dose' ~ 81),
         TRT01AN = case_when(TRT01A == 'Placebo' ~ 0,
                             TRT01A == 'Xanomeline Low Dose' ~ 54,
                             TRT01A == 'Xanomeline High Dose' ~ 81)) %>%
  filter(TRT01A %in% c('Placebo', 'Xanomeline Low Dose', 'Xanomeline High Dose')) %>%
  derive_vars_merged(
    dataset_add = sv_svt,
    filter_add = VISITNUM == 3,
    new_vars = vars(TRTSDT = SVSTDT),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    new_vars = vars(TRTEDT = EXENDT),
    by_vars = vars(STUDYID, USUBJID),
    order = vars(EXENDT, EXSEQ),
    mode = "last"
  ) %>%
  derive_vars_merged(
    dataset_add = ds_dst,
    new_vars = vars(TRTEDT2 = DSSTDT),
    by_vars = vars(STUDYID, USUBJID),
    order = vars(DSSTDT, DSSEQ),
    mode = "last"
  ) %>%
  mutate(TRTEDT = case_when((is.na(TRTEDT) & 
                              TRT01P != 'Screen Failure')~ TRTEDT2,
                            TRUE ~ TRTEDT)) %>%
  select(-TRTEDT2) %>%
  derive_var_trtdurd() %>%
  mutate(AGEGR1N = case_when(AGE < 65 ~ 1,
                             AGE > 80 ~ 3,
                             TRUE ~ 2),
         AGEGR1 = case_when(AGEGR1N == 1 ~ '<65',
                            AGEGR1N == 2 ~ '65-80',
                            AGEGR1N == 3 ~ '>80'),
         RACEN = case_when(RACE == 'WHITE' ~ 1,
                           RACE == 'BLACK OR AFRICAN AMERICAN' ~ 2,
                           RACE == 'AMERICAN INDIAN OR ALASKA NATIVE' ~ 6,
                           RACE == 'ASIAN' ~ 7),
         ITTFL = case_when(ARMCD != ' ' ~ 'Y',
                           TRUE ~ 'N'),
         SAFFL = case_when(ITTFL == 'Y' & !(is.na(TRTSDT)) ~ 'Y',
                           TRUE ~ 'N')) %>%
  derive_vars_merged(
    dataset_add = qs_eff,
    new_vars = vars(EFFFL = EFFFL),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  mutate(EFFFL = case_when(is.na(EFFFL) ~ 'N',
                           TRUE ~ EFFFL))

