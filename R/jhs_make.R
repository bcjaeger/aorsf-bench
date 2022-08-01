

jhs_make <- function(){

  library(magrittr)

  fpath <- file.path("data", "JHS_2018a_cvd", "Data")

  event_vars <- c('newid', 'CHD', 'stroke', 'years')

  inc_events <-
    list.files(
      file.path(fpath, "Events"),
      pattern = "incev",
      include.dirs = FALSE,
      full.names = TRUE
    ) %>%
    set_names(c('chd', 'hf', 'stroke')) %>%
    map(haven::read_sas) %>%
    map(~select(.x, any_of(event_vars)))

  inc_events$stroke %<>% rename(time_stroke = years,
                                status_stroke = stroke)

  inc_events$chd %<>% rename(time_chd = years,
                             status_chd = CHD)

  events <- inc_events[c('chd', 'stroke')] %>%
    reduce(.f = left_join, by = 'newid')

  analysis <- haven::read_sas(
    file.path(fpath, 'Analysis_Data', 'analysis1.sas7bdat')
  ) %>%
    mutate(creatinine = coalesce(CreatinineU24hr, CreatinineUSpot),
           albumin = coalesce(AlbuminU24hr, AlbuminUSpot),
           acr = albumin / creatinine) %>%
    select(
      newid,
      age,
      sex,
      alc,
      currentSmoker,
      everSmoker,
      weight,
      height,
      waist,
      neck,
      ends_with("Meds"),
      sbp,
      dbp,
      abi,
      BMI,
      HbA1c,
      FPG,
      FastHours,
      Diabetes,
      creatinine,
      albumin,
      acr,
      ldl,
      hdl,
      trigs,
      totchol,
      LEPTIN,
      HSCRP,
      QTcFram,
      ENDOTHELIN,
      ALDOSTERONE,
      sCort,
      SCrCC,
      eGFRckdepi,
      CHDHx,
      strokeHx,
      MedicaidIns,
      MedicareIns,
      perceivedStress,
      starts_with('nb'),
      activeIndex,
      eggs,
      fish,
      darkgrnVeg,
      ecgHR,
      CV,
      QRS,
      hyIndex,
      sportIndex,
      FEV6,
      LVMindex,
      Insured,
      PrivateIns,
      ends_with("Ever")
    )

  left_join(events, analysis, by = 'newid') %>%
    write_csv("data/jhs.csv")

  NULL

}
