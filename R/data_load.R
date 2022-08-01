#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source

nki_load <- function(...){

  data_in <- OpenML::getOMLDataSet(data.id = 1228)

  data_in$data %>%
    rename(status = event)

}

lung_load <- function(...){

  data_in <- OpenML::getOMLDataSet(data.id = 1245)

  data_in$data %>%
    rename(status = OS_event,
           time = OS_years) %>%
    mutate(status = as.numeric(status)-1)

}

lung_ncctg_load <- function(...){

  data(cancer, package="survival")

  out <- cancer %>%
    mutate(sex = sex - 1,
           status = status - 1) %>%
    select(-inst)

}

vdv_load <- function(...){

  data("vdv", package = 'randomForestSRC')

  vdv |>
    rename(status = Censoring,
           time = Time)


}
veteran_load <- function(...){

  data("veteran", package = 'randomForestSRC')
  veteran

}

colon_load <- function(outcome){

  data_tmp <- survival::colon |>
    as_tibble() |>
    filter(etype == switch(outcome,
                           'death' = 2,
                           'recur' = 1))

  data_tmp |>
    select(-id, -study, -node4, -etype) |>
    mutate(sex = factor(sex,
                        levels = c(0,1),
                        labels = c('female', 'male')),
           differ = factor(differ,
                           levels = c(1, 2, 3),
                           labels = c('well', 'moderate', 'poor')))

}


colon_recur_load <- function(...){
  colon_load(outcome = 'recur')
}

colon_acm_load <- function(...){
  colon_load(outcome = 'death')
}

pbc_orsf_load <- function(...){

  aorsf::pbc_orsf |>
    select(-id) |>
    as_tibble()

}
time_to_million_load <- function(...){

  data("time_to_million", package = 'censored')

  time_to_million |>
    rename(status = event) |>
    # dropping text and dates
    select(-title, -released, -distributor)

}
gbsg2_load <- function(...){

  data("GBSG2", package = "TH.data")

  GBSG2 |>
    as_tibble() |>
    rename(status = cens)

}
peakV02_load <- function(...){

  data("peakVO2", package = 'randomForestSRC')

  peakVO2 |>
    rename(time = ttodead, status = died)


}
flchain_load <- function(...){

  survival::flchain |>
    rename(time = futime, status = death) |>
    # the chapter variable indicates who died,
    # not fair to use it as a predictor
    select(-chapter) |>
    # 3 died on the same day their sample was collected,
    # let's call that a half day.
    mutate(time = pmax(time, 1/2))

}
nafld_load <- function(...){

  read_csv('data/nafld.csv')

}
rotterdam_load <- function(outcome){

  if(outcome == 'acm'){

    data_init <- survival::rotterdam |>
      rename(time = dtime, status = death) |>
      select(-rtime, -recur)

  } else {

    data_init <- survival::rotterdam |>
      rename(time = rtime, status = recur) |>
      select(-dtime, -death)
  }

   data_init |>
    select(year, age, meno, size, grade, nodes, pgr, er, hormon,
           time, status) |>
    as_tibble()

}

rotterdam_acm_load <- function(...){
  rotterdam_load(outcome = 'acm')
}
rotterdam_recur_load <- function(...){
  rotterdam_load(outcome = 'recur')
}

actg_load <- function(outcome){

  if(outcome == 'aids'){

    data_outcome <- mlr3proba::actg |>
      as_tibble() |>
      # tx is redundant with txgrp
      select(-id, -time_d, -censor_d, -tx) |>
      rename(status = censor)

  } else if (outcome == 'death'){

    data_outcome <- mlr3proba::actg |>
      as_tibble() |>
      # tx is redundant with txgrp
      select(-id, -time, -censor, -tx) |>
      rename(time = time_d,
             status = censor_d)

  }

  data_outcome |>
    mutate(
      raceth = factor(raceth,
                      levels = 1:6,
                      labels = c("White",
                                 "Black",
                                 "Hispanic",
                                 "Asian",
                                 "American_Indian",
                                 "Other")),
      txgrp = factor(txgrp,
                     levels = 1:4,
                     labels = c("ZDV_3TC",
                                "ZDV_3TC_IDV",
                                "d4T_3TC",
                                "d4T_3TC_IDV")),
      ivdrug = factor(ivdrug,
                      levels = 1:3,
                      labels = c("Never",
                                 "Currently",
                                 "Previously"))
    )

}

actg_aids_load <- function(...){
  actg_load(outcome = 'aids')
}

actg_death_load <- function(...){
  actg_load(outcome = 'death')
}

guide_it_cvd_load <- function(...){

  read_csv('data/guide-it/analysis.csv') |>
    rename(time = time_cvdeath,
           status = status_cvdeath) |>
    select(-deidnum, -time_hfhosp, -status_hfhosp)

}

guide_it_hfhosp_load <- function(...){

  read_csv('data/guide-it/analysis.csv') |>
    rename(time = time_hfhosp,
           status = status_hfhosp) |>
    select(-deidnum, -time_cvdeath, -status_cvdeath)

}

breast_load <- function(...){

  data("Breast",package='biospear')

  data_all <- Breast

  names(data_all)[4:ncol(data_all)] <-
    paste('var',4:ncol(data_all),sep='_')

  data_all

}
sprint_cvd_load <- function(...){
  sprint_ndi_load(outcome = 'cvd')
}
sprint_acm_load <- function(...){
  sprint_ndi_load(outcome = 'acm')
}
sim_load <- function(n_obs,
                     n_z,
                     correlated_x){

  sim_surv(n_obs = n_obs,
           n_z = n_z,
           correlated_x = correlated_x) |>
    getElement('data')

}


accord_cvd_load <- function(...){
  accord_load(outcome = "cvd")
}

accord_acm_load <- function(...){
  accord_load(outcome = "acm")
}

accord_load <- function(outcome){
  data_baseline <-
    haven::read_sas("Z:/ACCORD Data/Baseline/baseline.sas7bdat") |>
    transmute(
      patid,
      # arm_char = str_replace_all(arm_char, ' |/', '_'),
      race = factor(race),
      height,
      weight,
      gly,
      bp = if_else(bp == "", "blank", bp),
      lipid = if_else(lipid == "", "blank", lipid),
      sbp,
      dbp,
      loopDiur,
      thiaDiur,
      ksparing,
      potsup,
      angio2,
      periphab,
      caaa,
      diccb,
      nodiccb,
      vasodil,
      reserpin,
      OBPMED,
      aceinhib,
      bb,
      antihyp,
      bp_medsum,
      hba1c,
      glucose,
      ldl,
      hdl,
      trig,
      chol,
      screat,
      ucreat,
      alt,
      cpk,
      creatAlbRatio,
      kul,
      ualb,
      gfr,
      vldl,
      UAlbCr,
      smoking,
      Waist,
      hf,
      edu = factor(edu),
      statins,
      f_therm,
      LipidLower,
      lp_medsum,
      Amputation,
      FtUlcer,
      Neuropathy,
      HistDepr,
      EyeDis,
      LiveAlon,
      Uprot,
      FamHist,
      Uninsured,
      Medicaid,
      PrivCom,
      HMO,
      VA_ins,
      TCHVA,
      PHIP,
      medicare,
      DrugCov,
      TZD,
      Metformin,
      Sulfonylureas,
      AGI,
      meglitinides,
      AntiDep,
      Aspirin,
      NSaid,
      Estrogen,
      WtLoss,
      SteroidO,
      SteroidI,
      AntiCoagulants,
      Progestins,
      OralAsthma,
      Heparins,
      Thyroid,
      LittleBluePill,
      Antiinfl,
      Inhibitors,
      Digitalis,
      AArrhythmics,
      Nitrates,
      BASequest,
      Fibrates,
      Niacin,
      g_medsum,
      SnellenR,
      SnellenL,
      VizScoreR,
      VizScoreL,
      diabmeds,
      CholAbsorb,
      Vitamins,
      OTCmeds,
      Herbals,
      BlindR,
      BlindL,
      Rtnpthy,
      VizLoss,
      RtnpthyL,
      VizLossL,
      RtnpthyR,
      VizLossR,
      livewith,
      randresult,
      Insu,
      Bolus,
      Basal,
      Premix,
      ExerCalExp,
      ExerKCalHr,
      ModExCalExp,
      ModExKCalHr,
      allfreq,
      ExerFreq,
      ModExFreq,
      Substitution,
      ModifyMeat,
      AvoidFrying,
      Replace,
      AvoidFat,
      DietSumry,
      HUI3Scor,
      HUI2pf,
      PHQ9,
      sf36_pf,
      sf36rs1,
      sf36_pfz,
      sf36_rp,
      sf36rs2,
      sf36_rpz,
      sf36rs3,
      sf36_bp,
      sf36_bpz,
      sf36_gh,
      sf36rs4,
      sf36_ghz,
      sf36_vt,
      sf36rs5,
      sf36_vtz,
      sf36_sf,
      sf36rs6,
      sf36_sfz,
      sf36_re,
      sf36rs7,
      sf36_rez,
      sf36_mh,
      sf36rs8,
      sf36_mhz,
      PHYSICAL,
      MENTAL,
      brazindex,
      DTSQ,
      HYPER_TS = factor(HYPER_TS),
      HYPO_TS = factor(HYPO_TS),
      prev_mi = if_else(prev_mi == '1', 1, 0),
      mnsi_score,
      p_neurop,
      qrs_dur,
      qt_dur,
      qtc_dur,
      QTI,
      VisAcuity
    )


  if(outcome == 'cvd'){

    data_outcomes <-
      haven::read_sas("Z:/ACCORD Data/Outcomes/forsurvival_f_bth.sas7bdat") |>
      mutate(
        time = if_else(
          class == '',
          as.numeric(d_status - d_rand),
          as.numeric(event_date - d_rand)
        ),
        # no zero values allowed
        time = pmax(time, 1/2),
        status = if_else(class == '', 0, 1)
      ) |>
      select(patid, time, status) |>
      drop_na()

  } else if(outcome == 'acm'){

    date_randomized <-
      haven::read_sas("Z:/ACCORD Data/Outcomes/forsurvival_f_bth.sas7bdat") |>
      select(patid, d_rand)

    date_acm <-
      haven::read_sas("Z:/ACCORD Data/Outcomes/death_bth.sas7bdat") |>
      select(patid, status = class, event_date)

    date_censored <-
      haven::read_sas("Z:/ACCORD Data/Outcomes/visd.sas7bdat")

    data_outcomes <- left_join(date_censored, date_acm) |>
      left_join(date_randomized) |>
      mutate(
        status = if_else(
          is.na(status),
          true = 0,
          false = 1
        ),
        time = if_else(
          status == 0,
          true = d_status - d_rand,
          false = event_date - d_rand
        ),
        time = as.numeric(time)
      ) |>
      select(patid, time, status) |>
      drop_na()

  }

  left_join(data_outcomes, data_baseline) |>
    select(-patid)

}


phts_load <- function(...){

  read_rds("../PHTS-graft-loss-targets/data/phts_all.rds") |>
    select(time, status,
           prim_dx,
           height_txpl,
           tx_mcsd,
           txbun_r,
           chd_sv,
           bsa_txpl,
           txpl_year,
           hxsurg,
           txsa_r,
           list_year,
           e_gfr_listing,
           txecmo,
           pra_max_txpl,
           chd_hlh,
           hxother,
           ls_surg_norwood_shunt,
           txvent,
           txtp_r,
           lsrhfcl_t,
           lsnyha_t,
           age_txpl,
           anxinj,
           bicaval,
           bmi_listing,
           bmi_txpl,
           chd_tof,
           cmv,
           donisch,
           donspecp,
           e_gfr_txpl,
           ebv,
           hbcab,
           hbsab,
           hbsag,
           hcab,
           hisp,
           hiv,
           hxaf_fl,
           hxaicd,
           hxarrunk,
           hxarryth,
           hxchb,
           hxcpr,
           hxcva,
           hxdiab,
           hxfail,
           hxgi,
           hxhep,
           hxmalig,
           hxmaloth)

}

follic_load <- function(outcome){

  data(follic, package = 'randomForestSRC')

  if(outcome == 'death'){

    follic$status <- ifelse(follic$status == 2, 1, 0)

  } else if (outcome == 'relapse'){

    follic$status <- ifelse(follic$status == 1, 1, 0)

  }

  follic

}

follic_death_load <- function(...){
  follic_load(outcome = 'death')
}
follic_relapse_load <- function(...){
  follic_load(outcome = 'relapse')
}


mgus2_load <- function(outcome){

  data_in <- as_tibble(survival::mgus2)

  switch(
    outcome,
    'death' = {
      data_in |>
        select(-id, -ptime, -pstat) |>
        rename(status = death,
               time = futime)
    },
    'pcm' = {
      data_in |>
        select(-id, -death, -futime) |>
        rename(status = pstat,
               time = ptime)
    }
  )
}

mgus2_death_load <- function(...){
  mgus2_load(outcome = 'death')
}
mgus2_pcm_load <- function(...){
  mgus2_load(outcome = 'pcm')
}

rotterdam_recurrence_load <- function(...){

  survival::rotterdam |>
    mutate(time = pmin(rtime, dtime),
           status = if_else(rtime < dtime, recur, death)) |>
    select(age, meno, size, grade, nodes, pgr, er, hormon,
           time, status) |>
    as_tibble()

}

sprint_ndi_load <- function(outcome) {

  stopifnot(outcome %in% c('acm', 'cvd'))

  data_in <- read_csv("Z:/npajewski/NDI/Data/longterm_death.csv")

  if(outcome == 'acm')
    data_in$status <- data_in$acm_event

  if(outcome == 'cvd')
    data_in$status <- as.numeric(data_in$cvd_event_cr == 1)

  data_selected <- data_in |>
    transmute(
      time = acm_years,
      status,
      intensive,
      sub_senior,
      altdevice,
      hr,
      unable,
      stand_sbp,
      stand_dbp,
      stand_hr,
      dizzy,
      n_agents,
      ACEINH,
      ALDORB,
      ALPBBL,
      ALPHA,
      is_bbl,
      ANG_RB,
      CENTRL,
      DHPCCB,
      DIRVAS,
      KSPDIU,
      LOODIU,
      NDHCCB,
      NI_BBL,
      RENINI,
      THZDIU,
      n_classes,
      liveWithOthers,
      education,
      fulltime,
      Work_FulPrt,
      Insured,
      drugbene,
      atrialFib,
      angina,
      heartAtt,
      conHeartFail,
      irrHeartBeat,
      ulcer,
      crohns,
      diverticulitis,
      hepatitis,
      gallbladder,
      kidinfect,
      BPH,
      prostatitis,
      osteoarthritis,
      rheArthritis,
      gout,
      othArthritis,
      hipprob,
      cancer,
      skinCancer,
      pvd,
      seizure,
      stroke,
      tia,
      thyroidDis,
      anemia,
      diabetes,
      hypertens,
      lowBkPain,
      cataracts,
      schizo,
      depress,
      bipolar,
      anxiety,
      ptsd,
      alcohol,
      CVDFamHst,
      drink_4cat = factor(drink_4cat),
      smoke_pkyrs,
      vigactiv,
      lessvigactiv,
      aspirin,
      othmed,
      egfr,
      screat,
      sub_ckd,
      eGFR_CKDEPI,
      spanishno,
      myocardinfarc,
      acutecorsynd,
      coronaryrevas,
      carotid,
      padrevas,
      stenosis50,
      aaa5repair,
      calcscore400,
      lowabi90,
      lvhcompecg,
      age,
      female,
      sub_cvd,
      race4,
      BUN,
      CHR,
      CL,
      CO2,
      CRDUR,
      GLUR,
      HDL,
      LDLR,
      TRR,
      UMALCR,
      UMALI,
      POTASSIUM,
      age_interview,
      mind,
      moca_complete,
      moca_score_new,
      dsc_total,
      lmir,
      lm_delayed1,
      traila_errors,
      trailatime,
      trailb_errors,
      trailbtime,
      animals_total,
      boston_total,
      hvltir,
      hvlt_trial4,
      hvltp,
      hvlt_true,
      dsforward_total,
      dsbackward_total,
      digitsa,
      copytotal,
      irtotal,
      reypct,
      manfix,
      trigger,
      faq_score,
      faq_deficits,
      distance_m,
      Fortym_flag,
      gait_speed_max,
      gait_speed_mean,
      walkaid = factor(walkaid),
      moca_executive,
      moca_naming,
      moca_attention1,
      moca_attention2,
      moca_attention3,
      moca_language1,
      moca_language2,
      moca_abstraction,
      moca_recall,
      moca_orientation,
      sum_complete_norms,
      ca_impairment1,
      ca_impairment2,
      IIEF_score,
      FESI_score,
      sex_Active,
      FSFI_score,
      EQ5D_index,
      EQ5D_sum,
      PHQ9_score,
      PHQ9_MSDep,
      PHQ9_suicide,
      VR12_PCS,
      VR12_MCS,
      gen_health = factor(gen_health),
      weight,
      height,
      BMI,
      refuMiss,
      antiAnginal,
      nitrate,
      antiArrhythmic,
      nonStatinLL,
      statin,
      alzheimer,
      antiDepressant,
      antiPsychotic,
      analgesic,
      NSAID,
      antiThrombotic,
      antiCoagulant,
      antiRheumatic,
      corticosteroid,
      salycilates,
      CVDagent,
      anitInflam,
      antiPlatelet,
      antiParkinsons,
      vitaminD,
      oralDiabetes,
      antiGout,
      hormones,
      osteoporosis,
      stimulant,
      MMAS_score,
      MMASpoorAdh,
      cystatinc,
      eGFR_CKDEPI_CyC,
      eGFR_CKDEPI_CrCyC,
      sbp,
      dbp,
      InclusionFRS,
      fr_risk10yrs,
      cog_global_v1,
      cog_global_v2,
      cog_memory,
      cog_executive_v1,
      cog_executive_v2,
      cog_attention,
      cog_language,
      fi_new,
      edu_grp_moca,
      moca_status
    )

  too_many_miss <- map_dbl(data_selected, ~mean(is.na(.x))) |>
    enframe() |>
    filter(value >= 0.4) |>
    pull(name)

  data_selected[, too_many_miss] <- NULL

  too_few_unique <-
    map_int(data_selected, ~length(unique(na.omit(.x)))) |>
    enframe() |>
    filter(value == 1) |>
    pull(name)


  data_selected[, too_few_unique] <- NULL

  data_selected


}


mesa_load <- function(outcome){

  fctrs <- c("Sex", "Race", "Alcohol", "Smoking")

  outcomes_keep <- switch(
    outcome,
    'hf' = paste(c("time", "censor"), 'hf', sep = '_'),
    'chd' = paste(c("time", "censor"), 'chd', sep = '_'),
    'stroke' = paste(c("time", "censor"), 'stroke', sep = '_'),
    'death' = paste(c("time", "censor"), 'death', sep = '_')
  )

  to_drop <- c('ID',
               'database',
               'VisitDate',
               'LAnova',
               'CV',
               "QTc",
               "CV",
               "LVHcv",
               "LVHsl",
               "hxMI",
               "hxCABG",
               "CVD",
               "CHF",
               "time_hf",
               "censor_hf",
               "time_death",
               "censor_death",
               "time_chd",
               "censor_chd",
               "time_stroke",
               "censor_stroke") %>%
    setdiff(outcomes_keep)

  dres <-
    fread("data/mesa/Primary/Exam1/Data/mesae1dres06192012.csv") %>%
    .[, .(
      ID = as.character(MESAID),
      hipcm1,
      agatum1c,
      volum1c,
      volsum1c,
      hrtrate1,
      cepgfr1c,
      hcytot1,
      olvedm1,
      olvedv1,
      olvesv1,
      olvef1,
      olvsv1,
      oardis1,
      oaormn1,
      oaormx1,
      aad1c,
      crdout1c,
      ncohes1c,
      nprob1c,
      nhdtim1c
    )]

  fread("data/mesa_and_aric.csv") %>%
    .[database == 'mesa'] %>%
    merge(dres, by = 'ID') %>%
    .[, (fctrs) := lapply(.SD, as.factor), .SDcols = fctrs] %>%
    .[, (to_drop) := NULL] %>%
    setnames(old = outcomes_keep, new = c("time", "status")) %>%
    as.data.frame() %>%
    drop_na(time, status)

}


mesa_hf_load <- function(...){
  mesa_load(outcome = "hf")
}

mesa_chd_load <- function(...){
  mesa_load(outcome = "chd")
}

mesa_stroke_load <- function(...){
  mesa_load(outcome = "stroke")
}

mesa_death_load <- function(...){
  mesa_load(outcome = "death")
}


aric_load <- function(outcome){

  fctrs <- c("Sex", "Race", "Alcohol", "Smoking")

  outcomes_keep <- switch(
    outcome,
    'hf' = paste(c("time", "censor"), 'hf', sep = '_'),
    'chd' = paste(c("time", "censor"), 'chd', sep = '_'),
    'stroke' = paste(c("time", "censor"), 'stroke', sep = '_'),
    'death' = paste(c("time", "censor"), 'death', sep = '_')
  )

  to_drop <- c('ID',
               'database',
               'VisitDate',
               "LVEF",
               "LVMi",
               "hxMI",
               "hxCABG",
               "CVD",
               "CHF",
               "time_hf",
               "censor_hf",
               "time_death",
               "censor_death",
               "time_chd",
               "censor_chd",
               "time_stroke",
               "censor_stroke") %>%
    setdiff(outcomes_keep)

  abi <- fread('data/aric/data/Main_Study/v1/CSV/abi04.csv',
               na.strings = "") %>%
    .[, .(
      ID = ID_C,
      abi = suppressWarnings(as.numeric(ABI04))
    )]

  hmt <- fread('data/aric/data/Main_Study/v1/CSV/hmta.csv',
               na.strings = "") %>%
    .[, .(
      ID = ID_C,
      HMTA01 = suppressWarnings(as.numeric(HMTA01)),
      HMTA02 = suppressWarnings(as.numeric(HMTA02)),
      HMTA03 = suppressWarnings(as.numeric(HMTA03)),
      HMTA04 = suppressWarnings(as.numeric(HMTA04)),
      HMTA05 = suppressWarnings(as.numeric(HMTA05)),
      HMTA06 = suppressWarnings(as.numeric(HMTA06)),
      HMTA07 = suppressWarnings(as.numeric(HMTA07)),
      HMTA08 = suppressWarnings(as.numeric(HMTA08)),
      HMTA09 = suppressWarnings(as.numeric(HMTA09))
    )]

  fread("data/mesa_and_aric.csv") %>%
    .[database == 'aric'] %>%
    merge(abi, by = 'ID') %>%
    merge(hmt, by = 'ID') %>%
    .[, (fctrs) := lapply(.SD, as.factor), .SDcols = fctrs] %>%
    .[, (to_drop) := NULL] %>%
    setnames(old = outcomes_keep, new = c("time", "status")) %>%
    .[, time := pmax(time, 1/365)] %>%
    as.data.frame() %>%
    drop_na(time, status)

}

aric_hf_load <- function(...){
  aric_load(outcome = "hf")
}

aric_chd_load <- function(...){
  aric_load(outcome = "chd")
}

aric_stroke_load <- function(...){
  aric_load(outcome = "stroke")
}

aric_death_load <- function(...){
  aric_load(outcome = "death")
}


jhs_load <- function(outcome){

  new_names <- c('time', 'status')
  old_names <- paste(new_names, outcome, sep = '_')

  fread('data/jhs.csv') %>%
    setnames(old = old_names, new = new_names) %>%
    select(-starts_with("time_"),
           -starts_with("status_"),
           -newid) %>%
    mutate(sex = factor(sex)) %>%
    drop_na(time, status)

}

jhs_stroke_load <- function(){
  jhs_load("stroke")
}

jhs_chd_load <- function(){
  jhs_load("chd")
}
