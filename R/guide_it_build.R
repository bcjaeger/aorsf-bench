

guide_it_build <- function(){

  demog <- fread("data/guide-it/demog_ads.csv")[
    randfl == 1,
    .(
      ARM,
      ETHNIC,
      RACE,
      DIABETES,
      COPD,
      KIDNEY,
      HFETIOL,
      WEIGHTKG,
      afibbase,
      nyhabase,
      syspbase,
      hrbase,
      crtbase,
      age,
      HEIGHTM,
      asiand,
      blackd,
      whited,
      sex,
      bmi,
      us,
      ischemic,
      hfdurcat,
      deidnum,
      other
    )
  ]

  clabs <- fread("data/guide-it/adh_corelab_ads.csv")[
    visit == "evBLN",
    .(
      deidnum,
      pbnprslt,
      pbnpadj,
      efcontb
    )
  ]

  slabs <- fread("data/guide-it/labs_ads.csv")[
    VISIT == 'evBLN',
    .(
      CRTRSLT,
      POTRSLT,
      sodrslt,
      BUNRSLT,
      CHOLRSLT,
      URICRSLT,
      HEMORSLT,
      HEMARSLT,
      PLATRSLT,
      WBCRSLT,
      LYMPRSLT,
      deidnum
    )
  ]

  meds <- fread('data/guide-it/meds_ads.csv')[
    VISIT == 'evBLN',
    .(
      TORSEND,
      TORSEDD,
      # BUMETA,
      # BUMETAND,
      BBDOSE,
      FUROSND,
      FUROSE,
      ARBDOSE,
      BETAB,
      ARB,
      ALDOS,
      ALDODOSE,
      ACEDOSE,
      ACE,
      acepct,
      aldpct,
      arbpct,
      bbpct,
      # ivause,
      valsart,
      diurdose,
      acearbpt,
      double,
      triple,
      diur80,
      deidnum
    )
  ]

  endpt <- fread("data/guide-it/best_endpoints_ads.csv")[
    ,
    .(
      deidnum,
      # CV Death
      status_cvdeath = dthcvadj,
      time_cvdeath = pmax(1/2, dcvadjdy),
      # HF Hospitalization
      status_hfhosp = hoshfadj,
      time_hfhosp = pmax(1/2, dhfadjdy)
    )
  ]

  # endpt <- fread("data/guide-it/best_endpoints_ads.csv")[
  #   ,
  #   .(
  #     deidnum,
  #     # CV Death or HF Hospitalization
  #     status = dthhfadj,
  #     # Days from randomization to CV Death or HF Hospitalization
  #     time = pmax(1, dhfadjdy)
  #   )
  # ]

  out <- list(demog,
              endpt,
              clabs,
              slabs,
              meds) |>
    reduce(.f = merge,
           by = 'deidnum',
           all.x = TRUE)

  out[ARB == 0, ARBDOSE := 0]
  out[ALDOS == 0, ALDODOSE := 0]
  out[TORSEND != 1 | is.na(TORSEND), TORSEDD := 0]

  out[, `:=`(ARB = NULL, ALDOS = NULL, TORSEND = NULL)]

  fwrite(out, file = 'data/guide-it/analysis.csv')

  NULL

}
