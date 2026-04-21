## ----eval=FALSE---------------------------------------------------------------
# install.packages("RoME")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("remotes")
# # remotes::install_git("https://github.com/COISPA/RoME")

## ----eval=FALSE---------------------------------------------------------------
# library(RoME)
# wd <- tempdir()
# suffix <- NA
# DataTA <- data.frame(RoME::TA[RoME::TA$YEAR == 2012, ])
# DataTB <- data.frame(RoME::TB[RoME::TB$YEAR == 2012, ])
# DataTC <- data.frame(RoME::TC[RoME::TC$YEAR == 2012, ])
# DataTE <- NA
# DataTL <- NA
# RoME(DataTA, DataTB, DataTC, DataTE, DataTL, wd, suffix, verbose = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# wd <- tempdir()
# suffix <- NA
# DataTA <- data.frame(RoME::TA[RoME::TA$YEAR == 2012, ])
# DataTB <- data.frame(RoME::TB[RoME::TB$YEAR == 2012, ])
# DataTC <- data.frame(RoME::TC[RoME::TC$YEAR == 2012, ])
# DataTE <- NA
# DataTL <- NA
# RoMEcc(DataTA, DataTB, DataTC, DataTE, DataTL, wd, suffix, verbose = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# run_RoME_app()

