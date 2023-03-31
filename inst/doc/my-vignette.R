## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(Mmcsd)
library(simstudy)
library(kableExtra)
library(tidyverse)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  mmcsd=function(formula, waves, ids, weights, stratum, cluster, data, sigma = "identity")

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  sigmaThetaExpr_viewer=function(sigmaThetaExpr, numWaves = NULL)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  cov_mmcsd=function(fit, fittingType, sigmaThetaExpr, optimParams)

## -----------------------------------------------------------------------------
fit <- mmcsd(
  score ~ wave + ageg + ecacg + qualifg,
  waves = wave, ids = id,
  weights = weight, stratum = strata, cluster = cluster,
  data = example_data, sigma = "exchangeable"
)

## -----------------------------------------------------------------------------
sigmaThetaExpr_viewer("UCM", 5)

## -----------------------------------------------------------------------------
sigmaThetaExpr_viewer("AR1", 5)

## -----------------------------------------------------------------------------
fitTheta_ucm <- cov_mmcsd(fit,
  fittingType = "PML", sigmaThetaExpr = "UCM",
  optimParams = list(par = c(7, 5))
)


fitTheta_ar1 <- cov_mmcsd(fit,
  fittingType = "PML", sigmaThetaExpr = "AR1",
  optimParams = list(par = c(0.8, 1,0.8))
)


## ----echo=FALSE---------------------------------------------------------------
fitTheta_df=data.frame(RMR=c(fitTheta_ucm$gofMeasures$RMR,fitTheta_ar1$gofMeasures$RMR),AGFI=c(fitTheta_ucm$gofMeasures$AGFI,fitTheta_ar1$gofMeasures$AGFI),N_Params=c(2,3))

row.names(fitTheta_df)=c("UCM_structure","AR1_structure")

kbl(fitTheta_df) %>%
    kable_classic(full_width = F, html_font = "Cambria")


## ----include=FALSE------------------------------------------------------------
write_matex <- function(x) {
  begin <- "$$\\begin{bmatrix}"
  end <- "\\end{bmatrix}$$"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  writeLines(c(begin, X, end))
}


## ---- results = 'asis'--------------------------------------------------------
write_matex(fitTheta_ucm$sigmaTheta)

## -----------------------------------------------------------------------------

set.seed(1108)

tdef <- defData(varname = "T", dist = "binary", formula = 0.5)
tdef <- defData(tdef, varname = "Y0", dist = "normal", formula = 10, variance = 1)
tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 + 2",
                variance = 1)
tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y0 + 3",
                variance = 1)
tdef <- defData(tdef, varname = "Y3", dist = "normal", formula = "Y0 + 4",
                variance = 1)
tdef <- defData(tdef, varname = "Y4", dist = "normal", formula = "Y0 + 5",
                variance = 1)



dtTrial <- genData(1000, tdef)

dtTime <- addPeriods(dtTrial, nPeriods = 5, idvars = "id", timevars = c("Y0", "Y1",
                                                                        "Y2", "Y3", "Y4"), timevarName = "Y")

dtTime %>% 
  ggplot(aes(x = Y)) + geom_histogram() + facet_wrap(~period) + theme_bw()


## -----------------------------------------------------------------------------
fit=mmcsd(Y~period,ids = id,waves =  period, data=dtTime,sigma = 'exchangeable' )


## -----------------------------------------------------------------------------

fitTheta_ucm <- cov_mmcsd(fit,
                      fittingType = "PML", sigmaThetaExpr = "UCM",
                      optimParams = list(par = c(1, 1))
)

fitTheta_ar1 <- cov_mmcsd(fit,
                      fittingType = "PML", sigmaThetaExpr = "AR1",
                      optimParams = list(par = c(1, 1, -0.5))
)


## ----echo=FALSE---------------------------------------------------------------
fitTheta_df=data.frame(RMR=c(fitTheta_ucm$gofMeasures$RMR,fitTheta_ar1$gofMeasures$RMR),AGFI=c(fitTheta_ucm$gofMeasures$AGFI,fitTheta_ar1$gofMeasures$AGFI),N_Params=c(2,3))

row.names(fitTheta_df)=c("UCM_structure","AR1_structure")

kbl(fitTheta_df) %>%
    kable_classic(full_width = F, html_font = "Cambria")


## ---- results = 'asis'--------------------------------------------------------
write_matex(fitTheta_ucm$sigmaTheta)

## -----------------------------------------------------------------------------
tdef <- defData(varname = "T", dist = "binary", formula = 0.5)
tdef <- defData(tdef, varname = "Y0", dist = "normal", formula = 10, variance = 1)
tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 * 0.7 + 1",
                variance = 1)
tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y1 * 0.7 + 1",
                variance = 1)
tdef <- defData(tdef, varname = "Y3", dist = "normal", formula = "Y2 * 0.7 + 1",
                variance = 1)
tdef <- defData(tdef, varname = "Y4", dist = "normal", formula = "Y3 * 0.7 + 1",
                variance = 1)



dtTrial <- genData(1000, tdef)

dtTime <- addPeriods(dtTrial, nPeriods = 5, idvars = "id", timevars = c("Y0", "Y1",
                                                                        "Y2", "Y3", "Y4"), timevarName = "Y")


dtTime %>% 
  ggplot(aes(x = Y)) + geom_histogram() + facet_wrap(~period) + theme_bw()


## -----------------------------------------------------------------------------
fit=mmcsd(Y~period,ids = id,waves =  period, data=dtTime,sigma = 'exchangeable' )

## -----------------------------------------------------------------------------
fitTheta_ucm <- cov_mmcsd(fit,
                      fittingType = "PML", sigmaThetaExpr = "UCM",
                      optimParams = list(par = c(1, 1))
)

fitTheta_ar1 <- cov_mmcsd(fit,
                      fittingType = "PML", sigmaThetaExpr = "AR1",
                      optimParams = list(par = c(1, 1, -0.5))
)

## ----echo=FALSE---------------------------------------------------------------

fitTheta_df=data.frame(RMR=c(fitTheta_ucm$gofMeasures$RMR,fitTheta_ar1$gofMeasures$RMR),AGFI=c(fitTheta_ucm$gofMeasures$AGFI,fitTheta_ar1$gofMeasures$AGFI),N_Params=c(2,3))

row.names(fitTheta_df)=c("UCM_structure","AR1_structure")

kbl(fitTheta_df) %>%
    kable_classic(full_width = F, html_font = "Cambria")


## ---- results = 'asis'--------------------------------------------------------
write_matex(fitTheta_ar1$sigmaTheta)

