
## Rethinking player evaluation in sports: Goals above expectation and beyond

This GitHub repository accompanies the arXiv preprint …

## Folder structure

- `data` contains all relevant and already preprocessed data for the
  rGAX, rqSI, rCPAE, and rIAX

  - `models` contains a pre-trained xG model. This model can also be
    obtained by running the `R/rGAX/fit_xg_mods.R` file

- `R` contains relevant R code to reproduce the results of the paper

  - `R/dependencies.R` R script for installing necessary dependencies  
  - `R/rGAX` contains R scripts to fit an xG model, to obtain rGAX for
    all players in the data, and to reproduce the figures in the paper
  - `R/rGSAX` contains R scripts to fit a psxG model, to obtain rGSAX
    for all players in the data, and to reproduce the figures in the
    paper
  - `R/rqSI` contains R scripts to obtain rqSI for all players in the
    data and code to reproduce the figures in the paper
  - `R/rCPAE` contains R scripts to obtain rCPAE for all players in the
    data and code to reproduce the figures in the paper

## Reproducing the results

Reproducing all results from the paper requires substantial
computational effort. The code can be easily run on a server or in
parallel and is also structured in that way. For only testing the
procedure (without running the full study) we provide a code example
below.

## Quick code example

We showcase how to obtain rGAX using the `comets` (Kook and Lundborg
2024) package for the case of Luis Suárez. First, we load and transform
the data.

``` r
## dependencies

library(tidyverse)
library(comets)
library(coin)

## load data

shots <- readRDS("data/shot_data_rel_1516_div_new.rds")

## make column for Suarez

shots <- shots |>
  mutate(Luis_Suarez = ifelse(player_name_fac == "Luis Alberto Suárez Díaz",1,0))

shots1 <- shots  |>
  dplyr::select(-player_name_fac,-player_name_GK_fac,-team.name,-opp.team.name,-Att)
```

Next to obtain rGAX, we use the GCM test implemented in the `comets`
package. `comets` allows to define which machine learning model to use
for the regressions of Y on Z and X on Z. As in the main text, we use a
pre-trained xG model for the regression of Y on Z and a tuned random
forest for the regression of X on Z. For the former, we need to define a
suitable regression method, the latter is preimplemented in the package.
To test for $Y$ independent of $X$ given $Z$, the formula-based
interface of the `comet` function can be used by providing a formula of
the type `Y ~ X | Z`.

``` r
###############
### Define y reg
###############

xg_mod <- readRDS("data/models/xgb_xg_mod.rds")

xG_reg <- function(y,x,xg_mod = NULL,...){
  out <- xg_mod
  class_out <- c("xG",class(out))
  return(out)
}

predict.xG <- function(object,data = NULL,...){
  class(object) <- class(object)[-1]
  predict(object, data, ...)
}

residuals.xG <- function(object, response = NULL, data = NULL, ...) {
  preds <- predict(object, data = data, ...)
  .compute_residuals(response, preds)
}

###############
### GCM test
###############

set.seed(123)

GCM_suarez <- comet("shot_y ~ Luis_Suarez | . - Luis_Suarez", data = shots1,
                    test = "gcm", reg_YonZ = "xG_reg", reg_XonZ = "tuned_rf",
                    args_YonZ = list(xg_mod = xg_mod), args_XonZ = list(probability = TRUE),
                    type = "scalar", verbose = 0)
```

``` r
GCM_suarez
```

    ## 
    ##  Generalized covariance measure test
    ## 
    ## data:  comet(formula = "shot_y ~ Luis_Suarez | . - Luis_Suarez", data = shots1, 
    ##     test = "gcm", reg_YonZ = "xG_reg", reg_XonZ = "tuned_rf", 
    ##     args_YonZ = list(xg_mod = xg_mod), args_XonZ = list(probability = TRUE), 
    ##     type = "scalar", verbose = 0)
    ## Z = 3.5948, p-value = 0.0003247
    ## alternative hypothesis: true E[cov(Y, X | Z)] is not equal to 0

From the test result, we can extract $p$-values and the test statistic
to see that Luis Suárez has a significant positive impact on the
probability of scoring a goal in our semiparametric framework.  
With the GCM test result, we are also able to obtain rGAX and the
corresponding 95% confidence interval. To compute the confidence
interval, we use the `coin` package (Hothorn et al. 2008), which relies
on an approximation of the asymptotic permutation distribution, to
estimate the standard deviation of the test statistic.

``` r
rGAX <- sum(GCM_suarez$rY*GCM_suarez$rX)
tst <- independence_test(GCM_suarez$rY ~ GCM_suarez$rX, teststat = "scalar")
sd <- sqrt(variance(tst))
ci <- c(rGAX-1.96*sd, rGAX+1.96*sd)

rGAX
```

    ## [1] 9.969451

``` r
ci
```

    ## [1]  4.533501 15.405400

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Hothorn08coin" class="csl-entry">

Hothorn, Torsten, Kurt Hornik, Mark A. van de Wiel, and Achim Zeileis.
2008. “Implementing a Class of Permutation Tests: The
<span class="nocase">coin</span> Package.” *Journal of Statistical
Software* 28 (8): 1–23. <https://doi.org/10.18637/jss.v028.i08>.

</div>

<div id="ref-kook24comets" class="csl-entry">

Kook, Lucas, and Anton Rask Lundborg. 2024. “Algorithm-Agnostic
Significance Testing in Supervised Learning with Multimodal Data.”
*Briefings in Bioinformatics* 25 (6): bbae475.
<https://doi.org/10.1093/bib/bbae475>.

</div>

</div>
