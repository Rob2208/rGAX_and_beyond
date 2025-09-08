######################################################
######################################################
#### Fit xG models for GAX and comets
######################################################
######################################################


library(tidyverse)
library(comets)
library(xgboost)

############################
#### data prep
############################

shots <- readRDS("data/shot_data_rel_1516_div_new.rds")

shots1 <- shots  |>
  dplyr::select(-player_name_fac,-player_name_GK_fac,-team.name,-opp.team.name,-Att)


tuned_xgb2.1 <- function(y, x, etas = c(0.1, 0.5, 1), max_depths = 1:5,
                       nfold = 5, nrounds = c(2, 10, 50), verbose = 0,
                       metrics = list("rmse"), ...) {
  if (requireNamespace("xgboost")) {
    cvres <- lapply(etas, \(teta) {
      lapply(max_depths, \(tmd) {
        lapply(nrounds, \(tnr) {
          cv <- do.call("xgb.cv", c(list(
            data = x, label = y, nrounds = tnr,
            verbose = verbose, eta = teta, max_depth = tmd,
            metrics = metrics, nfold = nfold
          ), list(...)))
          err <- min(cv$evaluation_log[[paste0("test_", metrics[[1]], "_mean")]])
          data.frame(
            nrounds = tnr, eta = teta, max_depth = tmd, error = err
          )
        }) |> do.call("rbind", args = _)
      }) |> do.call("rbind", args = _)
    }) |> do.call("rbind", args = _)
    best <- which.min(cvres$error)[1]
    bst <- comets:::xgb(y, x,
                        nrounds = cvres$nrounds[best], verbose = verbose,
                        max_depth = cvres$max_depth[best], eta = cvres$eta[best], ...
    )
    class(bst) <- c("xgb", class(bst))
    return(bst)
  }
  stop("Package `xgboost` not available.")
}



fit_xg_mod <- function(dta,
                      method = c("tuned_xgb","tuned_rf","glrm"),
                      ...){
  Y <- dta$shot_y
  X <- dta[,-c(grep("shot_y",names(dta)))]

  method <- match.arg(method)

  du <- cbind(Y,X)

  fm <- as.formula(paste0("Y ~ ."))
  Y <- stats::model.response(stats::model.frame(fm, du))
  Z <- comets:::.rm_int(stats::model.matrix(fm, du, rhs = 1))
  Y <- comets:::.check_data(Y,"Y")
  Z <- comets:::.check_data(Z,"Z")

  if(method == "tuned_xgb"){
    # comets:::tuned_xgb(Y,Z, objective = "binary:logistic",
    #                    metrics = list("logloss"),
    #                    etas = c(0.01,01,0.5,1),
    #                    nrounds = c(10,50,100,500),
    #                    early_stopping_rounds = 15)
    mod <- tuned_xgb2.1(y = Y, x = Z, objective = "binary:logistic",
                       metrics = list("logloss"),
                       ...)
  }else if(method == "tuned_rf"){
    mod <- comets:::tuned_rf(y = Y,x = Z,
                             mtrys = list(1,2,sqrt(ncol(Z))),...)
  }else if(method == "glrm"){
    mod <- comets:::glrm(y = Y,x = Z,family = binomial())
  }
  return(mod)
}

cat("Start fitting...\n")


############################
#### xgb
############################

set.seed(123)

xgb_svo <- fit_xg_mod(shots1,
                      method = "tuned_xgb",
                      etas = c(0.001,0.005,0.01,01,0.5,1),
                      nrounds = c(10,50,100,500),
                      max_depths = c(1,3,4,5,7,9),
                      early_stopping_rounds = 50)
#saveRDS(xgb_svo,"data/prelim_res/xG_mods_comets/xgb_svo.rds")
saveRDS(xgb_svo,"data/models/xgb_xg_mod.rds")

#cat("xgb_svo done!\n")
