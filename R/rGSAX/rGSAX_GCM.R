######################################################
######################################################
#### xG player significances || Cluster version
#### Goalkeeper analysis via GSAX
######################################################
######################################################


library(tidyverse)
library(comets)
library(xgboost)
library(caret)
#source("R/alr_lr.R")
source("R/rGAX/test_player_new.R")


############################
#### For array jobs
############################

args <- commandArgs(trailingOnly = TRUE)
array_id <- if (identical(args, character(0))) -1 else as.numeric(args[1])
cat("Simulation Setting:", array_id)

############################
#### Setup
############################

date = "140825"
#xreg = "tuned_rf" # tuned_xgb, rf

############################
#### data prep
############################

shots <- readRDS("data/shot_data_psxg_rel_1516.rds")

shot_mm_pl <- model.matrix(~0+player_name_fac,data = shots)
shot_mm_GK <- model.matrix(~0+player_name_GK_fac,data = shots)

shots_pre <- bind_cols(shots |> dplyr::select(-player_name_fac,-player_name_GK_fac,-team.name,-opp.team.name),
                       shot_mm_GK[,-1],
                       shot_mm_pl)

lsf_col <- min(grep("player",names(shots_pre)))-1 ## last shot-specific feature column

el_players <- which(colSums(shots_pre[,-c(1:lsf_col)]) < 20)
shots_el <- shots_pre[,-(lsf_col+el_players)]

# eliminate players with no goals in data
el_ngp <- which(colSums(shots_el[shots_el$shot_y == 0,-(1:lsf_col)]) == colSums(shots_el[,-(1:lsf_col)]))

if(length(el_ngp) == 0){
  shots1 = shots_el
}else{
  shots1 <- shots_el[,-(lsf_col+el_ngp)]
}

shots1 <- shots1 |> select(-Def)

###############
### Define x reg
### necessary for comets version 0.1.1
### With newer version of comets (see github) not necessary anymore
###############

tuned_xgb2 <- function(y, x, etas = c(0.1, 0.5, 1), max_depths = 1:5,
                       folds = NULL, nrounds = c(2, 10, 50), verbose = 0,
                       metrics = list("rmse"), ...) {
  if (requireNamespace("xgboost")) {
    cvres <- lapply(etas, \(teta) {
      lapply(max_depths, \(tmd) {
        lapply(nrounds, \(tnr) {
          cv <- do.call("xgb.cv", c(list(
            data = x, label = y, nrounds = tnr,
            verbose = verbose, eta = teta, max_depth = tmd,
            metrics = metrics, folds = folds
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


###############
### Define y reg
###############

psxg_mod <- readRDS("data/models/xgb_psxg_mod.rds")

psxG_reg <- function(y,x,psxg_mod = NULL,...){
  out <- psxg_mod
  class_out <- c("psxG",class(out))
  return(out)
}

predict.psxG <- function(object,data = NULL,...){
  class(object) <- class(object)[-1]
  predict(object, data, ...)
}

residuals.psxG <- function(object, response = NULL, data = NULL, ...) {
  preds <- predict(object, data = data, ...)
  .compute_residuals(response, preds)
}

###############
### Fit models
###############

if(array_id < 0){
  #cat("we re in if?")
  players <- colnames(shots1)[grep("player_name_GK_fac",colnames(shots1))]
}else{
  #cat("do we reach else?")
  players_full <- colnames(shots1)[grep("player_name_GK_fac",colnames(shots1))]
  if(array_id == 1){
    #cat("if so... back to if?")
    players <- players_full[1:30]
  }else{
    #cat("if so... again in else?")
    s <- (array_id-1)*30+1
    e <- array_id*30
    if(e > length(players_full)){
      e <- length(players_full)
    }
    players <- players_full[s:e]
  }
}


set.seed(123)
pb <- txtProgressBar(min = 0, max = length(players), style = 3)
tsts_gcm <- lapply(players, function(player,nfold = 5){

  target <- shots1[[player]]
  xfolds <- createFolds(target, k = nfold, list = TRUE)
  setTxtProgressBar(pb, which(player == players))
  test_GK(player, "gcm",covs = "svo",reg_YonZ = "psxG_reg",reg_XonZ = "tuned_rf",
          args_YonZ = list(psxg_mod = psxg_mod),
          args_XonZ = list(probability = TRUE),
          type = "scalar", coin = TRUE)
})
names(tsts_gcm) <- players

saveRDS(tsts_gcm,paste0("data/rGSAX/gcm_",array_id,"_tuned_rf_",date,".rds"))
