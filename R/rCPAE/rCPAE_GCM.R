######################################################
######################################################
#### AF CPAE // rCPAE
######################################################
######################################################

library(tidyverse)
library(comets)
library(xgboost)
library(caret)


############################
#### For array jobs
############################

args <- commandArgs(trailingOnly = TRUE)
array_id <- if (identical(args, character(0))) -1 else as.numeric(args[1])
cat("Simulation Setting:", array_id)

############################
#### Setup
############################

date = "060925"

############################
#### data prep
############################


passes <- as.data.frame(readRDS("data/nfl_passing_data.rds")) |>
  select(-passer_position,-passer_team,-passer_id) |>
  mutate(player = as.factor(passer_name)) |>
  select(-passer_name)

passes_mm_pl <- model.matrix(~0+player,data = passes)

passes_pre <- bind_cols(passes |> dplyr::select(-player),
                       passes_mm_pl)

lsf_col <- min(grep("player",names(passes_pre)))-1 ## last shot-specific feature column

el_players <- which(colSums(passes_pre[,-c(1:lsf_col)]) < 300)
passes_el <- passes_pre[,-(lsf_col+el_players)]

passes1 <- passes_el


############################
#### test_player_func
############################


test_player <- function(player,
                        tst = c("gcm","pcm","glmw"),
                        #covs = c("svo","wGK","wteam","wGK_wteam","all"),
                        ...){
  tst <- match.arg(tst)
  #covs <- match.arg(covs)


  Y <- passes1$complete_pass
  X <- passes1[,-c(grep("complete_pass",names(passes1)))]

  du <- cbind(Y,X[,-c(grep("player",colnames(X)))],
              X[,grep(player,colnames(X)),drop = FALSE])

  out <- try(comet(as.formula(paste0("Y ~ `",player,"` | . - `",player,"`")),data = du, test = "gcm",...))
  if (!inherits(out, "try-error"))
    return(out)
  list()

}


###############
### Fit models
###############

if(array_id < 0){
  #cat("we re in if?")
  players <- colnames(passes1)[grep("player",colnames(passes1))]
}else{
  #cat("do we reach else?")
  players_full <- colnames(passes1)[grep("player",colnames(passes1))]
  if(array_id == 1){
    #cat("if so... back to if?")
    players <- players_full[1:20]
  }else{
    #cat("if so... again in else?")
    s <- (array_id-1)*20+1
    e <- array_id*20
    if(e > length(players_full)){
      e <- length(players_full)
    }
    players <- players_full[s:e]
  }
}


#test <- match.fun("pcm")
set.seed(123)
pb <- txtProgressBar(min = 0, max = length(players), style = 3)
tsts_gcm <- lapply(players, function(player,nfold = 5){

  target <- passes1[[player]]
  #xfolds <- createFolds(target, k = nfold, list = TRUE)
  setTxtProgressBar(pb, which(player == players))
  test_player(player, "gcm",reg_YonZ = "tuned_rf",reg_XonZ = "tuned_rf",
              args_YonZ = list(probability = TRUE),
              args_XonZ = list(probability = TRUE),
              type = "scalar", coin = TRUE)
})
names(tsts_gcm) <- players

saveRDS(tsts_gcm,paste0("data/rCPAE/rcpae_tuned_rf_",array_id,"_060925.rds"))
