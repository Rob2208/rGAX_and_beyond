######################################################
######################################################
#### BB sQI // rsQI
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

date = "040925"

############################
#### data prep
############################

#shots <- as.data.frame(readRDS("data/nba_data23_pp.rds")) |> mutate(player = as.factor(player))
shots <- as.data.frame(readRDS("data/nba_data23_pp2.rds")) |>
  mutate(player = as.factor(player))

shot_mm_pl <- model.matrix(~0+player,data = shots)

shots_pre <- bind_cols(shots |> dplyr::select(-player),
                       shot_mm_pl)

lsf_col <- min(grep("player",names(shots_pre)))-1 ## last shot-specific feature column

el_players <- which(colSums(shots_pre[,-c(1:lsf_col)]) < 300)
shots_el <- shots_pre[,-(lsf_col+el_players)]

# eliminate players with no goals in data
# el_ngp <- which(colSums(shots_el[shots_el$shot_y == 0,-(1:lsf_col)]) == colSums(shots_el[,-(1:lsf_col)]))
#
# shots1 <- shots_el[,-(lsf_col+el_ngp)]

# shots1 <- shots1 |>
#   select(-Att,-shot_y)
shots1 <- shots_el |>
  select(-scoring_ind)

############################
#### test_player_func
############################


test_player <- function(player,
                        tst = c("gcm","pcm","glmw"),
                        #covs = c("svo","wGK","wteam","wGK_wteam","all"),
                        ...){
  tst <- match.arg(tst)
  #covs <- match.arg(covs)


  Y <- shots1$score_value
  X <- shots1[,-c(grep("score_value",names(shots1)))]

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
  players <- colnames(shots1)[grep("player",colnames(shots1))]
}else{
  #cat("do we reach else?")
  players_full <- colnames(shots1)[grep("player",colnames(shots1))]
  if(array_id == 1){
    #cat("if so... back to if?")
    players <- players_full[1:50]
  }else{
    #cat("if so... again in else?")
    s <- (array_id-1)*50+1
    e <- array_id*50
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

  target <- shots1[[player]]
  #xfolds <- createFolds(target, k = nfold, list = TRUE)
  setTxtProgressBar(pb, which(player == players))
  test_player(player, "gcm",reg_YonZ = "tuned_rf",reg_XonZ = "tuned_rf",
              args_XonZ = list(probability = TRUE),
              type = "scalar", coin = TRUE)
})
names(tsts_gcm) <- players

saveRDS(tsts_gcm,paste0("data/rqSI/rqsi_score_tuned_rf_",array_id,"_",date,".rds"))
