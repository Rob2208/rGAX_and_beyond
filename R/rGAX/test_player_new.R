######################################################
######################################################
#### test_player_new
######################################################
######################################################


test_player <- function(player,
                        tst = c("gcm","pcm"),
                        covs = c("svo","wGK","all"),
                        ...){
  tst <- match.arg(tst)
  covs <- match.arg(covs)


  Y <- shots1$shot_y
  X <- shots1[,-c(grep("shot_y",names(shots1)))]

  if(covs == "svo"){
    du <- cbind(Y,X[,-c(grep("player_name_fac",colnames(X)),
                        grep("player_name_GK_fac",colnames(X)))],
                X[,grep(player,colnames(X)),drop = FALSE])
  }else if(covs == "wGK"){
    du <- cbind(Y,X[,-c(grep("player_name_fac",colnames(X)))],
                X[,player,drop = FALSE])
  }else{
    du <- cbind(Y,X)
  }

  out <- try(comet(as.formula(paste0("Y ~ `",player,"` | . - `",player,"`")),data = du, test = tst,...))
  if (!inherits(out, "try-error"))
    return(out)
  list()

}


test_GK <- function(player,
                    tst = c("gcm","pcm"),
                    covs = c("svo","wPL","all"),
                    ...){
  tst <- match.arg(tst)
  covs <- match.arg(covs)


  Y <- shots1$shot_y
  X <- shots1[,-c(grep("shot_y",names(shots1)))]

  if(covs == "svo"){
    du <- cbind(Y,X[,-c(grep("player_name_fac",colnames(X)),
                        grep("player_name_GK_fac",colnames(X)))],
                X[,grep(player,colnames(X)),drop = FALSE])
  }else if(covs == "wPL"){
    du <- cbind(Y,X[,-c(grep("player_name_GK_fac",colnames(X)))],
                X[,player,drop = FALSE])
  }else{
    du <- cbind(Y,X)
  }

  out <- try(comet(as.formula(paste0("Y ~ `",player,"` | . - `",player,"`")),data = du, test = tst,...))
  if (!inherits(out, "try-error"))
    return(out)
  list()

}
