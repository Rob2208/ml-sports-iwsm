######################################################
######################################################
#### IWSM RGAX replication || written for usage on cluster
#### xG mod for regression
#### with team strengths
######################################################
######################################################


library(tidyverse)
library(comets)
library(xgboost)
library(caret)


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


############################
#### For array jobs
############################

args <- commandArgs(trailingOnly = TRUE)
array_id <- if (identical(args, character(0))) -1 else as.numeric(args[1])
cat("Simulation Setting:", array_id)

############################
#### data prep
############################

shots <- readRDS("data/shot_data_rel_1516.rds")

### create model matrix
shot_mm_pl <- model.matrix(~0+player_name_fac,data = shots)
shot_mm_GK <- model.matrix(~0+player_name_GK_fac,data = shots)

shots_pre <- bind_cols(shots |> dplyr::select(-player_name_fac,-player_name_GK_fac,-team.name,-opp.team.name),
                       shot_mm_GK[,-1],
                       shot_mm_pl)

lsf_col <- min(grep("player",names(shots_pre)))-1 ## last shot-specific feature column

## eliminate players with less than 20 shots
el_players <- which(colSums(shots_pre[,-c(1:lsf_col)]) < 20)
shots_el <- shots_pre[,-(lsf_col+el_players)]

# eliminate players with no goals in data
el_ngp <- which(colSums(shots_el[shots_el$shot_y == 0,-(1:lsf_col)]) == colSums(shots_el[,-(1:lsf_col)]))

shots1 <- shots_el[,-(lsf_col+el_ngp)]

shots1 <- shots1 |> select(-Att)

###############
### Define x reg
### In newer versions of "comets" not necessary anymore
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

xg_mod <- readRDS("models/xgb_svo_new_natt.rds")

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
### Fit models
###############

if(array_id < 0){
  #cat("we re in if?")
  players <- colnames(shots1)[grep("player_name_fac",colnames(shots1))]
}else{
  #cat("do we reach else?")
  players_full <- colnames(shots1)[grep("player_name_fac",colnames(shots1))]
  if(array_id == 1){
    #cat("if so... back to if?")
    players <- players_full[1:100]
  }else{
    #cat("if so... again in else?")
    s <- (array_id-1)*100+1
    e <- array_id*100
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
  xfolds <- createFolds(target, k = nfold, list = TRUE)
  setTxtProgressBar(pb, which(player == players))
  test_player(player, "gcm",covs = "svo",reg_YonZ = "xG_reg",reg_XonZ = "tuned_xgb2",
              args_YonZ = list(xg_mod = xg_mod),
              #args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
              #                 etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100,500)),
              args_XonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                               etas = c(0.005,0.01,0.1,0.5,1),nrounds = c(10,50,100,500), max_depths = c(1,3,4,5,7),folds = xfolds),
              type = "scalar")
})
names(tsts_gcm) <- players

saveRDS(tsts_gcm,paste0("../data/gcm_tests_soccer_players_",array_id,".rds"))
