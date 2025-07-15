######################################################################################
######################################################################################
#### IWSM short paper reproduction of HMM feature testing
######################################################################################
######################################################################################

library(comets) ## Version 0.1.1 (defaults change in newer versions!)
library(xgboost)
library(coin)
#library(comets)


test_zvm_features <- function(model_data,
                              test = c("gcm","pcm"),
                              test_vars,
                              excl_vars = NULL,
                              ...){
  dta <- model_data |>
    select(-gameId,-playId,
           -gameplayId,-pff_passCoverage,-c({{excl_vars}}))

  comet(as.formula(paste0("pff_manZone_num ~ ", paste0(test_vars,collapse = " + ")," | . -",paste0(test_vars,collapse = " - "))),
        data = dta, test = test,...)
}


ffs <- readRDS("../data/HMM_model_data_for_gcm.rds")


####################
#### model tests (exclude orientation and dist_fb)
####################

set.seed(123)

gcm_o_s <- test_zvm_features(ffs,
                             test = "gcm",
                             test_vars = c("sum","average_ent","nr_player_changes"),
                             excl_vars = c("week","average",
                                           names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                           names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                             reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                             args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                             args_XonZ = list(early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)))


gcm_ae_s <- test_zvm_features(ffs,
                              test = "gcm",
                              test_vars = "average_ent",
                              excl_vars = c("average","week",
                                            names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                            names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                              reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                              args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              args_XonZ = list(early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              type = "scalar")


gcm_s_s <- test_zvm_features(ffs,
                             test = "gcm",
                             test_vars = "sum",
                             excl_vars = c("average","week",
                                           names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                           names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]
                             ),
                             reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                             args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                             args_XonZ = list(early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                             type = "scalar")


gcm_nc_s <- test_zvm_features(ffs,
                              test = "gcm",
                              test_vars = "nr_player_changes",
                              excl_vars = c("average","week",
                                            names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                            names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]
                              ),
                              reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                              args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              args_XonZ = list(early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              type = "scalar")

GCM_unscaled <- sapply(list(gcm_ae_s,gcm_nc_s,gcm_s_s), \(x) sum(x$rY*x$rX))
sds <- sapply(list(gcm_ae_s,gcm_nc_s,gcm_s_s), \(x) { #### coin variances. They are smaller than from manual gcm
  tst <- independence_test(x$rY ~ x$rX, teststat = "scalar")
  sqrt(variance(tst))
})

pvals_tbl_s <- data.frame(predictor = c("average entropy","# player changes","sum changes","omnibus"),
                          p.value = c(gcm_ae_s$p.value,gcm_nc_s$p.value,gcm_s_s$p.value,gcm_o_s$p.value),
                          tstat = c(gcm_ae_s$statistic,gcm_nc_s$statistic,gcm_s_s$statistic,gcm_o_s$statistic))
pvals_tbl_s_2 <- data.frame(predictor = c("average entropy","# player changes","sum changes","omnibus"),
                            p.value = c(gcm_ae_s$p.value,gcm_nc_s$p.value,gcm_s_s$p.value,gcm_o_s$p.value),
                            tstat = c(gcm_ae_s$statistic,gcm_nc_s$statistic,gcm_s_s$statistic,gcm_o_s$statistic),
                            GCSM_unscaled = c(GCM_unscaled,NA),
                            sds = c(sds,NA))

saveRDS(pvals_tbl_s2,"pvals_hmm_features.rds")
