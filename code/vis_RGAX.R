######################################################################################
######################################################################################
#### IWSM visualizations for RGAX
######################################################################################
######################################################################################

library(tidyverse)
library(comets)
library(coin)
library(latex2exp)


############################
#### load data
############################

xg_mod <- readRDS("models/xgb_svo_new_natt.rds")

shots <- readRDS("data/shot_data_rel_1516.rds")
Y <- shots$shot_y
#X <- dta[,-c(grep("shot_y",names(dta)))]
X <- shots |>
  select(-shot_y,-Att,-team.name,-opp.team.name)

du <- cbind(Y,X[,-c(grep("player_name_fac",colnames(X)),
                    grep("player_name_GK_fac",colnames(X)))])

fm <- as.formula(paste0("Y ~ ."))
#Y <- stats::model.response(stats::model.frame(fm, du))
Z <- comets:::.rm_int(stats::model.matrix(fm, du, rhs = 1))
#Y <- comets:::.check_data(Y,"Y")
Z <- comets:::.check_data(Z,"Z")


GAX_data <- shots |>
  mutate(xG = predict(xg_mod,data = as.matrix(Z)),
         GAX = shot_y-xG)

res_GAX <- GAX_data |>
  group_by(player_name_fac) |>
  summarise(xG = sum(xG),GAX = sum(GAX),n = n()) |>
  arrange(desc(GAX))


fu <- paste0("data/")
ff <- list.files(fu)
ff_rel <- ff[grep("soccer",ff)]
l1 <- readRDS(paste0(fu,ff_rel[1]))
if(length(ff_rel) > 1){
  for(i in 2:length(ff_rel)){
    l1 <- append(l1,readRDS(paste0(fu,ff_rel[i])))
  }
}
pv <- sapply(l1, \(x) x$p.value)
ts <- sapply(l1, \(x) x$statistic)
rGAX <- sapply(l1, \(x) sum(x$rY*x$rX))
sds <- sapply(l1, \(x) { #### coin variances. They are smaller than from manual gcm
  tst <- independence_test(x$rY ~ x$rX, teststat = "scalar")
  sqrt(variance(tst))
})
rGAX_res <- data.frame(pl = gsub("player_name_fac","",names(pv)),
                           pval = unname(pv),
                           tstat = unname(ts),
                           rGAX = unname(rGAX),
                           #rGAX = unname(rGAX),
                           sds = unname(sds)) |>
  arrange(desc(rGAX))

full_res <- rGAX_res |>
  left_join(GAX_res |> mutate(pl = as.character(player_name_fac)),by = "pl")


critv <- 1.64
full_res |>
  arrange(tstat) |> ### only that orange dots are more prominently...
  #mutate(sig = ifelse(pval_gcm <= 0.05,"p <= 0.05","p > 0.05")) |>
  mutate(sig = ifelse(tstat >= critv,"p <= 0.05","p > 0.05"),
         sig_alpha = ifelse(tstat >= critv,0.6,0.8)) |>
  mutate(bin_sig = as.factor(ifelse(tstat >= critv,1,0))) |>
  ggplot(aes(x = rGAX,y = GAXp_xgb))+
  geom_abline()+
  #geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
  geom_point(aes(color = sig, alpha = bin_sig)) +
  scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
  scale_color_manual(values = c("orange","gray50"), labels = unname(latex2exp::TeX(c("$\\leq 0.05$","$ > 0.05$"))))+
  annotate("text",x = 6,y =-7,label = paste0("R = ",round(cor(ff$GAXp_xgb,ff$rGAX),2)))+
  theme_classic()+
  theme(text = element_text(size = 10.5), legend.position="top")+
  labs(color = "p-value",
       x = "rGAX",
       y = "GAX")
