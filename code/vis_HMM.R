######################################################################################
######################################################################################
#### IWSM visualizations for RGAX
######################################################################################
######################################################################################

library(tidyverse)
#library(ggsoccer)
#library(ggforce)

sig_df <- readRDS("../data/pvals_hmm_features.rds")


### lollipop chart (short paper and presentation)
sig_df |>
  mutate(predictor = factor(predictor,levels = c("omnibus",
                                                 "average entropy",
                                                 "# player changes",
                                                 "sum changes"))) |>
  ggplot(aes(x = predictor, y = - log10(p.value), color = -log10(p.value)))+
  geom_point( size = 5) +
  geom_segment(
    aes(y = 0, yend = -log10(p.value), x = predictor, xend = predictor),
    linewidth = 1.5,
    #col = -log10(p.value)
  )+
  geom_hline(yintercept = -log10(0.05))+
  geom_vline(xintercept = 1.5,linetype = 2)+
  theme_classic()+
  scale_color_gradient2(midpoint = -log10(0.05),low = "gray50",mid = "yellowgreen",high = "#38761d")+
  theme(text = element_text(size = 13.5), legend.position="top",
        axis.text.x = element_text(angle = 20, vjust = 0.9,hjust = 1))


### confidence bands (presentation only)

ci_data <- sig_df |>
  mutate(up = GCM_unscaled+1.96*sds,
         low = GCM_unscaled-1.96*sds) |>
  mutate(sig = as.numeric(p.value <= 0.05))

ci_data |>
  ggplot(aes(y = reorder(predictor,-GCM_unscaled),color = p.value)) +
  geom_point(aes(x = GCM_unscaled),size = 2) +
  geom_errorbar(aes(xmin = low, xmax = up), width = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "Unnormalized GCM test statistic", y = NULL) +
  #scale_color_manual(values = c("gray50","#38761d"),labels = unname(latex2exp::TeX(c("$ > 0.05$","$\\leq 0.05$")))) +
  scale_color_gradient2(midpoint = 0.045,low = "#38761d",mid = "yellowgreen",high = "gray50")+
  theme_classic() +
  theme(legend.position = "top")+
  labs(color = "p-value")
