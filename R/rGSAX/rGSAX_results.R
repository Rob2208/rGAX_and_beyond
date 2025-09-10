######################################################
######################################################
#### psxg and GSAX
######################################################
######################################################

library(tidyverse)
library(comets)
library(coin)
library(latex2exp)
library(ggpubr)
library(Cairo)

############################
#### load data
############################

#xgmod_xgb <- readRDS("data/prelim_res/xG_mods_comets/psxg_xgb_wteam.rds")
xgmod_xgb <- readRDS("data/models/xgb_psxg_mod.rds")

shots <- readRDS("data/shot_data_psxg_rel_1516.rds")

create_shots1 <- function(shots){
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

  return(shots1)
}


dta <- create_shots1(shots)

Y <- dta$shot_y
X <- dta[,-c(grep("shot_y",names(dta)))]

du <- cbind(Y,X[,-c(grep("player_name_fac",colnames(X)),
                    grep("player_name_GK_fac",colnames(X)))])

fm <- as.formula(paste0("Y ~ ."))
#Y <- stats::model.response(stats::model.frame(fm, du))
Z <- comets:::.rm_int(stats::model.matrix(fm, du, rhs = 1))
#Y <- comets:::.check_data(Y,"Y")
Z <- comets:::.check_data(Z,"Z")

############################
#### psxG comp
############################

psxG_dta_rel <- shots |>
  mutate(psxG_xgb = predict(xgmod_xgb,data = as.matrix(Z)),
         GSAX_xgb = -shot_y+psxG_xgb)

res_tab <- psxG_dta_rel |>
  group_by(player_name_GK_fac) |>
  summarise(psxG = sum(psxG_xgb),
            GSAX_xgb = sum(GSAX_xgb),
            goals_against = sum(shot_y), n= n()) |>
  filter(n >= 10) |>
  arrange(desc(GSAX_xgb)) |>
  mutate(rank_GSAX = row_number())

############################
#### rGSAX
############################

create_rGSAX_table <- function(rd,
                              name_addon = c("tuned_rf",
                                             "tuned_xgb",
                                             "rf")){
  fu <- paste0("data/rGSAX/")
  ff <- list.files(fu)
  ff_rel <- ff[grep(rd,ff)]
  if(!is.null(name_addon)){
    cat(length(ff_rel),"\n")
    ff_rel <- ff_rel[grep(name_addon,ff_rel)]
    cat(length(ff_rel),"\n")
  }
  l1 <- readRDS(paste0(fu,ff_rel[1]))
  if(length(ff_rel) > 1){
    for(i in 2:length(ff_rel)){
      l1 <- append(l1,readRDS(paste0(fu,ff_rel[i])))
    }
  }
  pv <- sapply(l1, \(x) x$p.value)
  ts <- sapply(l1, \(x) x$statistic)
  rGSAX <- sapply(l1, \(x) -sum(x$rY*x$rX))
  sds <- sapply(l1, \(x) {
    tst <- independence_test(x$rY ~ x$rX, teststat = "scalar")
    sqrt(variance(tst))
  })
  rGSAX_tab_GK <- data.frame(pl = gsub("player_name_GK_fac","",names(pv)),
                             pval = unname(pv),
                             tstat = unname(ts),
                             rGSAX = unname(rGSAX),
                             sds = unname(sds)) |>
    arrange(desc(rGSAX))


  return(rGSAX_tab_GK)
}

rGSAX_tab_GK <- create_rGSAX_table("140825","tuned_rf")
full_tab_GK <- rGSAX_tab_GK |>
  left_join(res_tab |> mutate(pl = as.character(player_name_GK_fac)) |>
              select(-player_name_GK_fac),by = "pl",suffix =c("_gcm","_glm")) |>
  mutate(rank_rGSAX = row_number())



critv <- -1.64
p_xgb_var_95p_GK <- full_tab_GK |>
  arrange(tstat) |>
  #mutate(sig = ifelse(pval_gcm <= 0.05,"p <= 0.05","p > 0.05")) |>
  mutate(sig = ifelse(tstat <= critv,"p <= 0.05","p > 0.05"),
         sig_alpha = ifelse(tstat <= critv,0.6,0.8)) |>
  mutate(bin_sig = as.factor(ifelse(tstat <= critv,1,0))) |>
  ggplot(aes(x = rGSAX,y = GSAX_xgb))+
  #geom_smooth(method = "lm",se= FALSE,size = 0.3, color = "black")+
  geom_abline()+
  geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
  scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
  #scale_color_manual(values = c("#009E73","#D55E00","#0072B2","#F0E442"))+
  scale_color_manual(values = c("#38761d","gray50"), labels = unname(TeX(c("$\\leq 0.05$","$ > 0.05$"))))+ # #D55E00
  annotate("text",x = 6,y =-7,label = paste0("R = ",round(cor(full_tab_GK$GSAX_xgb,full_tab_GK$rGSAX),3)))+
  #geom_smooth(method = "lm",se= FALSE,size = 0.3,aes(color = role.name2))+
  theme_classic()+
  theme(text = element_text(size = 10.5), legend.position="top")+
  labs(color = "p-value",
       size = "precision",
       x = "empirical rGSAX",
       y = "empirical GSAX",
       tag = "A")

ci_data_GK <- full_tab_GK |>
  mutate(up = rGSAX+1.96*sds,
         low = rGSAX-1.96*sds,
         low1s = rGSAX-1.64*sds)

ci_plot_wGSAX <- ci_data_GK |>
  mutate(
         sig = ifelse(tstat >= 1.96, 1, 0),
         label = paste0(pl, " (", rank_GSAX,"|",rank_rGSAX, ")")) |>
  dplyr::slice(1:10) |>
  ggplot(aes(y = reorder(label, rGSAX))) +
  geom_point(aes(x = rGSAX, shape = "rGSAX"),size = 2) +
  geom_errorbar(aes(xmin = low, xmax = up), width = 0.5) +
  geom_point(aes(x = GSAX_xgb, shape = "GSAX"), position = position_nudge(y = 0),size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "empirical GSAX and RSGAX", y = NULL, color = "Metric", shape = "Metric",tag = "B") +
  #scale_color_manual(values = c("rGSAX" = "gray50", "GSAX" = "#38761d")) +
  scale_shape_manual(values = c("rGSAX" = 16, "GSAX" = 4)) +
  theme_light() +
  theme(legend.position = "top")

ci_plot_wGSAX_1s <- ci_data_GK |>
  mutate(
         sig = ifelse(tstat >= 1.96, 1, 0),
         label = paste0(pl, " (", rank_GSAX,"|",rank_rGSAX, ")")) |>
  dplyr::slice(1:10) |>
  ggplot(aes(y = reorder(label, rGSAX))) +
  geom_point(aes(x = rGSAX, shape = "rGSAX"),size = 2) +
  geom_errorbar(aes(xmin = low1s, xmax = Inf), width = 0.5) +
  geom_point(aes(x = GSAX_xgb, shape = "GSAX"), position = position_nudge(y = 0),size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "empirical GSAX and rGSAX", y = NULL, color = "Metric", shape = "Metric",tag = "B") +
  #scale_color_manual(values = c("rGSAX" = "gray50", "GSAX" = "#38761d")) +
  scale_shape_manual(values = c("rGSAX" = 16, "GSAX" = 4)) +
  theme_light() +
  theme(legend.position = "top")

ggarrange(p_xgb_var_95p_GK, ci_plot_wGSAX, align = "h")
ggsave("rGSAX_plots_wGSAX_2s.pdf", height = 6, width = 11, scale = 0.9,device = cairo_pdf)
ggarrange(p_xgb_var_95p_GK, ci_plot_wGSAX_1s, align = "h")
ggsave("plots/rGSAX_1s_tuned_rf.pdf", height = 6, width = 12, scale = 0.9,device = cairo_pdf)
