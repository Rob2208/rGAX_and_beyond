################################################################
################################################################
##### qSI and rqSI | results
################################################################
################################################################


library(tidyverse)
library(comets)
library(xgboost)
library(coin)
library(ggpubr)

############################
#### load data
############################

shots <- as.data.frame(readRDS("data/nba_data23_pp2.rds")) |>
  mutate(player = as.factor(player))

id_tab <- data.frame(id = c(3975,3202,1966,
                   3112335,3136193,3136776,
                   6442,3593,6478,
                   6450,3945274,2990984,
                   3134908,3155942,3448,
                   3078576,3059318,3133628,
                   3936299,3934672,3913174
                   ),
            name = c("Stephen Curry","Kevin Durant","LeBron James",
                     "Nikola Jokic","Devin Booker","D'Angelo Russell",
                     "Kyrie Irving","Bojan Bogdanovic","Nikola Vucevic",
                     "Kawhi Leonard","Luca Doncic","Buddy Hield",
                     "Jakob Poeltl","Domantas Sabonis","Brook Lopez",
                     "Derrick White","Joel Embiid","Myles Turner",
                     "Jamal Murray","Jalen Brunson","Luke Kennard"
                     ))

create_rqSI_table <- function(rd){
  #name_addon = "xgb"
  fu <- paste0("data/rqSI/")
  ff <- list.files(fu)
  ff_rel <- ff[grep(rd,ff)]
  # if(!is.null(name_addon)){
  #   cat(length(ff_rel),"\n")
  #   ff_rel <- ff_rel[grep(name_addon,ff_rel)]
  #   cat(length(ff_rel),"\n")
  # }
  l1 <- readRDS(paste0(fu,ff_rel[1]))
  if(length(ff_rel) > 1){
    for(i in 2:length(ff_rel)){
      l1 <- append(l1,readRDS(paste0(fu,ff_rel[i])))
    }
  }
  pv <- sapply(l1, \(x) x$p.value)
  ts <- sapply(l1, \(x) x$statistic)
  rqSI <- sapply(l1, \(x) sum(x$rY*x$rX))
  qSI <- sapply(l1,\(x){
    X <- as.numeric(x$rX > 0)
    sum(x$rY*X)
  })
  n <- sapply(l1,\(x){
    sum(as.numeric(x$rX > 0))
  })
  sds <- sapply(l1, \(x) { #### coin variances. They are smaller than from manual gcm
    tst <- independence_test(x$rY ~ x$rX, teststat = "scalar")
    sqrt(variance(tst))
  })
  rqSI_tab <- data.frame(pl = gsub("player_name_fac","",names(pv)),
                         pval = unname(pv),
                         tstat = unname(ts),
                         rqSI = unname(rqSI),
                         qSI = unname(qSI),
                         qSI_avg = unname(qSI)/unname(n),
                         #rEGA = unname(rEGA),
                         sds = unname(sds),
                         n = unname(n)) |>
    arrange(desc(rqSI))

  return(rqSI_tab)
}

rqSI_ind <- create_rqSI_table("030925")
rqSI_ind <- rqSI_ind |>
  mutate(id = as.numeric(gsub("player","",pl))) |>
  left_join(id_tab) |>
  arrange(desc(qSI)) |>
  mutate(rank_qSI = row_number()) |>
  arrange(desc(qSI_avg)) |>
  mutate(rank_qSI_avg = row_number()) |>
  arrange(desc(rqSI)) |>
  mutate(rank_rqSI = row_number())
rqSI_score <- create_rqSI_table("040925")
rqSI_score <- rqSI_score |>
  mutate(id = as.numeric(gsub("player","",pl))) |>
  left_join(id_tab) |>
  arrange(desc(qSI)) |>
  mutate(rank_qSI = row_number()) |>
  arrange(desc(qSI_avg)) |>
  mutate(rank_qSI_avg = row_number()) |>
  arrange(desc(rqSI)) |>
  mutate(rank_rqSI = row_number())


rqSI_v_qSI_plots <- function(ff,
                             sides = "1s",
                             critv = ifelse(sides == "1s",1.64,1.96),
                             tag = "A"){

  if(sides == "1s"){
    p_95p <- ff |>
      arrange(tstat) |> ### only that orange dots are more prominently...
      #mutate(sig = ifelse(pval_gcm <= 0.05,"p <= 0.05","p > 0.05")) |>
      mutate(sig = ifelse(tstat >= critv,"p <= 0.05","p > 0.05"),
             sig_alpha = ifelse(tstat >= critv,0.6,0.8)) |>
      mutate(bin_sig = as.factor(ifelse(tstat >= critv,1,0))) |>
      ggplot(aes(x = rqSI,y = qSI))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3, color = "black")+
      geom_abline()+
      geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
      scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
      #scale_color_manual(values = c("#009E73","#D55E00","#0072B2","#F0E442"))+
      scale_color_manual(values = c("#38761d","gray50"), labels = unname(latex2exp::TeX(c("$\\leq 0.05$","$ > 0.05$"))))+ # #D55E00
      annotate("text",x = 50,y =-50,label = paste0("R = ",round(cor(ff$qSI,ff$rqSI),3)))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3,aes(color = role.name2))+
      theme_classic()+
      theme(text = element_text(size = 10.5), legend.position="top")+
      labs(color = "p-value",
           size = "precision",
           x = "empricial rqSI",
           y = "empricial qSI",
           tag = tag)
  }else{
    p_95p <- ff |>
      arrange(tstat) |> ### only that orange dots are more prominently...
      #mutate(sig = ifelse(pval_gcm <= 0.05,"p <= 0.05","p > 0.05")) |>
      mutate(sig = ifelse(abs(tstat) >= critv,"p <= 0.05","p > 0.05"),
             sig_alpha = ifelse(tstat >= critv,0.6,0.8)) |>
      mutate(bin_sig = as.factor(ifelse(tstat >= critv,1,0))) |>
      ggplot(aes(x = rqSI,y = qSI))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3, color = "black")+
      geom_abline()+
      geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
      scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
      #scale_color_manual(values = c("#009E73","#D55E00","#0072B2","#F0E442"))+
      scale_color_manual(values = c("#38761d","gray50"), labels = unname(latex2exp::TeX(c("$\\leq 0.05$","$ > 0.05$"))))+ # #D55E00
      annotate("text",x = 50,y =-50,label = paste0("R = ",round(cor(ff$qSI,ff$rqSI),3)))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3,aes(color = role.name2))+
      theme_classic()+
      theme(text = element_text(size = 10.5), legend.position="top")+
      labs(color = "p-value",
           size = "precision",
           x = "empricial rqSI",
           y = "empricial qSI",
           tag = tag)
  }

  return(p_95p)
}

ind_p1 <- rqSI_v_qSI_plots(rqSI_ind)
score_p1 <- rqSI_v_qSI_plots(rqSI_score)


ci_plots_bb <- function(ff,
                     sides = "1s",
                     selected = "LeBron",
                     tag = "B"){
  ci_data <- ff |>
    mutate(up = rqSI+1.96*sds,
           low = rqSI-1.96*sds,
           low1s = rqSI-1.64*sds)

  if(is.null(selected)){
    selinds = NULL
  }else{
    selinds <- grep(selected,ff$name)
  }


  if(sides == "1s"){
    ci_plot <- ci_data |>
      mutate(
             sig = ifelse(tstat >= 1.64,1,0),
             label = paste0(name, " (", rank_qSI,"|",rank_rqSI, ")")) |>
      dplyr::slice(1:15, selinds) |>
      ggplot(aes(y = reorder(label, rqSI))) +
      geom_point(aes(x = rqSI, shape = "rqSI"),size = 2) +
      geom_errorbar(aes(xmin = low1s,xmax = Inf), width = 0.5) +
      geom_point(aes(x = qSI, shape = "qSI"), position = position_nudge(y = 0),size = 2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      labs(x = "empricial qSI and rqSI", y = NULL, color = "Metric", shape = "Metric",tag = tag) +
      #scale_color_manual(values = c("rqSI" = "gray50", "qSI" = "#38761d")) +
      scale_shape_manual(values = c("rqSI" = 16, "qSI" = 4)) +
      theme_light() +
      theme(legend.position = "top")
  }else{
    ci_plot <- ci_data |>
      mutate(
             sig = ifelse(tstat >= 1.96, 1, 0),
             label = paste0(name, " (", rank_qSI,"|",rank_rqSI, ")")) |>
      dplyr::slice(1:15, selinds) |>
      ggplot(aes(y = reorder(label, rqSI))) +
      geom_point(aes(x = rqSI, shape = "rqSI"),size = 2) +
      geom_errorbar(aes(xmin = low, xmax = up), width = 0.5) +
      geom_point(aes(x = qSI, shape = "qSI"), position = position_nudge(y = 0),size = 2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      labs(x = "empricial qSI and rqSI", y = NULL, color = "Metric", shape = "Metric",tag = tag) +
      #scale_color_manual(values = c("rqSI" = "gray50", "qSI" = "#38761d")) +
      scale_shape_manual(values = c("rqSI" = 16, "qSI" = 4)) +
      theme_light() +
      theme(legend.position = "top")
  }
  return(ci_plot)
}

ind_p2 <- ci_plots_bb(rqSI_ind, selected = NULL)
score_p2 <- ci_plots_bb(rqSI_score, selected = NULL)


################################
##### Appendix, rqSI (Figures 8 and 9)
################################

ggarrange(ind_p1, ind_p2, align = "h")
ggsave("plots/rqSI_1s_ind.pdf", height = 6, width = 12, scale = 0.9,device = cairo_pdf)

ggarrange(score_p1, score_p2, align = "h")
ggsave("plots/rqSI_1s_score.pdf", height = 6, width = 12, scale = 0.9,device = cairo_pdf)



################################
##### Appendix, compare both approaches for rqSI (Figure 10)
################################


full_tab <- rqSI_ind |>
  select(pl,pval,tstat,rqSI,qSI,qSI_avg,sds,n,id,name) |>
  left_join(rqSI_score |> select(pl,pval,tstat,rqSI,qSI,qSI_avg,sds),
            by = "pl",suffix = c("_ind","_score"))


cp_p1 <- full_tab |>
  mutate(sig_ind = ifelse(tstat_ind > 1.64,1,0),
         sig_score = ifelse(tstat_score > 1.64,1,0),
         sigs = sig_ind+sig_score,
         sigs_text = paste0(sigs," models"),
         sigs_text = ifelse(sigs == 1,"1 model",sigs_text)) |>
  ggplot(aes(x = rqSI_ind,y = rqSI_score, col = sigs_text,alpha = sigs_text)) +
  geom_point()+
  scale_alpha_manual(values = c(0.3,0.5,1),guide = 'none')+
  scale_color_manual(values = c("gray50","#CC79A7","#38761d"))+
  annotate("text",x = 50,y =-55,label = paste0("R = ",round(cor(full_tab$rqSI_ind,full_tab$rqSI_score),3)))+
  theme_classic()+
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14), legend.position="top")+
  labs(color = "Significant in ",
       x = "empricial rqSI (score indicator)",
       y = "empricial rqSI (score value)",
       tag = "A")

cp_p2 <- full_tab |>
  mutate(sig_ind = ifelse(tstat_ind > 1.64,1,0),
         sig_score = ifelse(tstat_score > 1.64,1,0),
         sigs = sig_ind+sig_score,
         sigs_text = paste0(sigs," models"),
         sigs_text = ifelse(sigs == 1,"1 model",sigs_text)) |>
  ggplot(aes(x = tstat_ind,y = tstat_score, col = sigs_text,alpha = sigs_text)) +
  geom_abline()+
  geom_point()+
  scale_alpha_manual(values = c(0.3,0.5,1),guide = 'none')+
  scale_color_manual(values = c("gray50","#CC79A7","#38761d"))+
  annotate("text",x = 5,y =-3,label = paste0("R = ",round(cor(full_tab$tstat_ind,full_tab$tstat_score),3)))+
  theme_classic()+
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14), legend.position="top")+
  labs(color = "Significant in ",
       x = "emp. GCM test statistic (score indicator)",
       y = "emp. GCM test statistic (score value)",
       tag = "B")

ggarrange(cp_p1,cp_p2, align = "h",common.legend = TRUE)
#ggsave("plots/rqSI_mod_comp.pdf", height = 6, width = 12, scale = 1.2,device = cairo_pdf)
ggsave("plots/rqSI_mod_comp.pdf", height = 4, width = 10, scale = 1.2,device = cairo_pdf)
