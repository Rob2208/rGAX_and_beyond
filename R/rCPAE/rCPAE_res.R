################################################################
################################################################
##### CPAE and rCPAE | results
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

passes <- as.data.frame(readRDS("data/nfl_passing_data.rds")) |>
  mutate(player = as.factor(passer_name)) |>
  select(-passer_name)


create_rCPAE_table <- function(rd){
  fu <- paste0("data/rCPAE/")
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
  rCPAE <- sapply(l1, \(x) sum(x$rY*x$rX))
  CPAE <- sapply(l1,\(x){
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
  rCPAE_tab <- data.frame(pl = gsub("player","",names(pv)),
                         pval = unname(pv),
                         tstat = unname(ts),
                         rCPAE = unname(rCPAE),
                         CPAE = unname(CPAE),
                         CPAE_avg = unname(CPAE)/unname(n),
                         #rEGA = unname(rEGA),
                         sds = unname(sds),
                         n = unname(n)) |>
    arrange(desc(rCPAE))

  return(rCPAE_tab)
}

rCPAE_ind <- create_rCPAE_table("060925") |>
  arrange(desc(CPAE)) |>
  mutate(rank_CPAE = row_number()) |>
  arrange(desc(CPAE_avg)) |>
  mutate(rank_CPAE_avg = row_number()) |>
  arrange(desc(rCPAE)) |>
  mutate(rank_rCPAE = row_number())


rCPAE_v_CPAE_plots <- function(ff,
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
      ggplot(aes(x = rCPAE,y = CPAE))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3, color = "black")+
      geom_abline()+
      geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
      scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
      #scale_color_manual(values = c("#009E73","#D55E00","#0072B2","#F0E442"))+
      scale_color_manual(values = c("#38761d","gray50"), labels = unname(latex2exp::TeX(c("$\\leq 0.05$","$ > 0.05$"))))+ # #D55E00
      annotate("text",x = 30,y =-20,label = paste0("R = ",round(cor(ff$CPAE,ff$rCPAE),3)))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3,aes(color = role.name2))+
      theme_classic()+
      theme(text = element_text(size = 10.5), legend.position="top")+
      labs(color = "p-value",
           size = "precision",
           x = "empricial rCPAE",
           y = "empricial CPAE",
           tag = tag)
  }else{
    p_95p <- ff |>
      arrange(tstat) |> ### only that orange dots are more prominently...
      #mutate(sig = ifelse(pval_gcm <= 0.05,"p <= 0.05","p > 0.05")) |>
      mutate(sig = ifelse(abs(tstat) >= critv,"p <= 0.05","p > 0.05"),
             sig_alpha = ifelse(tstat >= critv,0.6,0.8)) |>
      mutate(bin_sig = as.factor(ifelse(tstat >= critv,1,0))) |>
      ggplot(aes(x = rCPAE,y = CPAE))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3, color = "black")+
      geom_abline()+
      geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
      scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
      #scale_color_manual(values = c("#009E73","#D55E00","#0072B2","#F0E442"))+
      scale_color_manual(values = c("#38761d","gray50"), labels = unname(latex2exp::TeX(c("$\\leq 0.05$","$ > 0.05$"))))+ # #D55E00
      annotate("text",x = 30,y =-20,label = paste0("R = ",round(cor(ff$CPAE,ff$rCPAE),3)))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3,aes(color = role.name2))+
      theme_classic()+
      theme(text = element_text(size = 10.5), legend.position="top")+
      labs(color = "p-value",
           size = "precision",
           x = "emprical rCPAE",
           y = "empirical CPAE",
           tag = tag)
  }

  return(p_95p)
}

p1 <- rCPAE_v_CPAE_plots(rCPAE_ind)

ci_plots_nfl <- function(ff,
                        sides = "1s",
                        selected = "LeBron",
                        tag = "B"){
  ci_data <- ff |>
    mutate(up = rCPAE+1.96*sds,
           low = rCPAE-1.96*sds,
           low1s = rCPAE-1.64*sds)

  if(is.null(selected)){
    selinds = NULL
  }else{
    selinds <- grep(selected,ff$pl)
  }


  if(sides == "1s"){
    ci_plot <- ci_data |>
      mutate(
        sig = ifelse(tstat >= 1.64,1,0),
        label = paste0(pl, " (", rank_CPAE,"|",rank_rCPAE, ")")) |>
      dplyr::slice(1:15, selinds) |>
      ggplot(aes(y = reorder(label, rCPAE))) +
      geom_point(aes(x = rCPAE, shape = "rCPAE"),size = 2) +
      geom_errorbar(aes(xmin = low1s,xmax = Inf), width = 0.5) +
      geom_point(aes(x = CPAE, shape = "CPAE"), position = position_nudge(y = 0),size = 2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      labs(x = "emprical CPAE and rCPAE", y = NULL, color = "Metric", shape = "Metric",tag = tag) +
      #scale_color_manual(values = c("rCPAE" = "gray50", "CPAE" = "#38761d")) +
      scale_shape_manual(values = c("rCPAE" = 16, "CPAE" = 4)) +
      theme_light() +
      theme(legend.position = "top")
  }else{
    ci_plot <- ci_data |>
      mutate(
        sig = ifelse(tstat >= 1.96, 1, 0),
        label = paste0(pl, " (", rank_CPAE,"|",rank_rCPAE, ")")) |>
      dplyr::slice(1:15, selinds) |>
      ggplot(aes(y = reorder(label, rCPAE))) +
      geom_point(aes(x = rCPAE, shape = "rCPAE"),size = 2) +
      geom_errorbar(aes(xmin = low, xmax = up), width = 0.5) +
      geom_point(aes(x = CPAE, shape = "CPAE"), position = position_nudge(y = 0),size = 2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      labs(x = "emprical CPAE and rCPAE", y = NULL, color = "Metric", shape = "Metric",tag = tag) +
      #scale_color_manual(values = c("rCPAE" = "gray50", "CPAE" = "#38761d")) +
      scale_shape_manual(values = c("rCPAE" = 16, "CPAE" = 4)) +
      theme_light() +
      theme(legend.position = "top")
  }
  return(ci_plot)
}

p2 <- ci_plots_nfl(rCPAE_ind, selected = NULL)

ggarrange(p1, p2, align = "h")
ggsave("plots/rCPAE_1s.pdf", height = 6, width = 12, scale = 0.9,device = cairo_pdf)
