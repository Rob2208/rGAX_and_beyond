######################################################
######################################################
#### rGAX and GAX res
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


### load relevant xG model
xg_mod <- readRDS("data/models/xgb_xg_mod.rds")

shots <- readRDS("data/shot_data_rel_1516_div_new.rds")

create_shots1 <- function(shots){
  shot_mm_pl <- model.matrix(~0+player_name_fac,data = shots)
  shot_mm_GK <- model.matrix(~0+player_name_GK_fac,data = shots)

  shots_pre <- bind_cols(shots |> dplyr::select(-player_name_fac,-player_name_GK_fac,-team.name,-opp.team.name),
                         shot_mm_GK[,-1],
                         shot_mm_pl)

  lsf_col <- min(grep("player",names(shots_pre)))-1 ## last shot-specific feature column

  el_players <- which(colSums(shots_pre[,-c(1:lsf_col)]) < 70)
  shots_el <- shots_pre[,-(lsf_col+el_players)]

  # eliminate players with no goals in data
  el_ngp <- which(colSums(shots_el[shots_el$shot_y == 0,-(1:lsf_col)]) == colSums(shots_el[,-(1:lsf_col)]))

  if(length(el_ngp)==0){
    shots1 <- shots_el
  }else{
    shots1 <- shots_el[,-(lsf_col+el_ngp)]
  }

  shots1 <- shots1 |> select(-Att)

  return(shots1)
}

dta <- create_shots1(shots)

Y <- dta$shot_y
X <- dta |> select(-shot_y)

du <- cbind(Y,X[,-c(grep("player_name_fac",colnames(X)),
                    grep("player_name_GK_fac",colnames(X)))])

fm <- as.formula(paste0("Y ~ ."))
#Y <- stats::model.response(stats::model.frame(fm, du))
Z <- comets:::.rm_int(stats::model.matrix(fm, du, rhs = 1))
#Y <- comets:::.check_data(Y,"Y")
Z <- comets:::.check_data(Z,"Z")

#### xG computation

xG_tab <- shots |>
  mutate(xG = predict(xg_mod,data = as.matrix(Z)),
         GAX = shot_y-xG)

GAX_tab <- xG_tab |>
  group_by(player_name_fac) |>
  summarise(xG = sum(xG),GAX = sum(GAX),
            goals = sum(shot_y), n= n()) |>
  filter(n >= 10) |>
  arrange(desc(GAX)) #|>
#arrange(pval)

############################
#### load gcm results
############################


create_rGAX_table <- function(rd,
                              name_addon = c("tuned_rf",
                                             "tuned_xgb",
                                             "rf")){
  fu <- paste0("data/rGAX/")
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
  rGAX <- sapply(l1, \(x) sum(x$rY*x$rX))
  sds <- sapply(l1, \(x) {
    tst <- independence_test(x$rY ~ x$rX, teststat = "scalar")
    sqrt(variance(tst))
  })
  theta <- rGAX/sapply(l1,function(x) var(x$rX) * NROW(x$rX))
  #hist(theta, breaks = 30)
  #plot(rGAX,theta,col = 1+(pv <= 0.05))
  rGAX_tab_wgt <- data.frame(pl = gsub("player_name_fac","",names(pv)),
                             pval = unname(pv),
                             tstat = unname(ts),
                             rGAX = unname(rGAX),
                             #rGAX = unname(rGAX),
                             sds = unname(sds),
                             theta = unname(theta)) |>
    arrange(desc(rGAX))

  return(rGAX_tab_wgt)
}

# "110825" ... rf
rGAX_rf <- create_rGAX_table("110825","rf")
full_rf <- rGAX_rf |>
  left_join(GAX_tab |> mutate(pl = as.character(player_name_fac)),by = "pl",suffix =c("_gcm","_glm")) |>
  select(pl,GAX,rGAX,tstat,pval,n,goals,xG,sds) |>
  mutate(rank_rGAX = row_number()) |>
  arrange(desc(GAX)) |>
  mutate(rank_GAX = row_number()) |>
  arrange(desc(rGAX))
# "130825" ... tuned rf
rGAX_tuned_rf <- create_rGAX_table("130825","tuned_rf")
full_trf <- rGAX_tuned_rf |>
  left_join(GAX_tab |> mutate(pl = as.character(player_name_fac)),by = "pl",suffix =c("_gcm","_glm")) |>
  select(pl,GAX,rGAX,tstat,pval,n,goals,xG,sds) |>
  mutate(rank_rGAX = row_number()) |>
  arrange(desc(GAX)) |>
  mutate(rank_GAX = row_number()) |>
  arrange(desc(rGAX))
# "110625" ... tuned xgb
rGAX_tuned_xgb <- create_rGAX_table("110625","tuned_xgb")
full_txgb <- rGAX_tuned_xgb |>
  left_join(GAX_tab |> mutate(pl = as.character(player_name_fac)),by = "pl",suffix =c("_gcm","_glm")) |>
  select(pl,GAX,rGAX,tstat,pval,n,goals,xG,sds) |>
  mutate(rank_rGAX = row_number()) |>
  arrange(desc(GAX)) |>
  mutate(rank_GAX = row_number()) |>
  arrange(desc(rGAX))


#### combine all

rGAX_all <- rGAX_rf |>
  left_join(rGAX_tuned_rf,by="pl",suffix = c("_rf","_trf")) |>
  left_join(rGAX_tuned_xgb,by = "pl") |>
  mutate(sigs = (tstat_rf > 1.64)+(tstat_trf > 1.64)+(tstat > 1.64))


################################
##### plots
################################


n_used_f <- function(ff){
  sapply(ff$pl,\(x){
    known_last_names <- c("Messi", "Ronaldo", "Neymar", "Salah","Fàbregas", "Williams", "Sánchez",
                          "Suárez", "Modrić", "Firmino", "Dybala", "Benzema", "Griezmann", "Hernández",
                          "Guerreiro","Gomes","Parejo","Ayew","Kalou")
    sl <- str_split(x," ")[[1]]
    match <- intersect(sl, known_last_names)
    if(length(sl) > 2){
      if(length(match) > 0){
        sout <- paste(sl[1], match[1])
      }else{
        sout <- paste0(sl[1]," ",sl[3])
      }
      if(grepl("Di María",x)){
        sout <- "Ángel Di María"
      }
      if(grepl("Ben Arfa",x)){
        sout <- "Hatem Ben Arfa"
      }
      if(grepl("Willian José",x)){
        sout <- "Willian José"
      }
      if(grepl("Neymar",x)){
        sout <- "Neymar Jr."
      }
    }else{
      sout <- paste0(sl,collapse = " ")
    }
    return(sout)
  })
}

ci_plots <- function(ff,
                     sides = "1s",
                     selected = "Messi|Ronaldo|Benzema|Bale|Lewandowski|Salah",
                     tag = "B"){
  ci_data <- ff |>
    mutate(up = rGAX+1.96*sds,
           low = rGAX-1.96*sds,
           low1s = rGAX-1.64*sds)

  if(is.null(selected)){
    selinds = NULL
  }else{
    selinds <- grep(selected,ff$pl)
  }

  n_used = n_used_f(ff)

  if(sides == "1s"){
    ci_plot <- ci_data |>
      mutate(n_used = n_used,
             sig = ifelse(tstat >= 1.64,1,0),
             label = paste0(n_used, " (", rank_GAX,"|",rank_rGAX, ")")) |>
      dplyr::slice(1:10, selinds) |>
      ggplot(aes(y = reorder(label, rGAX))) +
      geom_point(aes(x = rGAX, shape = "rGAX"),size = 2) +
      geom_errorbar(aes(xmin = low1s,xmax = Inf), width = 0.5) +
      geom_point(aes(x = GAX, shape = "GAX"), position = position_nudge(y = 0),size = 2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      labs(x = "empricial GAX and rGAX", y = NULL, color = "Metric", shape = "Metric",tag = tag) +
      #scale_color_manual(values = c("rGAX" = "gray50", "GAX" = "#38761d")) +
      scale_shape_manual(values = c("rGAX" = 16, "GAX" = 4)) +
      theme_light() +
      theme(legend.position = "top")
  }else{
    ci_plot <- ci_data |>
      mutate(n_used = n_used,
             sig = ifelse(tstat >= 1.96, 1, 0),
             label = paste0(n_used, " (", rank_GAX,"|",rank_rGAX, ")")) |>
      dplyr::slice(1:10, selinds) |>
      ggplot(aes(y = reorder(label, rGAX))) +
      geom_point(aes(x = rGAX, shape = "rGAX"),size = 2) +
      geom_errorbar(aes(xmin = low, xmax = up), width = 0.5) +
      geom_point(aes(x = GAX, shape = "GAX"), position = position_nudge(y = 0),size = 2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      labs(x = "empricial GAX and rGAX", y = NULL, color = "Metric", shape = "Metric",tag = tag) +
      #scale_color_manual(values = c("rGAX" = "gray50", "GAX" = "#38761d")) +
      scale_shape_manual(values = c("rGAX" = 16, "GAX" = 4)) +
      theme_light() +
      theme(legend.position = "top")
  }
  return(ci_plot)
}

rGAX_v_GAX_plots <- function(ff,
                             sides = "1s",
                             critv = ifelse(sides == "1s",1.64,1.96),
                             tag = "A"){

  if(sides == "1s"){
    p_95p <- ff |>
      arrange(tstat) |>
      #mutate(sig = ifelse(pval_gcm <= 0.05,"p <= 0.05","p > 0.05")) |>
      mutate(sig = ifelse(tstat >= critv,"p <= 0.05","p > 0.05"),
             sig_alpha = ifelse(tstat >= critv,0.6,0.8)) |>
      mutate(bin_sig = as.factor(ifelse(tstat >= critv,1,0))) |>
      ggplot(aes(x = rGAX,y = GAX))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3, color = "black")+
      geom_abline()+
      geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
      scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
      #scale_color_manual(values = c("#009E73","#D55E00","#0072B2","#F0E442"))+
      scale_color_manual(values = c("#38761d","gray50"), labels = unname(latex2exp::TeX(c("$\\leq 0.05$","$ > 0.05$"))))+ # #D55E00
      annotate("text",x = 4,y =-7,label = paste0("R = ",round(cor(ff$GAX,ff$rGAX),3)))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3,aes(color = role.name2))+
      theme_classic()+
      theme(text = element_text(size = 10.5), legend.position="top")+
      labs(color = "p-value",
           size = "precision",
           x = "empricial rGAX",
           y = "empricial GAX",
           tag = tag)
  }else{
    p_95p <- ff |>
      arrange(tstat) |>
      #mutate(sig = ifelse(pval_gcm <= 0.05,"p <= 0.05","p > 0.05")) |>
      mutate(sig = ifelse(abs(tstat) >= critv,"p <= 0.05","p > 0.05"),
             sig_alpha = ifelse(tstat >= critv,0.6,0.8)) |>
      mutate(bin_sig = as.factor(ifelse(tstat >= critv,1,0))) |>
      ggplot(aes(x = rGAX,y = GAX))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3, color = "black")+
      geom_abline()+
      geom_point(aes(color = sig, size = 1/sds^2, alpha = bin_sig)) +
      scale_alpha_manual(values = c(0.5, 0.9),guide = 'none')+
      #scale_color_manual(values = c("#009E73","#D55E00","#0072B2","#F0E442"))+
      scale_color_manual(values = c("#38761d","gray50"), labels = unname(latex2exp::TeX(c("$\\leq 0.05$","$ > 0.05$"))))+ # #D55E00
      annotate("text",x = 4,y =-7,label = paste0("R = ",round(cor(ff$GAX,ff$rGAX),3)))+
      #geom_smooth(method = "lm",se= FALSE,size = 0.3,aes(color = role.name2))+
      theme_classic()+
      theme(text = element_text(size = 10.5), legend.position="top")+
      labs(color = "p-value",
           size = "precision",
           x = "empricial rGAX",
           y = "empricial GAX",
           tag = tag)
  }

  return(p_95p)
}


################################
##### main plot (Figure 2)
################################

pm1 <- rGAX_v_GAX_plots(full_trf,tag = "A")
pm2 <- ci_plots(full_trf,tag = "B")

ggarrange(pm1,pm2, align = "h")
ggsave("plots/rGAX_1s_tuned_rf.pdf", height = 6, width = 12, scale = 0.9,device = cairo_pdf)


################################
##### Appendix, model comp (Figure 6)
################################

p1 <- rGAX_v_GAX_plots(full_rf)
p2 <- ci_plots(full_rf)
p3 <- rGAX_v_GAX_plots(full_txgb,tag = "C")
p4 <- ci_plots(full_txgb,tag = "D")


ggarrange(p1,p2,p3,p4,nrow = 2,ncol = 2)
ggsave("plots/rGAX_over_underfit_xreg.pdf", height = 10, width = 12, scale = 0.9,device = cairo_pdf)


################################
##### Appendix, doubly robustness (Figure 7)
################################

p11 <- rGAX_all |>
  arrange(sigs) |>
  mutate(sigs_text = paste0(sigs," models"),
         sigs_text = ifelse(sigs == 1,"1 model",sigs_text)) |>
  ggplot(aes(x= rGAX_trf,y = rGAX_rf,col = sigs_text,alpha = sigs_text))+
  geom_point()+
  scale_alpha_manual(values = c(0.3,0.5,0.7,1),guide = 'none')+
  scale_color_manual(values = c("gray50","#CC79A7","#E69F00","#38761d"))+
  theme_classic()+
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 13.5), legend.position="top")+
  labs(color = "Significant in ",
       x = "empricial rGAX (tuned rf)",
       y = "empricial rGAX (untuned rf)",
       tag = "A")

p21 <- rGAX_all |>
  arrange(sigs) |>
  mutate(sigs_text = paste0(sigs," models"),
         sigs_text = ifelse(sigs == 1,"1 model",sigs_text)) |>
  ggplot(aes(x= rGAX_trf,y = rGAX,col = sigs_text,alpha = sigs_text))+
  geom_point()+
  scale_alpha_manual(values = c(0.3,0.5,0.7,1),guide = 'none')+
  scale_color_manual(values = c("gray50","#CC79A7","#E69F00","#38761d"))+
  theme_classic()+
    theme(text = element_text(size = 14),
          axis.text = element_text(size = 13.5), legend.position="top")+
  labs(color = "",
       x = "empricial rGAX (tuned rf)",
       y = "empricial rGAX (tuned xgb)",
       tag = "B")

ggarrange(p11,p21, align = "h",common.legend = TRUE)
#ggsave("plots/rGAX_mod_comp.pdf", height = 6, width = 12, scale = 1.2,device = cairo_pdf)
ggsave("plots/rGAX_mod_comp.pdf", height = 4, width = 10, scale = 1.2,device = cairo_pdf)
