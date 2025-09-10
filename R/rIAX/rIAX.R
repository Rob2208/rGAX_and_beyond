### Time to first injury: Liverpool data
### LK 2025

set.seed(42)

### Dependencies
library("comets")
library("survival")
library("tidyverse")
library("ggpubr")

if (!file.exists("data.rds")) {
  source("./prepare-data.R")
}

### Load data
dd <- readRDS("data.rds")

dat <- dd |>
  mutate(surv = Surv(tstop_day, status)) |>
  separate(position, into = c("position", "details"), sep = "_") |>
  mutate(position = factor(position)) |>
  select(surv, player, season, age, height, position, yellows, reds)

### For feature-wise tests
features <- c("age", "height", "yellows", "position", "reds")
rYZ <- "survforest"
rXZ <- "rf"

### Players
players <- sort(unique(as.character(dat$player)))

### Test each player
pb <- txtProgressBar(min = 0, max = length(players), width = 60, style = 3)
tsts <- lapply(seq_along(players), \(idx) {
  setTxtProgressBar(pb, idx)
  dat$pl <- as.numeric(dat$player == players[idx])
  fm <- surv ~ pl | age + height + yellows + position + season
  comets(fm, data = dat, reg_YonZ = rYZ, reg_XonZ = rXZ, coin = FALSE)
})
names(tsts) <- players

### Summarize results
res <- lapply(seq_along(players), \(idx) {
  RP <- tsts[[idx]]$rY * tsts[[idx]]$rX
  n <- NROW(RP)
  reax <- mean(RP)
  eax <- mean(tsts[[idx]]$rY * as.numeric(dat$player == players[idx]))
  sdx <- sqrt(mean(RP^2) - mean(RP)^2)
  data.frame(
    player = players[idx],
    p.value = tsts[[idx]]$p.value,
    est = reax,
    lower = reax - qnorm(0.975) * sdx / sqrt(n),
    upper = reax + qnorm(0.975) * sdx / sqrt(n),
    eax = eax,
    scale = 1 / sdx
  )
}) |> do.call("rbind", args = _)

res

### Plot player GCMs
p1 <- ggplot(res, aes(y = fct_reorder(player, est), x = est, xmin = lower, xmax = upper, shape = "rIAX")) +
  geom_errorbarh(height = 0.5) +
  geom_point() +
  geom_point(aes(x = eax, shape = "IAX"), color = "gray50") +
  theme_bw() +
  geom_vline(xintercept = 0, col = "darkred", linetype = 2) +
  labs(x = "empirical IAX and rIAX", y = element_blank(), shape = "Metric") +
  theme(legend.position = "top") +
  scale_shape_manual(values = c("IAX" = 4, "rIAX" = 16)) +
  theme(text = element_text(size = 13.5))

### Plot IAX vs rIAX
p2 <- ggplot(res, aes(x = est, y = eax, size = scale, color = p.value <= 0.05)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "empirical rIAX", y = "empirical IAX", color = "p-value") +
  theme(legend.position = "top") +
  scale_color_manual(
    values = c("FALSE" = "gray50", "TRUE" = "darkgreen"),
    labels = c("FALSE" = latex2exp::TeX("$> 0.05$"), "TRUE" = latex2exp::TeX("$\\leq 0.05$"))
  ) +
  theme(text = element_text(size = 13.5)) +
  scale_size_continuous(breaks = c(20, 40, 60))

### Test each feature
pb <- txtProgressBar(min = 0, max = length(features), width = 60, style = 3)
ftsts <- lapply(seq_along(features), \(idx) {
  setTxtProgressBar(pb, idx)
  to_x <- features[idx]
  to_z <- c(features[-idx], "position", "season")
  fm <- as.formula(paste0("surv ~", to_x, "|", paste0(to_z, collapse = "+")))
  comets(fm, data = dat, reg_YonZ = rYZ, reg_XonZ = rXZ, coin = FALSE)
})
names(ftsts) <- features

### Summarize results
fres <- lapply(seq_along(features), \(idx) {
  RP <- ftsts[[idx]]$rY * ftsts[[idx]]$rX
  apply(cbind(RP), 2, \(col) {
    n <- NROW(col)
    reax <- mean(col)
    sdx <- sqrt(mean(col^2) - mean(col)^2)
    data.frame(
      feature = features[idx],
      p.value = ftsts[[idx]]$p.value,
      est = reax,
      lower = reax - qnorm(0.975) * sdx / sqrt(n),
      upper = reax + qnorm(0.975) * sdx / sqrt(n)
    )
  }, simplify = FALSE) |> do.call("rbind", args = _)
}) |>
  do.call("rbind", args = _) |>
  rownames_to_column() |>
  mutate(feature = rowname)

fres

### Plot
p3 <- ggplot(fres, aes(y = fct_reorder(feature, est), x = est, xmin = lower, xmax = upper)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  theme_bw() +
  geom_vline(xintercept = 0, col = "darkred", linetype = 2) +
  labs(x = "empirical non-player rIAX ", y = element_blank()) +
  theme(text = element_text(size = 13.5))

ggarrange(p2 + labs(tag = "A"), p1 + labs(tag = "B"), p3 + labs(tag = "C"),
  nrow = 1, ncol = 3,
  widths = c(0.4, 0.4, 0.3),
  align = "h"
)
ggsave(paste0(rYZ, "-reax.pdf"), height = 5, width = 14)
