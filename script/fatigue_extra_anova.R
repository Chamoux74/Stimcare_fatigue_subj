library(tibble)
library(purrr)
library(dplyr)
library(readr)
library(readxl)
library(rstatix)
library(ggsignif)
library(ggpubr)

dffatigue <- read_xlsx("C:/Users/maxch/Git/FATIGUE_SUBJECTIVE/data/fatigue.xlsx")

dffatigue$Instant <- factor(dffatigue$Instant, levels = c("Pre", "Post", "Post 48"))

res.aov1 <- rstatix::anova_test(
  data = dffatigue, dv = Fatigue, wid = Sujet ,
  within = c(Conditon, Instant) , effect.size = "ges",
  detailed = TRUE,
)

anova2 <- get_anova_table(res.aov1 , correction = "auto")

one.way <- dffatigue %>%
  group_by(Instant) %>%
  anova_test(dv = Fatigue, wid = Sujet, within = Conditon) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- dffatigue %>%
  group_by(Conditon) %>%
  anova_test(dv = Fatigue, wid = Sujet, within = Instant) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

ttesttime <- dffatigue %>%
  group_by(Conditon) %>%
  pairwise_t_test(
    Fatigue ~ Instant, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

ttesttime1 <- ttesttime[ttesttime$Conditon == "Patch",] %>% add_xy_position(x = "Instant")
ttesttime2 <- ttesttime[ttesttime$Conditon == "Placebo",] %>% add_xy_position(x = "Instant")

ttesttime1$xmin <- c(0.8 , 2 , 1.8)
ttesttime1$xmax <- c(1.8, 1 , 2.8 )

ttesttime2$xmin <- c(1.2 , 1.2 , 2.3)
ttesttime2$xmax <- c(2.2 , 3.2 , 3.2 )


plot <- dffatigue %>%  ggplot(aes(x = Instant , y = Fatigue )) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_boxplot(aes(fill = Conditon , color = Conditon), alpha = 0.4) +
  #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
  stat_summary(aes(group = Conditon),
               fun.y = mean ,
               shape = 23  ,
               fill = "black" ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")
  ) +
  stat_pvalue_manual(
    ttesttime1 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(0.7 , 1.5)
  ) +
  stat_pvalue_manual(
    ttesttime2 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(9.2 , 9.8 , 8.9)
  )

plot
