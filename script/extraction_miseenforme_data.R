#library

library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(coin)


#read data

dffatsub <-
  read.csv("C:/Users/maxch/Git/FATIGUE_SUBJECTIVE/data/subjective_fatigue_answers.csv")

dffatsub <- dffatsub[,-c(1,4)]
dffatsub <- filter(dffatsub, Nom != "Allie")
dffatsub <- filter(dffatsub, Nom != "Lopez")
dffatsub <- filter(dffatsub, Nom != "Pelissier")

dffatsub$Quel.est.ton.niveau.de.fatigue.générale.. <- as.numeric(dffatsub$Quel.est.ton.niveau.de.fatigue.générale..)

dffatsubpost <- dffatsub %>% filter(Timing.de.test == "POST TEST")
dffatsubpost <- filter(dffatsubpost, Nom != "Allie")
dffatsubpost <- filter(dffatsubpost, Nom != "Rieger")


dffatsub$Choisi.la.phrase.qui.correspond.plus.à.tes.sensations.de.douleurs.des.dernières.24h <-
  as.numeric(dffatsub$Choisi.la.phrase.qui.correspond.plus.à.tes.sensations.de.douleurs.des.dernières.24h)

dffatsubpost %>% identify_outliers(Ressenti.post.exercice)

#test normalité

dfPREJ1 <- dffatsub %>% filter(Timing.de.test =="PRE TEST J1")
dfPREJ2 <- dffatsub %>% filter(Timing.de.test == "PRE TEST J2")
dfPOSTtest <- dffatsub %>% filter(Timing.de.test == "POST TEST")
dfpost48 <- dffatsub %>% filter(Timing.de.test == "POST 48H")

dfPREJ1 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.quadriceps..)
dfPREJ2 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.quadriceps..)
dfPOSTtest %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.quadriceps..)
dfpost48 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.quadriceps..)

dfPREJ1 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.mollets..)
dfPREJ2 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.mollets..)
dfPOSTtest %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.mollets..)
dfpost48 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(A.quel.niveau.ce.situe.ta.douleur.aux.mollets..)

dfPREJ1 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(Quel.est.ton.niveau.de.fatigue.générale..)
dfPREJ2 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(Quel.est.ton.niveau.de.fatigue.générale..)
dfPOSTtest %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(Quel.est.ton.niveau.de.fatigue.générale..)
dfpost48 %>% group_by(Quelle.est.la.condition.du.passage..) %>% shapiro_test(Quel.est.ton.niveau.de.fatigue.générale..)

## analyse quadriceps

dffatsub$A.quel.niveau.ce.situe.ta.douleur.aux.mollets.. <- as.numeric(dffatsub$A.quel.niveau.ce.situe.ta.douleur.aux.mollets..)

ttesttime1 <- dffatsub %>%
  group_by(Quelle.est.la.condition.du.passage..) %>%
  rstatix::wilcox_test(
    A.quel.niveau.ce.situe.ta.douleur.aux.quadriceps.. ~ Timing.de.test, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime1

ttesttime <- dffatsub %>%
  group_by(Timing.de.test) %>%
  rstatix::wilcox_test(
    A.quel.niveau.ce.situe.ta.douleur.aux.quadriceps.. ~ Quelle.est.la.condition.du.passage.., paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

ttesttime1 <- ttesttime1 %>%  add_xy_position(x= "Quelle.est.la.condition.du.passage..")

# ttesttime <- dffatsubpost %>%
#   # group_by(Conditon) %>%
#   pairwise_t_test(
#     Choisi.la.phrase.qui.correspond.plus.à.tes.sensations.de.douleurs.des.dernières.24h ~ Quelle.est.la.condition.du.passage..,
#     paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# ttesttime1
#
# dffatsub %>% ggplot(aes(x = Timing.de.test , y = Choisi.la.phrase.qui.correspond.plus.à.tes.sensations.de.douleurs.des.dernières.24h)) +
#   geom_jitter(aes(shape = Quelle.est.la.condition.du.passage..) , position = position_dodge2(width = 0.75,
#                                                                                             preserve = "single"))

ttesttime2 <- ttesttime1[ttesttime1$Quelle.est.la.condition.du.passage.. == "patch",] %>% add_xy_position(x = "Timing.de.test")
ttesttime3 <- ttesttime1[ttesttime1$Quelle.est.la.condition.du.passage.. == "placebo",] %>% add_xy_position(x = "Timing.de.test")

ttesttime2$xmin <- c(2.82, 0.8, 1, 0.8 , 1.8, 2.82)
ttesttime3$xmin <- c(1, 1.2, 1, 1.2, 2.2, 1)
ttesttime2$xmax <- c(3.8, 2.8, 1, 2.8 , 2.8, 3.8)
ttesttime3$xmax <- c(1, 3.2, 1, 3.2, 3.2, 1)

#plot quadriceps

dffatsub$Timing.de.test <-
  factor(
    dffatsub$Timing.de.test ,
    levels = c("PRE TEST J1" ,"PRE TEST J2" , "POST TEST" , "POST 48H"))

plot <- dffatsub %>%  ggplot(aes(x = Timing.de.test , y = A.quel.niveau.ce.situe.ta.douleur.aux.quadriceps.. )) +
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
  geom_boxplot(aes(fill = Quelle.est.la.condition.du.passage..), alpha = 0.4) +
  #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
  stat_summary(aes(group = Quelle.est.la.condition.du.passage.. , fill= Quelle.est.la.condition.du.passage..),
               fun.y = mean ,
               shape = 23 ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")
  ) +
  stat_pvalue_manual(
    ttesttime2 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(9, 6.25, 8))+
  stat_pvalue_manual(
    ttesttime3 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(-0.5, -1.5)) +
  labs(x = "Timing" , y = "Niveau de douleur au quadriceps" , fill = "Condition")

plot

##analyse mollet

ttesttime4 <- dffatsub %>%
  group_by(Quelle.est.la.condition.du.passage..) %>%
  rstatix::wilcox_test(
    A.quel.niveau.ce.situe.ta.douleur.aux.mollets.. ~ Timing.de.test, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime4

ttesttime5 <- dffatsub %>%
  group_by(Timing.de.test) %>%
  rstatix::wilcox_test(
    A.quel.niveau.ce.situe.ta.douleur.aux.mollets.. ~ Quelle.est.la.condition.du.passage.., paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime5


ttesttime6 <- ttesttime4[ttesttime1$Quelle.est.la.condition.du.passage.. == "patch",] %>% add_xy_position(x = "Timing.de.test")
ttesttime7 <- ttesttime4[ttesttime1$Quelle.est.la.condition.du.passage.. == "placebo",] %>% add_xy_position(x = "Timing.de.test")

ttesttime6$xmin <- c(0.8, 0.8, 1, 1.8 , 1, 2.82)
ttesttime7$xmin <- c(1, 1.2, 1.2, 2.2, 2.2, 1)
ttesttime6$xmax <- c(1, 2.8, 1, 2.8 , 1, 3.8)
ttesttime7$xmax <- c(1, 3.2, 4.2, 3.2, 4.2, 1)

#plot mollet

plot <- dffatsub %>%  ggplot(aes(x = Timing.de.test , y = A.quel.niveau.ce.situe.ta.douleur.aux.mollets..)) +
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
  geom_boxplot(aes(fill = Quelle.est.la.condition.du.passage..), alpha = 0.4) +
  #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
  stat_summary(aes(group = Quelle.est.la.condition.du.passage.. , fill= Quelle.est.la.condition.du.passage..),
               fun.y = mean ,
               shape = 23 ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")
  ) +
  stat_pvalue_manual(
    ttesttime6 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(7.5, 2.4, 9.2))+
  stat_pvalue_manual(
    ttesttime7 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(-1.2, -1.9, -0.6, -2.5)) +
  labs(x = "Timing" , y = "Niveau de douleur aux mollets" , fill = "Condition")

plot

##analyse fatique générale

ttesttime8 <- dffatsub %>%
  group_by(Quelle.est.la.condition.du.passage..) %>%
  rstatix::wilcox_test(
    Quel.est.ton.niveau.de.fatigue.générale.. ~ Timing.de.test, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime8

ttesttime9 <- dffatsub %>%
  group_by(Timing.de.test) %>%
  rstatix::wilcox_test(
    Quel.est.ton.niveau.de.fatigue.générale.. ~ Quelle.est.la.condition.du.passage.., paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime9


ttesttime10 <- ttesttime8[ttesttime1$Quelle.est.la.condition.du.passage.. == "patch",] %>% add_xy_position(x = "Timing.de.test")
ttesttime11 <- ttesttime8[ttesttime1$Quelle.est.la.condition.du.passage.. == "placebo",] %>% add_xy_position(x = "Timing.de.test")

ttesttime10$xmin <- c(0.8, 0.8, 1, 1.8 , 1, 2.82)
ttesttime11$xmin <- c(1, 1.2, 1.2, 2.2, 2.2, 3.2)
ttesttime10$xmax <- c(1, 2.8, 1, 2.8 , 1, 3.8)
ttesttime11$xmax <- c(1, 3.2, 4.2, 3.2, 4.2, 4.2)

ttesttime9 <- ttesttime9 %>%  add_xy_position(x = "Quelle.est.la.condition.du.passage..")
ttesttime9$xmax <- c(1, 1, 1 , 4)
ttesttime9$xmin <- c(1,1, 1, 4)

#plot fatigue générale

plot <- dffatsub %>%  ggplot(aes(x = Timing.de.test , y = Quel.est.ton.niveau.de.fatigue.générale..)) +
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
  geom_boxplot(aes(fill = Quelle.est.la.condition.du.passage..), alpha = 0.4) +
  #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
  stat_summary(aes(group = Quelle.est.la.condition.du.passage.. , fill= Quelle.est.la.condition.du.passage..),
               fun.y = mean ,
               shape = 23 ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")
  ) +
  stat_pvalue_manual(
    ttesttime10 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(9, 4.6, 9.2))+
  stat_pvalue_manual(
    ttesttime11 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(0.9, 1.9, -0.6)) +
  stat_pvalue_manual(
    ttesttime9 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p ={p}" , color = "black" , y.position = 5) +
  labs(x = "Timing" , y = "Fatigue générale" , fill = "Condition")

plot


# comparaison de pourcentage POST - POST48H

postplacebo <-
  dffatsub %>%  filter(Quelle.est.la.condition.du.passage.. == "placebo") %>% filter(Timing.de.test == "POST TEST") %>% select(Quel.est.ton.niveau.de.fatigue.générale..)

postpatch <-
  dffatsub %>%  filter(Quelle.est.la.condition.du.passage.. == "patch") %>% filter(Timing.de.test == "POST TEST") %>% select(Quel.est.ton.niveau.de.fatigue.générale..)

post48placebo <-
  dffatsub %>%  filter(Quelle.est.la.condition.du.passage.. == "placebo") %>% filter(Timing.de.test == "POST 48H") %>% select(Quel.est.ton.niveau.de.fatigue.générale..)

post48patch <-
  dffatsub %>%  filter(Quelle.est.la.condition.du.passage.. == "patch") %>% filter(Timing.de.test == "POST 48H") %>% select(Quel.est.ton.niveau.de.fatigue.générale..)

dfpostpost48 <- cbind(postplacebo, postpatch, post48placebo, post48patch)
colnames(dfpostpost48) <- c("RPE_POST_PB", "RPE_POST_P" , "RPE_POST48_PB", "RPE_POST48_P")

dfpostpost48pb <- mutate(dfpostpost48 , POST_POST48_PB = (RPE_POST_PB - RPE_POST48_PB)/RPE_POST_PB*100) %>% select(POST_POST48_PB)
dfpostpost48p <- mutate(dfpostpost48 , POST_POST48_P = (RPE_POST_P - RPE_POST48_P)/RPE_POST_P*100) %>% select(POST_POST48_P)

placebo <- "placebo"
patch <- "patch"

dfpostpost48pb <- cbind(dfpostpost48pb, placebo)
dfpostpost48p <- cbind(dfpostpost48p, patch)

colnames(dfpostpost48p) <- c("RPE_POST_POST48" , "condition")
colnames(dfpostpost48pb) <- c("RPE_POST_POST48" , "condition")

dfpostpost48 <- rbind(dfpostpost48p , dfpostpost48pb)

dfpostpost48 %>%  group_by(condition) %>% shapiro_test(RPE_POST_POST48)

wilpourc <- dfpostpost48 %>% rstatix::wilcox_test(RPE_POST_POST48 ~ condition , paired = T)

# plot différence PRE POST pourcentage + pvalue

dfpostpost48 %>% ggplot(aes(x = condition , y = RPE_POST_POST48)) +
  geom_boxplot(aes(color = condition)) +
  geom_point(aes(shape = condition , color = condition) , size = 1) +
  ylab("Récupération Fatigue Générale POST-POST48H (%)") +
  stat_summary(aes(label = round(..y.., 2)),
               geom = "text",
               fun.y = mean,
               size = 4) +
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
  stat_compare_means(paired = TRUE , label.x = 0.5)
