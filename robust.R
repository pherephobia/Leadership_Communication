## Robustness check
#### DV: PSM binaries ----------------------------------------------------------

labeled_POS2020 %>% dplyr::select(PSM6) %>% group_by(PSM6) %>%
  summarize(
  n = n()) %>% ungroup() %>% mutate(rate = n/sum(n)) %>%
  ggplot(aes(PSM6, y = rate, fill = PSM6)) + 
  geom_col(alpha = 0.7, show.legend = F) + 
  labs(x = "", y = ""#,
       #title = "DV: 공공봉사동기"
  ) + 
  geom_text(aes(label = paste0(round(rate, 2)*100, "%")), 
            position = position_stack(0.8), color = "black") + 
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.6)) + 
  # scale_x_continuous(breaks = c(seq(1, 5, 1)), 
  #                    labels = c("전혀\n그렇지\n않다", "그렇지\n않다", "보통이다", "그렇다", "매우\n그렇다")) + 
  scale_fill_manual(values = c(futurevisions::futurevisions("pegasi")[2],
                               futurevisions::futurevisions("pegasi")[3],
                               futurevisions::futurevisions("pegasi")[4],
                               futurevisions::futurevisions("pegasi")[5],
                               futurevisions::futurevisions("pegasi")[6])) +
  theme_nice()
??futurevisions
futurevisions::show_palette("pegasi")
??scale_fill_gradient
ggsave("Documents/Figures/fig1_dv_binaries_revised.png", 
       height = 3, width = 3, dpi = 1200)

names(labeled_POS2020)

MASS::polr(as.factor(PSM6) ~ 
             TAL + 
             PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = labeled_POS2020, Hess = T) -> 
  robust_base1

MASS::polr(as.factor(PSM6) ~ 
             TFL + 
             PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = labeled_POS2020, Hess = T) -> 
  robust_base2

MASS::polr(as.factor(PSM6) ~ 
             TAL + TFL + 
             PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = labeled_POS2020, Hess = T) -> 
  robust_base3

MASS::polr(as.factor(PSM6) ~ 
             COMM +
             PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = labeled_POS2020, Hess = T) -> 
  robust_base4

MASS::polr(as.factor(PSM6) ~ TAL + TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, Hess = T) -> 
  robust_base5

MASS::polr(as.factor(PSM6) ~ 
      TAL*COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, Hess = T) -> 
  robust_ologit1

MASS::polr(as.factor(PSM6) ~ 
      TAL + TFL*COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, Hess = T) -> 
  robust_ologit2

MASS::polr(as.factor(PSM6) ~ TAL*TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, Hess = T) -> 
  robust_ologit3

texreg::screenreg(list(robust_base1, robust_base2, robust_base3,
                       robust_base4, robust_base5, 
                       robust_ologit1, robust_ologit2, robust_ologit3))
class(labeled_POS2020$TAL1)
labeled_POS2020 %>% 
  rowwise() %>%
  mutate(
    TAL_mean = mean(q19_1, q19_2, q19_8, na.rm = T),
    TFL_mean = mean(q19_4, q19_6, q19_7, na.rm = T),
    Comm_mean = mean(q23_1, q23_2, q23_3, na.rm = T),
    PERFM_mean = mean(q24_1, q24_2, q24_3, na.rm = 3),
    TAC_mean = mean(q20_1, q20_2, na.rm = T),
    TFC_mean = mean(q20_3, q20_4, q20_5, na.rm = T),
    EXC_mean = mean(q20_7, q20_8, na.rm = T)
  ) -> labeled_POS2020
summary(labeled_POS2020$TAL_mean)


glm(PSM6_binary ~ TAL_mean + 
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base1

glm(PSM6_binary ~ TFL_mean + 
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base2

glm(PSM6_binary ~ TAL_mean + TFL_mean + 
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base3

glm(PSM6_binary ~ Comm_mean +
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base4

glm(PSM6_binary ~ TAL_mean + TFL_mean + Comm_mean +
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base5

glm(PSM6_binary ~ 
      TAL_mean*Comm_mean + TFL_mean + PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base6

glm(PSM6_binary ~ TAL_mean + TFL_mean*Comm_mean + PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base7

glm(PSM6_binary ~ TAL_mean*TFL_mean + Comm_mean +
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_base8

texreg::screenreg(list(robust_m_base1, robust_m_base2, robust_m_base3,
                      robust_m_base4, robust_m_base5, robust_m_base6,
                      robust_m_base7, robust_m_base8))


glm(PSM6 ~ TAL_mean + 
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal1

glm(PSM6 ~ TFL_mean + 
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal2

glm(PSM6 ~ TAL_mean + TFL_mean + 
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal3

glm(PSM6 ~ Comm_mean +
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal4

glm(PSM6 ~ TAL_mean + TFL_mean + Comm_mean +
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal5

glm(PSM6 ~ 
      TAL_mean*Comm_mean + TFL_mean + PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal6

glm(PSM6 ~ TAL_mean + TFL_mean*Comm_mean + PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal7

glm(PSM6 ~ TAL_mean*TFL_mean + Comm_mean +
      PERFM_mean + TAC_mean + TFC_mean +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  robust_m_ordinal8

texreg::screenreg(list(robust_m_ordinal1, robust_m_ordinal2, robust_m_ordinal3,
                       robust_m_ordinal4, robust_m_ordinal5, robust_m_ordinal6,
                       robust_m_ordinal7, robust_m_ordinal8))
