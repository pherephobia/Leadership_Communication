# Intormation of the Project ---------------------------------------------------
## Empirical analyses script
## Project: KIPA-KAPA 논문공모전 ----------------------------------------------
### Authors: ----
###   - Park, Sanghoon
###   - Kang, Jiyoon
###   - Yang, Sungue Susan
### Log update: June 9th 2021  / Raw data download and Rproj set ----
###             July 15th 2021 / EDA for PSM Hypothesis
###             July 17th 2021 / Change DV as binary

rm(list=ls())

### You can load the data: -----------------------------------------------------
load("Data/Analysis_data/KIPA-KAPA2021.RData")


table(labeled_POS2020$PSM6)
## Load packages ---------------------------------------------------------------
pacman::p_load(
  ezpickr, magrittr, psych, extrafont, tidyverse,
  interplot
)

## 경험분석 --------------------------------------------------------------------

### 모델 구축 ------------------------------------------------------------------
## DV: 1) 국가와 국민을 위한 봉사는 나에게 매우 중요하다 ------

glm(PSM1_binary ~ TAL + TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model1_base

glm(PSM1_binary ~ 
      TAL*COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model1_logit1

glm(PSM1_binary ~ TAL + TFL*COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model1_logit2

glm(PSM1_binary ~ TAL*TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model1_logit3

exp(coef(model1_base))

texreg::screenreg(list(model1_base, model1_logit3, 
                       model1_logit1, model1_logit2))

texreg::htmlreg(list(model1_base, model1_logit1, 
                     model1_logit2, model1_logit3),
                single.row = F,
                file = "Documents/Tables/Table 2.doc", doctype = T,
                head.tag = T, body.tag = T)
library(wesanderson)

coefpal <- c(wes_palette("Royal1")[2], wes_palette("Royal1")[1])

broom::tidy(model1_logit1, conf.int = T) %>%
  mutate(insig = if_else(conf.high >0 & conf.low < 0, 0, 1)) %>%
  dplyr::filter(!term %in% "(Intercept)") %>%
  mutate(vars = case_when(
    term == "TAL" ~ "거래적 리더십",
    term == "TFL" ~ "변혁적 리더십",
    term == "COMM" ~ "협업/의사소통",
    term == "PERF" ~ "성과관리",
    term == "TAC" ~ "거래적 리더십 문화",
    term == "TFC" ~ "변혁적 리더십 문화",
    term == "dq1" ~ "성별 (여성 = 1)",
    term == "dq2" ~ "연령대",
    term == "dq3" ~ "혼인 여부 (기혼 = 1)",
    term == "dq4_1" ~ "자녀의 수",
    term == "dq5_2" ~ "최종 학력",
    term == "dq9_2" ~ "현재 직급",
    term == "dq10" ~ "주평균 근무시간",
    term == "dq11" ~ "주평균 초과근무시간",
    term == "TAL:TFL" ~ "거래적 리더십 X 변혁적 리더십",
    term == "TFL:COMM" ~ "변혁적 리더십 X 의사소통",
    term == "TAL:COMM" ~ "거래적 리더십 X 의사소통",
  )) %>%
  ggplot(aes(x = reorder(vars, estimate), y = estimate,
             color = as.factor(insig),
             fill = as.factor(insig))) + 
  geom_point(show.legend = F) + 
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high), 
                  show.legend = F) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "모델1: 거래적 리더십 X 협업/의사소통 모델",
       x = "변수", y = "추정량") + 
  scale_color_manual(values = coefpal) +
  coord_flip() + theme_nice() + theme(legend.position = "bottom",
                                    legend.title = element_blank(),
                                    text=element_text(size=12,
                                                      family="NanumMyeongjo")) -> 
  model1

broom::tidy(model1_logit2, conf.int = T) %>%
  mutate(insig = if_else(conf.high >0 & conf.low < 0, 0, 1)) %>%
  dplyr::filter(!term %in% "(Intercept)") %>%
  mutate(vars = case_when(
    term == "TAL" ~ "거래적 리더십",
    term == "TFL" ~ "변혁적 리더십",
    term == "COMM" ~ "협업/의사소통",
    term == "PERF" ~ "성과관리",
    term == "TAC" ~ "거래적 리더십 문화",
    term == "TFC" ~ "변혁적 리더십 문화",
    term == "dq1" ~ "성별 (여성 = 1)",
    term == "dq2" ~ "연령대",
    term == "dq3" ~ "혼인 여부 (기혼 = 1)",
    term == "dq4_1" ~ "자녀의 수",
    term == "dq5_2" ~ "최종 학력",
    term == "dq9_2" ~ "현재 직급",
    term == "dq10" ~ "주평균 근무시간",
    term == "dq11" ~ "주평균 초과근무시간",
    term == "TAL:TFL" ~ "거래적 리더십 X 변혁적 리더십",
    term == "TFL:COMM" ~ "변혁적 리더십 X 의사소통",
    term == "TAL:COMM" ~ "거래적 리더십 X 의사소통",
  )) %>%
  ggplot(aes(x = reorder(vars, estimate), y = estimate,
             color = as.factor(insig),
             fill = as.factor(insig))) + 
  geom_point(show.legend = F) + 
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high), 
                  show.legend = F) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "모델2: 변혁적 리더십 X 협업/의사소통 모델",
       x = "변수", y = "추정량") + 
  scale_color_manual(values = coefpal) +
  coord_flip() -> model2

broom::tidy(model1_logit3, conf.int = T) %>%
  mutate(insig = if_else(conf.high >0 & conf.low < 0, 0, 1)) %>%
  dplyr::filter(!term %in% "(Intercept)") %>%
  mutate(vars = case_when(
    term == "TAL" ~ "거래적 리더십",
    term == "TFL" ~ "변혁적 리더십",
    term == "COMM" ~ "협업/의사소통",
    term == "PERF" ~ "성과관리",
    term == "TAC" ~ "거래적 리더십 문화",
    term == "TFC" ~ "변혁적 리더십 문화",
    term == "dq1" ~ "성별 (여성 = 1)",
    term == "dq2" ~ "연령대",
    term == "dq3" ~ "혼인 여부 (기혼 = 1)",
    term == "dq4_1" ~ "자녀의 수",
    term == "dq5_2" ~ "최종 학력",
    term == "dq9_2" ~ "현재 직급",
    term == "dq10" ~ "주평균 근무시간",
    term == "dq11" ~ "주평균 초과근무시간",
    term == "TAL:TFL" ~ "거래적 리더십 X 변혁적 리더십",
    term == "TFL:COMM" ~ "변혁적 리더십 X 의사소통",
    term == "TAL:COMM" ~ "거래적 리더십 X 의사소통",
  )) %>%
  ggplot(aes(x = reorder(vars, estimate), y = estimate,
             color = as.factor(insig),
             fill = as.factor(insig))) + 
  geom_point(show.legend = F) + 
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high), 
                  show.legend = F) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "모델3: 거래적 리더십 X 변혁적 리더십 모델",
       x = "변수", y = "추정량") + 
  scale_color_manual(values = wes_palette("Royal1")) +
  coord_flip() -> model3

broom::tidy(model1_base, conf.int = T) %>%
  mutate(insig = if_else(conf.high >0 & conf.low < 0,
                         "Insignificant", "Significant"),
         insig = factor(insig,
                        levels = c("Insignificant", "Significant"))) %>%
  dplyr::filter(!term %in% "(Intercept)") %>%
  mutate(vars = case_when(
    term == "TAL" ~ "거래적 리더십",
    term == "TFL" ~ "변혁적 리더십",
    term == "COMM" ~ "협업/의사소통",
    term == "PERF" ~ "성과관리",
    term == "TAC" ~ "거래적 리더십 문화",
    term == "TFC" ~ "변혁적 리더십 문화",
    term == "dq1" ~ "성별 (여성 = 1)",
    term == "dq2" ~ "연령대",
    term == "dq3" ~ "혼인 여부 (기혼 = 1)",
    term == "dq4_1" ~ "자녀의 수",
    term == "dq5_2" ~ "최종 학력",
    term == "dq9_2" ~ "현재 직급",
    term == "dq10" ~ "주평균 근무시간",
    term == "dq11" ~ "주평균 초과근무시간",
    term == "TAL:TFL" ~ "거래적 리더십 X 변혁적 리더십",
    term == "TFL:COMM" ~ "변혁적 리더십 X 의사소통",
    term == "TAL:COMM" ~ "거래적 리더십 X 의사소통",
  )) %>%
  ggplot(aes(x = reorder(vars, estimate), y = estimate,
             color = as.factor(insig),
             fill = as.factor(insig))) + 
  geom_point(show.legend = F) + 
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high), 
                  show.legend = F) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "모델0: 벤치마크 모델 (상호작용X)",
       x = "변수", y = "추정량") + 
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  coord_flip() + theme_bw() + theme(legend.position = "bottom",
                                    legend.title = element_blank(),
                                    text=element_text(size=12,
                                                      family="NanumMyeongjo")) ->
  base

base + model1 + model2 + model3 + patchwork::plot_layout(ncol = 2)

### Model 1: TAL X COMM --------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model1_logit1), 
                            Sigma = vcov(model1_logit1))
summary(labeled_POS2020$COMM)

COMM <- c(seq(-3, 3, 0.5))
TAL <- c(min(labeled_POS2020$TAL, na.rm = T),
         mean(labeled_POS2020$TAL, na.rm = T),
         max(labeled_POS2020$TAL, na.rm = T))
Xi.mat.low <- cbind(1, TAL[1],
                    COMM,
                    mean(labeled_POS2020$TFL, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TAL[1]*COMM))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))

Xibcoef.low <- tibble(
  TAL = "Low",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xi.mat.mean <- cbind(1, TAL[2],
                    COMM,
                    mean(labeled_POS2020$TFL, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TAL[2]*COMM))

Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))

Xibcoef.mean <- tibble(
  TAL = "Mean",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)


Xi.mat.high <- cbind(1, TAL[3],
                    COMM,
                    mean(labeled_POS2020$TFL, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TAL[3]*COMM))

Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.high <- tibble(
  TAL = "High",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)

Xibcoef <- bind_rows(Xibcoef.low, Xibcoef.mean, Xibcoef.high)


Xibcoef %>% rename(`거래적 리더십` = TAL) %>% 
  mutate(`거래적 리더십` = factor(`거래적 리더십`,
                            levels = c("Low", "Mean", "High"),
                            labels = c("낮음", "평균", "높음"))) %>%
  ggplot(aes(x = Communication, y = Mean,
                       fill = `거래적 리더십`,
                       color = `거래적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.4) + 
  scale_x_continuous(limits = c(-3, 3),
                     breaks = c(seq(-3, 3, 1))) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준"#,
     #  subtitle = "협업/의사소통 수준에 따른 거래적 리더십의 
     #  공공봉사동기의 긍정적 응답에 대한 예측확률 변화"
     ) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) +
  theme_nice() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig1_layered_revised.png",
       width = 12, height = 5, dpi = 600)


Xibcoef %>% ggplot(aes(x = Communication, y = Mean, group = TAL)) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준",
       subtitle = "협업/의사소통 수준에 따른 거래적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme(plot.subtitle = element_text(size = 8))
ggsave("Documents/Figures/Analysis_fig1.png",
       width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------
getwd()
### PSM = b1 + b3 * COMM
saveRDS(model1_logit1, "model1_logit1.RDS")
XZ <- cbind(1, COMM)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model1_logit1),  sigma = vcov(model1_logit1))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(2, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model1.data <- data.frame(Communication=COMM,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

test.model1.data <- data.frame(Communication = COMM,
                               M = exp(xz_m),
                               ll=exp(xz_se[1,]),
                               ul=exp(xz_se[2,]))

test.model1.data %>% ggplot(aes(y = M, x = Communication)) + 
  #geom_point(show.legend = F, size = 3, shape = 21) + 
  #geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(
    subtitle = "협업/의사소통 수준에 따른 거래적 리더십 변수의 한계효과 변화",
    x = "협업/의사소통 수준",
    caption = "협업/의사소통 수준과 거래적 리더십 변수는 요인분석을 통해 표준화되었음.",
    y = expression(frac(partialdiff*paste("공공봉사동기"),
                        partialdiff*paste("거래적 리더십")))) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_bw() +   theme(plot.subtitle = element_text(size = 12),
                       axis.title = element_text(size = 12),
                       legend.position = "bottom")

labeled_POS2020 %>%
  ggplot(aes(COMM)) + geom_histogram(fill = "white",
                                     color = "black") + 
  theme_clean() ->
  panel2

panel1 + panel2 + patchwork::plot_layout(ncol = 1)

ggsave("Documents/Figures/Analysis_fig1_marginal.png",
       width = 6, height = 5, dpi = 600)

#### Using interplot -----------------------------------------------------------
theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
  theme(panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold", color = "black"),
  plot.subtitle = element_text(face = "bold", color = "black"),
  axis.title = element_text(family = "Barlow Semi Condensed Medium", color = "black"),
  axis.text = element_text(color = "black"),
  strip.text = element_text(family = "Barlow Semi Condensed",
                            face = "bold", size = rel(1), hjust = 0,
                            color = "black"),
  strip.background = element_rect(fill = "black", color = NA),
  plot.caption = element_text(hjust = 0, color = "black"),
  legend.position = "bottom")
}
interplot(model1_logit1, var1 = "TAL", var2 = "COMM", hist = T, sims = 4000) + 
  theme_clean()


### Model 2: TFL X COMM --------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model1_logit12), 
                            Sigma = vcov(model1_logit12))
TFL <- c(
  min(labeled_POS2020$TFL, na.rm = T),
  mean(labeled_POS2020$TFL, na.rm = T),
  max(labeled_POS2020$TFL, na.rm = T)
)
Xi.mat.low <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                TFL[1],
                COMM,
                mean(labeled_POS2020$PERF, na.rm = T),
                mean(labeled_POS2020$TAC, na.rm = T),
                mean(labeled_POS2020$TFC, na.rm = T),
                median(labeled_POS2020$dq1, na.rm = T),
                median(labeled_POS2020$dq2, na.rm = T),
                median(labeled_POS2020$dq3, na.rm = T),
                median(labeled_POS2020$dq4_1, na.rm = T),
                median(labeled_POS2020$dq5_2, na.rm = T),
                median(labeled_POS2020$dq9_2, na.rm = T),
                median(labeled_POS2020$dq10, na.rm = T),
                median(labeled_POS2020$dq11, na.rm = T),
                I(TFL[1]*COMM))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))

Xibcoef.low <- tibble(
  `변혁적 리더십` = "낮음",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xi.mat.mean <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                    TFL[2],
                    COMM,
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[2]*COMM))

Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))

Xibcoef.mean <- tibble(
  `변혁적 리더십` = "평균",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)

Xi.mat.high <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                    TFL[3],
                    COMM,
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[3]*COMM))

Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.high <- tibble(
  `변혁적 리더십` = "높음",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)


Xibcoef <- bind_rows(Xibcoef.low, Xibcoef.mean, Xibcoef.high)


Xibcoef %>% 
  mutate(`변혁적 리더십` = factor(`변혁적 리더십`,
                            levels = c("낮음", "평균", "높음"))) %>%
  ggplot(aes(x = Communication, y = Mean,
             fill = `변혁적 리더십`,
             color = `변혁적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준"#,
    #   subtitle = "협업/의사소통 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화"
    ) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) +
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("Documents/Figures/Analysis_fig2_layered.png",
       width = 6, height = 5, dpi = 600)


Xibcoef %>% ggplot(aes(x = Communication, y = Mean)) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준",
       subtitle = "협업/의사소통 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme(plot.subtitle = element_text(size = 8))
ggsave("Documents/Figures/Analysis_fig2.png",
       width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------
XZ <- cbind(1, COMM)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model1_logit12),  sigma = vcov(model1_logit12))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(3, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model2.data <- data.frame(Communication=COMM,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model2.data %>% ggplot(aes(y = M, x = Communication)) + 
  # geom_point(show.legend = F, size = 3, shape = 21) + 
  # geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) +
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(subtitle = "협업/의사소통 수준에 따른 변혁적 리더십 변수의 한계효과 변화",
       x = "협업/의사소통 수준",
       caption = "협업/의사소통 수준과 변혁적 리더십 변수는 요인분석을 통해 표준화되었음.",
       y = expression(frac(partialdiff*paste("공공봉사동기"),
                           partialdiff*paste("변혁적 리더십십")))) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig2_marginal.png",
       width = 6, height = 5, dpi = 600)

### Model 3: TAL X TFL ---------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model1_logit13), 
                            Sigma = vcov(model1_logit13))
TAL <- c(seq(-5, 5, 0.5))
Xi.mat.low <- cbind(1, TAL,
                TFL[1],
                mean(labeled_POS2020$COMM, na.rm = T),
                mean(labeled_POS2020$PERF, na.rm = T),
                mean(labeled_POS2020$TAC, na.rm = T),
                mean(labeled_POS2020$TFC, na.rm = T),
                median(labeled_POS2020$dq1, na.rm = T),
                median(labeled_POS2020$dq2, na.rm = T),
                median(labeled_POS2020$dq3, na.rm = T),
                median(labeled_POS2020$dq4_1, na.rm = T),
                median(labeled_POS2020$dq5_2, na.rm = T),
                median(labeled_POS2020$dq9_2, na.rm = T),
                median(labeled_POS2020$dq10, na.rm = T),
                median(labeled_POS2020$dq11, na.rm = T),
                I(TFL[1]*TAL))

Xi.mat.mean <- cbind(1, TAL,
                    TFL[2],
                    mean(labeled_POS2020$COMM, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[2]*TAL))

Xi.mat.high <- cbind(1, TAL,
                    TFL[3],
                    mean(labeled_POS2020$COMM, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[3]*TAL))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))
Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))
Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.low <- tibble(
  `변혁적 리더십` = "낮음",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xibcoef.mean <- tibble(
  `변혁적 리더십` = "평균",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)

Xibcoef.high <- tibble(
  `변혁적 리더십` = "높음",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)

Xibcoef <- bind_rows(
  Xibcoef.low, Xibcoef.mean, Xibcoef.high
)

Xibcoef %>%
  mutate(`변혁적 리더십` = factor(`변혁적 리더십`,
                            levels = c("낮음", "평균", "높음"))) %>%
#  dplyr::filter(!`변혁적 리더십` %in% "평균") %>%
  ggplot(aes(x = `거래적 리더십`, y = Mean,
             color = `변혁적 리더십`, fill = `변혁적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.3) + 
  # geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  # geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
#  facet_wrap(~`변혁적 리더십`) +
  labs(y = "예측확률",
       x = "거래적 리더십",
       subtitle = "거래적 리더십 수준에 따른 변혁적 리더십의 
       공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig3_layered.png",
       width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------
XZ <- cbind(1, TAL)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model1_logit13),  sigma = vcov(model1_logit13))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(2, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model3.data <- data.frame(`Transactional Leadership`=TAL,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model3.data %>% ggplot(aes(y = M, x = Transactional.Leadership)) + 
  # geom_point(show.legend = F, size = 3, shape = 21) + 
  # geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) +
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(subtitle = "거래적 리더십 수준에 따른 변혁적 리더십 변수의 한계효과 변화",
       x = "거래적 리더십 수준",
       caption = "거래적/변혁적 리더십 변수는 요인분석을 통해 표준화되었음.",
       y = expression(frac(partialdiff*paste("공공봉사동기"),
                           partialdiff*paste("변혁적 리더십십")))) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig3_marginal.png",
       width = 6, height = 5, dpi = 600)


## DV: 3) 나에게는 사회에 어떤 바람직한 변화를 가져오는 것이 -----
## 개인적인 성취보다 더욱 큰 의미가 있다
### For H1, H2, H3
glm(PSM3_binary ~ TAL + 
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") ->
  model1

glm(PSM3_binary ~ TFL +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") ->
  model2

glm(PSM3_binary ~ TAL + TFL +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") ->
  model3

glm(PSM3_binary ~ COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") ->
  model4

glm(PSM3_binary ~ TAL + TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") ->
  model5

glm(PSM3_binary ~ TAL*TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model6

glm(PSM3_binary ~ 
      TAL*COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") ->
  model7

glm(PSM3_binary ~ 
      TAL + TFL*COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") ->
  model8

texreg::htmlreg(list(model1, model2, model3, model4, model5,
                     model6, model7),
                single.row = F,
                file = "Documents/Tables/Table 1_revised.doc", doctype = T,
                head.tag = T, body.tag = T)

### Model 4: TAL X COMM --------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model7), 
                            Sigma = vcov(model7))
summary(labeled_POS2020$COMM)

COMM <- c(seq(-3, 3, 0.5))
TAL <- c(min(labeled_POS2020$TAL, na.rm = T),
         mean(labeled_POS2020$TAL, na.rm = T),
         max(labeled_POS2020$TAL, na.rm = T))
Xi.mat.low <- cbind(1, TAL[1],
                    COMM,
                    mean(labeled_POS2020$TFL, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TAL[1]*COMM))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))

Xibcoef.low <- tibble(
  TAL = "Low",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xi.mat.mean <- cbind(1, TAL[2],
                     COMM,
                     mean(labeled_POS2020$TFL, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TAL[2]*COMM))

Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))

Xibcoef.mean <- tibble(
  TAL = "Mean",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)


Xi.mat.high <- cbind(1, TAL[3],
                     COMM,
                     mean(labeled_POS2020$TFL, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TAL[3]*COMM))

Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.high <- tibble(
  TAL = "High",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)

Xibcoef <- bind_rows(Xibcoef.low, Xibcoef.mean, Xibcoef.high)


Xibcoef %>% rename(`거래적 리더십` = TAL) %>% 
  mutate(`거래적 리더십` = factor(`거래적 리더십`,
                            levels = c("Low", "Mean", "High"),
                            labels = c("낮음", "평균", "높음"))) %>%
  ggplot(aes(x = Communication, y = Mean,
             fill = `거래적 리더십`,
             color = `거래적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준"#,
#       subtitle = "협업/의사소통 수준에 따른 거래적 리더십의 
#       공공봉사동기의 긍정적 응답에 대한 예측확률 변화"
) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) +
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig4_layered.png",
       width = 7, height = 3.5, dpi = "retina")


Xibcoef %>% ggplot(aes(x = Communication, y = Mean, group = TAL)) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준",
       subtitle = "협업/의사소통 수준에 따른 거래적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme(plot.subtitle = element_text(size = 8))
ggsave("Documents/Figures/Analysis_fig1.png",
       width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------

### PSM = b1 + b3 * COMM

XZ <- cbind(1, COMM)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model2_logit1),  sigma = vcov(model2_logit1))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(2, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model2.data <- data.frame(Communication=COMM,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model2.data %>% ggplot(aes(y = M, x = Communication)) + 
  #geom_point(show.legend = F, size = 3, shape = 21) + 
  #geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(
    subtitle = "협업/의사소통 수준에 따른 거래적 리더십 변수의 한계효과 변화",
    x = "협업/의사소통 수준",
    caption = "협업/의사소통 수준과 거래적 리더십 변수는 요인분석을 통해 표준화되었음.",
    y = expression(frac(partialdiff*paste("공공봉사동기"),
                        partialdiff*paste("거래적 리더십")))) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_bw() +   theme(plot.subtitle = element_text(size = 12),
                       axis.title = element_text(size = 12),
                       legend.position = "bottom")

ggsave("Documents/Figures/Analysis_fig4_marginal.png",
       width = 6, height = 5, dpi = 600)


### Model 5: TFL X COMM --------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model8), 
                            Sigma = vcov(model8))
TFL <- c(
  min(labeled_POS2020$TFL, na.rm = T),
  mean(labeled_POS2020$TFL, na.rm = T),
  max(labeled_POS2020$TFL, na.rm = T)
)
Xi.mat.low <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                    TFL[1],
                    COMM,
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[1]*COMM))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))

Xibcoef.low <- tibble(
  `변혁적 리더십` = "낮음",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xi.mat.mean <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                     TFL[2],
                     COMM,
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[2]*COMM))

Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))

Xibcoef.mean <- tibble(
  `변혁적 리더십` = "평균",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)

Xi.mat.high <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                     TFL[3],
                     COMM,
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[3]*COMM))

Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.high <- tibble(
  `변혁적 리더십` = "높음",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)


Xibcoef <- bind_rows(Xibcoef.low, Xibcoef.mean, Xibcoef.high)


Xibcoef %>% 
  mutate(`변혁적 리더십` = factor(`변혁적 리더십`,
                            levels = c("낮음", "평균", "높음"))) %>%
  ggplot(aes(x = Communication, y = Mean,
             fill = `변혁적 리더십`,
             color = `변혁적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준"#,
#       subtitle = "협업/의사소통 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화"
) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) +
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("Documents/Figures/Analysis_fig5_layered.png",
       width = 7, height = 3.5, dpi = "retina")


# Xibcoef %>% ggplot(aes(x = Communication, y = Mean)) + 
#   geom_line() + 
#   geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
#   geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
#   scale_y_continuous(labels = scales::percent) + 
#   labs(y = "예측확률",
#        x = "협업/의사소통 수준",
#        subtitle = "협업/의사소통 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
#   theme(plot.subtitle = element_text(size = 8))
# ggsave("Documents/Figures/Analysis_fig2.png",
#        width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------
XZ <- cbind(1, COMM)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model2_logit2),  sigma = vcov(model2_logit2))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(3, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model2.data <- data.frame(Communication=COMM,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model2.data %>% ggplot(aes(y = M, x = Communication)) + 
  # geom_point(show.legend = F, size = 3, shape = 21) + 
  # geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) +
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(subtitle = "협업/의사소통 수준에 따른 변혁적 리더십 변수의 한계효과 변화",
       x = "협업/의사소통 수준",
       caption = "협업/의사소통 수준과 변혁적 리더십 변수는 요인분석을 통해 표준화되었음.",
       y = expression(frac(partialdiff*paste("공공봉사동기"),
                           partialdiff*paste("변혁적 리더십십")))) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig5_margin.png",
       width = 6, height = 5, dpi = 600)

### Model 6: TAL X TFL ---------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model6), 
                            Sigma = vcov(model6))
TAL <- c(seq(-5, 5, 0.5))
Xi.mat.low <- cbind(1, TAL,
                    TFL[1],
                    mean(labeled_POS2020$COMM, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[1]*TAL))

Xi.mat.mean <- cbind(1, TAL,
                     TFL[2],
                     mean(labeled_POS2020$COMM, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[2]*TAL))

Xi.mat.high <- cbind(1, TAL,
                     TFL[3],
                     mean(labeled_POS2020$COMM, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[3]*TAL))
summary(model5)
Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))
Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))
Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.low <- tibble(
  `변혁적 리더십` = "낮음",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xibcoef.mean <- tibble(
  `변혁적 리더십` = "평균",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)

Xibcoef.high <- tibble(
  `변혁적 리더십` = "높음",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)

Xibcoef <- bind_rows(
  Xibcoef.low, Xibcoef.mean, Xibcoef.high
)

Xibcoef %>%
  mutate(`변혁적 리더십` = factor(`변혁적 리더십`,
                            levels = c("낮음", "평균", "높음"))) %>%
  #  dplyr::filter(!`변혁적 리더십` %in% "평균") %>%
  ggplot(aes(x = `거래적 리더십`, y = Mean,
             color = `변혁적 리더십`, fill = `변혁적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.3) + 
  # geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  # geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  #  facet_wrap(~`변혁적 리더십`) +
  labs(y = "예측확률",
       x = "거래적 리더십"#,
  #     subtitle = "거래적 리더십 수준에 따른 변혁적 리더십의 
  #     공공봉사동기의 긍정적 응답에 대한 예측확률 변화"
  ) + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig6_layered.png",
       width = 7, height = 3.5, dpi = "retina")

#### Marginal Effects ----------------------------------------------------------
XZ <- cbind(1, TAL)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model2_logit3),  sigma = vcov(model2_logit3))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(2, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model3.data <- data.frame(`Transactional Leadership`=TAL,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model3.data %>% ggplot(aes(y = M, x = Transactional.Leadership)) + 
  # geom_point(show.legend = F, size = 3, shape = 21) + 
  # geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) +
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(subtitle = "거래적 리더십 수준에 따른 변혁적 리더십 변수의 한계효과 변화",
       x = "거래적 리더십 수준",
       caption = "거래적/변혁적 리더십 변수는 요인분석을 통해 표준화되었음.",
       y = expression(frac(partialdiff*paste("공공봉사동기"),
                           partialdiff*paste("변혁적 리더십십")))) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig6_marginal.png",
       width = 6, height = 5, dpi = 600)


## DV: 6) 나는 정책과정에 참여해 사회적으로 의미있는 일을 하는 것에 ------------
## 큰 보람을 느낀다.


glm(PSM6_binary ~ TAL + TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model3_base

glm(PSM6_binary ~ 
      TAL*COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model3_logit1

glm(PSM6_binary ~ TAL + TFL*COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model3_logit2

glm(PSM6_binary ~ TAL*TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = labeled_POS2020, family = "binomial") -> 
  model3_logit3

texreg::htmlreg(list(model3_base, model3_logit1, 
                     model3_logit2, model3_logit3),
                single.row = F,
                file = "Documents/Tables/Table 4.doc", doctype = T,
                head.tag = T, body.tag = T)

### Model 7: TAL X COMM --------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model3_logit1), 
                            Sigma = vcov(model3_logit1))
summary(labeled_POS2020$COMM)

COMM <- c(seq(-3, 3, 0.5))
TAL <- c(min(labeled_POS2020$TAL, na.rm = T),
         mean(labeled_POS2020$TAL, na.rm = T),
         max(labeled_POS2020$TAL, na.rm = T))
Xi.mat.low <- cbind(1, TAL[1],
                    COMM,
                    mean(labeled_POS2020$TFL, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TAL[1]*COMM))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))

Xibcoef.low <- tibble(
  TAL = "Low",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xi.mat.mean <- cbind(1, TAL[2],
                     COMM,
                     mean(labeled_POS2020$TFL, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TAL[2]*COMM))

Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))

Xibcoef.mean <- tibble(
  TAL = "Mean",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)


Xi.mat.high <- cbind(1, TAL[3],
                     COMM,
                     mean(labeled_POS2020$TFL, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TAL[3]*COMM))

Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.high <- tibble(
  TAL = "High",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)

Xibcoef <- bind_rows(Xibcoef.low, Xibcoef.mean, Xibcoef.high)


Xibcoef %>% rename(`거래적 리더십` = TAL) %>% 
  mutate(`거래적 리더십` = factor(`거래적 리더십`,
                            levels = c("Low", "Mean", "High"),
                            labels = c("낮음", "평균", "높음"))) %>%
  ggplot(aes(x = Communication, y = Mean,
             fill = `거래적 리더십`,
             color = `거래적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준",
       subtitle = "협업/의사소통 수준에 따른 거래적 리더십의 
       공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) +
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig7_layered.png",
       width = 6, height = 5, dpi = 600)


Xibcoef %>% ggplot(aes(x = Communication, y = Mean, group = TAL)) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준",
       subtitle = "협업/의사소통 수준에 따른 거래적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme(plot.subtitle = element_text(size = 8))
ggsave("Documents/Figures/Analysis_fig7.png",
       width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------

### PSM = b1 + b3 * COMM

XZ <- cbind(1, COMM)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model3_logit1),  sigma = vcov(model3_logit1))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(2, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model3.data <- data.frame(Communication=COMM,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model3.data %>% ggplot(aes(y = M, x = Communication)) + 
  #geom_point(show.legend = F, size = 3, shape = 21) + 
  #geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(
    subtitle = "협업/의사소통 수준에 따른 거래적 리더십 변수의 한계효과 변화",
    x = "협업/의사소통 수준",
    caption = "협업/의사소통 수준과 거래적 리더십 변수는 요인분석을 통해 표준화되었음.",
    y = expression(frac(partialdiff*paste("공공봉사동기"),
                        partialdiff*paste("거래적 리더십")))) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_bw() +   theme(plot.subtitle = element_text(size = 12),
                       axis.title = element_text(size = 12),
                       legend.position = "bottom")

ggsave("Documents/Figures/Analysis_fig7_marginal.png",
       width = 6, height = 5, dpi = 600)


### Model 8: TFL X COMM --------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model3_logit2), 
                            Sigma = vcov(model3_logit2))
TFL <- c(
  min(labeled_POS2020$TFL, na.rm = T),
  mean(labeled_POS2020$TFL, na.rm = T),
  max(labeled_POS2020$TFL, na.rm = T)
)
Xi.mat.low <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                    TFL[1],
                    COMM,
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[1]*COMM))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))

Xibcoef.low <- tibble(
  `변혁적 리더십` = "낮음",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xi.mat.mean <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                     TFL[2],
                     COMM,
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[2]*COMM))

Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))

Xibcoef.mean <- tibble(
  `변혁적 리더십` = "평균",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)

Xi.mat.high <- cbind(1, mean(labeled_POS2020$TAL, na.rm = T),
                     TFL[3],
                     COMM,
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[3]*COMM))

Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.high <- tibble(
  `변혁적 리더십` = "높음",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)


Xibcoef <- bind_rows(Xibcoef.low, Xibcoef.mean, Xibcoef.high)


Xibcoef %>% 
  mutate(`변혁적 리더십` = factor(`변혁적 리더십`,
                            levels = c("낮음", "평균", "높음"))) %>%
  ggplot(aes(x = Communication, y = Mean,
             fill = `변혁적 리더십`,
             color = `변혁적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       x = "협업/의사소통 수준",
       subtitle = "협업/의사소통 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) +
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("Documents/Figures/Analysis_fig8_layered.png",
       width = 6, height = 5, dpi = 600)


# Xibcoef %>% ggplot(aes(x = Communication, y = Mean)) + 
#   geom_line() + 
#   geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
#   geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
#   scale_y_continuous(labels = scales::percent) + 
#   labs(y = "예측확률",
#        x = "협업/의사소통 수준",
#        subtitle = "협업/의사소통 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
#   theme(plot.subtitle = element_text(size = 8))
# ggsave("Documents/Figures/Analysis_fig2.png",
#        width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------
XZ <- cbind(1, COMM)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model3_logit2),  sigma = vcov(model3_logit2))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(3, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model3.data <- data.frame(Communication=COMM,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model3.data %>% ggplot(aes(y = M, x = Communication)) + 
  # geom_point(show.legend = F, size = 3, shape = 21) + 
  # geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) +
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(subtitle = "협업/의사소통 수준에 따른 변혁적 리더십 변수의 한계효과 변화",
       x = "협업/의사소통 수준",
       caption = "협업/의사소통 수준과 변혁적 리더십 변수는 요인분석을 통해 표준화되었음.",
       y = expression(frac(partialdiff*paste("공공봉사동기"),
                           partialdiff*paste("변혁적 리더십십")))) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig8_margin.png",
       width = 6, height = 5, dpi = 600)

### Model 6: TAL X TFL ---------------------------------------------------------
#### Predicted Probabilities ---------------------------------------------------
beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model3_logit3), 
                            Sigma = vcov(model3_logit3))
TAL <- c(seq(-5, 5, 0.5))
Xi.mat.low <- cbind(1, TAL,
                    TFL[1],
                    mean(labeled_POS2020$COMM, na.rm = T),
                    mean(labeled_POS2020$PERF, na.rm = T),
                    mean(labeled_POS2020$TAC, na.rm = T),
                    mean(labeled_POS2020$TFC, na.rm = T),
                    median(labeled_POS2020$dq1, na.rm = T),
                    median(labeled_POS2020$dq2, na.rm = T),
                    median(labeled_POS2020$dq3, na.rm = T),
                    median(labeled_POS2020$dq4_1, na.rm = T),
                    median(labeled_POS2020$dq5_2, na.rm = T),
                    median(labeled_POS2020$dq9_2, na.rm = T),
                    median(labeled_POS2020$dq10, na.rm = T),
                    median(labeled_POS2020$dq11, na.rm = T),
                    I(TFL[1]*TAL))

Xi.mat.mean <- cbind(1, TAL,
                     TFL[2],
                     mean(labeled_POS2020$COMM, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[2]*TAL))

Xi.mat.high <- cbind(1, TAL,
                     TFL[3],
                     mean(labeled_POS2020$COMM, na.rm = T),
                     mean(labeled_POS2020$PERF, na.rm = T),
                     mean(labeled_POS2020$TAC, na.rm = T),
                     mean(labeled_POS2020$TFC, na.rm = T),
                     median(labeled_POS2020$dq1, na.rm = T),
                     median(labeled_POS2020$dq2, na.rm = T),
                     median(labeled_POS2020$dq3, na.rm = T),
                     median(labeled_POS2020$dq4_1, na.rm = T),
                     median(labeled_POS2020$dq5_2, na.rm = T),
                     median(labeled_POS2020$dq9_2, na.rm = T),
                     median(labeled_POS2020$dq10, na.rm = T),
                     median(labeled_POS2020$dq11, na.rm = T),
                     I(TFL[3]*TAL))

Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))
Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))
Xib.mat.sim.high <- plogis(beta_draws %*% t(Xi.mat.high))

Xibcoef.low <- tibble(
  `변혁적 리더십` = "낮음",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xibcoef.mean <- tibble(
  `변혁적 리더십` = "평균",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)

Xibcoef.high <- tibble(
  `변혁적 리더십` = "높음",
  `거래적 리더십` = TAL,
  Mean = apply(Xib.mat.sim.high, 2, mean),
  ll = apply(Xib.mat.sim.high, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.high, 2, quantile, probs = 0.975)
)

Xibcoef <- bind_rows(
  Xibcoef.low, Xibcoef.mean, Xibcoef.high
)

Xibcoef %>%
  mutate(`변혁적 리더십` = factor(`변혁적 리더십`,
                            levels = c("낮음", "평균", "높음"))) %>%
  #  dplyr::filter(!`변혁적 리더십` %in% "평균") %>%
  ggplot(aes(x = `거래적 리더십`, y = Mean,
             color = `변혁적 리더십`, fill = `변혁적 리더십`)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.3) + 
  # geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  # geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  #  facet_wrap(~`변혁적 리더십`) +
  labs(y = "예측확률",
       x = "거래적 리더십",
       subtitle = "거래적 리더십 수준에 따른 변혁적 리더십의 
       공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig9_layered.png",
       width = 6, height = 5, dpi = 600)

#### Marginal Effects ----------------------------------------------------------
XZ <- cbind(1, TAL)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model3_logit3),  sigma = vcov(model3_logit3))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(2, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model3.data <- data.frame(`Transactional Leadership`=TAL,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model3.data %>% ggplot(aes(y = M, x = Transactional.Leadership)) + 
  # geom_point(show.legend = F, size = 3, shape = 21) + 
  # geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) +
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(subtitle = "거래적 리더십 수준에 따른 변혁적 리더십 변수의 한계효과 변화",
       x = "거래적 리더십 수준",
       caption = "거래적/변혁적 리더십 변수는 요인분석을 통해 표준화되었음.",
       y = expression(frac(partialdiff*paste("공공봉사동기"),
                           partialdiff*paste("변혁적 리더십십")))) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig9_marginal.png",
       width = 6, height = 5, dpi = 600)
table(labeled_POS2020$gender)
table(labeled_POS2020$edu)
table(labeled_POS2020$work_hr)
table(labeled_POS2020$extra_work)
#### Marginal Effects ----------------------------------------------------------
XZ <- cbind(1, TAL)
beta_draws <- mvtnorm::rmvnorm(4000, mean = coef(model3_logit3),  sigma = vcov(model3_logit3))
beta_draws[1,]
ME_xz <- t(XZ %*% t(beta_draws[, c(2, 16)]))
xz_m <- apply(ME_xz, 2, mean)
xz_se <- apply(ME_xz, 2, quantile, probs = c(0.025, 0.975))
model3.data <- data.frame(`Transactional Leadership`=TAL,
                          M=xz_m,
                          ll=xz_se[1,],
                          ul=xz_se[2,])

model3.data %>% ggplot(aes(y = M, x = Transactional.Leadership)) + 
  # geom_point(show.legend = F, size = 3, shape = 21) + 
  # geom_pointrange(aes(ymin = ll, ymax = ul), size = 0.8) +
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  labs(subtitle = "거래적 리더십 수준에 따른 변혁적 리더십 변수의 한계효과 변화",
       x = "거래적 리더십 수준",
       caption = "거래적/변혁적 리더십 변수는 요인분석을 통해 표준화되었음.",
       y = expression(frac(partialdiff*paste("공공봉사동기"),
                           partialdiff*paste("변혁적 리더십십")))) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme(plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom")
ggsave("Documents/Figures/Analysis_fig9_marginal.png",
       width = 6, height = 5, dpi = 600)

