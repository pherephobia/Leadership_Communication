# Intormation of the Project ---------------------------------------------------
# Data manipulation
## Project: KIPA-KAPA 논문공모전 ----------------------------------------------
### Authors: ----
###   - Park, Sanghoon
###   - Kang, Jiyoon
###   - Yang, Sungue Susan
### Log update: June 9th 2021  / Raw data download and Rproj set ----
###             July 15th 2021 / EDA for PSM Hypothesis
###             July 17th 2021 / Change DV as binary
###                            / Separate R-script for manipulation, visualization
###                              from analysis
## Load packages ---------------------------------------------------------------
pacman::p_load(
  ezpickr, magrittr, psych, extrafont, ltm, tidyverse
)

## Load function for crosstab
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

### Set the theme of ggplot ---------------------------------------------------
ggplot2::theme_set(theme_bw() + 
                     theme(legend.position = "bottom",
                           legend.title = element_blank(),
                           text=element_text(size=12,  
                                             family="NanumMyeongjo")))
theme_nice <- function() {
  theme_minimal(base_family = "NanumMyeongjo") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}
## Raw data import: 공직생활실태조사 -------------------------------------------
Sys.setlocale("LC_ALL", "Korean")
POS2020 <- pick("Data/한국행정연구원_공직생활실태조사_데이터_2020.sav")
glimpse(POS2020)
table(POS2020$q29_6)
### Make a subset we need to explore -------------------------------------------
POS2020 %>% dplyr::select(
  ID, 기관명ID,
  q29_1, q29_2, q29_3, q29_4, q29_5, q29_6, # 공공봉사동기
  q19_1, q19_2, q19_8, # 거래적 리더십
  q19_4, q19_6, q19_7, # 변혁적 리더십
  q23_1, q23_2, q23_3, # 협업/의사소통
  q24_1, q24_2, q24_3, # 성과관리
  q20_1, q20_2,        # 조직문화 (거래적 리더십과 유사한)
  q20_3, q20_4, q20_5, # 조직문화 (변혁적 리더십과 유사한)
  q20_7, q20_8,
  dq1,   # 성별
  dq2,   # 연령
  dq3,   # 혼인상태
  dq4_1, # 자녀의 수
  dq5_2, # 현재 학력
  dq9_2, # 현재 직급
  dq10,  # 1주일 평균 총 근무시간 (1개월 기준)
  dq11,  # 1주일 평균 시간외 근무시간 (1개월 기준)
) -> POS2020_sub

### Glimpse the data
glimpse(POS2020_sub)
psych::describe(POS2020_sub)[, c(2, 3, 4, 5, 8, 9)] %>%
  knitr::kable(caption = "Descriptive Statistics") -> table1

### Make a labeled subset for EDA ----------------------------------------------
pt5 <- c(1, 2, 3, 4, 5)
labeller <- c("전혀\n그렇지\n않다",
              "그렇지\n않다",
              "보통이다", "그렇다", "매우\n그렇다")

labeled_POS2020 <- POS2020_sub %>%
  mutate(# Public Service Motivation
    PSM1 = q29_1, PSM2 = q29_2, PSM3 = q29_3, 
    PSM4 = q29_4, PSM5 = q29_5, PSM6 = q29_6,
    # TAL: Transcational Leadership variables
    TAL1 = q19_1, TAL2 = q19_2, TAL3 = q19_8, # 거래적 리더십
    # TFL: Transformational Leadership variables
    TFL1 = q19_4, TFL2 = q19_6, TFL3 = q19_7, # 변혁적 리더십
    # Comm: Communication variable
    Comm1 = q23_1, Comm2 = q23_2, Comm3 = q23_3, # 협업/의사소통
    # PERFM: Performance Management variables 
    PERFM1 = q24_1, PERFM2 = q24_2, PERFM3 = q24_3, # 성과관리
    # TAOrgCul: 조직문화 (거래적 리더십과 유사한)
    TAOrgCul1 = q20_1, TAOrgCul2 = q20_2, 
    # TFOrgCul: 조직문화 (변혁적 리더십과 유사한)
    TFOrgCul1 = q20_3, TFOrgCul2 = q20_4, TFOrgCul3 = q20_5, 
    # EXOrgCul: 조직문화 (기타)
    EXOrgCul1 = q20_7, EXOrgCul2 = q20_8,
    gender = dq1,   # 성별
    age = dq2,   # 연령
    marry = dq3,   # 혼인상태
    child = dq4_1, # 자녀의 수
    edu = dq5_2, # 현재 학력
    position = dq9_2, # 현재 직급
    work_hr = dq10,  # 1주일 평균 총 근무시간 (1개월 기준)
    extra_work = dq11,  # 1주일 평균 시간외 근무시간 (1개월 기준)
  ) %>% mutate(
    PSM1 = factor(PSM1, levels = pt5, labels = labeller),
    PSM2 = factor(PSM2, levels = pt5, labels = labeller),
    PSM3 = factor(PSM3, levels = pt5, labels = labeller),
    PSM4 = factor(PSM4, levels = pt5, labels = labeller),
    PSM5 = factor(PSM5, levels = pt5, labels = labeller),
    PSM6 = factor(PSM6, levels = pt5, labels = labeller),
    TAL1 = factor(TAL1, levels = pt5, labels = labeller),
    TAL2 = factor(TAL2, levels = pt5, labels = labeller),
    TAL3 = factor(TAL3, levels = pt5, labels = labeller),
    TFL1 = factor(TFL1, levels = pt5, labels = labeller),
    TFL2 = factor(TFL2, levels = pt5, labels = labeller),
    TFL3 = factor(TFL3, levels = pt5, labels = labeller),
    Comm1 = factor(Comm1, levels = pt5, labels = labeller),
    Comm2 = factor(Comm2, levels = pt5, labels = labeller),
    Comm3 = factor(Comm3, levels = pt5, labels = labeller),
    PERFM1 = factor(PERFM1, levels = pt5, labels = labeller),
    PERFM2 = factor(PERFM2, levels = pt5, labels = labeller),
    PERFM3 = factor(PERFM3, levels = pt5, labels = labeller),
    TAOrgCul1 = factor(TAOrgCul1, levels = pt5, labels = labeller),
    TAOrgCul2 = factor(TAOrgCul2, levels = pt5, labels = labeller),
    TFOrgCul1 = factor(TFOrgCul1, levels = pt5, labels = labeller),
    TFOrgCul2 = factor(TFOrgCul2, levels = pt5, labels = labeller),
    TFOrgCul3 = factor(TFOrgCul3, levels = pt5, labels = labeller),
    EXOrgCul1 = factor(EXOrgCul1, levels = pt5, labels = labeller),
    EXOrgCul2 = factor(EXOrgCul2, levels = pt5, labels = labeller),
    gender = factor(gender, levels = c(1, 2), labels = c("남자", "여자")),
    age = factor(age, levels = c(1, 2, 3, 4), 
                 labels = c("20대", "30대", "40대", "50대 이상")),
    marry = factor(marry, levels = c(1, 2), 
                   labels = c("미혼", "기혼")),
    edu = factor(edu, levels = c(1, 2, 3, 4, 5), 
                 labels = c("고졸 이하", "전문대 졸업",
                            "대학 졸업", "석사 졸업",
                            "박사 졸업")),
    position = factor(position, levels = c(9, 8, 7, 6, 5, 4, 3, 2, 1), 
                      labels = c("9급", "8급", "7급", "6급", "5급", "4급",
                                 "3급", "2급", "1급"))
  )

#### Factor analysis -----------------------------------------------------------
##### TAL: Transcational Leadership variables
##### TFL: Transformational Leadership variables
labeled_POS2020 %>% 
  dplyr::select(q19_1, q19_2, #q19_8,
                q19_4, q19_6, q19_7) -> leader
leader.fafit <- fa(leader, nfactors = 2, fm = "pa", rotate = "varimax")
n_factors <- length(leader.fafit$e.values)
print(leader.fafit, cut = .32, sort = TRUE, digits = 3)
leader.fascores <- psych::factor.scores(leader, leader.fafit)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAL = PA2, TFL = PA1)

cronbach.alpha(leader, CI = T)
# 
# library(psych)
# leader.fafit2 <- factanal(leader, 2)
# print(leader.fafit2, digits = 2, sort = T)
# 1 - leader.fafit2$uniquenesses
# 
# load = leader.fafit2$loadings
tibble(
  `요인` = c("q19_1", "q19_2", "q19_4", "q19_6", "q19_7"),
  `거래적 리더십` = load[,2],
  `변혁적 리더십` = load[,1]
) %>% ggplot(aes(x = `거래적 리더십`, y = `변혁적 리더십`)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label =
                                 c("q19_1", "q19_2", "q19_4", "q19_6", "q19_7")))
plot(load, type = "n")
text(load, labels = colnames(leader.fafit2), cex = 0.7)


##### Comm: Communication variable
labeled_POS2020 %>% 
  dplyr::select(q23_1, q23_2, q23_3) -> communication
comm.fafit <- fa(communication, nfactors = 1, fm = "pa", rotate = "varimax")
n_factors <- length(comm.fafit$e.values)
print(comm.fafit, cut = .32, sort = TRUE, digits = 3)
comm.fafit$loadings
comm.fascores <- psych::factor.scores(communication, comm.fafit)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(COMM = PA1)
cronbach.alpha(communication, CI = T)
##### PERFM: Performance Management variables 
labeled_POS2020 %>% 
  dplyr::select(q24_1, q24_2, q24_3) -> performance
perf.fafit <- fa(performance, nfactors = 1, fm = "pa", rotate = "varimax")
n_factors <- length(perf.fafit$e.values)
print(perf.fafit, cut = .32, sort = TRUE, digits = 3)
perf.fafit$loadings
perf.fascores <- psych::factor.scores(performance, perf.fafit)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(PERF = PA1)
cronbach.alpha(performance, CI = T)


##### OrgCul 조직문화
##### TAOrgCul: 조직문화 (거래적 리더십과 유사한); q20_1, q20_2
##### TFOrgCul: 조직문화 (변혁적 리더십과 유사한); q20_3, q20_4, q20_5
##### EXOrgCul: 조직문화 (기타); q20_7, q20_8
labeled_POS2020 %>% 
  dplyr::select(q20_1, q20_2, q20_3, q20_4, q20_5) -> orgcul
orgcul.fafit <- fa(orgcul, nfactors = 2, fm = "pa", rotate = "varimax")
n_factors <- length(orgcul.fafit$e.values)
print(orgcul.fafit, cut = .32, sort = TRUE, digits = 3)
orgcul.fafit$loadings
orgcul.fascores <- psych::factor.scores(orgcul, orgcul.fafit)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAC = PA2, TFC = PA1)
cronbach.alpha(orgcul, CI = T)


##### Merge

labeled_POS2020 %>% 
  left_join(
    leader.fascores, by = c("ID")) %>% 
  left_join(
    comm.fascores, by = c("ID")) %>% 
  left_join(
    perf.fascores, by = c("ID")) %>% 
  left_join(
    orgcul.fascores, by = c("ID")) ->
  labeled_POS2020

#### Make Binaries -----

labeled_POS2020 %>% mutate(
  PSM1_binary = if_else(q29_1 > 3, 1, 0),
  PSM2_binary = if_else(q29_2 > 3, 1, 0),
  PSM3_binary = if_else(q29_3 > 3, 1, 0),
  PSM4_binary = if_else(q29_4 > 3, 1, 0),
  PSM5_binary = if_else(q29_5 > 3, 1, 0),
  PSM6_binary = if_else(q29_6 > 3, 1, 0),
) -> labeled_POS2020


labeled_POS2020
### Descriptive Statistics -------------------------------------
labeled_POS2020 %>%
  dplyr::select(
    PSM1_binary, PSM3_binary,
    PSM6_binary,
    TAL, TFL, COMM, PERF, TAC, TFC,
    dq1, dq2, dq3, dq4_1, dq5_2, dq9_2, dq10, dq11
  ) %>% psych::describe() -> sample
glimpse(sample)
sample$variables <- rownames(sample)
sample %>% as_tibble() -> sample

sample <- sample %>% dplyr::select(
  "변수명" = "variables",
  "관측치" = "n",
  "평균" = "mean",
  "표준편차" = "sd",
  "중앙값" = "median",
  "최소값" = "min",
  "최대값" = "max"
) %>% mutate_if(is.numeric, round, 2)
sample %>% knitr::kable(type = text)

