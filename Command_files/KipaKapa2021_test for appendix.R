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

POS2020 <- pick("Data/한국행정연구원_공직생활실태조사_데이터_2020.sav")
POS2019 <- pick("Data/한국행정연구원_공직생활실태조사_데이터_2019.sav")
POS2018 <- pick("Data/한국행정연구원_공직생활실태조사_데이터_2018.sav")
POS2017 <- pick("Data/한국행정연구원_공직생활실태조사_데이터_2017.sav")
POS <- bind_rows(
  POS2017 %>% mutate(year = "2017"),
  POS2018 %>% mutate(year = "2018"),
  POS2019 %>% mutate(year = "2019"),
  POS2020 %>% mutate(year = "2020")
)

POS %>% dplyr::select(
  ID, 기관명ID, year,
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
) -> POS_sub


### Make a labeled subset for EDA ----------------------------------------------
pt5 <- c(1, 2, 3, 4, 5)
labeller <- c("전혀\n그렇지\n않다",
              "그렇지\n않다",
              "보통이다", "그렇다", "매우\n그렇다")
labeled_POS <- POS_sub %>%
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

labeled_POS2020 <- labeled_POS %>% dplyr::filter(year == 2020)
labeled_POS2019 <- labeled_POS %>% dplyr::filter(year == 2019)
labeled_POS2018 <- labeled_POS %>% dplyr::filter(year == 2018)
labeled_POS2017 <- labeled_POS %>% dplyr::filter(year == 2017)
names(labeled_POS2017)
##### TAL: Transcational Leadership variables
##### TFL: Transformational Leadership variables



labeled_POS2017 %>% 
  dplyr::select(q19_1, q19_2, #q19_8, q19_7m
                q19_4, q19_6) -> leader2017
labeled_POS2018 %>% 
  dplyr::select(q19_1, q19_2, #q19_8, q19_7m
                q19_4, q19_6) -> leader2018
labeled_POS2019 %>% 
  dplyr::select(q19_1, q19_2, #q19_8, q19_7m
                q19_4, q19_6) -> leader2019
labeled_POS2020 %>% 
  dplyr::select(q19_1, q19_2, #q19_8, q19_7m
                q19_4, q19_6) -> leader2020

leader.fafit2017 <- fa(leader2017, nfactors = 2, fm = "pa", rotate = "varimax")
leader.fafit2018 <- fa(leader2018, nfactors = 2, fm = "pa", rotate = "varimax")
leader.fafit2019 <- fa(leader2019, nfactors = 2, fm = "pa", rotate = "varimax")
leader.fafit2020 <- fa(leader2020, nfactors = 2, fm = "pa", rotate = "varimax")
n_factors17 <- length(leader.fafit2017$e.values)
n_factors18 <- length(leader.fafit2018$e.values)
n_factors19 <- length(leader.fafit2019$e.values)
n_factors20 <- length(leader.fafit2020$e.values)

leader.fascores17 <- psych::factor.scores(leader2017, leader.fafit2017)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAL = PA1, TFL = PA2)
leader.fascores18 <- psych::factor.scores(leader2018, leader.fafit2018)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAL = PA1, TFL = PA2)

leader.fascores19 <- psych::factor.scores(leader2019, leader.fafit2019)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAL = PA1, TFL = PA2)

leader.fascores20 <- psych::factor.scores(leader2020, leader.fafit2020)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAL = PA2, TFL = PA1)

##### Comm: Communication variable
labeled_POS2017 %>% 
  dplyr::select(q23_1, q23_2, q23_3) -> communication17
labeled_POS2018 %>% 
  dplyr::select(q23_1, q23_2, q23_3) -> communication18
labeled_POS2019 %>% 
  dplyr::select(q23_1, q23_2, q23_3) -> communication19
labeled_POS2020 %>% 
  dplyr::select(q23_1, q23_2, q23_3) -> communication20


comm.fafit17 <- fa(communication17, nfactors = 1, fm = "pa", rotate = "varimax")
comm.fafit18 <- fa(communication18, nfactors = 1, fm = "pa", rotate = "varimax")
comm.fafit19 <- fa(communication19, nfactors = 1, fm = "pa", rotate = "varimax")
comm.fafit20 <- fa(communication20, nfactors = 1, fm = "pa", rotate = "varimax")
n_factors17 <- length(comm.fafit17$e.values)
n_factors18 <- length(comm.fafit18$e.values)
n_factors19 <- length(comm.fafit19$e.values)
n_factors20 <- length(comm.fafit20$e.values)


comm.fascores17 <- psych::factor.scores(communication17, comm.fafit17)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(COMM = PA1)
comm.fascores18 <- psych::factor.scores(communication18, comm.fafit18)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(COMM = PA1)
comm.fascores19 <- psych::factor.scores(communication19, comm.fafit19)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(COMM = PA1)
comm.fascores20 <- psych::factor.scores(communication20, comm.fafit20)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(COMM = PA1)


##### PERFM: Performance Management variables 
labeled_POS2017 %>% 
  dplyr::select(q24_1, q24_2, q24_3) -> performance17
labeled_POS2018 %>% 
  dplyr::select(q24_1, q24_2, q24_3) -> performance18
labeled_POS2019 %>% 
  dplyr::select(q24_1, q24_2, q24_3) -> performance19
labeled_POS2020 %>% 
  dplyr::select(q24_1, q24_2, q24_3) -> performance20


perf.fafit17 <- fa(performance17, nfactors = 1, fm = "pa", rotate = "varimax")
perf.fafit18 <- fa(performance18, nfactors = 1, fm = "pa", rotate = "varimax")
perf.fafit19 <- fa(performance19, nfactors = 1, fm = "pa", rotate = "varimax")
perf.fafit20 <- fa(performance20, nfactors = 1, fm = "pa", rotate = "varimax")
n_factors17 <- length(perf.fafit17$e.values)
n_factors18 <- length(perf.fafit18$e.values)
n_factors19 <- length(perf.fafit19$e.values)
n_factors20 <- length(perf.fafit20$e.values)


perf.fascores17 <- psych::factor.scores(performance17, perf.fafit17)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(PERF = PA1)
perf.fascores18 <- psych::factor.scores(performance18, perf.fafit18)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(PERF = PA1)
perf.fascores19 <- psych::factor.scores(performance19, perf.fafit19)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(PERF = PA1)
perf.fascores20 <- psych::factor.scores(performance20, perf.fafit20)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(PERF = PA1)


##### OrgCul 조직문화
##### TAOrgCul: 조직문화 (거래적 리더십과 유사한); q20_1, q20_2
##### TFOrgCul: 조직문화 (변혁적 리더십과 유사한); q20_3, q20_4, q20_5
##### EXOrgCul: 조직문화 (기타); q20_7, q20_8
labeled_POS2017 %>% 
  dplyr::select(q20_1, q20_2, q20_3, q20_4, q20_5) -> orgcul17
labeled_POS2018 %>% 
  dplyr::select(q20_1, q20_2, q20_3, q20_4, q20_5) -> orgcul18
labeled_POS2019 %>% 
  dplyr::select(q20_1, q20_2, q20_3, q20_4, q20_5) -> orgcul19
labeled_POS2020 %>% 
  dplyr::select(q20_1, q20_2, q20_3, q20_4, q20_5) -> orgcul20

orgcul.fafit17 <- fa(orgcul17, nfactors = 2, fm = "pa", rotate = "varimax")
orgcul.fafit18 <- fa(orgcul18, nfactors = 2, fm = "pa", rotate = "varimax")
orgcul.fafit19 <- fa(orgcul19, nfactors = 2, fm = "pa", rotate = "varimax")
orgcul.fafit20 <- fa(orgcul20, nfactors = 2, fm = "pa", rotate = "varimax")

n_factors17 <- length(orgcul.fafit17$e.values)
n_factors18 <- length(orgcul.fafit18$e.values)
n_factors19 <- length(orgcul.fafit19$e.values)
n_factors20 <- length(orgcul.fafit20$e.values)


orgcul.fascores17 <- psych::factor.scores(orgcul17, orgcul.fafit17)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAC = PA2, TFC = PA1)
orgcul.fascores18 <- psych::factor.scores(orgcul18, orgcul.fafit18)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAC = PA2, TFC = PA1)
orgcul.fascores19 <- psych::factor.scores(orgcul19, orgcul.fafit19)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAC = PA2, TFC = PA1)
orgcul.fascores20 <- psych::factor.scores(orgcul20, orgcul.fafit20)$scores %>%
  as_tibble() %>% rowid_to_column(var = "ID") %>%
  rename(TAC = PA2, TFC = PA1)

##### Merge

labeled_POS2017 %>% 
  left_join(
    leader.fascores17, by = c("ID")) %>% 
  left_join(
    comm.fascores17, by = c("ID")) %>% 
  left_join(
    perf.fascores17, by = c("ID")) %>% 
  left_join(
    orgcul.fascores17, by = c("ID")) ->
  labeled_POS2017
dlookr::describe(labeled_POS2017)

labeled_POS2018 %>% dplyr::select(-ID) %>% rowid_to_column(var = "ID") %>%
  left_join(
    leader.fascores18, by = c("ID")) %>% 
  left_join(
    comm.fascores18, by = c("ID")) %>% 
  left_join(
    perf.fascores18, by = c("ID")) %>% 
  left_join(
    orgcul.fascores18, by = c("ID")) ->
  labeled_POS2018
dlookr::describe(labeled_POS2018)

labeled_POS2019 %>% dplyr::select(-ID) %>% rowid_to_column(var = "ID") %>%
  left_join(
    leader.fascores19, by = c("ID")) %>% 
  left_join(
    comm.fascores19, by = c("ID")) %>% 
  left_join(
    perf.fascores19, by = c("ID")) %>% 
  left_join(
    orgcul.fascores19, by = c("ID")) ->
  labeled_POS2019
dlookr::describe(labeled_POS2019)

labeled_POS2020 %>% 
  left_join(
    leader.fascores20, by = c("ID")) %>% 
  left_join(
    comm.fascores20, by = c("ID")) %>% 
  left_join(
    perf.fascores20, by = c("ID")) %>% 
  left_join(
    orgcul.fascores20, by = c("ID")) ->
  labeled_POS2020

labeled_POS <- bind_rows(
  labeled_POS2017, labeled_POS2018, labeled_POS2019, labeled_POS2020  
)

#### Make Binaries -----

labeled_POS %>% mutate(
  PSM1_binary = if_else(q29_1 > 3, 1, 0),
  
  PSM3_binary = if_else(q29_3 > 3, 1, 0)
  
  
  
) -> labeled_POS



### Descriptive Statistics -------------------------------------
labeled_POS %>%
  dplyr::select(
    PSM1_binary, PSM3_binary,
    TAL, TFL, COMM, PERF, TAC, TFC,
    dq1, dq2, dq3, dq4_1, dq5_2, dq9_2, dq10, dq11, year
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

POS_sub %>% dplyr::select(year, q19_1, q19_2, q19_4, q19_6, q19_7) %>%
  drop_na() %>% tidyr::gather(variable, score, -year) %>%
  group_by(variable) %>% count()
POS_sub$year

labeled_POS %>% dplyr::select(
  PSM1_binary, TAL, COMM, TFL, PERF, TAC, TFC, dq1, dq2, dq3, dq4_1,
  dq5_2, year) -> sample
psych::describe(sample)


glm(PSM1_binary ~ 
      TAL + COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model1_logit0

glm(PSM1_binary ~ 
      TAL*COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model1_logit1

glm(PSM1_binary ~ TAL + TFL*COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model1_logit2

glm(PSM1_binary ~ TAL*TFL + COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model1_logit3



glm(PSM3_binary ~ 
      TAL + COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model3_logit0

glm(PSM3_binary ~ 
      TAL*COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model3_logit1

glm(PSM3_binary ~ TAL + TFL*COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model3_logit2

glm(PSM3_binary ~ TAL*TFL + COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq5_2 +
      as.factor(year),
    data = labeled_POS, family = "binomial") -> 
  model3_logit3

texreg::screenreg(list(model1_logit0, model1_logit1, model1_logit2, model1_logit3,
                       model3_logit0, model3_logit1, model3_logit2, model3_logit3))

## test

beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model1_logit1), 
                            Sigma = vcov(model1_logit1))
TFL <- c(
  min(labeled_POS$TFL, na.rm = T),
  mean(labeled_POS$TFL, na.rm = T),
  max(labeled_POS$TFL, na.rm = T)
)
summary(labeled_POS$COMM)
coef(model1_logit1)
COMM <- seq(-3, 3, 0.5)
Xi.mat.low <- cbind(1, mean(labeled_POS$TAL, na.rm = T),
                    COMM,
                    TFL[1],
                    mean(labeled_POS$PERF, na.rm = T),
                    mean(labeled_POS$TAC, na.rm = T),
                    mean(labeled_POS$TFC, na.rm = T),
                    median(labeled_POS$dq1, na.rm = T),
                    median(labeled_POS$dq2, na.rm = T),
                    median(labeled_POS$dq3, na.rm = T),
                    median(labeled_POS$dq5_2, na.rm = T),
                    0, 0, 0,
                    I(TFL[1]*COMM))
dim(Xi.mat.low)
dim(beta_draws)
Xib.mat.sim.low <- plogis(beta_draws %*% t(Xi.mat.low))

Xibcoef.low <- tibble(
  `변혁적 리더십` = "낮음",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.low, 2, mean),
  ll = apply(Xib.mat.sim.low, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.low, 2, quantile, probs = 0.975)
)

Xi.mat.mean <- cbind(1, mean(labeled_POS$TAL, na.rm = T),
                    COMM,
                    TFL[2],
                    mean(labeled_POS$PERF, na.rm = T),
                    mean(labeled_POS$TAC, na.rm = T),
                    mean(labeled_POS$TFC, na.rm = T),
                    median(labeled_POS$dq1, na.rm = T),
                    median(labeled_POS$dq2, na.rm = T),
                    median(labeled_POS$dq3, na.rm = T),
                    median(labeled_POS$dq5_2, na.rm = T),
                    0, 0, 0,
                    I(TFL[2]*COMM))

Xib.mat.sim.mean <- plogis(beta_draws %*% t(Xi.mat.mean))

Xibcoef.mean <- tibble(
  `변혁적 리더십` = "평균",
  Communication = COMM,
  Mean = apply(Xib.mat.sim.mean, 2, mean),
  ll = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim.mean, 2, quantile, probs = 0.975)
)

Xi.mat.high <- cbind(1, mean(labeled_POS$TAL, na.rm = T),
                     COMM,
                     TFL[3],
                     mean(labeled_POS$PERF, na.rm = T),
                     mean(labeled_POS$TAC, na.rm = T),
                     mean(labeled_POS$TFC, na.rm = T),
                     median(labeled_POS$dq1, na.rm = T),
                     median(labeled_POS$dq2, na.rm = T),
                     median(labeled_POS$dq3, na.rm = T),
                     median(labeled_POS$dq5_2, na.rm = T),
                     0, 0, 0,
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
