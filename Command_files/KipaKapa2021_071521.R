# Intormation of the Project ---------------------------------------------------
## Project: KIPA-KAPA 논문공모전 ----------------------------------------------
### Authors: ----
###   - Park, Sanghoon
###   - Kang, Jiyoon
###   - Yang, Sungue Susan
### Log update: June 9th 2021  / Raw data download and Rproj set ----
###             July 15th 2021 / EDA for PSM Hypothesis
### You can load the data: 
load("D:/Dropbox/Scholar/2_Graduates/2021_02_Summer/05_KIPA-KAPA/Data/Analysis_data/POS2020_sub.RData")


## Load packages ---------------------------------------------------------------
pacman::p_load(
  ezpickr, magrittr, extrafont, tidyverse
)

## Load function for crosstab
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

### Set the theme of ggplot ---------------------------------------------------
ggplot2::theme_set(theme_bw() + 
                     theme(legend.position = "bottom",
                           legend.title = element_blank(),
                           text=element_text(size=12,  
                                             family="NanumMyeongjo")))

## Raw data import: 공직생활실태조사 -------------------------------------------

POS2020 <- pick("Data/한국행정연구원_공직생활실태조사_데이터_2020.sav")
glimpse(POS2020)

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


POS2020_sub <- POS2020_sub %>% mutate(
  TAL = (q19_1 + q19_2)/2,
  TFL = (q19_4 + q19_6 + q19_7)/3,
  COMM = (q23_1 + q23_2 + q23_3)/3,
  PERF = (q24_1 + q24_2 + q24_3)/3,
  TAC = (q20_1 + q20_2)/2,
  TFC = (q20_3 + q20_4+ q20_5)/3)

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
  rename(# Public Service Motivation
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


      

## Variable EDA ----------------------------------------------------------------
### Check the DV Distribution --------------------------------------------------
#### DV: PSM -------------------------------------------------------------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6) %>%
  tidyr::gather(PSM, value) -> PSM2020
PSM2020$PSM <- factor(PSM2020$PSM,
                      levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))
PSM2020$value <- factor(PSM2020$value,
                        levels = labeller, labels = labeller)
PSM2020 %>% group_by(PSM, value) %>% 
  mutate(n = n(), rate = n/length(POS2020_sub$q29_1)) %>%
  ggplot(aes(x = value, group = PSM, color = PSM, fill = PSM)) + 
  geom_bar(aes(y = (..count..)/length(POS2020_sub$q29_1)),
               alpha = 0.4, show.legend = F) + 
  labs(x = "", y = "",
       title = "DV: 공공봉사동기") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)

ggsave("Documents/Figures/preliminary_fig1_dv.png", 
       height = 6, width = 8.5, dpi = 600)

#### EV1: TALs and TFLs --------------------------------------------------------
labeled_POS2020 %>% dplyr::select(TAL1, TAL2, TAL3, TFL1, TFL2, TFL3) %>%
  tidyr::gather(leadership, value) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         leadership = factor(leadership,
                             levels = c("TAL1", "TAL2", "TAL3",
                                        "TFL1", "TFL2", "TFL3"),
                             labels = c("거래적 리더십1\n나의 상급자는 목표가 달성될 경우 내가 받게 될\n 보상/ 이익에 대해 잘 이해시켜 준다",
                                        "거래적 리더십2\n나의 상급자는 업무성과에 따른 보상/이익을\n 얻기 위해 내가 어떻게 해야 하는지를 구체적으로 알려준다",
                                        "거래적 리더십3\n나의 상급자는 다른 사람이 자신의 능력을\n어떻게 평가하는지 정확히 안다",
                                        "변혁적 리더십1\n나의 상급자는 내가 미래에 지향해야 할\n 확고한 비전을 제시해 준다",
                                        "변혁적 리더십2\n나의 상급자는 내가 새로운 시각에\n 업무를 수행할 수 있도록 장려한다",
                                        "변혁적 리더십3\n나의 상급자는 나 자신이 스스로 \n개발해 나가도록 도와준다"))) %>%
  ggplot(aes(x = value, color = leadership, fill = leadership)) + 
  geom_bar(aes(y = (..count..)/4339),
    alpha = 0.6, show.legend = F) +
  labs(y = "밀도", x = "", 
       title = "리더십 유형에 따른 분포",
       subtitle = "거래적 리더십 vs. 변혁적 리더십") + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~leadership)

ggsave("Documents/Figures/preliminary_fig2_taltfl.png", 
       height = 6, width = 12, dpi = 600)

#### Average PSM by TALs -------------------------------------------------------
##### TAL1: 나의 상급자는 목표가 달성될 경우 내가 받게 될 보상/이익에 대해 잘 이해시켜 준다 ----
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TAL1) %>%
  tidyr::gather(PSM, value, -TAL1) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TAL1, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TAL1, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "거래적 리더십 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "나의 상급자는 목표가 달성될 경우 내가 받게 될 보상/이익에 대해 잘 이해시켜 준다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)
  
ggsave("Documents/Figures/preliminary_fig2a_tal1.png", 
       height = 6, width = 8.5, dpi = 600)

##### TAL2: 나의 상급자는 업무성과에 따른 보상/이익을 얻기 위해 내가 어떻게 해야 -----
#####       하는지를 구체적으로 알려준다

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TAL2) %>%
  tidyr::gather(PSM, value, -TAL2) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TAL2, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TAL2, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "거래적 리더십 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "나의 상급자는 업무성과에 따른 보상/이익을 얻기 위해 내가 어떻게 해야 하는지를 구체적으로 알려준다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig2b_tal2.png", 
       height = 6, width = 8.5, dpi = 600)

##### TAL3: 나의 상급자는 다른 사람이 자신의 능력을 어떻게 평가하는지 정확히 안다 -----

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TAL3) %>%
  tidyr::gather(PSM, value, -TAL3) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TAL3, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TAL3, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "거래적 리더십 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "나의 상급자는 다른 사람이 자신의 능력을 어떻게 평가하는지 정확히 안다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig2c_tal3.png", 
       height = 6, width = 8.5, dpi = 600)

#### Factor Analsys ------------------------------------------------------------

apply(POS2020 %>% 
        dplyr::select(q19_1, q19_2, q19_8,
                      q19_4, q19_6, q19_7), 
      2, shapiro.test)
# all P-values < 0.05, i.e. not normal.
# Multivariate normalit
# To say the data are multivariate normal:
#   z-kurtosis < 5 (Bentler, 2006) and the P-value should be ≥ 0.05.
#   The plot should also form a straight line (Arifin, 2015).
# Run Mardia’s multivariate normality test,

mvn(POS2020 %>%
      dplyr::select(q19_1, q19_2, q19_8,
                    q19_4, q19_6, q19_7), 
    mvnTest = "mardia",
    multivariatePlot = "qq")

# The data are not normally distributed at multivariate level.
# Bartlet's test of sphericity
# Basically it tests whether the correlation matrix is an identity matrix
# (Bartlett, 1951; Gorsuch, 2014).
# A significant test indicates worthwhile correlations between the items 
# (i.e. off-diagonal values are not 0). Test our data,

cortest.bartlett(POS2020 %>%
                   dplyr::select(q19_1, q19_2, q19_8,
                                 q19_4, q19_6, q19_7))

# P-value < 0.05, significant.

# See the scree plot
scree = scree(POS2020 %>%
                dplyr::select(q19_1, q19_2, q19_8,
                              q19_4, q19_6, q19_7))
print(scree)
# Based on our judgement on the scree plot and eigenvalues (of factor analysis), 
# the suitable number of factors = 1.
# The scree plot based on the data is compared to the scree plot based on the 
# randomly generated data (Brown, 2015). 
# The number of factors is the number of points above the intersection between 
# the plots.

parallel = fa.parallel(POS2020 %>%
                         dplyr::select(q19_1, q19_2, q19_8,
                                       q19_4, q19_6, q19_7), 
                       fm = "pa", fa = "fa")

# The parallel-analysis scree plot is also suggestive of 2 factors

print(parallel)

vss(POS2020 %>%
      dplyr::select(q19_1, q19_2, q19_8,
                    q19_4, q19_6, q19_7))

## Do the FA
stats::factanal(POS2020 %>% 
                  dplyr::select(q19_1, q19_2, q19_8,
                                q19_4, q19_6, q19_7), 
                factor = 2) -> factor_taltfl
1 - apply(factor_taltfl$loadings^2,1,sum) # uniqueness
apply(factor_taltfl$loadings^2,1,sum) # communality
factor_taltfl.varimax <- 
  factanal(POS2020 %>% 
             dplyr::select(q19_1, q19_2, q19_8,
                           q19_4, q19_6, q19_7), 
           factors = 2, rotation = "varimax")
factor_taltfl.varimax
plot(factor_taltfl.varimax$loadings[,1], 
     factor_taltfl.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(factor_taltfl.varimax$loadings[,1]-0.02, 
     factor_taltfl.varimax$loadings[,2]+0.02,
     colnames(POS2020 %>% 
                dplyr::select(q19_1, q19_2, q19_8,
                              q19_3, q19_5,
                              q19_4, q19_6, q19_7)),
     col="blue")
abline(h = 0, v = 0)


PA1 = c("q19_1", "q19_2", "q19_8")
PA2 = c("q19_4", "q19_6", "q19_7")
pacman::p_load(ltm)
cronbach.alpha(POS2020 %>% 
                 dplyr::select(q19_1, q19_2, q19_8),
               CI = T)
cronbach.alpha(POS2020 %>% 
                 dplyr::select(q19_4, q19_6, q19_7),
               CI = T)

factor_taltfl.varimax#
bind_cols(POS2020_sub, factor_taltfl.varimax$factors) -> fa

#### Average PSM by TFLs -------------------------------------------------------
##### TFL1: 나의 상급자는 내가 미래에 지향해야 할 확고한 비전을 제시해 준다-----
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TFL1) %>%
  tidyr::gather(PSM, value, -TFL1) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TFL1, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TFL1, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "변혁적 리더십 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "나의 상급자는 내가 미래에 지향해야 할 확고한 비전을 제시해 준다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig3a_tfl1.png", 
       height = 6, width = 8.5, dpi = 600)

##### TFL2: 나의 상급자는 내가 새로운 시각에서 업무를 수행할 수 있도록 장려한다-----
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TFL2) %>%
  tidyr::gather(PSM, value, -TFL2) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TFL2, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TFL2, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "변혁적 리더십 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "나의 상급자는 내가 새로운 시각에서 업무를 수행할 수 있도록 장려한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig3b_tfl2.png", 
       height = 6, width = 8.5, dpi = 600)

##### TFL3: 나의 상급자는 나 자신이 스스로 개발해 나가도록 도와준다-----
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TFL3) %>%
  tidyr::gather(PSM, value, -TFL3) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TFL3, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TFL3, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "변혁적 리더십 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "나의 상급자는 나 자신이 스스로 개발해 나가도록 도와준다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig3c_tfl3.png", 
       height = 6, width = 8.5, dpi = 600)

#### EV2: 협업 및 의사소통 -----------------------------------------------------

labeled_POS2020 %>% dplyr::select(Comm1, Comm2, Comm3) %>%
  tidyr::gather(communication, value) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         communication = factor(communication,
                             levels = c("Comm1", "Comm2", "Comm3"),
                             labels = c("협업/의사소통1\n우리 기관에서는 업무상 협조가 필요한 경우\n부서 간 협업이 대체로 원활하다",
                                        "협업/의사소통2\n우리 기관에서는 부서의 업무를 수행함에 있어\n상하 간(수직적) 의사소통이 원활하다",
                                        "협업/의사소통3\n우리 기관에서는 부서의 업무를 수행함에 있어\n직원 간(수평적) 의사소통이 원활하다"
                                        ))) %>%
  ggplot(aes(x = value, color = communication, fill = communication)) + 
  geom_bar(aes(y = (..count..)/4339),
           alpha = 0.6, show.legend = F) +
  labs(y = "밀도", x = "", 
       title = "협업/의사소통에 대한 응답의 분포",
       subtitle = "") + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~communication)

ggsave("Documents/Figures/preliminary_fig3_comms.png", 
       height = 6, width = 12, dpi = 600)

#### Factor Analsys ------------------------------------------------------------

apply(POS2020_sub %>% 
        dplyr::select(q23_1, q23_2, q23_3), 
      2, shapiro.test)

mvn(POS2020 %>%
      dplyr::select(q23_1, q23_2, q23_3), 
    mvnTest = "mardia",
    multivariatePlot = "qq")

cortest.bartlett(POS2020 %>%
                   dplyr::select(q23_1, q23_2, q23_3))

scree = scree(POS2020 %>% dplyr::select(q23_1, q23_2, q23_3))
print(scree)

parallel = fa.parallel(POS2020 %>%
                         dplyr::select(q23_1, q23_2, q23_3), 
                       fm = "pa", fa = "fa")

print(parallel)

vss(POS2020 %>%
      dplyr::select(q23_1, q23_2, q23_3))

## Do the FA
stats::factanal(POS2020 %>% 
                  dplyr::select(q23_1, q23_2, q23_3), 
                factor = 1) -> factor_comm
1 - apply(factor_comm$loadings^2,1,sum) # uniqueness
apply(factor_comm$loadings^2,1,sum) # communality
factor_comm.varimax <- 
  factanal(POS2020 %>% 
             dplyr::select(q23_1, q23_2, q23_3), 
           factors = 1, rotation = "varimax")
factor_comm.varimax
plot(factor_comm.varimax$loadings[,1],
     factor_comm.varimax$loadings[,1], 
     xlab = "Factor 1", 
     ylab = "Factor 1", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(factor_taltfl.varimax$loadings[,1]-0.02, 
     factor_taltfl.varimax$loadings[,2]+0.02,
     colnames(POS2020 %>% 
                dplyr::select(q23_1, q23_2, q23_3)),
     col="blue")
abline(h = 0, v = 0)


cronbach.alpha(POS2020 %>% 
                 dplyr::select(q23_1, q23_2, q23_3),
               CI = T)

# 하나의 요인으로 묶이기에 충분함

#### Average PSM by Comm -------------------------------------------------------
##### Comm1: 우리 기관에서는 업무상 협조가 필요한 경우 부서 간 협업이  ----
##### 대체로 원활하다

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  Comm1) %>%
  tidyr::gather(PSM, value, -Comm1) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(Comm1, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = Comm1, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "협업/의사소통 응답", 
       title = "협업/의사소통 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관에서는 업무상 협조가 필요한 경우 부서 간 협업이 대체로 원활하다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig4a_comm1.png", 
       height = 6, width = 8.5, dpi = 600)

##### Comm2: 우리 기관에서는 부서의 업무를 수행함에 있어 상하 간(수직적)  ----
#####        의사소통이 원활하다

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  Comm2) %>%
  tidyr::gather(PSM, value, -Comm2) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(Comm2, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = Comm2, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "협업/의사소통 응답", 
       title = "협업/의사소통 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관에서는 부서의 업무를 수행함에 있어 상하 간(수직적) 의사소통이 원활하다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig4b_comm2.png", 
       height = 6, width = 8.5, dpi = 600)

##### Comm3: 우리 기관에서는 업무상 협조가 필요한 경우 부서 간 협업이 대체로  ----
#####        원활하다

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  Comm3) %>%
  tidyr::gather(PSM, value, -Comm3) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(Comm3, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = Comm3, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "협업/의사소통 응답", 
       title = "협업/의사소통 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관에서는 부서의 업무를 수행함에 있어 직원 간(수평적) 의사소통이 원활하다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig4c_comm3.png", 
       height = 6, width = 8.5, dpi = 600)

#### EV3: 성과관리 -------------------------------------------------------------
labeled_POS2020 %>% dplyr::select(PERFM1, PERFM2, PERFM3) %>%
  tidyr::gather(performance, value) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         performance = factor(performance,
                                levels = c("PERFM1", "PERFM2", "PERFM3"),
                                labels = c("조직성과관리1\n우리 기관은 비용절감을 하고 있다",
                                           "조직성과관리2\n우리 기관의 성과는 꾸준히 향상되고 있다.",
                                           "조직성과관리3\n우리 기관 성과의 질은 개선되고 있다"
                                ))) %>%
  ggplot(aes(x = value, color = performance, fill = performance)) + 
  geom_bar(aes(y = (..count..)/4339),
           alpha = 0.6, show.legend = F) +
  labs(y = "밀도", x = "", 
       title = "조직성과관리에 대한 응답의 분포",
       subtitle = "") + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~performance)

ggsave("Documents/Figures/preliminary_fig5_perfom.png", 
       height = 6, width = 12, dpi = 600)

#### Factor Analsys ------------------------------------------------------------


apply(POS2020_sub %>% 
        dplyr::select(q24_1, q24_2, q24_3), 
      2, shapiro.test)

mvn(POS2020 %>%
      dplyr::select(q24_1, q24_2, q24_3), 
    mvnTest = "mardia",
    multivariatePlot = "qq")

cortest.bartlett(POS2020 %>%
                   dplyr::select(q24_1, q24_2, q24_3))

scree = scree(POS2020 %>% dplyr::select(q24_1, q24_2, q24_3))
print(scree)

parallel = fa.parallel(POS2020 %>%
                         dplyr::select(q24_1, q24_2, q24_3), 
                       fm = "pa", fa = "fa")

print(parallel)

vss(POS2020 %>%
      dplyr::select(q24_1, q24_2, q24_3))

## Do the FA
stats::factanal(POS2020 %>% 
                  dplyr::select(q24_1, q24_2, q24_3), 
                factor = 1) -> factor_perf
1 - apply(factor_perf$loadings^2,1,sum) # uniqueness
apply(factor_perf$loadings^2,1,sum) # communality
factor_perf.varimax <- 
  factanal(POS2020 %>% 
             dplyr::select(q24_1, q24_2, q24_3), 
           factors = 1, rotation = "varimax")
factor_perf.varimax
plot(factor_perf.varimax$loadings[,1],
     factor_perf.varimax$loadings[,1], 
     xlab = "Factor 1", 
     ylab = "Factor 1", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(factor_perf.varimax$loadings[,1]-0.02, 
     factor_perf.varimax$loadings[,2]+0.02,
     colnames(POS2020 %>% 
                dplyr::select(q24_1, q24_2, q24_3)),
     col="blue")
abline(h = 0, v = 0)


cronbach.alpha(POS2020 %>% 
                 dplyr::select(q24_1, q24_2, q24_3),
               CI = T)

# 하나의 요인으로 보기에 충분함
pos.pca <- prcomp(POS2020_sub %>% 
                     dplyr::select(q19_1, q19_2, q19_8,
                                   q19_4, q19_6, q19_7),
                   center=TRUE, scale.=TRUE)
summary(pos.pca)
ggbiplot(pos.pca, alpha = 0.1) + theme_bw()
#### Average PSM by PERFM -------------------------------------------------------
##### PERFM1: 우리 기관은 비용절감을 하고 있다 ---------------------------------
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  PERFM1) %>%
  tidyr::gather(PSM, value, -PERFM1) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(PERFM1, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = PERFM1, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "협업/의사소통 응답", 
       title = "조직성과관리 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 비용절감을 하고 있다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig5a_perform1.png", 
       height = 6, width = 8.5, dpi = 600)

##### PERFM2: 우리 기관의 성과는 꾸준히 향상되고 있다. -------------------------
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  PERFM2) %>%
  tidyr::gather(PSM, value, -PERFM2) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(PERFM2, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = PERFM2, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "협업/의사소통 응답", 
       title = "조직성과관리 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관의 성과는 꾸준히 향상되고 있다.") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig5b_perform2.png", 
       height = 6, width = 8.5, dpi = 600)

##### PERFM3: 우리 기관 성과의 질은 개선되고 있다 ------------------------------
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  PERFM3) %>%
  tidyr::gather(PSM, value, -PERFM3) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(PERFM3, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = PERFM3, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "협업/의사소통 응답", 
       title = "조직성과관리 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관 성과의 질은 개선되고 있다.") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig5c_perform3.png", 
       height = 6, width = 8.5, dpi = 600)

#### EV4: 조직문화 ------------------------------------------------------------
#### 잘 묶였나 체크
library(broom)
library(ggfortify) # for plotting pca

culture_pca <- POS2020 %>% 
  dplyr::select(q20_1, q20_2, q20_3, q20_4, q20_5, q20_7, q20_8) %>%
  mutate_all(as.numeric) %>%
  nest() %>% 
  mutate(pca = map(data, ~prcomp(.x, center = TRUE, scale = TRUE)), 
         pca_tidy = map2(pca, data, ~broom::augment(.x, data = .y))) 

culture_pca %>%
  unnest(pca_tidy) %>% 
  summarize(.fittedPC1 = var(.fittedPC1),
            .fittedPC2 = var(.fittedPC2),
            .fittedPC3 = var(.fittedPC3),
            .fittedPC4 = var(.fittedPC4),
            .fittedPC5 = var(.fittedPC5),
            .fittedPC6 = var(.fittedPC6),
            .fittedPC6 = var(.fittedPC7)) %>%
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance))

culture_pca %>% 
  mutate(
    pca_graph = map2(
      .x = pca, 
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE, 
                 data = .y)
    )
  ) %>% 
  pull(pca_graph)

##### 대체로 잘 묶인 것을 확인
 
labeled_POS2020 %>% dplyr::select(TAOrgCul1, TAOrgCul2, TFOrgCul1, TFOrgCul2, 
                                 TFOrgCul3,EXOrgCul1, EXOrgCul2) %>%
  tidyr::gather(culture, value) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         culture = factor(culture,
                              levels = c("TAOrgCul1", "TAOrgCul2", "TFOrgCul1",
                                         "TFOrgCul2", "TFOrgCul3", "EXOrgCul1", 
                                         "EXOrgCul2"),
                              labels = c("거래적 조직문화1\n우리 기관은 계획수립/목표설정/목표달성을\n강조한다",
                                         "거래적 조직문화2\n우리 기관은 경쟁력/성과/실적을\n중시한다",
                                         "혁신적 조직문화1\n우리 기관은 창의성/혁신성/도전을\n강조한다",
                                         "혁신적 조직문화2\n우리 기관은 새로운 도전과제 해결을 위해\n직원들의 직관/통찰력, 성장을\n중시한다",
                                         "혁신적 조직문화3\n우리 기관은 참여/협력/신뢰를\n강조한다",
                                         "기타 조직문화1\n우리 기관은 안정성/일관성/규칙준수를\n강조한다",
                                         "기타 조직문화1\n우리 기관은 문서화/책임/통제/정보관리를\n중시한다"
                              ))) %>%
  ggplot(aes(x = value, color = culture, fill = culture)) + 
  geom_bar(aes(y = (..count..)/4339),
           alpha = 0.6, show.legend = F) +
  labs(y = "밀도", x = "", 
       title = "조직문화에 대한 응답의 분포",
       subtitle = "거래적 vs. 혁신적 vs. 기타") + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~culture)

ggsave("Documents/Figures/preliminary_fig6_culture.png", 
       height = 8, width = 12, dpi = 600)

#### Factor Analsys ------------------------------------------------------------
apply(POS2020_sub %>% 
        dplyr::select(q20_1, q20_2, q20_3,
                      q20_4, q20_5, q20_7,
                      q20_8), 
      2, shapiro.test)

mvn(POS2020 %>%
      dplyr::select(q20_1, q20_2, q20_3,
                    q20_4, q20_5, q20_7,
                    q20_8), 
    mvnTest = "mardia",
    multivariatePlot = "qq")

cortest.bartlett(POS2020 %>%
                   dplyr::select(q20_1, q20_2, q20_3,
                                 q20_4, q20_5, q20_7,
                                 q20_8))

scree = scree(POS2020 %>% 
                dplyr::select(q20_1, q20_2, q20_3,
                              q20_4, q20_5, q20_7,
                              q20_8))
print(scree)

parallel = fa.parallel(POS2020 %>%
                         dplyr::select(q20_1, q20_2, q20_3,
                                       q20_4, q20_5, q20_7,
                                       q20_8), 
                       fm = "pa", fa = "fa")

print(parallel)

vss(POS2020 %>%
      dplyr::select(q20_1, q20_2, q20_3,
                    q20_4, q20_5, q20_7,
                    q20_8))

## Do the FA
stats::factanal(POS2020 %>% 
                  dplyr::select(q20_1, q20_2, q20_3,
                                q20_4, q20_5, q20_7,
                                q20_8), 
                factor = 3) -> factor_culture
1 - apply(factor_culture$loadings^2,1,sum) # uniqueness
apply(factor_culture$loadings^2,1,sum) # communality
factor_culture.varimax <- 
  factanal(POS2020 %>% 
             dplyr::select(q20_1, q20_2, q20_3,
                           q20_4, q20_5, q20_7,
                           q20_8), 
           factors = 3, rotation = "varimax")
factor_culture.varimax
plot(factor_culture.varimax$loadings[,1],
     factor_culture.varimax$loadings[,2], 
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(factor_perf.varimax$loadings[,1]-0.02, 
     factor_perf.varimax$loadings[,2]+0.02,
     colnames(POS2020 %>% 
                dplyr::select(q20_1, q20_2, q20_3,
                              q20_4, q20_5, q20_7,
                              q20_8)),
     col="blue")
abline(h = 0, v = 0)


cronbach.alpha(POS2020 %>% 
                 dplyr::select(q24_1, q24_2, q24_3),
               CI = T)

# 하나의 요인으로 보기에 충분함

#### Average PSM by OrgCul: 조직문화 -------------------------------------------
##### TOArgCul: 거래적 조직문화 ------------------------------------------------

###### TAOrgCul1: 우리 기관은 계획수립/목표설정/목표달성을 강조한다 ------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TAOrgCul1) %>%
  tidyr::gather(PSM, value, -TAOrgCul1) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TAOrgCul1, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TAOrgCul1, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "조직문화에 대한 응답", 
       title = "거래적 조직문화 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 계획수립/목표설정/목표달성을 강조한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig6a_culture1.png", 
       height = 6, width = 8.5, dpi = 600)


###### TAOrgCul2: 우리 기관은 경쟁력/성과/실적을 중시한다 ----------------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TAOrgCul2) %>%
  tidyr::gather(PSM, value, -TAOrgCul2) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TAOrgCul2, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TAOrgCul2, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "조직문화에 대한 응답", 
       title = "거래적 조직문화 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 경쟁력/성과/실적을 중시한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig6b_culture2.png", 
       height = 6, width = 8.5, dpi = 600)

##### TFOrgCul: 혁신적 조직문화 ------------------------------------------------
###### TFOrgCul1: 우리 기관은 창의성/혁신성/도전을 강조한다 --------------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TFOrgCul1) %>%
  tidyr::gather(PSM, value, -TFOrgCul1) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TFOrgCul1, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TFOrgCul1, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "조직문화에 대한 응답", 
       title = "혁신적 조직문화 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 창의성/혁신성/도전을 강조한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig6c_culture3.png", 
       height = 6, width = 8.5, dpi = 600)


###### TFOrgCul2: 우리 기관은 새로운 도전과제 해결을 위해         --------------
######            직원들의 직관/통찰력, 성장을 중시한다
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TFOrgCul2) %>%
  tidyr::gather(PSM, value, -TFOrgCul2) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TFOrgCul2, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TFOrgCul2, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "조직문화에 대한 응답", 
       title = "혁신적 조직문화 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 새로운 도전과제 해결을 위해 직원들의 직관/통찰력, 성장을 중시한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig6d_culture4.png", 
       height = 6, width = 8.5, dpi = 600)

###### TFOrgCul3: 우리 기관은 참여/협력/신뢰를 강조한다 ------------------------
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  TFOrgCul3) %>%
  tidyr::gather(PSM, value, -TFOrgCul3) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(TFOrgCul3, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = TFOrgCul3, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "조직문화에 대한 응답", 
       title = "혁신적 조직문화 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 참여/협력/신뢰를 강조한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig6e_culture5.png", 
       height = 6, width = 8.5, dpi = 600)


##### EXOrgCul: 기타 조직문화 ------------------------------------------------

###### EXOrgCul1: 우리 기관은 안정성/일관성/규칙준수를 강조한다 ----------------
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  EXOrgCul1) %>%
  tidyr::gather(PSM, value, -EXOrgCul1) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(EXOrgCul1, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = EXOrgCul1, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "조직문화에 대한 응답", 
       title = "기타 조직문화 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 안정성/일관성/규칙준수를 강조한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig6f_culture6.png", 
       height = 6, width = 8.5, dpi = 600)


###### EXOrgCul2: 우리 기관은 문서화/책임/통제/정보관리를 중시한다 -------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  EXOrgCul2) %>%
  tidyr::gather(PSM, value, -EXOrgCul2) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(EXOrgCul2, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = EXOrgCul2, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "조직문화에 대한 응답", 
       title = "기타 조직문화 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "우리 기관은 문서화/책임/통제/정보관리를 중시한다") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig6g_culture7.png", 
       height = 6, width = 8.5, dpi = 600)

#### Ctrl1: 성별 ----------------------------------------------------------------
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  gender) %>%
  tidyr::gather(PSM, value, -gender) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  ggplot(aes(x = value, color = gender, fill = gender, group = gender)) + 
  geom_bar(
    aes(y = (..count..)/4339),
    alpha = 0.6, position = position_dodge2(1)) +
  labs(y = "", x = "공공봉사동기에 대한 응답", 
       title = "성별에 따른 공공봉사동기 유형 별 분포") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig7_gender.png", 
       height = 8, width = 12, dpi = 600)

#### Ctrl2: 연령 ----------------------------------------------------------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  age) %>%
  ggplot(aes(x = PSM1, color = age, fill = age, group = age)) + 
  geom_bar(
    aes(y = (..count..)/sum(..count..)),
    alpha = 0.6, position = position_dodge2(1),
    show.legend = F) +
  labs(y = "", x = "", 
       subtitle = "국가와 국민을 위한 봉사는 나에게 매우 중요하다",
       title = "연령대에 따른 공공봉사동기 응답의 분포",
       caption = "y축은 전체 응답자 4339명에 대한 각 응답의 비율임.") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  facet_wrap(~age, scales = "free") -> fig8_panel1

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  age) %>%
  ggplot(aes(x = PSM2, color = age, fill = age, group = age)) + 
  geom_bar(
    aes(y = (..count..)/sum(..count..)),
    alpha = 0.6, position = position_dodge2(1),
    show.legend = F) +
  labs(y = "", x = "", 
       subtitle = "다른 사람들의 권리를 옹호하기 위해 나설 용의가 있다",
       title = "연령대에 따른 공공봉사동기 응답의 분포",
       caption = "y축은 전체 응답자 4339명에 대한 각 응답의 비율임.") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  facet_wrap(~age, scales = "free") -> fig8_panel2

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  age) %>%
  ggplot(aes(x = PSM3, color = age, fill = age, group = age)) + 
  geom_bar(
    aes(y = (..count..)/sum(..count..)),
    alpha = 0.6, position = position_dodge2(1),
    show.legend = F) +
  labs(y = "", x = "", 
       subtitle = "사회에 바람직한 변화를 가져오는 것이 더 의미가 있다",
       title = "연령대에 따른 공공봉사동기 응답의 분포",
       caption = "y축은 전체 응답자 4339명에 대한 각 응답의 비율임.") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  facet_wrap(~age, scales = "free") -> fig8_panel3
  
labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  age) %>%
  ggplot(aes(x = PSM4, color = age, fill = age, group = age)) + 
  geom_bar(
    aes(y = (..count..)/sum(..count..)),
    alpha = 0.6, position = position_dodge2(1),
    show.legend = F) +
  labs(y = "", x = "", 
       subtitle = "사회의 선을 위해 큰 희생을 감수할 준비가 되어 있다",
       title = "연령대에 따른 공공봉사동기 응답의 분포",
       caption = "y축은 전체 응답자 4339명에 대한 각 응답의 비율임.") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  facet_wrap(~age, scales = "free") -> fig8_panel4

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  age) %>%
  ggplot(aes(x = PSM5, color = age, fill = age, group = age)) + 
  geom_bar(
    aes(y = (..count..)/sum(..count..)),
    alpha = 0.6, position = position_dodge2(1),
    show.legend = F) +
  labs(y = "", x = "", 
       subtitle = "일상을 통해 얼마나 서로 의존적인 존재인지를 되새긴다",
       title = "연령대에 따른 공공봉사동기 응답의 분포",
       caption = "y축은 전체 응답자 4339명에 대한 각 응답의 비율임.") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  facet_wrap(~age, scales = "free") -> fig8_panel5

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  age) %>%
  ggplot(aes(x = PSM6, color = age, fill = age, group = age)) + 
  geom_bar(
    aes(y = (..count..)/sum(..count..)),
    alpha = 0.6, position = position_dodge2(1),
    show.legend = F) +
  labs(y = "", x = "", 
       subtitle = "정책과정에 참여해 사회적으로 의미있는 일을 하고싶다",
       title = "연령대에 따른 공공봉사동기 응답의 분포",
       caption = "y축은 전체 응답자 4339명에 대한 각 응답의 비율임.") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = futurevisions::futurevisions("mars")) +
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  facet_wrap(~age, scales = "free") -> fig8_panel6

ggsave("Documents/Figures/preliminary_fig8a_age1.png", fig8_panel1, 
       height = 6, width = 8.5, dpi = 600)
ggsave("Documents/Figures/preliminary_fig8b_age2.png", fig8_panel2, 
       height = 6, width = 8.5, dpi = 600)
ggsave("Documents/Figures/preliminary_fig8c_age3.png", fig8_panel3, 
       height = 6, width = 8.5, dpi = 600)
ggsave("Documents/Figures/preliminary_fig8d_age4.png", fig8_panel4, 
       height = 6, width = 8.5, dpi = 600)
ggsave("Documents/Figures/preliminary_fig8e_age5.png", fig8_panel5, 
       height = 6, width = 8.5, dpi = 600)
ggsave("Documents/Figures/preliminary_fig8f_age6.png", fig8_panel6, 
       height = 6, width = 8.5, dpi = 600)

#### Ctrl3: 현재 직급 ----------------------------------------------------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  position) %>%
  tidyr::gather(PSM, value, -position) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))) %>%
  group_by(position, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = position, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "현재 직급 응답에 따른 공공봉사동기 평균 점수",
       subtitle = "9급 - 1급",
       caption = "응답자 중 1급은 존재하지 않으며, 2급도 전체 4339명 중 17명 (0.4%)에 불과함") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig9_position.png", 
       height = 6, width = 8.5, dpi = 600)

#### Ctrl4: 근무시간 -----------------------------------------------------------
##### 총 근무시간 --------------------------------------------------------------
haven::print_labels(labeled_POS2020$work_hr)
haven::print_labels(labeled_POS2020$extra_work)

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  work_hr) %>%
  tidyr::gather(PSM, value, -work_hr) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는 나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해 나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이 더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을 감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인 존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로 의미있는 일을 하고싶다")),
         work_hr = factor(work_hr, levels = c(1, 2, 3, 4, 5),
                          labels = c("35시간\n미만",
                                     "35~40시간\n미만",
                                     "40~45시간\n미만",
                                     "45~50시간\n미만",
                                     "50시간\n이상"))) %>%
  group_by(work_hr, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = work_hr, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "주 평균 근무시간에 따른 공공봉사동기 평균 점수",
       subtitle = "1개월 기준 1주일 평균 근무시간") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig10_workinghours.png", 
       height = 10, width = 12, dpi = 600)

##### 총 초과근무시간 ----------------------------------------------------------

labeled_POS2020 %>% dplyr::select(PSM1, PSM2, PSM3, PSM4, PSM5, PSM6, 
                                  extra_work) %>%
  tidyr::gather(PSM, value, -extra_work) %>%
  mutate(value = factor(value, levels = labeller,
                        labels = labeller),
         value = as.numeric(value),
         PSM = factor(PSM, levels = c("PSM1", "PSM2", "PSM3", "PSM4", "PSM5", "PSM6"),
                      labels = c("1) 국가와 국민을 위한 봉사는 나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해 나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이 더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을 감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인 존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로 의미있는 일을 하고싶다")),
         extra_work = factor(extra_work, levels = c(1, 2, 3, 4, 5),
                          labels = c("정시퇴근(없음)",
                                     "5시\n미만",
                                     "5~10시간\n미만",
                                     "5~10시간\n미만",
                                     "15시\n이상"))) %>%
  group_by(extra_work, PSM) %>%
  summarize(mean = mean(value, na.rm = T)) %>% ungroup() %>%
  ggplot(aes(x = extra_work, y = mean, color = PSM, fill = PSM)) + 
  geom_col(alpha = 0.6, show.legend = F) +
  labs(y = "평균 점수", x = "", 
       title = "주 평균 초과근무시간에 따른 공공봉사동기 평균 점수",
       subtitle = "1개월 기준 1주일 평균 초과근무시간") + 
  scale_color_manual(values = futurevisions::futurevisions("pegasi")) +
  scale_fill_manual(values = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~PSM)  

ggsave("Documents/Figures/preliminary_fig11_extra_working.png", 
       height = 10, width = 12, dpi = 600)


## 경험분석 --------------------------------------------------------------------
### 테스트로 DV를 binary로 바꿔서 logit 한 번 보자

POS2020_sub %<>% mutate(
  PSM1_binary = if_else(q29_1 > 3, 1, 0))
library(magrittr)
POS2020 %<>% mutate(
  PSM1_binary = if_else(q29_1 > 3, 1, 0))
POS2020 %>% group_by(ID) %>% mutate(
  TAL_diff = q19_2 - q19_1
) %>% ggplot(aes(TAL_diff)) + geom_density()

POS2020 %>% group_by(ID) %>% mutate(
  TAL_diff_q2q1 = q19_2 - q19_1,
  TAL_diff_q8q1 = q19_8 - q19_1,
  TAL_diff_q8q2 = q19_8 - q19_2
) %>% 
  dplyr::select(TAL_diff_q2q1,
                TAL_diff_q8q1, 
                TAL_diff_q8q2) %>%
  tidyr::gather(diff, value, -ID) %>%
  ggplot(aes(value, color = diff)) + facet_wrap(~diff) + 
  geom_density(show.legend = F) + theme_bw()

POS2020 %>% group_by(ID) %>% mutate(
  TFL_diff_q4q6 = q19_4 - q19_6,
  TFL_diff_q6q7 = q19_6 - q19_7,
  TFL_diff_q7q4 = q19_7 - q19_4
) %>% 
  dplyr::select(TFL_diff_q4q6,
                TFL_diff_q6q7, 
                TFL_diff_q7q4) %>%
  tidyr::gather(diff, value, -ID) %>%
  ggplot(aes(value, ..scaled.., color = diff)) + 
  facet_wrap(~diff) + 
  geom_density(show.legend = F) + theme_bw()

POS2020 %>% group_by(ID) %>% mutate(
  `q19_1 - q19_4` = q19_1 - q19_4,
  `q19_1 - q19_6` = q19_1 - q19_6,
  `q19_1 - q19_7` = q19_1 - q19_7,
  `q19_2 - q19_4` = q19_2 - q19_4,
  `q19_2 - q19_6` = q19_2 - q19_6,
  `q19_2 - q19_7` = q19_2 - q19_7,
  `q19_8 - q19_4` = q19_8 - q19_4,
  `q19_8 - q19_6` = q19_8 - q19_6,
  `q19_8 - q19_7` = q19_8 - q19_7,
) %>% 
  dplyr::select(`q19_1 - q19_4`,
                `q19_1 - q19_6`, 
                `q19_1 - q19_7`,
                `q19_2 - q19_4`,
                `q19_2 - q19_6`,
                `q19_2 - q19_7`,
                `q19_8 - q19_4`,
                `q19_8 - q19_6`,
                `q19_8 - q19_7`) %>%
  tidyr::gather(diff, value, -ID) %>%
  ggplot(aes(value, color = diff)) + 
  geom_density(show.legend = F) + facet_wrap(~diff) + theme_bw() + 
  theme(legend.position = "bottom")


glm(PSM1_binary ~ TAL + TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = POS2020_sub, family = "binomial") -> model3_base

glm(PSM1_binary ~ 
      TAL*COMM + TFL + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = POS2020_sub, family = "binomial") -> model1_logit

glm(PSM1_binary ~ TAL + TFL*COMM + PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = POS2020_sub, family = "binomial") -> model2_logit

glm(PSM1_binary ~ TAL*TFL + COMM +
      PERF + TAC + TFC +
      dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
    data = POS2020_sub, family = "binomial") -> model3_logit

texreg::screenreg(list(model3_base, model3_logit, 
                       model1_logit, model2_logit))

texreg::htmlreg(list(model3_base, model3_logit, 
                    model1_logit, model2_logit),
                single.row = T,
                file = "Documents/Tables/Table 2.doc", doctype = T,
                head.tag = T, body.tag = T)
??texreg
broom::tidy(model1_logit, conf.int = T) %>%
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
  labs(title = "모델1: 거래적 리더십 X 협업/의사소통 모델",
      x = "변수", y = "추정량") + 
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  coord_flip() + theme_bw() + theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       text=element_text(size=12,
                                         family="NanumMyeongjo")) -> model1

broom::tidy(model2_logit, conf.int = T) %>%
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
  labs(title = "모델2: 변혁적 리더십 X 협업/의사소통 모델",
       x = "변수", y = "추정량") + 
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  coord_flip() -> model2

broom::tidy(model3_logit, conf.int = T) %>%
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
  labs(title = "모델3: 거래적 리더십 X 변혁적 리더십 모델",
       x = "변수", y = "추정량") + 
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  coord_flip() -> model3

broom::tidy(model3_base, conf.int = T) %>%
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

beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model1_logit), 
                            Sigma = vcov(model1_logit))
COMM <- c(seq(1, 5, 0.5))
Xi.mat <- cbind(1, mean(POS2020_sub$TAL, na.rm = T),
                COMM,
                mean(POS2020_sub$TFL, na.rm = T),
                mean(POS2020_sub$PERF, na.rm = T),
                mean(POS2020_sub$TAC, na.rm = T),
                mean(POS2020_sub$TFC, na.rm = T),
                median(POS2020_sub$dq1, na.rm = T),
                median(POS2020_sub$dq2, na.rm = T),
                median(POS2020_sub$dq3, na.rm = T),
                median(POS2020_sub$dq4_1, na.rm = T),
                median(POS2020_sub$dq5_2, na.rm = T),
                median(POS2020_sub$dq9_2, na.rm = T),
                median(POS2020_sub$dq10, na.rm = T),
                median(POS2020_sub$dq11, na.rm = T),
                I(mean(POS2020_sub$TAL, na.rm = T)*COMM))

Xib.mat.sim <- plogis(beta_draws %*% t(Xi.mat))

Xibcoef <- tibble(
  Communication = COMM,
  Mean = apply(Xib.mat.sim, 2, mean),
  ll = apply(Xib.mat.sim, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim, 2, quantile, probs = 0.975)
)

Xibcoef %>% ggplot(aes(x = Communication, y = Mean)) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       subtitle = "협업/의사소통 수준에 따른 거래적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme(plot.subtitle = element_text(size = 8))
ggsave("Documents/Figures/Analysis_fig1.png",
       width = 6, height = 5, dpi = 600)

beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model2_logit), 
                            Sigma = vcov(model2_logit))
COMM <- c(seq(1, 5, 0.5))
Xi.mat <- cbind(1, mean(POS2020_sub$TAL, na.rm = T),
                mean(POS2020_sub$TFL, na.rm = T),
                COMM,
                mean(POS2020_sub$PERF, na.rm = T),
                mean(POS2020_sub$TAC, na.rm = T),
                mean(POS2020_sub$TFC, na.rm = T),
                median(POS2020_sub$dq1, na.rm = T),
                median(POS2020_sub$dq2, na.rm = T),
                median(POS2020_sub$dq3, na.rm = T),
                median(POS2020_sub$dq4_1, na.rm = T),
                median(POS2020_sub$dq5_2, na.rm = T),
                median(POS2020_sub$dq9_2, na.rm = T),
                median(POS2020_sub$dq10, na.rm = T),
                median(POS2020_sub$dq11, na.rm = T),
                I(mean(POS2020_sub$TFL, na.rm = T)*COMM))

Xib.mat.sim <- plogis(beta_draws %*% t(Xi.mat))

Xibcoef <- tibble(
  Communication = COMM,
  Mean = apply(Xib.mat.sim, 2, mean),
  ll = apply(Xib.mat.sim, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim, 2, quantile, probs = 0.975)
)

Xibcoef %>% ggplot(aes(x = Communication, y = Mean)) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       subtitle = "협업/의사소통 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme(plot.subtitle = element_text(size = 8))
ggsave("Documents/Figures/Analysis_fig2.png",
       width = 6, height = 5, dpi = 600)

beta_draws <- MASS::mvrnorm(n = 4000, mu = coef(model3_logit), 
                            Sigma = vcov(model3_logit))
TAL <- c(seq(1, 5, 0.5))
Xi.mat <- cbind(1, TAL,
                mean(POS2020_sub$TFL, na.rm = T),
                mean(POS2020_sub$COMM, na.rm = T),
                mean(POS2020_sub$PERF, na.rm = T),
                mean(POS2020_sub$TAC, na.rm = T),
                mean(POS2020_sub$TFC, na.rm = T),
                median(POS2020_sub$dq1, na.rm = T),
                median(POS2020_sub$dq2, na.rm = T),
                median(POS2020_sub$dq3, na.rm = T),
                median(POS2020_sub$dq4_1, na.rm = T),
                median(POS2020_sub$dq5_2, na.rm = T),
                median(POS2020_sub$dq9_2, na.rm = T),
                median(POS2020_sub$dq10, na.rm = T),
                median(POS2020_sub$dq11, na.rm = T),
                I(mean(POS2020_sub$TFL, na.rm = T)*TAL))

Xib.mat.sim <- plogis(beta_draws %*% t(Xi.mat))

Xibcoef <- tibble(
  `Transactional Leadership` = TAL,
  Mean = apply(Xib.mat.sim, 2, mean),
  ll = apply(Xib.mat.sim, 2, quantile, probs = 0.025),
  ul = apply(Xib.mat.sim, 2, quantile, probs = 0.975)
)

Xibcoef %>% ggplot(aes(x = `Transactional Leadership`, y = Mean)) + 
  geom_line() + 
  geom_line(aes(y = ll), colour = "grey50", linetype = "dashed") +
  geom_line(aes(y = ul), colour = "grey50", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "예측확률",
       subtitle = "거래적 리더십 수준에 따른 변혁적 리더십의 공공봉사동기의 긍정적 응답에 대한 예측확률 변화") + 
  theme(plot.subtitle = element_text(size = 8))
ggsave("Documents/Figures/Analysis_fig1.png",
       width = 6, height = 5, dpi = 600)



### 테스트로 DV에 대한 ordered logit 한 번 보자
MASS::polr(as.factor(q29_1) ~ q19_1 + q19_2 + q19_8 +
             q19_4 + q19_6 + q19_7 + 
             q23_1 + q23_2 + q23_3 +
             q24_1 + q24_2 + q24_3 +
             q20_1 + q20_2 + q20_3 + q20_4 + q20_5 + 
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
     data = POS2020_sub, Hess=TRUE) -> model1

MASS::polr(as.factor(q29_2) ~ q19_1 + q19_2 + q19_8 +
             q19_4 + q19_6 + q19_7 + 
             q23_1 + q23_2 + q23_3 +
             q24_1 + q24_2 + q24_3 +
             q20_1 + q20_2 + q20_3 + q20_4 + q20_5 + 
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model2

MASS::polr(as.factor(q29_3) ~ q19_1 + q19_2 + q19_8 +
             q19_4 + q19_6 + q19_7 + 
             q23_1 + q23_2 + q23_3 +
             q24_1 + q24_2 + q24_3 +
             q20_1 + q20_2 + q20_3 + q20_4 + q20_5 + 
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model3

MASS::polr(as.factor(q29_4) ~ q19_1 + q19_2 + q19_8 +
             q19_4 + q19_6 + q19_7 + 
             q23_1 + q23_2 + q23_3 +
             q24_1 + q24_2 + q24_3 +
             q20_1 + q20_2 + q20_3 + q20_4 + q20_5 + 
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model4

MASS::polr(as.factor(q29_5) ~ q19_1 + q19_2 + q19_8 +
             q19_4 + q19_6 + q19_7 + 
             q23_1 + q23_2 + q23_3 +
             q24_1 + q24_2 + q24_3 +
             q20_1 + q20_2 + q20_3 + q20_4 + q20_5 + 
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model5

MASS::polr(as.factor(q29_6) ~ q19_1 + q19_2 + q19_8 +
             q19_4 + q19_6 + q19_7 + 
             q23_1 + q23_2 + q23_3 +
             q24_1 + q24_2 + q24_3 +
             q20_1 + q20_2 + q20_3 + q20_4 + q20_5 + 
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model6

texreg::screenreg(list(model1, model2, model3, model4, model5, model6))

beta_draws <- MASS::mvrnorm(n = 1000, mu = c(model1_int$coefficients, model1_int$zeta), 
                            Sigma = vcov(model1_int))

Xi.mat <- cbind(mean(POS2020_sub$TAL, na.rm = T),
                TFL,
                mean(POS2020_sub$COMM, na.rm = T),
                mean(POS2020_sub$PERF, na.rm = T),
                mean(POS2020_sub$TAC, na.rm = T),
                mean(POS2020_sub$TFC, na.rm = T),
                median(POS2020_sub$dq1, na.rm = T),
                median(POS2020_sub$dq2, na.rm = T),
                median(POS2020_sub$dq3, na.rm = T),
                median(POS2020_sub$dq4_1, na.rm = T),
                median(POS2020_sub$dq5_2, na.rm = T),
                median(POS2020_sub$dq9_2, na.rm = T),
                median(POS2020_sub$dq10, na.rm = T),
                median(POS2020_sub$dq11, na.rm = T),
                I(mean(POS2020_sub$TAL, na.rm = T)*TFL))

pr.logit <- as.vector(plogis(Xi.mat %*% t(beta_draws)))

pr.tibble <- tibble(
  Communication = COMM,
  Mean = apply(pr.logit, 2, mean)
)

mat.plot <- matrix(NA, nrow = 8, ncol = 3)
rownames(mat.plot) <- names(coef(logit))[-1]
colnames(mat.plot) <- c("Mean", "Lower", "Upper")
# Compute the first differences
for (i in 1:8){ # loop over covariates
  # Profiles
  x.1 <- x
  x.1[i + 1] <- x[i + 1] + 1 # add 1
  # Compute the predicted probs
  pr <- as.vector(plogis(x %*% t(logit.draws)))
  pr.1 <- as.vector(plogis(x.1 %*% t(logit.draws)))
  # First differences
  fdiff <- pr.1 - pr
  mat.plot[i, 1] <- mean(fdiff) # simulated mean
  mat.plot[i, 2:3] <- quantile(fdiff, probs = c(0.025, 0.975)) # simulated 95%CIs
}
mat.plot <- as_tibble(mat.plot)
var.labels <- c("Party ID", "Gender: Female", "Income", 
                "Age: Over 65", "Race: Black", "Race: Latinx", 
                "Race: Others", "Edu: BA and Grads")
mat.plot <- mat.plot %>% mutate(
  names = factor(var.labels, levels = rev(var.labels)),
  sig = case_when(
    Lower < 0 & Upper > 0 ~ "Unsignificant",
    T ~ "Significnat") %>% 
    parse_factor(., levels = c("Unsignificant","Significnat"), 
                 ordered = T, include_na = F)
)

apply(Xib.mat.sim, 2, mean)


MASS::polr(as.factor(q29_1) ~ TAL + TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model1_re

MASS::polr(as.factor(q29_2) ~ TAL + TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model2_re

MASS::polr(as.factor(q29_3) ~ TAL + TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model3_re

MASS::polr(as.factor(q29_4) ~ TAL + TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model4_re

MASS::polr(as.factor(q29_5) ~ TAL + TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model5_re

MASS::polr(as.factor(q29_6) ~ TAL + TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model6_re

texreg::screenreg(list(model1_re, model2_re, model3_re, 
                       model4_re, model5_re, model6_re))


MASS::polr(as.factor(q29_1) ~ TAL*TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model1_int

MASS::polr(as.factor(q29_2) ~ TAL*TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model2_int

MASS::polr(as.factor(q29_3) ~ TAL*TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model3_int

MASS::polr(as.factor(q29_4) ~ TAL*TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model4_int

MASS::polr(as.factor(q29_5) ~ TAL*TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model5_int

MASS::polr(as.factor(q29_6) ~ TAL*TFL + COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model6_int


texreg::screenreg(list(model1_int, model2_int, model3_int, 
                       model4_int, model5_int, model6_int))

TFL <- seq(1, 5, 1)
library(mvtnorm)
beta_draws <- MASS::mvrnorm(n = 1000, mu = c(model1_int$coefficients, model1_int$zeta), 
                            Sigma = vcov(model1_int))

Xi.mat <- cbind(mean(POS2020_sub$TAL, na.rm = T),
                TFL,
                mean(POS2020_sub$COMM, na.rm = T),
                mean(POS2020_sub$PERF, na.rm = T),
                mean(POS2020_sub$TAC, na.rm = T),
                mean(POS2020_sub$TFC, na.rm = T),
                median(POS2020_sub$dq1, na.rm = T),
                median(POS2020_sub$dq2, na.rm = T),
                median(POS2020_sub$dq3, na.rm = T),
                median(POS2020_sub$dq4_1, na.rm = T),
                median(POS2020_sub$dq5_2, na.rm = T),
                median(POS2020_sub$dq9_2, na.rm = T),
                median(POS2020_sub$dq10, na.rm = T),
                median(POS2020_sub$dq11, na.rm = T),
                I(mean(POS2020_sub$TAL, na.rm = T)*TFL))

Xib.mat.sim <- beta_draws[,1:15] %*% t(Xi.mat)

prdv1 <- 1/(1+exp(-(beta_draws[, 16] - Xib.mat.sim))) - 0
prdv2 <- 1/(1+exp(-(beta_draws[, 17] - Xib.mat.sim))) - prdv1
prdv3 <- 1/(1+exp(-(beta_draws[, 18] - Xib.mat.sim))) - prdv2
prdv4 <- 1/(1+exp(-(beta_draws[, 19] - Xib.mat.sim))) - prdv3
prdv5 <- (1 - prdv1 - prdv2 - prdv3 - prdv4)
prdv1.mean <- apply(prdv1, 2, mean)
prdv1.se <- apply(prdv1, 2, quantile, c(0.025, 0.975))
prdv2.mean <- apply(prdv2, 2, mean)
prdv2.se <- apply(prdv2, 2, quantile, c(0.025, 0.975))
prdv3.mean <- apply(prdv3, 2, mean)
prdv3.se <- apply(prdv3, 2, quantile, c(0.025, 0.975)) 
prdv4.mean <- apply(prdv4, 2, mean)
prdv4.se <- apply(prdv4, 2, quantile, c(0.025, 0.975)) 
prdv5.mean <- apply(prdv5, 2, mean)
prdv5.se <- apply(prdv5, 2, quantile, c(0.025, 0.975)) 

prTFL1 <- tibble(TFL = TFL,
                 Mean = prdv1.mean,
                 ll = prdv1.se[1, ],
                 ul = prdv1.se[2, ],
                 DV = "Y = 1")
prTFL2 <- tibble(TFL = TFL,
                 Mean = prdv2.mean,
                 ll = prdv2.se[1, ],
                 ul = prdv2.se[2, ],
                 DV = "Y = 2")

prTFL3 <- tibble(TFL = TFL,
                 Mean = prdv3.mean,
                 ll = prdv3.se[1, ],
                 ul = prdv3.se[2, ],
                 DV = "Y = 3")

prTFL4 <- tibble(TFL = TFL,
                 Mean = prdv4.mean,
                 ll = prdv4.se[1, ],
                 ul = prdv4.se[2, ],
                 DV = "Y = 4")

prTFL5 <- tibble(TFL = TFL,
                 Mean = prdv5.mean,
                 ll = prdv5.se[1, ],
                 ul = prdv5.se[2, ],
                 DV = "Y = 5")

prTFL.add <- bind_rows(prTFL1, prTFL2, prTFL3, prTFL4, prTFL5) %>%
  mutate(DV = DV %>% 
           parse_factor(., levels= c("Y = 1", "Y = 2", "Y = 3", "Y = 4", "Y = 5"),
                        ordered = T, include_na = F))
pal <- c(futurevisions::futurevisions("mars")[1],
         futurevisions::futurevisions("mars")[2],
         futurevisions::futurevisions("mars")[3],
         futurevisions::futurevisions("mars")[4],
         futurevisions::futurevisions("mars")[6])
prTFL.add %>%
  ggplot(aes(x = TFL, y = Mean, color = DV)) + 
  geom_linerange(aes(ymin = ll, ymax = ul), 
                  show.legend = F, size = 1.5) +
  geom_point(size = 4) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) + 
  scale_x_continuous(breaks=c(seq(1, 5, 1))) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  labs(y = "Predicted probability", x = "TFL") +
  facet_wrap(~DV)

ggsave("Documents/Figures/preliminary_fig12_ologit_pr_test.png",
       width = 10, height = 6, dpi = 600)



MASS::polr(as.factor(q29_1) ~ TAL*COMM + TFL*COMM + PERF + TAC + TFC +
             dq1 + dq2 + dq3 + dq4_1 + dq5_2 + dq9_2 + dq10 + dq11,
           data = POS2020_sub, Hess=TRUE) -> model1_intcom
texreg::screenreg(model1_intcom)


beta_draws <- MASS::mvrnorm(n = 1000, 
                            mu = c(model1_intcom$coefficients, 
                                   model1_intcom$zeta), 
                            Sigma = vcov(model1_intcom))
COMM <- c(1:5)
Xi.mat <- cbind(mean(POS2020_sub$TAL, na.rm = T),
                COMM,
                mean(POS2020_sub$TFL, na.rm = T),
                mean(POS2020_sub$PERF, na.rm = T),
                mean(POS2020_sub$TAC, na.rm = T),
                mean(POS2020_sub$TFC, na.rm = T),
                median(POS2020_sub$dq1, na.rm = T),
                median(POS2020_sub$dq2, na.rm = T),
                median(POS2020_sub$dq3, na.rm = T),
                median(POS2020_sub$dq4_1, na.rm = T),
                median(POS2020_sub$dq5_2, na.rm = T),
                median(POS2020_sub$dq9_2, na.rm = T),
                median(POS2020_sub$dq10, na.rm = T),
                median(POS2020_sub$dq11, na.rm = T),
                I(mean(POS2020_sub$TAL, na.rm = T)*COMM),
                I(mean(POS2020_sub$TFL, na.rm = T)*COMM))


Xib.mat.sim <- beta_draws[,1:16] %*% t(Xi.mat)

prdv1 <- 1/(1+exp(-(beta_draws[, 17] - Xib.mat.sim))) - 0
prdv2 <- 1/(1+exp(-(beta_draws[, 18] - Xib.mat.sim))) - prdv1
prdv3 <- 1/(1+exp(-(beta_draws[, 19] - Xib.mat.sim))) - prdv2
prdv4 <- 1/(1+exp(-(beta_draws[, 20] - Xib.mat.sim))) - prdv3
prdv5 <- (1 - prdv1 - prdv2 - prdv3 - prdv4)
prdv1.mean <- apply(prdv1, 2, mean)
prdv1.se <- apply(prdv1, 2, quantile, c(0.025, 0.975))
prdv2.mean <- apply(prdv2, 2, mean)
prdv2.se <- apply(prdv2, 2, quantile, c(0.025, 0.975))
prdv3.mean <- apply(prdv3, 2, mean)
prdv3.se <- apply(prdv3, 2, quantile, c(0.025, 0.975)) 
prdv4.mean <- apply(prdv4, 2, mean)
prdv4.se <- apply(prdv4, 2, quantile, c(0.025, 0.975)) 
prdv5.mean <- apply(prdv5, 2, mean)
prdv5.se <- apply(prdv5, 2, quantile, c(0.025, 0.975)) 

prCOMM1 <- tibble(COMM = COMM,
                 Mean = prdv1.mean,
                 ll = prdv1.se[1, ],
                 ul = prdv1.se[2, ],
                 DV = "Y = 전혀 그렇지 않다")
prCOMM2 <- tibble(COMM = COMM,
                 Mean = prdv2.mean,
                 ll = prdv2.se[1, ],
                 ul = prdv2.se[2, ],
                 DV = "Y = 그렇지 않다")
prCOMM3 <- tibble(COMM = COMM,
                 Mean = prdv3.mean,
                 ll = prdv3.se[1, ],
                 ul = prdv3.se[2, ],
                 DV = "Y = 보통이다")
prCOMM4 <- tibble(COMM = COMM,
                 Mean = prdv4.mean,
                 ll = prdv4.se[1, ],
                 ul = prdv4.se[2, ],
                 DV = "Y = 그렇다")
prCOMM5 <- tibble(COMM = COMM,
                 Mean = prdv5.mean,
                 ll = prdv5.se[1, ],
                 ul = prdv5.se[2, ],
                 DV = "Y = 매우 그렇다")

prCOMM.add <- bind_rows(prCOMM1, prCOMM2, prCOMM3, prCOMM4, prCOMM5) %>%
  mutate(DV = factor(DV, levels= c("Y = 전혀 그렇지 않다",
                                   "Y = 그렇지 않다", 
                                   "Y = 보통이다",
                                   "Y = 그렇다", 
                                   "Y = 매우 그렇다")),
         COMM = factor(COMM, 
                       levels = c(1, 2, 3, 4, 5),
                       labels= c("전혀\n그렇지\n않다",
                                 "그렇지\n않다", 
                                 "보통이다",
                                 "그렇다", 
                                 "매우\n그렇다")))
pal <- c(futurevisions::futurevisions("mars")[1],
         futurevisions::futurevisions("mars")[2],
         futurevisions::futurevisions("mars")[3],
         futurevisions::futurevisions("mars")[4],
         futurevisions::futurevisions("mars")[6])
prCOMM.add %>%
  ggplot(aes(x = COMM, y = Mean, color = DV)) + 
  geom_linerange(aes(ymin = ll, ymax = ul), 
                 show.legend = F, size = 1.5) +
  geom_point(size = 4, show.legend = F) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  labs(y = "예측확률", x = "협업/의사소통 수준",
       title = "공공봉사동기에 대한 결정요인 (순위형 로지스틱)",
       subtitle = "국가와 국민을 위한 봉사는 나에게 매우 중요하다") +
  facet_wrap(~DV)
ggsave("Documents/Figures/preliminary_fig13_ologit_pr_test.png",
       width = 10, height = 6, dpi = 600)
