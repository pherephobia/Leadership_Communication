# Intormation of the Project ---------------------------------------------------
## Visualization script for paper
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
table(labeled_POS2020$PSM6_binary)
## Load packages ---------------------------------------------------------------
pacman::p_load(
  ezpickr, magrittr, psych, extrafont, tidyverse,
  tidyquant
)

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
          plot.title = element_text(face = "bold", size = rel(3)),
          axis.title = element_text(face = "bold", size = rel(2.5)),
          strip.text = element_text(face = "bold", size = rel(2.5), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}
Sys.setlocale("LC_ALL", "Korean")

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
#  scale_fill_tq() + 
#  scale_color_tq() + 
#  theme_tq()

ggsave("Documents/Figures/preliminary_fig1_dv.png", 
       height = 6, width = 8.5, dpi = 600)

#### DV: PSM binaries ----------------------------------------------------------

labeled_POS2020 %>% dplyr::select(ends_with("_binary")) %>%
  tidyr::gather(PSM, value) -> PSM2020_binaries
PSM2020_binaries$PSM <- factor(PSM2020_binaries$PSM,
                      levels = c("PSM1_binary", "PSM2_binary", "PSM3_binary", 
                                 "PSM4_binary", "PSM5_binary", "PSM6_binary"),
                      labels = c("1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                                 "2) 다른 사람들의 권리를 옹호하기 위해\n나설 용의가 있다",
                                 "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다",
                                 "4) 사회의 선을 위해 큰 희생을\n감수할 준비가 되어 있다",
                                 "5) 일상을 통해 얼마나 서로 의존적인\n존재인지를 되새긴다",
                                 "6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다"))
PSM2020_binaries %>% group_by(PSM) %>% 
  dplyr::filter(PSM %in% c(#"1) 국가와 국민을 위한 봉사는\n나에게 매우 중요하다",
                           #"6) 정책과정에 참여해 사회적으로\n의미있는 일을 하고싶다",
                           "3) 사회에 바람직한 변화를 가져오는 것이\n더 의미가 있다"
                           )) %>%
  mutate(n = n(), rate = n/4339) -> 
  dv_plot
dv_plot %>% group_by(value) %>% summarize(count = n(),
                                          ratio = count/4339) ->
  dv_plot

dv_plot %>%
  mutate(value_rev = 
           case_when(
             value == 0L ~ 1L,
             value == 1L ~ 0L,
             T ~ NA_integer_
           )) -> dv_plot

dv_plot %>%
  ggplot(aes(x = value_rev, y = ratio, fill = factor(value_rev))) + 
  geom_col(alpha = 0.7, show.legend = F) + 
  labs(x = "", y = ""#,
       #title = "DV: 공공봉사동기"
       ) + 
  geom_text(aes(label = paste0(round(ratio, 2)*100, "%")), 
            position = position_stack(0.8), color = "white") + 
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.6)) + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("부정적 응답", "긍정적 응답")) + 
  scale_fill_manual(values = c(futurevisions::futurevisions("mars")[1],
                               futurevisions::futurevisions("mars")[3])) + 
  theme_nice()

table(labeled_POS2020$PSM6_binary)
table(labeled_POS2020$PSM6)
ggsave("Documents/Figures/fig1_dv_revised.png", 
       height = 3, width = 3, dpi = 1200)

labeled_POS2020 %>% dplyr::select(PSM6) %>%
  group_by(PSM6) %>% summarize(count = n(),
                               ratio = count/4339) ->
  dv_raw_plot
library(forcats)
dv_raw_plot %>% mutate(
  PSM6_rev = factor(PSM6,
                levels = c("매우\n그렇다", "그렇다",
                           "보통이다",
                           "그렇지\n않다", "전혀\n그렇지\n않다"))
) -> dv_raw_plot

dv_raw_plot %>%
  ggplot(aes(x = PSM6, y = ratio, fill = factor(PSM6))) + 
  geom_col(alpha = 0.7, show.legend = F) + 
  labs(x = "", y = ""#,
       #title = "DV: 공공봉사동기"
  ) + 
  geom_text(aes(label = paste0(round(ratio, 2)*100, "%")), 
            position = position_stack(0.8), color = "black") + 
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.6)) + 
  scale_fill_manual(values = c("#DB3A2F", "#F9A38A", "#EAB33A", "#83C6DD", "#275D8E")) + 
  theme_nice()

ggsave("Documents/Figures/fig1_dv_raw_revised.png", 
       height = 3, width = 3, dpi = 1200)

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