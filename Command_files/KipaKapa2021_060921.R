################################################################################
### Project: KIPA-KAPA 논문공모전
### Authors: 
###   - Park, Sanghoon
###   - Kang, Jiyoon
###   - Yang, Sungue Susan
### Log update: June 9th 2021 / Raw data download and Rproj set
################################################################################

## Load packages
pacman::p_load(
  ezpickr, tidyverse
)

## Raw data import

### 사회통합실태조사: SIS_연도로 구분

SIS_2014 <- pick("Data/한국행정연구원_사회통합실태조사_데이터_2014.sav")
SIS_2015 <- pick("Data/한국행정연구원_사회통합실태조사_데이터_2015.sav")
SIS_2016 <- pick("Data/한국행정연구원_사회통합실태조사_데이터_2016.sav")
SIS_2017 <- pick("Data/한국행정연구원_사회통합실태조사_데이터_2017.sav")
SIS_2018 <- pick("Data/한국행정연구원_사회통합실태조사_데이터_2018.sav")
SIS_2019 <- pick("Data/한국행정연구원_사회통합실태조사_데이터_2019.sav")
SIS_2020 <- pick("Data/한국행정연구원_사회통합실태조사_데이터_2020.sav")

### 앞으로의 계획

### 1. 연구문제 생각
### 2. 가능한 변수 set 생각
### 3. 설문지별로 동일한 문항이 존재하는지 체크
### 4. 패널이 아니기 때문에 동일한 모델을 시차에 따라 적용한다고 해도
###    인과적 주장은 하기 힘듦.
### 5. 연구설계에 있어서 연구문제를 잘 보여줄 수 있는 방향에 대한 고민
### 6. 2020년만 쓸 것인가? 아니면 다양하게 볼 것인가?


table(SIS_2020$q48_2)

SIS_2020 %>% mutate(
  q48_1_bi = 2 - q48_1,
  q48_2_bi = 2 - q48_2,
  q48_3_bi = 2 - q48_3,
  q48_4_bi = 2 - q48_4,
  q48_5_bi = 2 - q48_5,
  q48_6_bi = 2 - q48_6,
  social_security_insurance = q48_1_bi + q48_2_bi + q48_3_bi + q48_4_bi +
    q48_5_bi + q48_6_bi,
  social_security_old = q49
) -> SIS_2020

SIS_2020 %>% head()
SIS_2020 %>% dplyr::select(
  id, q44_1, q44_2, q44_3, q44_4, q44_5, q44_6, q44_7, q44_8, q44_9,
  q44_10, q44_11, social_security_insurance, social_security_old,
  gender = d1, generation = d2
) -> sub
table(SIS_2020$d5_1_1)
sub %>% tidyr::gather(
  temp, score, -id, -social_security_insurance, -social_security_old, -gender, -generation
) -> sub_long

sub_long 

table(SIS_2020$q44_1, SIS_2020$social_security_insurance)
table(SIS_2020$q44_2, SIS_2020$social_security_insurance)

sub_long %>% ggplot(aes(social_security_insurance)) + geom_bar()
sub_long %>% ggplot(aes(social_security_old)) + geom_bar()

sub_long %>% 
  group_by(temp) %>% 
  summarize(mean_score = mean(score, na.rm = T)) -> sub_long_mean
sub_long_mean$temp <- factor(sub_long_mean$temp,
                             levels = c("q44_1", "q44_2", "q44_3", "q44_4", 
                                        "q44_5", "q44_6", "q44_7", "q44_8", 
                                        "q44_9", "q44_10", "q44_11"),
                             labels = c("정치적\n견해가\n다른\n사람",
                                        "고령층", "청년층", "장애인",
                                        "이성", "동성애자", "타종교",
                                        "전과자", "처음\n만난\n낯선\n사람",
                                        "탈북자", "국내\n거주\n외국인인"))
library(viridis)
sub_long_mean %>%
  ggplot(aes(x = reorder(temp, mean_score), y = mean_score, 
             group = temp,
             color = temp,
             fill = temp)) + geom_col(show.legend = F) + 
  labs(x = "", y = "", 
       title = "귀하는 다음과 같은 사람들에 대한 감정이 어느 정도라고 생각하십니까?",
       subtitle = "0도씨(℃)에서 100도씨(℃) 사이 중 자신의 감정을 온도로 말씀해 주십시오.")  +               
  scale_y_continuous(breaks = c(seq(1, 5, 1))) +
                     # labels = c("0C 차갑다", 
                     #            "25C ", 
                     #            "50℃ ",  
                     #            "75℃ ", 
                     #            "100℃ 따뜻하다")) + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) + theme_bw()

sub_long %>% 
  group_by(temp, social_security_old) %>% 
  summarize(mean_score = mean(score, na.rm = T)) -> sub_long_mean_old
sub_long_mean_old$temp <- factor(sub_long_mean_old$temp,
                             levels = c("q44_1", "q44_2", "q44_3", "q44_4", 
                                        "q44_5", "q44_6", "q44_7", "q44_8", 
                                        "q44_9", "q44_10", "q44_11"),
                             labels = c("정치적\n견해가\n다른\n사람",
                                        "고령층", "청년층", "장애인",
                                        "이성", "동성애자", "타종교",
                                        "전과자", "처음\n만난\n낯선\n사람",
                                        "탈북자", "국내\n거주\n외국인인"))

sub_long_mean_old %>%
  ggplot(aes(x = reorder(temp, mean_score), y = mean_score, 
             group = temp,
             color = temp,
             fill = temp)) + geom_col(show.legend = F) + 
  labs(x = "", y = "", 
       title = "귀하는 다음과 같은 사람들에 대한 감정이 어느 정도라고 생각하십니까?",
       subtitle = "0도씨(℃)에서 100도씨(℃) 사이 중 자신의 감정을 온도로 말씀해 주십시오.")  +               
  scale_y_continuous(breaks = c(seq(1, 5, 1)), 
                     labels = c("0C 차갑다", 
                                "25C ", 
                                "50℃ ",  
                                "75℃ ", 
                                "100℃ 따뜻하다")) + 
  facet_wrap(~social_security_old) + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) + theme_bw()

sub_long %>% 
  group_by(temp, social_security_insurance) %>% 
  summarize(mean_score = mean(score, na.rm = T)) -> sub_long_mean_insecure
sub_long_mean_insecure$temp <- factor(sub_long_mean_insecure$temp,
                                 levels = c("q44_1", "q44_2", "q44_3", "q44_4", 
                                            "q44_5", "q44_6", "q44_7", "q44_8", 
                                            "q44_9", "q44_10", "q44_11"),
                                 labels = c("정치적\n견해가\n다른\n사람",
                                            "고령층", "청년층", "장애인",
                                            "이성", "동성애자", "타종교",
                                            "전과자", "처음\n만난\n낯선\n사람",
                                            "탈북자", "국내\n거주\n외국인인"))

sub_long_mean_insecure %>%
  ggplot(aes(x = reorder(temp, mean_score), y = mean_score, 
             group = temp,
             color = temp,
             fill = temp)) + geom_col(show.legend = F) + 
  labs(x = "", y = "", 
       title = "귀하는 다음과 같은 사람들에 대한 감정이 어느 정도라고 생각하십니까?",
       subtitle = "0도씨(℃)에서 100도씨(℃) 사이 중 자신의 감정을 온도로 말씀해 주십시오.")  +               
  scale_y_continuous(breaks = c(seq(1, 5, 1)), 
                     labels = c("0C 차갑다", 
                                "25C ", 
                                "50℃ ",  
                                "75℃ ", 
                                "100℃ 따뜻하다")) + 
  facet_wrap(~social_security_insurance) + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) + theme_bw()

sub_long %>% 
  group_by(temp, gender) %>% 
  summarize(mean_score = mean(score, na.rm = T)) -> sub_long_mean_gender
sub_long_mean_gender$temp <- factor(sub_long_mean_gender$temp,
                                      levels = c("q44_1", "q44_2", "q44_3", "q44_4", 
                                                 "q44_5", "q44_6", "q44_7", "q44_8", 
                                                 "q44_9", "q44_10", "q44_11"),
                                      labels = c("정치적\n견해가\n다른\n사람",
                                                 "고령층", "청년층", "장애인",
                                                 "이성", "동성애자", "타종교",
                                                 "전과자", "처음\n만난\n낯선\n사람",
                                                 "탈북자", "국내\n거주\n외국인인"))
sub_long_mean_gender$gender <- factor(sub_long_mean_gender$gender,
                                      levels = c("1", "2"),
                                      labels = c("남성", "여성"))
sub_long_mean_gender %>%
  ggplot(aes(x = reorder(temp, mean_score), y = mean_score, 
             group = as.factor(gender),
             color = as.factor(gender),
             fill = as.factor(gender))) + geom_col(
               position = position_dodge(0.8),
               show.legend = T, alpha = 2/3) + 
  labs(x = "", y = "", 
       title = "귀하는 다음과 같은 사람들에 대한 감정이 어느 정도라고 생각하십니까?",
       subtitle = "0도씨(℃)에서 100도씨(℃) 사이 중 자신의 감정을 온도로 말씀해 주십시오.")  +               
  scale_y_continuous(breaks = c(seq(1, 5, 1)), 
                     labels = c("0C 차갑다", 
                                "25C ", 
                                "50℃ ",  
                                "75℃ ", 
                                "100℃ 따뜻하다")) + 
  scale_color_manual(values  = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values  = futurevisions::futurevisions("mars")) + 
#  scale_color_viridis(discrete = TRUE, option = "D")+
#  scale_fill_viridis(discrete = TRUE) + 
  theme_bw() + theme(legend.position = "bottom",
                     legend.title = element_blank())

sub_long %>% 
  group_by(temp, generation) %>% 
  summarize(mean_score = mean(score, na.rm = T)) -> sub_long_mean_gen
sub_long_mean_gen$temp <- factor(sub_long_mean_gen$temp,
                                 levels = c("q44_1", "q44_2", "q44_3", "q44_4", 
                                            "q44_5", "q44_6", "q44_7", "q44_8", 
                                            "q44_9", "q44_10", "q44_11"),
                                 labels = c("정치적\n견해가\n다른\n사람",
                                            "고령층", "청년층", "장애인",
                                            "이성", "동성애자", "타종교",
                                            "전과자", "처음\n만난\n낯선\n사람",
                                            "탈북자", "국내\n거주\n외국인인"))
sub_long_mean_gen$generation <- factor(sub_long_mean_gen$generation,
                                   levels = c("1", "2", "3", "4", "5"),
                                   labels = c("20대", "30대", "40대", 
                                              "50대", "60대 이상"))
sub_long_mean_gen %>%
  ggplot(aes(x = reorder(temp, mean_score), y = mean_score, 
             group = as.factor(generation),
             color = as.factor(generation),
             fill = as.factor(generation))) + geom_col(
               position = position_dodge(0.8),
               show.legend = T, alpha = 2/3) + 
  labs(x = "", y = "", 
       title = "귀하는 다음과 같은 사람들에 대한 감정이 어느 정도라고 생각하십니까?",
       subtitle = "0도씨(℃)에서 100도씨(℃) 사이 중 자신의 감정을 온도로 말씀해 주십시오.")  +               
  scale_y_continuous(breaks = c(seq(1, 5, 1)), 
                     labels = c("0C 차갑다", 
                                "25C ", 
                                "50℃ ",  
                                "75℃ ", 
                                "100℃ 따뜻하다")) + 
  scale_color_manual(values  = futurevisions::futurevisions("pegasi")) + 
  scale_fill_manual(values  = futurevisions::futurevisions("pegasi")) + 
  facet_wrap(~generation, ncol = 2) + 
  #  scale_color_viridis(discrete = TRUE, option = "D")+
  #  scale_fill_viridis(discrete = TRUE) + 
  theme_bw() + theme(legend.position = "bottom",
                     legend.title = element_blank())
