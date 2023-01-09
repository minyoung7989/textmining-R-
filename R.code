raw_batman <- readLines("darknight.txt", encoding = "UTF-8")
batman <- raw_batman %>% as_tibble() %>% mutate(movie = 'batman')

raw_superman <- readLines("superman.txt", encoding = "UTF-8")
superman <- raw_superman %>% as_tibble() %>% mutate(movie = 'superman')

###두 대본 합치기
bind_movie <- bind_rows(batman, superman) %>% select(movie, value)

###기본적인 전처리
library(stringr)
movies <- bind_movie %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
#토큰화
library(tidytext)
library(KoNLP)

movies <- movies %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

#두 평점의 단어 빈도 구하기
frequency <- movies %>%
  count(movie, word) %>%
  filter(str_count(word)>1)
head(frequency)

#자주사용되는 단어추출
top10 <- frequency %>%
  group_by(movie) %>%
  slice_max(n, n=10, with_ties = F)

top10 %>% filter(movie == 'batman')
top10 %>% filter(movie == 'superman')

#막대그래프만들기
library(ggplot2)
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = movie)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~movie)

#
ggplot(top10, aes(x = reorder_within(word, n, movie),
                  y = n,
                  fill = movie)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~movie,
             scale = "free_y") +
  scale_x_reordered() +
  labs(x = NULL)

##단어빈도를 와이드프롬으로 변환
library(tidyr)
frequency_wide <- frequency %>%
  pivot_wider(names_from = movie,
              values_from = n,
              values_fill = list(n=0))

frequency_wide <- frequency_wide %>%
  mutate(ratio_bat = ((batman + 1)/(sum(batman + 1))),
         ratio_sup = ((superman + 1)/(sum(superman + 1))))

###오즈비변수추가
frequency_wide <- frequency_wide %>% mutate(odds_ratio = ratio_bat/ratio_sup)
frequency_wide

###오즈비가 가장 높거나 가장 낮은 단어 추출
top10 <- frequency_wide %>% filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <=10)
top10 %>% arrange(-odds_ratio)

#로그오즈비 구하기
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(odds_ratio))
frequency_wide  

#로그오즈비 기준 top10 추출및 그래프
top10 <- frequency_wide %>%
  group_by(movie = ifelse(log_odds_ratio >0, 'batman', 'superman')) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10 %>% arrange(-log_odds_ratio) %>%
  select(word, log_odds_ratio, movie)

###막대그래프 그리기 (로그오즈비 그래프)
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = movie)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)


###
raw_rogan <- readLines("rogan.txt", encoding = "UTF-8")
rogan <- raw_rogan %>% as_tibble() %>% mutate(movie = 'rogan')

raw_ironman <- readLines("ironman.txt", encoding = "UTF-8")
ironman <- raw_ironman %>% as_tibble() %>% mutate(movie = 'ironman')
###대본합치기
bind_movie <- bind_rows(batman, superman, rogan, ironman) %>% select(movie, value)
###전처리
movies <- bind_movie %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
###토큰화
movies <- movies %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

###단어빈도구하기
frequency <- movies %>%
  count(movie, word) %>%
  filter(str_count(word)>1)

frequency

#TF-IDF구하기

frequency <- frequency %>%
  bind_tf_idf(term = word,
              document = movie,
              n = n) %>%
  arrange(-tf_idf)
###막대그래프 그리기

###주요단어추출
top10 <- frequency %>%
  group_by(movie) %>%
  slice_max(tf_idf, n =10, with_ties=F)


#그래프 그리기
ggplot(top10, aes(x=reorder_within(word, tf_idf, movie),
                  y = tf_idf,
                  fill = movie)) +
  geom_col(show.legend = F) +
  coord_flip()+
  facet_wrap(~movie, scales = 'free', ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL)
