# 데이터 불러오기
library(tidyverse)
library(readxl)

file_name <- 'C:/Users/jspar/Desktop/유튜브 댓글 시각화/유튜브 댓글 크롤링.xlsx'  # 데이터 파일 디렉토리 설정하기

raw_data <- read_xlsx(file_name) %>% mutate(id = row_number())


# 폰트 설정
library(showtext)

font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()



# 품사 기준으로 단어 토큰화하기
library(tidytext)
library(KoNLP)

data_pos <- raw_data %>% 
  mutate(내용 = str_replace_all(내용, "[a-z, A-Z, @, ㅋ]", " "),
         내용 = str_squish(내용)) %>% 
  unnest_tokens(input = '내용', output = word,
                token = SimplePos22,
                drop = F) %>% print()


# 품사 분리하여 행 구성하기
data_pos <- data_pos %>% 
  separate_rows(word, sep = "[+]") %>% print()


# 명사, 동사, 형용사를 추출해 결합한 후 두 글자 이상만 남기기
noun_pvpa <- data_pos %>% 
  separate_rows(word, sep = "[+]") %>% 
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>% 
  filter(str_count(word) >= 2) %>% 
  arrange(id) %>% print()


# 단어 동시 출현 빈도 구하기
library(widyr)

pair <- noun_pvpa %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = T) %>% print()


# 동시 출현 네트워크 만들기
library(tidygraph)

graph_data <- pair %>% 
  filter(n >= 44) %>% 
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap())) %>% 
  print()

library(ggraph)

set.seed(1234)
ggraph(graph_data, layout = "fr") +
  geom_edge_link(color = "gray50",
                 alpha = 0.5) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = T) + 
  scale_size(range = c(5, 15)) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 4,
                 family = "nanumgothic") + theme_graph()



# 파이 계수 구하기
word_cors <- noun_pvpa %>% 
  add_count(word) %>% 
  filter(n >= 40) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = T) %>% print()

word_cors %>% 
  filter(item1 == '백신')  # 단어별 파이 계수 값 확인하기


target <- c("증상", "병원")  # 파이 계수 값을 구할 단어 지정하기

top_cors <- word_cors %>% 
  filter(item1 %in% target) %>% 
  group_by(item1) %>% 
  slice_max(correlation, n = 8) %>% print()


# 파이 계수로 막대 그래프 만들기
top_cors$item1 <- factor(top_cors$item1, levels = target)
p <- ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                          y = correlation,
                          fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

p