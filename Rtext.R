## Test for Sungchan Kang
## ----, echo=FALSE, cache=FALSE-------------------------------------------
library(knitr)
opts_chunk$set(warning=FALSE, message=FALSE)


## ------------------------------------------------------------------------
library(stringi)

#한글 유니코드 정규화
stri_trans_nfkc("ㄱㅏㄷㅏ") == "가다"



## ----preprocessing, fig.width=6, fig.height=9----------------------------
library(stringi)
library(extrafont)
library(ggplot2)
library(corrplot)
library(data.table)
library(Hmisc)
library(igraph)

#ggplot2 폰트 고정 
theme_set(theme_bw(base_family = "Un Dotum"))


#전체 10권의 데이터 로딩 
sam_1_v <- readLines("sam/sam_1.txt",encoding = "UTF-8")
sam_2_v <- readLines("sam/sam_2.txt",encoding = "UTF-8")
sam_3_v <- readLines("sam/sam_3.txt",encoding = "UTF-8")
sam_4_v <- readLines("sam/sam_4.txt",encoding = "UTF-8")
sam_5_v <- readLines("sam/sam_5.txt",encoding = "UTF-8")
sam_6_v <- readLines("sam/sam_6.txt",encoding = "UTF-8")
sam_7_v <- readLines("sam/sam_7.txt",encoding = "UTF-8")
sam_8_v <- readLines("sam/sam_8.txt",encoding = "UTF-8")
sam_9_v <- readLines("sam/sam_9.txt",encoding = "UTF-8")
sam_10_v <-readLines("sam/sam_10.txt",encoding = "UTF-8")

sam_all <- list(book_1=sam_1_v, book_2=sam_2_v, book_3=sam_3_v, book_4=sam_4_v, book_5=sam_5_v, book_6=sam_6_v, book_7=sam_7_v,
     book_8=sam_8_v, book_9=sam_9_v, book_10=sam_10_v)


#각 책권을 문장으로 분리한다. 
#문장 분리를 위해 stringi 패키지의 stri_split_boundaries 함수를 사용한다. 
sam_all_sentences <- lapply(sam_all, function(book){
  book_full <- paste0(book,collapse = "")
  stri_split_boundaries(book_full, stri_opts_brkiter(type="sentence"))[[1]]
  })


#챕터 텍스트 정보 
chapter_str_1  <- c('서사', '창천에 비키는 노을', '누운 용 엎드린 범', '영웅, 여기도 있다', '고목의 새싹은 흙을 빌어 자라고',
                  '황건의 회오리 드디어 일다', '복사꽃 핀 동산에서 형제가 되고', '도적을 베어 공을 이루다', '걷히지 않는 어둠',
                  '장락궁의 피바람', '여우 죽은 골에 이리가 들고', '차라리 내가 저버릴지언정')

chapter_str_2 <- c('가자, 낙양으로', '데운술이 식기전에', '낙양에는 이르렀건만', '어제의 동지, 오늘의 적',
                   '장성은 강동에 지고', '천하를 위해 내던진 미색', '두 이리 연환계에 걸리다' ,'큰 도적은 죽었으나',
                   '드디어 패자의 길로', '연못을 떠나 대해로', '복양성의 풍운 ', '서주는 봄바람만', '궁한 새는 쫓지 않으리',
                   '서도의 회오리')

chapter_str_3 <- c('안겨오는 천하', '풍운은 다시 서주로', '우리를 벗어나는 호랑이', '다져지는 또 하나의 기업', 
                   '교룡 다시 연못에 갇히다', '전공은 호색에 씻겨가고', '천자의 꿈은 수춘성의 잿더미로', 
                   '스스로 머리칼을 벰도 헛되이', '꿈은 다시 전진 속에 흩어지고', '가련하다 백문루의 주종', 
                   '아직은 한의 천하', '교룡은 다시 창해로', '원가도 중원을 향하고', '급한 불길은 잡았으나')

chapter_str_4 <- c('살아서는 한의 충신 죽어서는 한의 귀신', '자옥한 전진, 의기를 가리우고', "드높구나 춘추의 향내여\\(1\\)",
                   "다섯관을 지나며 여섯 장수를 베다", "아직도 길은 멀고", "다시 이어진 도원의 의", "아깝다 강동의 손랑",
                   "장강에 솟는 또 하나의 해", "양웅 다시 관도에서 맞붙다", 
                   "하북을 적시는 겨울비", "이제는 형주로", "조각나는 원가", "마침내 하북도 조조의 품에", "높이 솟는 동작대")

#5권은 완전한 책이 아님
chapter_str_5 <- c('용이 어찌 못 속의 물건이랴', '다시 다가오는 초야의 인맥', '드디어 복룡의 자취에 닿다', 
                   '와룡선생 ', '삼고초려와 삼분천하의 계', '높이 이는 장강의 물결')


chapter_str_6 <- c('계략과 계략 꾀와 꾀', '오가는 사항계로 전기는 무르익고', '전야, 그 현란함이여', 
                   '모든 것을 갖추었으되 동풍이 없구나', '혼일사해의 꿈은 동남풍에 타 버리고', '화용도를 끊기엔 옛 은의가 무거워라',
                   '한바탕 힘든 싸움 누구를 위함이었던고', '교룡은 드디어 삼일우를 얻고', 
                   '다시 이는 두 집안 사이의 불길', '형주는 못 찾고 미인만 바쳤구나', 
                   '주유가 시상으로  되돌아갔다', '비상을 재촉하는 또 하나의 날개', 
                   '이는 회오리', '젊은 범 묵은 용')

chapter_str_7 <- c('발톱 잃고 쫓겨가는 젊은 범', '서천은 절로 다가오고', '서쪽으로 뻗는 왕기', 
                   '장강엔 다시 거센 물결이 일고', '바뀐 깃발 돌려세운 칼끝', '새끼 봉은 땅에 떨어지고', 
                   '무너져 내리는 서천의 기둥', '서량의 풍운아 다시 일어나다', '서천엔 드디어 새로운 해가 뜨고', 
                   '장강을 뒤덮는 호기', '위공도 서쪽으로 눈을 돌리고', '한중이 떨어지니 불길은 장강으로', 
                   '장하구나 장문원, 씩씩하다 감흥패', '왕자와 술사들', '허창을 태우는 한신들의 충의')

chapter_str_8 <- c('드디어 터진 한중 쟁탈전', '정군산 남쪽에서 한팔 꺾였네', '가름나는 한중의 주인', 
                   '유비, 한중왕이 되다', '불길은 서천에서 형주로', '빛나구나, 관공의 무위', 
                   '옛 맹세를 어찌할거나', '흙으로 돌아가고', '콩깍지를 태워 콩을 볶누나  조조가', 
                   '한의 강산은 마침내 위에게로', '한스럽다, 익덕도 관공을 따라가고', '벌벌 떠는 동오의 산천',
                   '원수를 갚아도 한은 더욱 깊어가고')

chapter_str_9 <- c('강남의 서생 칠백 리 촉영을 불사르다', '긴 꿈은 백제성에서 지고', '촉과 오 다시 손을 잡다',
                   '위는 오에 맡기고 촉은 남만으', '두 번 사로잡고 두 번 놓아주다', '네 번을 사로잡혀도', 
                   '독룡동천에서 은갱동으로', '꺾일 줄 모르는 자유의 넋', '맹획은 드디어 꺾이고 공명은 성도로', 
                   '조비의 죽음과 출사표', '나이 일흔에 오히려 기공을 세웠네 ', '오리새끼를 놓아 주고 봉을 얻다',
                   '치솟는 촉의 기세 흔들리는 중원', '다시 쓰이게 된 사마의의 매운 첫솜씨', '한스럽구나, 가정의 싸움', 
                   '다시 올려지는 출사표')

chapter_str_10 <- c('왕쌍을 베어 진창의 한은 씻었으나', '세번째로 기산을 향하다', '이번에는 사마의가 서촉으로', 
                    '공명 네번째 기산으로 나아가다', '다섯번째 기산행도 안으로부터 꺾이고', '여섯번째  기산으로', 
                    '꾸미는 건 사람이되 이루는 건 다만 하늘일 뿐', '큰 별 마침내 오장원에 지다', '공명은 충무후로 정군산에 눕고',
                    '시드는 조위', '그 뒤 10년', '홍한의 꿈 한줌 재로 흩어지는구나', '패업도 부질없어라. 조위도 망하고',
                    '나뉜 것은 다시 하나로', '결사 ')


#챕터 타이틀을 list형태로 결합 
all_chapter_title <- list(chapter_str_1, chapter_str_2, chapter_str_3, chapter_str_4, chapter_str_5, chapter_str_6,
                    chapter_str_7, chapter_str_8, chapter_str_9, chapter_str_10)


#책 권 인덱스 생성 및 하나의 vector로 책을 결합 
book_end <- 0 
book_idx <- c()
all_book_sentence <- c()
for(book in sam_all_sentences){
  book_start <- book_end + 1
  book_end <- book_start + length(book) - 1
  book_idx <- c(book_idx, book_start)
  all_book_sentence <- append(all_book_sentence, book)
}

#마지막 인덱스 정보 입력 
book_idx[length(book_idx) + 1] <- length(all_book_sentence)


#책 챕터 인덱스 생성 
all_chapter_idx <- list()

for(b_idx in 1:(length(book_idx) - 1)){
  all_chapter_idx[[b_idx]] <- sapply(all_chapter_title[[b_idx]], function(chap_tit){
    which(grepl(chap_tit, all_book_sentence[book_idx[b_idx]:book_idx[b_idx + 1] - 1]))[1] + book_idx[b_idx]
  })
}



## 60명 장수의 이름파일 읽어들이기 
#  
sam_names <- readLines("names.txt",encoding = "UTF-8")



#각 권 제목
book_title <- c('도원에 피는 의', '구름처럼 이는 영웅', '헝클어진 천하', '칼 한자루 말 한 필로 천리를 닫다', '세번 천하를 돌아봄이여',
                '불타는 적벽', '가자 서촉으로', '솔밭처럼 갈라선 천하', '출사표, 드높아라 충신의 매운 얼이여', 
                '오장원에 지는 별')



## ----init_analysis, warning=FALSE, message=FALSE, fig.width=6, fig.height=9----
## 종합
# 10권의 문장 정보를 문자 벡터로 가지고 있음 
# all_book_sentence
head(all_book_sentence)

# 리스트 구조형태로 all_book_sentence에서 챕터의 시작 위치 정보를 가지고 있음 
# all_chapter_idx
print(all_chapter_idx[1])

# 주요 등장 인물들에 대한 리스트
# sam_names
head(sam_names)

#권별 문장 길이 분포

sentence_lengths <- lapply(sam_all_sentences, function(book){
  nchar(book)
  })


to_df <- data.frame(book=c(), len=c())
for(book_idx in 1:length(sentence_lengths)){
  to_df <- rbind(to_df, data.frame(book=names(sentence_lengths[book_idx]), len=sentence_lengths[[book_idx]]))
}

to_df$book <- factor(to_df$book, levels=c("book_1", "book_2", "book_3","book_4","book_5",
                                             "book_6","book_7","book_8","book_9","book_10"))

#ggplot(to_df, aes(len)) + geom_density(aes(fill=book), alpha=0.6) + scale_x_continuous(breaks=seq(1,300,by = 20))

ggplot(to_df, aes(len)) + geom_density(aes(fill=book), alpha=0.6) + scale_x_continuous(breaks=seq(1,300,by = 20)) + facet_grid(book~.)


## ----freq_ratio_plot, fig.width=6, fig.height=8,  warning=FALSE, message=FALSE, cache=TRUE, echo=-1----
library(pander)
#이름과 등장 횟수
nm_cnts <- sapply(sam_names, function(nm){
  sum(grepl(nm, all_book_sentence))
  })


ordered_freq <- nm_cnts[order(nm_cnts,decreasing = T)]

pander(data.frame(ordered_freq))

prop_ordered_nm <- prop.table(ordered_freq) 

ggplot(data.frame(nm=names(ordered_freq), ratio=ordered_freq), aes(reorder(nm, ratio), ratio))  + 
  geom_histogram(stat='identity')  + xlab("장수") + ylab("빈도") + coord_flip()

ggplot(data.frame(nm=names(prop_ordered_nm), ratio=prop_ordered_nm), aes(reorder(nm, ratio), ratio))  + 
  geom_histogram(stat='identity')  + xlab("장수") + ylab("비율") + coord_flip()



## ----dispersion,fig.height=4, fig.width=8, cache=TRUE,dev='quartz_png', warning=FALSE, message=FALSE, echo=-1, cache=TRUE----
par(family = "Un Dotum")

#1권에서 인물 분포
for(nm in c("조조", "유비", "관우", "장비")){
  n_times <- seq(1:length(sam_all_sentences$book_1))
  character_grep <- grepl(nm, sam_all_sentences$book_1)
  w_cnt <- rep(NA, length(n_times))
  w_cnt[character_grep] <- 1
  plot(w_cnt, main=sprintf("'%s' 산포도", nm), 
       xlab='', ylab=sprintf("총 %d 회 출현", sum(character_grep)), type="h", ylim=c(0,1), yaxt='n')
}

#1~ 10권 60명 장수 분포
#pdf("DP.pdf",width = 10, height = 6, onefile = T)
for(nm in names(ordered_freq)){
  n_times <- seq(1:length(all_book_sentence))
  character_grep <- grepl(nm, all_book_sentence)
  w_cnt <- rep(NA, length(n_times))
  w_cnt[character_grep] <- 1
  plot(w_cnt, main=sprintf("'%s' 산포도", nm), 
       xlab='', ylab=sprintf("총 %d 회 출현", sum(character_grep)), type="h", ylim=c(0,1), yaxt='n', xaxt='n')
  axis(1, at = sapply(all_chapter_idx, `[`, 1), labels = book_title, cex.axis=0.6, las=2)  
}
#dev.off()


## ----time,cache=TRUE-----------------------------------------------------

all_idx <- vector(mode="numeric")
for(bi in all_chapter_idx){
  all_idx <- append(all_idx,bi)
}

chp_cnt_lst <- list()

for(nm in names(ordered_freq)){
  n_times <- seq(1:length(all_book_sentence))
  character_grep <- grepl(nm, all_book_sentence)
  w_cnt <- rep(0, length(n_times))
  w_cnt[character_grep] <- 1
  w_cnt_dt <- data.table(w_cnt=w_cnt)  
  w_cnt_dt[,chapter_idx:=cut2(1:nrow(w_cnt_dt), cuts=all_idx)]
  chp_word_cnts <- w_cnt_dt[,list(w_cnt_sum=sum(w_cnt)),by=chapter_idx]$w_cnt_sum
  chp_cnt_lst[[nm]] <- chp_word_cnts
}

#같은 length를 가지고 같은 타입을 포함하고 있는 리스트 객체는 쉽게 data.frame이나 data.table로 변환 가능하다. 
chp_cnt_dt<- as.data.frame(chp_cnt_lst)

# general_dists <- dist(scale(t(chp_cnt_dt)),method = "euclidean")
# 
# clust <- hclust(general_dists)
# plot(clust)
# rect.hclust(clust,k=5)


## ----plot,fig.height=11,fig.width=11,warning=FALSE,message=FALSE,echo=-1, cache=TRUE----
par(family = "Un Dotum")

cor_obj <- cor(chp_cnt_dt)

corrplot(cor_obj,order = "hclust", addrect=5)


## ----, fig.height=11,fig.width=11,warning=FALSE,message=FALSE,echo=-1, cache=TRUE----
par(family = "Un Dotum")
sent_cnt_lst <- list()

for(nm in names(ordered_freq)){
  n_times <- seq(1:length(all_book_sentence))
  character_grep <- grepl(nm, all_book_sentence)
  w_cnt <- rep(0, length(n_times))
  w_cnt[character_grep] <- 1
  sent_cnt_lst[[nm]] <- w_cnt
}

sent_cnt_dt <- as.data.frame(sent_cnt_lst)


general_dist <- dist(t(sent_cnt_dt),method='binary')

clust <- hclust(general_dist)

plot(clust)



## ----sna,  fig.height=11,fig.width=11,warning=FALSE,message=FALSE,echo=-1----
par(family = "Un Dotum")

g <- graph.adjacency(1 - as.matrix(general_dist), weighted=T, mode ="undirected")

g <- simplify(g)
layout1 <- layout.circle(g)

plot.igraph(g,layout=layout1, edge.width=E(g)$weight * 130)


