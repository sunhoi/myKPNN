# <주어진 데이터에 기초해서 random lexicon (임의의 어휘부) 만들기>

# (36) 주어진 데이터 즉, 여기에서는 phoneme_klat에 있는 한국어 단어를 가지고 random lexicon을 만듭니다. 
# x = as.character(phoneme_klat)를 사용하여 문자로 변환한 x를 입력부로 하여 
# Phonotactic random lexicon을 만들기 위한 코드 random_lexicon를 작성합니다.  
# ngram 패키지와 발음기호 문자로 변한된 단어 목록이 필요합니다. 
# 아래 코드는 제너럴한 코드입니다. 이 코드를 random_lexicon에 저장한 뒤 다음 코드 즉, pseudo_lexicon1 = random_lexicon(x)에 
# 해당 input을 넣습니다.  

random_lexicon<-function(x,ngramn=NULL,rlex=NULL) { 		# x에는 klattese로 나열된 vector가 들어가고, ngram에는 ngram의 n (eg bigram의 경우 2).. rlex에는 생성할 random lexicon의 개수를 집어넣는다.
  if(is.null(rlex)) rlex=1			# ramdon lexicon 개수의 default는 1
  if(is.null(ngramn)) ngramn=2			# ngramn의 default는 2
  if (!require(ngram)) install.packages("ngram")
  library(ngram)
  termi<-0					# 생성된 렉시콘 개수를 counting하기 위한 변수
  x<-as.character(x)
  num<-length(x)					# 원래 lexicon의 단어 개수를 num으로 저장
  original_mean<-mean(nchar(x))			# 생성된 random lexicon의 단어길이 평균을 제어하기 위해 원래 렉시콘의 평균을 기준으로 삼음
  output<-vector()
  wstop<-paste0(rep("#",ngramn-1),collapse="")	# 단어 앞뒤로 들어갈 word boundary marker를 지정하기
  ngram_data<-paste0(wstop,paste0(x,wstop,collapse="")) # 학습 데이터 feeding ngramn=2인 경우, "#단어1##단어2##단어3##.....#마지막단어" 식으로.
  ngr<-ngram(ngram_data,n=ngramn,sep="")			# feeding data의 ngram 결과를 기술하여 ngr에 저장
  while(termi != rlex) {
    intermediate<-babble(ngr,num*15)		# 함수 babble은 ngr을 기준으로 markov chain 생성하는데, 단어길이 제어 및 중복단어 제거하면 떨어져나갈 outlier들을 고려하여 num에 비해 15배 생성.
    intermediate<-gsub(" ", "", intermediate, fixed = TRUE)
    intermediate<-unlist(strsplit(intermediate, wstop, fixed = TRUE)) # word boundary 기준으로 잘라준다.
    intermediate<-unique(intermediate)				  # 생성된 단어 상에서 중복단어 제거
    intermediate<-intermediate[intermediate != ""]				# 제거된 단어들을 지움
    if(length(which(nchar(intermediate)>max(nchar(x))))>0) intermediate <- intermediate[-which(nchar(intermediate)>max(nchar(x)))]	# 과도하게 길게 생성된 단어를 제거함
    if(length(intermediate)>num+1){						# 생성된 random lexicon의 단어개수가 원래 lexicon보다 많을 경우에는,
      intermediate<-intermediate[2:(num+1)] # 2번째부터 num+1번째까지를 random lexicon으로 삼음
      if (mean(nchar(intermediate))>original_mean*.9 & mean(nchar(intermediate))<original_mean*1.1){	# 평균단어길이 제약 통과시
        output<-cbind(output, intermediate)							# 출력값 output에 저장함
        termi<-termi+1
      }
    }
  }
  colnames(output) <- paste("lexicon" ,c(1:rlex))
  return(output)
} 

# (37) 이제 위의 코드에 x를 input으로 넣어 bigram pseudo-lexicon을 1개 만듭니다.

# input = phoneme_klat
# input

if(length(which(is.na(x)))>0) x <- x[-which(is.na(x))] 	# 이 명령어를 돌려서 혹시라도 NA가 포함되어 있으면 삭제한다.
pseudo_lexicon = random_lexicon(x) # input의 phonotactics를 기반으로 bigram pseudo-lexicon을 1개 만든다. (arguments ngram과 rlex는 default값으로서 각각 2와 1이 자동 입력됨)

# Klattese2의 결과
if(length(which(is.na(x2)))>0) x2 <- x2[-which(is.na(x2))]
pseudo_lexicon2 = random_lexicon(x2) 
# Klattese4의 결과
if(length(which(is.na(x4)))>0) x4 <- x4[-which(is.na(x4))]
pseudo_lexicon4 = random_lexicon(x4) 


# (38)  pseudo_lexicon의 결과를 확인합니다. 
pseudo_lexicon
pseudo_lexicon2
pseudo_lexicon4

# (39) input의 bigram phonotactics를 기반으로 pseudo-lexicon 3개를 만들고 확인하고, r 밖으로 저장해 봅니다. 

three_pseudo_lexicon = random_lexicon(x, ngram=2, rlex=3) # input의 bigram phonotactics를 기반으로 pseudo-lexicon 3개를 만든다.
three_pseudo_lexicon2 = random_lexicon(x2, ngram=2, rlex=3)
three_pseudo_lexicon4 = random_lexicon(x4, ngram=2, rlex=3)


write.csv(three_pseudo_lexicon2, file=file.choose())

# (40) input의 bigram phonotactics를 기반으로 pseudo-lexicon 3개를 만든다.

# pseudo_lexicon3 = random_lexicon(x, ngram=2, rlex=3) # input의 bigram phonotactics를 기반으로 pseudo-lexicon 3개를 만든다.

# (41) input의 trigram phonotactics를 기반으로 pseudo-lexicon 10개를 만든다.

ten_pseudo_lexicon2 = random_lexicon(x2, ngram=3, rlex=10) # input의 trigram phonotactics를 기반으로 pseudo-lexicon 10개를 만든다.