# <기존 데이터를 n-gram 데이터로 전환>

# (42) 실제 단어 목록의 단어들을 (필요시 어휘층위를 구별하여) bigram, trigram 등을 기술한다. 
# (input: Korean PNN from http://namsling.tistory.com/9 , 한글-Klattese 대응표)
# 1단계: 분석대상 단어 (single_data$entry) 추출한다. 

single_data$entry
# single_data$entry<-gsub(" ","",single_data$entry)

# 특정 층위만 고려하고 싶을 경우에는 아래와 같이 한다.
# 이 때, single_data의 하위 항으로 어휘 계층이 기재된 type이 포함되어 있어야 한다. 
# 특정 층위만 고려하고 싶은 경우 (원하는 층위) 부분에 native, sino 혹은 foreign을 입력
# strata <- single_data[which(single_data$type=="(원하는 층위)"),]
# strata_data = strata$entry

# 2단계: 표제어형으로부터 자모분리 (from R code 1): 이것은 초기에 음소 기반 표기로 분리하는 코드와 동일
# library(KoNLP)											# 패키지 KoNLP를 통해 자모분리를 합시다.
# phoneme_result <- vector()										# 자모분리된 결과를 저장할 vector 'result'를 선언합니다.
# for (i in 1:nrow(single_data)) {								# for문 (반복문)을 돌릴건데 i가 1부터 'single_data'의 element 총 개수까지 하나씩 올라갑니다.
#  syllable <- vector()								# 한글자(음절) 단위를 받아줄 vector 'syllable' 선언합니다
#  syllable <- convertHangulStringToJamos(single_data$entry[i])		# 벡터 'data'의 i번째 element를 자모분리(convertHangulStringToJamos)하고 그 결과를 벡터 'syllable'에 덮어씁니다.
#  for (j in 1:length(syllable)) {						# 반복문 안에서 또 반복문을 돌릴 건데 j는 1부터 'syllable'의 element 총 개수까지 하나씩 올라갑니다.
#    phonemic <- unlist(strsplit(syllable[j], split=""))	# 'syllable'의 j번째 element를 각 자모단위로 분리해서 새로운 vector 'phonemic'에 넣습니다.
#    if(phonemic[1] == "ㅇ") {phonemic[1] <- ""}		# 첫번째 자모(즉, 초성)가 'ㅇ'이면, 그것을 제거합니다.
#    syllable[j] <- paste(phonemic, collapse="")		# 'phonemic'을 결합해서 다시 음절단위로 만듭니다. 그러나 초성의 ㅇ은 제거된 상태입니다.
#  }
#  phoneme_result[i]<-paste(syllable, collapse="")				# 그 결과를 phoneme_result에 저장합니다.
# }

# phoneme_result을 돌려서 위 식이 제대로 작동했는지 확인할 수 있습니다.

phoneme_result

# 3단계: 패키지 ngram은 한글자모를 처리할 수 없으므로 Klattese형태로 분리. (나중에 다시 한글자모로 복원할 것임.) (from R code 1)

Klattese<-read.table(file = file.choose(), sep = ",", header=TRUE, comment.char = "") # 한글자모와 Klattese 사이의 대응표 파일을 불러옵니다. Klattese.csv 파일을 선택하면 됩니다.

# 이제 발음 기호로 변환합니다. 
# 결과를 담을 백터 이름을 phoneme_klat이라고 칭하겠습니다. 식은 다음과 같습니다.
# Klattese로 변환한 결과를 저장할 vector 'phoneme_klat'을 선언합니다.

phoneme_klat<-vector()														# Klattese로 변환한 결과를 저장할 vector 'phoneme_klat'을 선언합니다.
for (k in 1:length(phoneme_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
  letter<-unlist(strsplit(phoneme_result[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese$C))==TRUE){letter[l]<-as.character(Klattese$VKlattese[match(letter[l],Klattese$V)])
    
    }
    else{letter[l]<-as.character(Klattese$CKlattese[match(letter[l],Klattese$C)])}
  }
  phoneme_klat[k]<-paste(letter,collapse="")					
}

# 발음 기호 변환이 제대로 되었는지 확인이 필요합니다. 이를 위해 phoneme_klat을 확인합니다.

phoneme_klat

# 여기까지는 앞에서 음운이웃을 만들기 위해 작동한 코드와 동일합니다. 

# 4단계: ngram 정보를 기술하는 함수 nngram 만드는 코드입니다. 

nngram<-function(x,ngramn){
  wstop<-paste0(rep("#",ngramn-1),collapse="")
  ngram_data<-paste0(wstop,paste0(x,wstop,collapse="")) 
  ngr<-ngram(ngram_data,n=ngramn,sep="")
  return(ngr)
}

# 5단계: nngram 함수를 이용해서 phoneme_klat을 가지고 phrase table 만드는 코드입니다.

library(ngram)

# nngram의 인수 중 ngramn에 2를 넣으면 bigram phrasetable이 나옵니다.

bigram_result <- get.phrasetable(nngram(phoneme_klat,2))

# Klattese2이 결과

bigram_result2 <- get.phrasetable(nngram(phoneme_klat2,2))	

# Klattese4이 결과
bigram_result4 <- get.phrasetable(nngram(phoneme_klat4,2))	

# bigram phrasetable을 확인하고, r 밖으로 저장합니다. 

bigram_result
bigram_result2
bigram_result4

write.csv(bigram_result, file=file.choose(), row.names=F)
write.csv(bigram_result2, file=file.choose(), row.names=F)
write.csv(bigram_result4, file=file.choose(), row.names=F)

# nngram의 인수 중 ngramn에 2를 넣으면 bigram phrasetable이 나옵니다.

trigram_result <- get.phrasetable(nngram(phoneme_klat,3))		# nngram의 인수 중 ngramn에 3를 넣으면 trigram phrasetable이 나옴.. 마찬가지로 ngramn에 4를 넣으면... 4-gram이 될것.
trigram_result2 <- get.phrasetable(nngram(phoneme_klat2,3))
trigram_result4 <- get.phrasetable(nngram(phoneme_klat4,3))

# trigram phrasetable을 확인하고, r 밖으로 저장합니다. 

trigram_result
write.csv(trigram_result, file=file.choose(), row.names=F)

# 6단계: Klattese로 된 phrase table에서 Klattese를 한글자모로 복원합니다. 
# 즉, 3단계를 거꾸로 돌리는 것입니다. 
# 그런데 glide만 따로 떼서 한글로 복원될 방법은 없습니다. 그 이유는 한글이 <glide + 단모음> 표기만 가능할 뿐
# glide를 포함하는 그 외의 형태를 표기할 수 없기 때문입니다.
# 그러나 r 밖으로 결과를 저장한 뒤 j, w glide 형태를 그대로 둔 채로 분석하는 방법이 있을 수 있습니다.
#  biresult 중 bigram 목록만 추립니다.

bigram_korean_result <- bigram_result$ngrams
bigram_korean_result2 <- bigram_result2$ngrams
bigram_korean_result4 <- bigram_result4$ngrams

# 그 결과를 확인합니다.

bigram_korean_result

# 결과에 간격이 있으므로, 이 간격을 없앱니다.

bigram_korean_result <- gsub(" ","",bigram_korean_result)
bigram_korean_result2 <- gsub(" ","",bigram_korean_result2)
bigram_korean_result4 <- gsub(" ","",bigram_korean_result4)

# 이제 다시 복원합니다. 복원 결과를 bigram_klat에 저장합니다.

bigram_klat<-vector()

for (k in 1:length(bigram_korean_result)) {
  letter<-unlist(strsplit(bigram_korean_result[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese$CKlattese))==TRUE){letter[l]<-as.character(Klattese$V[match(letter[l],Klattese$VKlattese)])
    
    }
    else{letter[l]<-as.character(Klattese$C[match(letter[l],Klattese$CKlattese)])}
  }
  bigram_klat[k]<-paste(letter,collapse="")					
}

# Klattese2의 결과

bigram_klat2<-vector()

for (k in 1:length(bigram_korean_result2)) {
  letter<-unlist(strsplit(bigram_korean_result2[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese2$CKlattese))==TRUE){letter[l]<-as.character(Klattese2$V[match(letter[l],Klattese2$VKlattese)])
    
    }
    else{letter[l]<-as.character(Klattese2$C[match(letter[l],Klattese2$CKlattese)])}
  }
  bigram_klat2[k]<-paste(letter,collapse="")					
}

# Klattese4의 결과

bigram_klat4<-vector()

for (k in 1:length(bigram_korean_result4)) {
  letter<-unlist(strsplit(bigram_korean_result4[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese4$CKlattese))==TRUE){letter[l]<-as.character(Klattese4$V[match(letter[l],Klattese4$VKlattese)])
    
    }
    else{letter[l]<-as.character(Klattese4$C[match(letter[l],Klattese4$CKlattese)])}
  }
  bigram_klat4[k]<-paste(letter,collapse="")					
}

# 다시 bigram_result에 한글 자모 분리된 bigram결과 즉, bigram_klat을 넣습니다. 

bigram_result$ngrams <- bigram_klat
bigram_result2$ngrams <- bigram_klat2
bigram_result4$ngrams <- bigram_klat4

# 그 결과를 확인하고 r 밖으로 저장합니다. 

bigram_result
bigram_result2
bigram_result4

write.csv(bigram_result, file=file.choose(), row.names=F)
write.csv(bigram_result2, file=file.choose(), row.names=F)
write.csv(bigram_result4, file=file.choose(), row.names=F)

# trigram도 마찬가지입니다.

trigram_korean_result <- trigram_result$ngrams
trigram_korean_result2 <- trigram_result2$ngrams
trigram_korean_result4 <- trigram_result4$ngrams

trigram_korean_result <- gsub(" ","",trigram_korean_result)
trigram_korean_result2 <- gsub(" ","",trigram_korean_result2)
trigram_korean_result4 <- gsub(" ","",trigram_korean_result4)

trigram_klat<-vector()

for (k in 1:length(trigram_korean_result)) {
  letter<-unlist(strsplit(trigram_korean_result[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese$CKlattese))==TRUE){letter[l]<-as.character(Klattese$V[match(letter[l],Klattese$VKlattese)])
    
    }
    else{letter[l]<-as.character(Klattese$C[match(letter[l],Klattese$CKlattese)])}
  }
  trigram_klat[k]<-paste(letter,collapse="")					
}

trigram_result$ngrams <- trigram_klat

# Klattese2의 결과

trigram_klat2<-vector()

for (k in 1:length(trigram_korean_result2)) {
  letter<-unlist(strsplit(trigram_korean_result2[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese2$CKlattese))==TRUE){letter[l]<-as.character(Klattese2$V[match(letter[l],Klattese2$VKlattese)])
    
    }
    else{letter[l]<-as.character(Klattese2$C[match(letter[l],Klattese2$CKlattese)])}
  }
  trigram_klat2[k]<-paste(letter,collapse="")					
}

trigram_result2$ngrams <- trigram_klat2

# Klattese4의 결과

trigram_klat4<-vector()

for (k in 1:length(trigram_korean_result4)) {
  letter<-unlist(strsplit(trigram_korean_result4[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese4$CKlattese))==TRUE){letter[l]<-as.character(Klattese4$V[match(letter[l],Klattese4$VKlattese)])
    
    }
    else{letter[l]<-as.character(Klattese4$C[match(letter[l],Klattese4$CKlattese)])}
  }
  trigram_klat4[k]<-paste(letter,collapse="")					
}

trigram_result4$ngrams <- trigram_klat4
