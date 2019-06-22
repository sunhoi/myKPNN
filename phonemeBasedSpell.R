# <음소 단위로 철자 해체> 

# (10) 이제 single_data 즉, 꼬리번호가 제거되고 동음이의어가 제거된 데이터를 가지고 음절 단위 한글 표기를 음소 단위 한글 표기로 바꿉니다. 
# 이를 수행하려면 R 패키지인 KoNLP를 불러와야 합니다.   

if (!require(KoNLP)) install.packages("KoNLP")
library(KoNLP)

# (11) KoNLP 함수 convertHangulStringToJamos()를 사용하여 다음과 같은 식을 돌린 뒤 그 결과를 phoneme_result에 넣습니다.

phoneme_result <- vector()              # for loop 결과값 받아줄 result object 필요
for (i in 1:nrow(single_data)) {
  syllable <- vector()        # 한글자(음절) 단위를 받아줄 vector 'syllable' 선언합니다
  syllable <- convertHangulStringToJamos(single_data$entry[i])   # 벡터 'single_data$entry'의 i번째 element를 자모분리(convertHangulStringToJamos)하고 그 결과를 벡터 'syllable'에 덮어씁니다.
  for (j in 1:length(syllable)) {						# 반복문 안에서 또 반복문을 돌릴 건데 j는 1부터 'syllable'의 element 총 개수까지 하나씩 올라갑니다.
    phonemic <- unlist(strsplit(syllable[j], split=""))	# 'syllable'의 j번째 element를 각 자모단위로 분리해서 새로운 vector 'phonemic'에 넣습니다.
    if(phonemic[1] == "ㅇ") {phonemic[1] <- ""}		# 첫번째 자모(즉, 초성)가 'ㅇ'이면, 그것을 제거합니다.
    syllable[j] <- paste(phonemic, collapse="")		# 'phonemic'을 결합해서 다시 음절단위로 만듭니다. 그러나 초성의 ㅇ은 제거된 상태입니다.
  }
  phoneme_result[i]<-paste(syllable, collapse="")				# 그 결과를 phoneme_result에 저장합니다.
}

# (12) phoneme_result을 돌려서 위 식이 제대로 작동했는지 확인할 수 있습니다.

phoneme_result

# (파일 설명) phoneme_result는 음소단위로 표기된 단어 목록입니다. 

# (13) 음절 단위 한글 표기데이터 즉, phoneme_result를 R 밖으로 빼내어 저장하려면, 역시 write.csv()함수를 사용합니다.

write.csv(phoneme_result, file=file.choose(), row.names=F)