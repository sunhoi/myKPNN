# <기존 데이터를 음절경계 $가 포함된 n-gram으로 전환하기>

# (43) 음절경계 $를 포함해서 bigram trigram 기술하기(input: Korean PNN from http://namsling.tistory.com/9 , 한글-Klattese 대응표)
# 1단계: 사전 상의 단어만 추출해서 ngram_syllable_data로 저장

raw_data = read.csv(file = file.choose(), header = TRUE)

# (raw_data = read.csv(file = file.choose(), header = T, sep = ",", quote=""): 이건 엑셀 내부 쉼표 때문에 오류 발생)

# raw_data에 저장된 데이터 entry 즉, raw_data$entry에는 꼬리 번호가 있습니다. 이것을 제거하기 위해서는  gsub() 함수를 사용합니다.

raw_data$entry <- gsub("__[[:alnum:]]+", "", raw_data$entry)
raw_data$entry <- gsub("\\(.+\\)", "", raw_data$entry)    # 괄호 제거 
raw_data$entry <- gsub(" ", "", raw_data$entry) # 띄어쓰기 제거 

# 꼬리 번호 제거는 다음 두 식을 연달아 돌려도 해결 가능합니다. 
# raw_data$entry <- gsub("__[[:digit:]]+", "", raw_data$entry)
# raw_data$entry <- gsub("__x[[:digit:]]+", "", raw_data$entry)

# 이제 꼬리 번호가 제거된 데이터를 data라는 이름으로 저장합니다.

data = raw_data

# 꼬리 번호가 제거된 데이터를 write.csv()함수를 사용하여 R 밖읋 빼내어 csv 파일로 저장합니다.

write.csv(data, file=file.choose(), row.names=F)

# data에 저장된 데이터 즉, data$entry에는 아직 동음이의어들이 있습니다. 이 동음이의어들을 제거하고자 한다면, 다음 식을 사용하기 바랍니다. 

duplicated <- which(duplicated(data$entry))
if(length(duplicated)>0) single_data <- data[-duplicated,]

# single_data$entry을 사용해서 위 식이 제대로 작동했는지 확인할 수 있습니다.

single_data$entry

# 동음이의어가 제거된 데이터를 write.csv()함수를 사용하여 R 밖읋 빼내어 csv 파일로 저장합니다.

write.csv(single_data, file=file.choose(), row.names=F)

# 문자 변환 후 ngram_syllable_data로 저장하고 확인합니다. 

ngram_syllable_data<-as.character(single_data$entry)
ngram_syllable_data

## 2단계: 음소 기반 표기로 분리하고, 음절이 끝날 때마다 음절경계 "$"를 넣기 위한 코드입니다. 
# 앞에서 작동한 음소 기반 코드와 차이가 있습니다. 
# 우선, input이름이 ngram_syllable_data입니다.
# output은 음절경계가 표시된 음소기반 표기입니다. 이름은 phoneme_syllable_result라 칭하겠습니다. 


library(KoNLP)											# 패키지 KoNLP를 통해 자모분리를 합시다.

phoneme_syllable_result <- vector()										# 자모분리된 결과를 저장할 vector 'result'를 선언합니다.

for (i in 1:length(ngram_syllable_data)) { # for문 (반복문)을 돌릴건데 i가 1부터 'ngram_syllable_data'의 element 총 개수까지 하나씩 올라갑니다.
  syllable <- vector()								# 한글자(음절) 단위를 받아줄 vector 'syllable' 선언합니다
  syllable <- convertHangulStringToJamos(ngram_syllable_data[i])		# 벡터 'ngram_syllable_data'의 i번째 element를 자모분리(convertHangulStringToJamos)하고 그 결과를 벡터 'syllable'에 덮어씁니다.
  syllable <- paste0(syllable,"$")					# syllable 바로 뒤에 음절경계 기호 "$"를 넣어줍니다.
  for (j in 1:length(syllable)) {						# 반복문 안에서 또 반복문을 돌릴 건데 j는 1부터 'syllable'의 element 총 개수까지 하나씩 올라갑니다.
    phonemic <- unlist(strsplit(syllable[j], split=""))	# 'syllable'의 j번째 element를 각 자모단위로 분리해서 새로운 vector 'phonemic'에 넣습니다.
    if(phonemic[1] == "ㅇ") {phonemic[1] <- ""}		# 첫번째 자모(즉, 초성)가 'ㅇ'이면, 그것을 제거합니다.
    syllable[j] <- paste(phonemic, collapse="")		# 'phonemic'을 결합해서 다시 음절단위로 만듭니다. 그러나 초성의 ㅇ은 제거된 상태입니다.
  }
  phoneme_syllable_result[i]<-paste(syllable, collapse="")				# 그 결과를 phoneme_syllable_result에 저장합니다.
}

# 위 코드 작동의 결과인 phoneme_syllable_result를 확인하고 R 밖으로 저장합니다.

phoneme_syllable_result
write.csv(phoneme_syllable_result, file=file.choose(), row.names=F)

# 3단계: 그 결과를 klattese로 변환합니다. 이 때, 마지막 음절 끝에 표시된 음절 경계 $는 제되어야 합니다. 

Klattese <- read.table(file = file.choose(), sep = ",", header=TRUE)	# 한글자모와 Klattese 사이의 대응표 파일을 불러옵니다. Klattese.csv 파일을 선택하면 됩니다.
# Klattese<-read.table(file = file.choose(), sep = ",", header=TRUE, comment.char = "")

phoneme_syllable_klat<-vector()														# Klattese로 변환한 결과를 저장할 vector 'klat'을 선언합니다.

for (k in 1:length(phoneme_syllable_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
  letter<-unlist(strsplit(phoneme_syllable_result[k],split=""))
  letter<-letter[1:(length(letter)-1)]								# 마지막 음절경계는 삭제한다. 
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese$C))==TRUE){letter[l]<-as.character(Klattese$VKlattese[match(letter[l],Klattese$V)])
    
    }
    else{letter[l]<-as.character(Klattese$CKlattese[match(letter[l],Klattese$C)])}
  }
  phoneme_syllable_klat[k]<-paste(letter,collapse="")					
}

# Klattese2의 결과

phoneme_syllable_klat2<-vector()														# Klattese로 변환한 결과를 저장할 vector 'klat'을 선언합니다.

for (k in 1:length(phoneme_syllable_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
  letter<-unlist(strsplit(phoneme_syllable_result[k],split=""))
  letter<-letter[1:(length(letter)-1)]								# 마지막 음절경계는 삭제한다. 
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese2$C))==TRUE){letter[l]<-as.character(Klattese2$VKlattese[match(letter[l],Klattese2$V)])
    
    }
    else{letter[l]<-as.character(Klattese2$CKlattese[match(letter[l],Klattese2$C)])}
  }
  phoneme_syllable_klat2[k]<-paste(letter,collapse="")					
}


# Klattese4의 결과

phoneme_syllable_klat4<-vector()														# Klattese로 변환한 결과를 저장할 vector 'klat'을 선언합니다.

for (k in 1:length(phoneme_syllable_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
  letter<-unlist(strsplit(phoneme_syllable_result[k],split=""))
  letter<-letter[1:(length(letter)-1)]								# 마지막 음절경계는 삭제한다. 
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese4$C))==TRUE){letter[l]<-as.character(Klattese4$VKlattese[match(letter[l],Klattese4$V)])
    
    }
    else{letter[l]<-as.character(Klattese4$CKlattese[match(letter[l],Klattese4$C)])}
  }
  phoneme_syllable_klat4[k]<-paste(letter,collapse="")					
}

# 위 코드 작동의 결과인 phoneme_syllable_klat를 확인하고 R 밖으로 저장합니다. 
# 그 결과는 음절경계가 포함된 발음 기호로 변환된 데이터이되, 마지막 음절경계 $는 제거된 것입니다.  

phoneme_syllable_klat
phoneme_syllable_klat2
phoneme_syllable_klat4

write.csv(phoneme_syllable_klat, file=file.choose(), row.names=F)
write.csv(phoneme_syllable_klat2, file=file.choose(), row.names=F)
write.csv(phoneme_syllable_klat4, file=file.choose(), row.names=F)

# 꼬리 표시와 동음이의어가 제거된 데이터인 single_dat에서 id와 entry, afreq만 남긴 single_data_klat를 만들고
# 거기에다가 위 코드 작동 결과인 phoneme_syllable_klat를 더한 데이터 single_data_klat를 만듭니다.   
single_data_klat <- single_data[,c(1:2,5)]
single_data_klat2 <- single_data[,c(1:2,5)]

single_data_klat <- cbind(single_data_klat,phoneme_syllable_klat)
single_data_klat2 <- cbind(single_data_klat,phoneme_syllable_klat2)
single_data_klat4 <- cbind(single_data_klat,phoneme_syllable_klat4)

# 위 코드 작동의 결과인 single_data2를 확인하고 R 밖으로 저장합니다. 

single_data_klat
single_data_klat2
single_data_klat4

write.csv(single_data_klat, file=file.choose(), row.names=F)
write.csv(single_data_klat2, file=file.choose(), row.names=F)
write.csv(single_data_klat4, file=file.choose(), row.names=F)

# 4단계: klat을 ngram 학습 데이터로 넣어서 n-gram 기술하는 코드입니다.

doNgram <- function(ngramn,stra){ # ngramn을 입력받아야 함. stra는 입력하지 않을 시 total로 간주함.
  library(ngram)
  if (stra == "total") {
    stratum<-single_data_klat
  } else stratum<-single_data_klat[which(single_data_klat$type==stra),]
  x<-stratum$phoneme_syllable_klat
  wstop<-paste0(rep("#",ngramn-1),collapse="")
  ngram_data<-paste0(wstop,paste0(x,wstop,collapse=""))
  ngr<-ngram(ngram_data,n=ngramn,sep="")
  ngramtable<-get.phrasetable (ngr)
  ngram_syllable_result<-ngramtable$ngrams
  ngram_syllable_result<-gsub(" ","",ngram_syllable_result)
  
  ngram_syllable_klat<-vector()
  
  for (k in 1:length(ngram_syllable_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
    letter<-unlist(strsplit(ngram_syllable_result[k],split=""))
    letter<-letter[1:(length(letter))]								
    for (l in 1:length(letter)){
      if(is.na(match(letter[l],Klattese$CKlattese))==TRUE){
        if(is.na(match(letter[l],Klattese$VKlattese))==FALSE){
          letter[l]<-as.character(Klattese$V[match(letter[l],Klattese$VKlattese)])
        }
        else {letter[l]<-"#"}
      }
      else{letter[l]<-as.character(Klattese$C[match(letter[l],Klattese$CKlattese)])}
    }
    ngram_syllable_klat[k]<-paste(letter,collapse="")					
  }
  
  ngramtable$ngrams<-ngram_syllable_klat
  return(ngramtable)
}


# phoneme_syllable_klat<-vector()														# Klattese로 변환한 결과를 저장할 vector 'klat'을 선언합니다.

# for (k in 1:length(phoneme_syllable_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
#  letter<-unlist(strsplit(phoneme_syllable_result[k],split=""))
#  letter<-letter[1:(length(letter)-1)]								# 마지막 음절경계는 삭제한다. 
#  for (l in 1:length(letter)){
#    if(is.na(match(letter[l],Klattese$C))==TRUE){letter[l]<-as.character(Klattese$VKlattese[match(letter[l],Klattese$V)])
#    
#    }
#    else{letter[l]<-as.character(Klattese$CKlattese[match(letter[l],Klattese$C)])}
#  }
#  phoneme_syllable_klat[k]<-paste(letter,collapse="")					
# }

# 위 코드 작동의 결과인 phoneme_syllable_klat를 확인하고 R 밖으로 저장합니다. 
# 그 결과는 음절경계가 포함된 발음 기호로 변환된 데이터이되, 마지막 음절경계 $는 제거된 것입니다.  

# phoneme_syllable_klat
# write.csv(phoneme_syllable_klat, file=file.choose(), row.names=F)

# 꼬리 표시와 동음이의어가 제거된 데이터인 single_dat에서 id와 entry, afreq만 남긴 single_data2를 만들고
# 거기에다가 위 코드 작동 결과인 phoneme_syllable_klat를 더한 데이터 single_data2를 만듭니다.   

# single_data2 <- single_data[,c(1:2,5)]
# single_data2 <- cbind(single_data2,phoneme_syllable_klat)

# 위 코드 작동의 결과인 single_data2를 확인하고 R 밖으로 저장합니다. 

# single_data2
# write.csv(single_data2, file=file.choose(), row.names=F)

# 4단계: klat을 ngram 학습 데이터로 넣어서 n-gram 기술하는 코드입니다.

# doNgram <- function(ngramn,stra){ # ngramn을 입력받아야 함. stra는 입력하지 않을 시 total로 간주함.
#   library(ngram)
#   if (stra == "total") {
#     stratum<-single_data2
#   } else stratum<-single_data2[which(single_data2$type==stra),]
#   x<-stratum$phoneme_syllable_klat
#   wstop<-paste0(rep("#",ngramn-1),collapse="")
#   ngram_data<-paste0(wstop,paste0(x,wstop,collapse=""))
#   ngr<-ngram(ngram_data,n=ngramn,sep="")
#   ngramtable<-get.phrasetable (ngr)
#   ngram_syllable_result<-ngramtable$ngrams
#   ngram_syllable_result<-gsub(" ","",ngram_syllable_result)

#   ngram_syllable_klat<-vector()

#   for (k in 1:length(ngram_syllable_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
#     letter<-unlist(strsplit(ngram_syllable_result[k],split=""))
#    letter<-letter[1:(length(letter))]								
#    for (l in 1:length(letter)){
#      if(is.na(match(letter[l],Klattese$CKlattese))==TRUE){
#        if(is.na(match(letter[l],Klattese$VKlattese))==FALSE){
#          letter[l]<-as.character(Klattese$V[match(letter[l],Klattese$VKlattese)])
#        }
#        else {letter[l]<-"#"}
#      }
#      else{letter[l]<-as.character(Klattese$C[match(letter[l],Klattese$CKlattese)])}
#    }
#    ngram_syllable_klat[k]<-paste(letter,collapse="")					
#  }

#  ngramtable$ngrams<-ngram_syllable_klat
#  return(ngramtable)
# }

# Klattese2의 결과

doNgram2 <- function(ngramn,stra){ # ngramn을 입력받아야 함. stra는 입력하지 않을 시 total로 간주함.
  library(ngram)
  if (stra == "total") {
    stratum<-single_data_klat2
  } else stratum<-single_data_klat2[which(single_data_klat2$type==stra),]
  x<-stratum$phoneme_syllable_klat2
  wstop<-paste0(rep("#",ngramn-1),collapse="")
  ngram_data<-paste0(wstop,paste0(x,wstop,collapse=""))
  ngr<-ngram(ngram_data,n=ngramn,sep="")
  ngramtable<-get.phrasetable (ngr)
  ngram_syllable_result2<-ngramtable$ngrams
  ngram_syllable_result2<-gsub(" ","",ngram_syllable_result2)
  
  ngram_syllable_klat2<-vector()
  
  for (k in 1:length(ngram_syllable_result2)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
    letter<-unlist(strsplit(ngram_syllable_result2[k],split=""))
    letter<-letter[1:(length(letter))]								
    for (l in 1:length(letter)){
      if(is.na(match(letter[l],Klattese2$CKlattese))==TRUE){
        if(is.na(match(letter[l],Klattese2$VKlattese))==FALSE){
          letter[l]<-as.character(Klattese2$V[match(letter[l],Klattese2$VKlattese)])
        }
        else {letter[l]<-"#"}
      }
      else{letter[l]<-as.character(Klattese2$C[match(letter[l],Klattese2$CKlattese)])}
    }
    ngram_syllable_klat2[k]<-paste(letter,collapse="")					
  }
  
  ngramtable$ngrams<-ngram_syllable_klat2
  return(ngramtable)
}


# Klattese4의 결과

doNgram4 <- function(ngramn,stra){ # ngramn을 입력받아야 함. stra는 입력하지 않을 시 total로 간주함.
  library(ngram)
  if (stra == "total") {
    stratum<-single_data_klat4
  } else stratum<-single_data_klat4[which(single_data_klat4$type==stra),]
  x<-stratum$phoneme_syllable_klat4
  wstop<-paste0(rep("#",ngramn-1),collapse="")
  ngram_data<-paste0(wstop,paste0(x,wstop,collapse=""))
  ngr<-ngram(ngram_data,n=ngramn,sep="")
  ngramtable<-get.phrasetable (ngr)
  ngram_syllable_result4<-ngramtable$ngrams
  ngram_syllable_result4<-gsub(" ","",ngram_syllable_result4)
  
  ngram_syllable_klat4<-vector()
  
  for (k in 1:length(ngram_syllable_result4)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
    letter<-unlist(strsplit(ngram_syllable_result4[k],split=""))
    letter<-letter[1:(length(letter))]								
    for (l in 1:length(letter)){
      if(is.na(match(letter[l],Klattese4$CKlattese))==TRUE){
        if(is.na(match(letter[l],Klattese4$VKlattese))==FALSE){
          letter[l]<-as.character(Klattese4$V[match(letter[l],Klattese4$VKlattese)])
        }
        else {letter[l]<-"#"}
      }
      else{letter[l]<-as.character(Klattese4$C[match(letter[l],Klattese4$CKlattese)])}
    }
    ngram_syllable_klat4[k]<-paste(letter,collapse="")					
  }
  
  ngramtable$ngrams<-ngram_syllable_klat4
  return(ngramtable)
}

# 5단계: doNgram함수를 이용해서 고유어, 한자어, 외래어의 bigram, trigram, 4-gram을 만든 후, r 밖으로 저장합니다. 

# Klattese2의 결과
total_syllable_trigram2 = doNgram2(ngramn=3, stra="total")
total_syllable_bigram2 = doNgram2(ngramn=2, stra="total")
write.csv(total_syllable_trigram2, file=file.choose(), row.names=F)
write.csv(total_syllable_bigram2, file=file.choose(), row.names=F)

# Klattese4의 결과
total_syllable_trigram4 = doNgram4(ngramn=3, stra="total")
total_syllable_bigram4 = doNgram4(ngramn=2, stra="total")
write.csv(total_syllable_trigram4, file=file.choose(), row.names=F)
write.csv(total_syllable_bigram4, file=file.choose(), row.names=F)

native3<-doNgram(ngramn=3,stra="native")
native4<-doNgram(ngramn=4,stra="native")
sino3<-doNgram(ngramn=3,stra="sino")
sino4<-doNgram(ngramn=4,stra="sino")
foreign3<-doNgram(ngramn=3,stra="foreign")
foreign4<-doNgram(ngramn=4,stra="foreign")

# setwd(dir.choose())
write.csv(native3, file="native.csv",row.names = TRUE)
write.csv(sino3, file="sino.csv",row.names = TRUE)
write.csv(foreign3, file="foreign.csv",row.names = TRUE)
setwd(dir.choose())
write.csv(native3, file="native.csv",row.names = TRUE)
write.csv(sino3, file="sino.csv",row.names = TRUE)
write.csv(foreign3, file="foreign.csv",row.names = TRUE)