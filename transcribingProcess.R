# <발음기호로 전환>

# (14) 이제 한글 표기를 분석 가능한 발음 기호로 변환해야 합니다. 
# 이를 위해 sub 함수를 사용해서 phoneme_result를 발음기호 변환표를 이용하여 변환합니다. 여기서는 원조 Klattese를 사용하는 방법을 기술합니다,
# (sub 함수를 이용해서 X => Y 변환을 합니다. X는 한글자모 Y는 그것에 대응하는 klattese입니다. 
# 대응표 파일()을 확인하시고 무엇으로부터 무엇으로 변환되는지 조작하면 다른 convention으로도 변환할 수 있습니다.
# 대응표 파일의 링크는 다음과 같습니다: https://www.dropbox.com/s/tzfr4eadb0nc0nj/klattese.csv?dl=1 
# 예컨대 klattese symbols 대신 ipa symbols 넣으면 자모 to IPA가 됩니다.)
# 엑셀 파일 Klattese1은 우리가 최초로 정한 Klattese이고, Klattese2는 Klattese1에 직관을 반영하여 수정한 것이다.
# Klattese3은 복모음을 <전이음 + 단모음>으로 보고 정한 발음기호이다. 이것에서는 '외'가 'w@'로 표기되었는데, 
# 이것이 직관적이지 않은 단점이 있다. 
# Klattese4는 11 단모음 체계를 받아들여, '외'를 'O', '위'를 'y'로 표기한 발음기호이다. 
# 연습을 위해서는 Klattese2와 Klattese4를 가지고 시도해 볼 것을 권한다. Klattese2를 불러들일 때는 Klattese2로 저장하고
# Klattese4를 불러들일 때는 Klattese4로 저장하기로 한다. 

Klattese2<-read.table(file = file.choose(), sep = ",", header=TRUE, comment.char = "") 
Klattese4<-read.table(file = file.choose(), sep = ",", header=TRUE, comment.char = "") 

# comment.char = ""은 # -> # 변환처리를 위해 필요합니다.
# 실제 구현할 때는 아래와 같이 Klattese로 저장하자.
# Klattese<-read.table(file = file.choose(), sep = ",", header=TRUE, comment.char = "") 



# (15) Klattese 변화 표가 R 내부에 제대로 들어왔는지 다음과 같이 꼭 환인하여야 합니다. 때론 여기서 오류가 많이 발생합니다.

Klattese

# (16) 이제 발음 기호로 변환합니다. 결과를 담을 백터 이름을 phoneme_klat이라고 칭하겠습니다. 식은 다음과 같습니다.

phoneme_klat<-vector()														# Klattese로 변환한 결과를 저장할 vector 'klat'을 선언합니다.
for (k in 1:length(phoneme_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
  letter<-unlist(strsplit(phoneme_result[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese$C))==TRUE){letter[l]<-as.character(Klattese$VKlattese[match(letter[l],Klattese$V)])
    
    }
    else{letter[l]<-as.character(Klattese$CKlattese[match(letter[l],Klattese$C)])}
  }
  phoneme_klat[k]<-paste(letter,collapse="")					
}

# Klattese2의 결과 

phoneme_klat2<-vector()														# Klattese로 변환한 결과를 저장할 vector 'klat'을 선언합니다.
for (k in 1:length(phoneme_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
  letter<-unlist(strsplit(phoneme_result[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese2$C))==TRUE){letter[l]<-as.character(Klattese2$VKlattese[match(letter[l],Klattese2$V)])
    
    }
    else{letter[l]<-as.character(Klattese2$CKlattese[match(letter[l],Klattese2$C)])}
  }
  phoneme_klat2[k]<-paste(letter,collapse="")					
}

# Klattese4의 결과 

phoneme_klat4<-vector()														# Klattese로 변환한 결과를 저장할 vector 'klat'을 선언합니다.
for (k in 1:length(phoneme_result)) {										# for문을 이용해서 각 자모를 Klattese로 변환하고 그것을 합칩니다.
  letter<-unlist(strsplit(phoneme_result[k],split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l],Klattese4$C))==TRUE){letter[l]<-as.character(Klattese4$VKlattese[match(letter[l],Klattese4$V)])
    
    }
    else{letter[l]<-as.character(Klattese4$CKlattese[match(letter[l],Klattese4$C)])}
  }
  phoneme_klat4[k]<-paste(letter,collapse="")					
}

# (17) 발음 기호 변환이 제대로 되었는지 확인이 필요합니다. 이를 위해 phoneme_klat을 확인합니다.

phoneme_klat
phoneme_klat2
phoneme_klat4

# (18) phoneme_klat을 R 밖으로 저장하려면, 

write.csv(phoneme_klat, file=file.choose(), row.names=F)
write.csv(phoneme_klat2, file=file.choose(), row.names=F)
write.csv(phoneme_klat4, file=file.choose(), row.names=F)


# (19) 이제 여기까지의 결과를 모아서 표로 만듭니다. 

output_data <- cbind(single_data,phoneme_result,phoneme_klat)
output_data2 <- cbind(single_data,phoneme_result,phoneme_klat2)
output_data4 <- cbind(single_data,phoneme_result,phoneme_klat4)

# (20) output_data의 정상 여부를 확인하고 이것을 R 밖으로 저장합니다.

output_data
output_data2
output_data4

write.csv(output_data, file=file.choose(), row.names=F)
write.csv(output_data2, file=file.choose(), row.names=F)
write.csv(output_data4, file=file.choose(), row.names=F)