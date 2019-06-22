# <음절기반 음운이웃 만들기 전 단계>


# (44) 음절 기반 음운이웃을 만드는 방법입니다.  

# 아래는 위에서 한 작동과 동일합니다. 
## 표제어 자모단위로 분리. 위에서는 겹자음 처리를 Klattese에 직접 겹자음 변환표를 만들었습니다. 여기서는 그 대안입니다.  

#음절단위 character list는 철자형과 다릅니다. 예컨대, "얼음"과 같은 단어는 철자형이 "얼음", 음절단위 형태는 "어름"입니다.
# 음절단위 character list를 도출하기 위해 object 'result' (초성ㅇ제거한 자모단위 어휘형태 리스트)를 다시 조립합니다. KoNLP의 함수 HangulAutomata를 응용해서 사용합니다.

# KoNLP 패키지를 불러옵니다.
# if (!require(KoNLP)) install.packages("KoNLP")
# library(KoNLP)

# 함수 convertHangulStringToJamos를 이용해서 data 의 각 표제어를 자모로 분리합니다.
# 그런데 해당함수의 출력값은 음절단위로 나오므로 이 출력값을 받아줄 vector syllable을 선언합니다.
# 이 작업을 각 단어에 대해 실시해야 하는데 for-loop를 돌릴 것입니다. (for loop 대신, vectorization 혹은 apply계열 함수 사용해서 개선할 필요 있음)
# result <- vector()              # for loop 결과값 받아줄 result object 필요
# criteria_DoubleCoda<-read.table(file="https://www.dropbox.com/s/zy3h5hf6g3fsj75/double_coda.csv?dl=1", sep = ",", header=TRUE)
# for (i in 1:nrow(data)) {
#   syllable <- vector()
#   syllable <- convertHangulStringToJamos(data$entry[i])
#   for (j in 1:length(syllable)) {						# 반복문 안에서 또 반복문을 돌릴 건데 j는 1부터 'syllable'의 element 총 개수까지 하나씩 올라갑니다.
#     DC <- match(substr(syllable[j],3,3), criteria_DoubleCoda$double)
#     if (is.na(DC) == FALSE) {					#겹받침을 둘로 나눔 (eg. "ㄳ" -> "ㄱㅅ")
#       substr(syllable[j], 3, 4) <- as.character(criteria_DoubleCoda$separated[DC])
#     } 
#     phonemic <- unlist(strsplit(syllable[j], split=""))	# 'syllable'의 j번째 element를 각 자모단위로 분리해서 새로운 vector 'phonemic'에 넣습니다.
#     if(phonemic[1] == "ㅇ") {phonemic[1] <- ""}		# 첫번째 자모(즉, 초성)가 'ㅇ'이면, 그것을 제거합니다.
#     syllable[j] <- paste(phonemic, collapse="")		# 'phonemic'을 결합해서 다시 음절단위로 만듭니다. 그러나 초성의 ㅇ은 제거된 상태입니다.
#   }
#   result[i]<-paste(syllable, collapse="")				# 그 결과를 result에 저장합니다.
# }

# 음절단위의 이웃을 구하기 위해서는 object 'x'에 음절단위의 character list가 들어가야 합니다.
# 예컨대, 아래 예시처럼, 앞서 생성한 object 'phoneme_klat'을 x에 집어넣으면 음소기반 음운이웃으로 산출합니다.

# x <- as.character(phoneme_klat)

# 그러나 음절기반 음운이웃을 산출하려면, 아래와 같이, 음절단위 발음기호 목록 (예컨데,by_syllable)을 넣습니다. 

# x <- as.character(syllable_result)

# 먼저, 힌글 표기 단어들을 음절 단위로 구분하여야 합니다. 문제가 될 수 있는 것은 "ㅇ"이 초성에 나오는 음절입니다.
# 이를 해결하기 위해서 음절을 재정렬할 필요가 있습니다. 
# 즉, 자모를 음절로 바꾸기 위해서는 초성에 ㅇ이 추가되어야하는지 여부를 결정해야 합니다. 
# 어두 모음, 혹은 모음-모음 연쇄의 경우, dummy ㅇ의 삽입이 필요합니다.
# 그리고 어두에 모음이 오는지 혹은 모음-모음의 연쇄가 있는지 여부를 확인하려면, 한글철자 중 무엇이 모음이고 무엇이 자음인지 지정해야 합니다.
# 각 한글자모에 대해 자음이면 C, 모음이면 V를 마킹해주는 하는 함수가 CV_mark입니다.
# 먼저, Klattese를 불러 옵니다. 

CV_ref = Klattese
CV_ref2 = Klattese2
CV_ref4 = Klattese4

CV_ref
CV_ref2
CV_ref4

# 이제, 자모 (phoneme_result)를 재음절화된 형태의 음절로 바꿉니다. 
# CV_mark는 자모분리된 단어형을 input으로 받아서 각 자모가 자음인지 모음인지 알려주는 함수입니다. 
# 실제로 사용자가 이 함수를 쓸 일은 없고, HA_mod 또는 기타 자음/모음에 대한 정보가 필요한 함수가 내부적으로 사용할 수 있도록 의도되었습니다.

CV_mark <- function(input){
  output <- vector()
  phoneme <- unlist(strsplit(input,split=""))
  for (j in 1:length(phoneme)){
    if (is.na (match (phoneme[j], CV_ref$C)) == TRUE) {
      phoneme[j]="V"
    }
    else {phoneme[j]="C"
    }
  }
  output <- paste(phoneme, collapse="")
  return(output)
}

# Klattese2의 결과
CV_mark2 <- function(input){
  output <- vector()
  phoneme <- unlist(strsplit(input,split=""))
  for (j in 1:length(phoneme)){
    if (is.na (match (phoneme[j], CV_ref2$C)) == TRUE) {
      phoneme[j]="V"
    }
    else {phoneme[j]="C"
    }
  }
  output <- paste(phoneme, collapse="")
  return(output)
}

# Klattese4의 결과
CV_mark4 <- function(input){
  output <- vector()
  phoneme <- unlist(strsplit(input,split=""))
  for (j in 1:length(phoneme)){
    if (is.na (match (phoneme[j], CV_ref4$C)) == TRUE) {
      phoneme[j]="V"
    }
    else {phoneme[j]="C"
    }
  }
  output <- paste(phoneme, collapse="")
  return(output)
}

# CV_mark를 이용해서 자모음 표시를 해준 다음, 실제로 어두의 모음, 혹은 모음-모음 연쇄의 경우, 
# dummy ㅇ의 삽입한 다음, HangulAutomata를 돌리는 함수 HA_mod (HangulAutomata_modified)를 만듭니다.
# HangulAutomata는 에러가 발생할 것이 예상됩니다. 
# 따라서 HA_mod 함수를 구성할 때, error handling 기법을 이용해서, 
# HangulAutomata에서 error가 발생하는 경우 별도로 사용자의 입력을 받도록 했습니다. 
# R에서의 error handling 구현에 대해서는 https://www.r-bloggers.com/error-handling-in-r/ 이곳을 참고했습니다. 

HA_mod <- function(input){
  if (!is.character(input) | nchar(input) == 0) {
    stop("Input must be legitimate character!")
  }
  cv <- CV_mark(input)
  input_split <- unlist(strsplit(input,split=""))
  cv_split <- unlist(strsplit(cv,split=""))
  if (cv_split[1] == "V") {                        # add empty 'ㅇ' before a V-starting word.
    input_split <- c("N", input_split)
    cv_split <- c("C", cv_split)
  }
  i = 2
  j = length(input_split)
  while (i <= j){
    if (cv_split[i] == "V"){
      if (cv_split[i-1] == "V"|input_split[i-1] == "ㅇ") {
        cv_split <- c(cv_split[1:(i-1)], "C", cv_split[i:length(cv_split)])
        input_split <- c(input_split[1:(i-1)], "N", input_split[i:length(input_split)])
      }
    }
    i = i + 1
    j = length(input_split)
  }
  input_split <- gsub("N", "ㅇ", input_split)
  input <- paste(input_split, collapse="")
  tryCatch(
    output <- HangulAutomata(input, isForceConv = T),
    error = function(e) {
      confirm <- ""
      while (tolower(confirm) != "y"){
        userinput <- readline(prompt = paste0("What should be the Hangul syllables for ","\"", input,"\"?     \n>> "))
        confirm <- readline(prompt = paste0(input," = ", userinput, " . Are you sure (y/n)? (y나 Y 대신 'ㅛ'입력 가능) "))
        if (confirm =="ㅛ") {confirm <- "y"}
      }
      output <<- userinput
    }
  )
  return(output)
}

# Klattese2의 결과

HA_mod2 <- function(input){
  if (!is.character(input) | nchar(input) == 0) {
    stop("Input must be legitimate character!")
  }
  cv <- CV_mark2(input)
  input_split <- unlist(strsplit(input,split=""))
  cv_split <- unlist(strsplit(cv,split=""))
  if (cv_split[1] == "V") {                        # add empty 'ㅇ' before a V-starting word.
    input_split <- c("N", input_split)
    cv_split <- c("C", cv_split)
  }
  i = 2
  j = length(input_split)
  while (i <= j){
    if (cv_split[i] == "V"){
      if (cv_split[i-1] == "V"|input_split[i-1] == "ㅇ") {
        cv_split <- c(cv_split[1:(i-1)], "C", cv_split[i:length(cv_split)])
        input_split <- c(input_split[1:(i-1)], "N", input_split[i:length(input_split)])
      }
    }
    i = i + 1
    j = length(input_split)
  }
  input_split <- gsub("N", "ㅇ", input_split)
  input <- paste(input_split, collapse="")
  tryCatch(
    output <- HangulAutomata(input, isForceConv = T),
    error = function(e) {
      confirm <- ""
      while (tolower(confirm) != "y"){
        userinput <- readline(prompt = paste0("What should be the Hangul syllables for ","\"", input,"\"?     \n>> "))
        confirm <- readline(prompt = paste0(input," = ", userinput, " . Are you sure (y/n)? (y나 Y 대신 'ㅛ'입력 가능) "))
        if (confirm =="ㅛ") {confirm <- "y"}
      }
      output <<- userinput
    }
  )
  return(output)
}

# Klattese4의 결과

HA_mod4 <- function(input){
  if (!is.character(input) | nchar(input) == 0) {
    stop("Input must be legitimate character!")
  }
  cv <- CV_mark4(input)
  input_split <- unlist(strsplit(input,split=""))
  cv_split <- unlist(strsplit(cv,split=""))
  if (cv_split[1] == "V") {                        # add empty 'ㅇ' before a V-starting word.
    input_split <- c("N", input_split)
    cv_split <- c("C", cv_split)
  }
  i = 2
  j = length(input_split)
  while (i <= j){
    if (cv_split[i] == "V"){
      if (cv_split[i-1] == "V"|input_split[i-1] == "ㅇ") {
        cv_split <- c(cv_split[1:(i-1)], "C", cv_split[i:length(cv_split)])
        input_split <- c(input_split[1:(i-1)], "N", input_split[i:length(input_split)])
      }
    }
    i = i + 1
    j = length(input_split)
  }
  input_split <- gsub("N", "ㅇ", input_split)
  input <- paste(input_split, collapse="")
  tryCatch(
    output <- HangulAutomata(input, isForceConv = T),
    error = function(e) {
      confirm <- ""
      while (tolower(confirm) != "y"){
        userinput <- readline(prompt = paste0("What should be the Hangul syllables for ","\"", input,"\"?     \n>> "))
        confirm <- readline(prompt = paste0(input," = ", userinput, " . Are you sure (y/n)? (y나 Y 대신 'ㅛ'입력 가능) "))
        if (confirm =="ㅛ") {confirm <- "y"}
      }
      output <<- userinput
    }
  )
  return(output)
}


# HA_mod <- function(input){
#   if (!is.character(input) | nchar(input) == 0) {
#     stop("Input must be legitimate character!")
#   }
#   cv <- CV_mark(input)
#   input_split <- unlist(strsplit(input,split=""))
#   cv_split <- unlist(strsplit(cv,split=""))
#   if (cv_split[1] == "V") {                        # solving empty onset problem
#     input_split <- c("ㅇ", input_split)
#     cv_split <- c("C", cv_split)
#   }
#   i = 2
#   j = length(input_split)
#   while (i <= j){
#     if (cv_split[i] == "V" & cv_split[i-1] == "V") {
#       cv_split <- c(cv_split[1:(i-1)], "C", cv_split[i:length(cv_split)])
#       input_split <- c(input_split[1:(i-1)], "ㅇ", input_split[i:length(input_split)])
#     }
#     i = i + 1
#     j = length(input_split)
#   }
#   input <- paste(input_split, collapse="")
#   tryCatch(
#     output <- HangulAutomata(input, isForceConv = T),
#     error = function(e) {
#       confirm <- ""
#       while (tolower(confirm) != "y"){
#         userinput <- readline(prompt = paste0("What should be the Hangul syllables for ","\"", input,"\"?     \n>> "))
#         confirm <- readline(prompt = paste0(input," = ", userinput, " . Are you sure (y/n)? (y나 Y 대신 'ㅛ'입력 가능) "))
#         if (confirm =="ㅛ") {confirm <- "y"}
#       }
#       output <<- userinput
#     }
#   )
#   return(output)
# }

# 정의된 함수를 이용해서, 자모로 된 분석대상어휘(r object 'phoneme_result')를 음절단위로 묶어주고 그것을 r object 'syllable_result'이라고 합니다.
# 이때 연산의 속도를 위해 rapply 함수를 사용합니다.

list_phoneme_result <- as.list(phoneme_result)
syllable_result <- rapply(list_phoneme_result,HA_mod)
write.csv(syllable_result, file=file.choose(), row.names=F)

# Klattese2의 결과
syllable_result2 <- rapply(list_phoneme_result,HA_mod2)
write.csv(syllable_result2, file=file.choose(), row.names=F)

# Klattese4의 결과
syllable_result4 <- rapply(list_phoneme_result,HA_mod4)
write.csv(syllable_result4, file=file.choose(), row.names=F)


# 음절기반 재음절화 목록 syllable_result를 확인합니다.
# 확인 결과 종성 "ㅇ", 초성 "ㅇ" 연쇄, 예를 들면, "그동안" 같은 것을 완벽히 음절화하지 못합니다. 
# 자동화 방법이 없다면 r 밖으로 저장한 뒤 수동화하여야 합니다. 
# 그러나 위의 수정된 코드는 종성 "ㅇ", 초성 "ㅇ" 연쇄, 예를 들면, "그동안" 같은 것을 완벽히 음절화한 결과를 
# 산출합니다. 
# 마지막으로 음운이웃쌍을 계산할 단위인 x에 syllable_result을 지정하고, R 밖으로 저장합니다. 

syllable_result2
syllable_result4

x <- syllable_result
x2 <- syllable_result2
x4 <- syllable_result4

write.csv(syllable_result, file=file.choose(), row.names=F)