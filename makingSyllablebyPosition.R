# <단어 위치별 음절 목록 및 빈도 구하기>

# (49) 어두 어중 어말 별로 음절 분리하기 
# (아무런 음운작용, 재음절화 등 하지 않고 글자단위로 나누고 어두 어중 어말 추출하기) (input: Korean PNN from http://namsling.tistory.com/9)
# firstsyllable.csv, lastsyllable.csv, totalsyllable.csv, wordinternalsyllable.csv 를 저장할 위치 지정
setwd(choose.dir())

# 한국어 raw data를 불러오기 위해 read.csv() 함수를 사용하고 불러온 것을 raw_data라는 이름으로 저장합니다.  

raw_data = read.csv(file = file.choose(), header = TRUE)

# (raw_data = read.csv(file = file.choose(), header = T, sep = ",", quote=""): 이건 엑셀 내부 쉼표 때문에 오류 발생)

# raw_data에 저장된 데이터 entry 즉, raw_data$entry에는 꼬리 번호가 있습니다. 이것을 제거하기 위해서는  gsub() 함수를 사용합니다.

raw_data$entry <- gsub("__[[:alnum:]]+", "", raw_data$entry) # 원 데이터에 이상한 기호 또는 한자 포함된 것 해결 안 됨
raw_data$entry <- gsub("\\(.+\\)", "", raw_data$entry)    # 괄호 제거 
raw_data$entry <- gsub(" ", "", raw_data$entry) # 띄어쓰기 제거 

# 꼬리 번호 제거는 다음 두 식을 연달아 돌려도 해결 가능합니다. 
# raw_data$entry <- gsub("__[[:digit:]]+", "", raw_data$entry)
# raw_data$entry <- gsub("__x[[:digit:]]+", "", raw_data$entry)

# 이제 꼬리 번호가 제거된 데이터를 data라는 이름으로 저장합니다.

data = raw_data
data$entry

# 꼬리 번호가 제거된 데이터를 write.csv()함수를 사용하여 R 밖읋 빼내어 csv 파일로 저장합니다.

write.csv(data, file=file.choose(), row.names=F)

# data에 저장된 데이터 즉, data$entry에는 아직 동음이의어들이 있습니다. 이 동음이의어들을 제거하고자 한다면, 다음 식을 사용하기 바랍니다. 
# 만약 (8)번을 수행하여 품사별로 나누어 분석하고자 한다면, (8)번을 먼저 수행ㅘㄴ 후, 동음이의어를 제거하기 위해 (5)번을 수행하는 것이 
# 바람직하다. 

duplicated <- which(duplicated(data$entry))
if(length(duplicated)>0) single_data <- data[-duplicated,]

# single_data$entry을 사용해서 위 식이 제대로 작동했는지 확인할 수 있습니다.

single_data$entry

# 동음이의어가 제거된 데이터를 write.csv()함수를 사용하여 R 밖읋 빼내어 csv 파일로 저장합니다.

write.csv(single_data, file=file.choose(), row.names=F)

# 층위별로 음절(글자) 단위로 나누는 함수 syllablefreq 만들기

# 어두 어중 어말 별로 음절 분리하기 (아무런 음운작용, 재음절화 등 하지 않고 글자단위로 나누고 어두 어중 어말 추출하기) (input: Korean PNN from http://namsling.tistory.com/9)
## firstsyllable.csv, lastsyllable.csv, totalsyllable.csv, wordinternalsyllable.csv 를 저장할 위치 지정
setwd(choose.dir())

# 층위별로 음절(글자) 단위로 나누는 함수 syllablefreq 만들기

# syllablefreq <- function(stra=NULL) {
#   library(ngram)
#   if(is.null(stra)) stra="total"
#   stra<-as.character(stra)
#   if (stra == "total") {
#   stratum<-word.list
#   } else stratum<-word.list[which(word.list$type==stra),]


# 자모분리 하지 않고 음절별 (글자별)로 분리한다.
freq_data = as.character(single_data$entry) 
freq_data = gsub(" ","",freq_data)
freq_data = strsplit(freq_data,split="")

#lapply 이용해서 list의 첫번째 element들만 firstsyllable 이라는 vector로 모은다.

first_syllable = lapply(freq_data, `[[`, 1)
first_syllable = unlist(first_syllable)
freq_first_syllable = table(first_syllable)
prop_first_syllable = prop.table(freq_first_syllable) 
first_syllable_output = cbind(freq_first_syllable, prop_first_syllable)
write.csv(first_syllable_output,"firstsyllable.csv")

#sapply 이용해서 list의 마지막 element들만 lastsyllable 이라는 vector로 모은다.

last_syllable = sapply(freq_data, tail, 1)
last_syllable = unlist(last_syllable)
freq_last_syllable = table(last_syllable)
prop_last_syllable = prop.table(freq_last_syllable) 
last_syllable_output = cbind(freq_last_syllable, prop_last_syllable)
write.csv(last_syllable_output,"lastsyllable.csv")

# 전체 syllable 구하기 위해,

total_syllable = unlist(freq_data)
freq_total_syllable = table(total_syllable)
prop_total_syllable = prop.table(freq_total_syllable) 
total_syllable_output <- cbind(freq_total_syllable, prop_total_syllable)
write.csv(total_syllable_output,"totalsyllable.csv")

# 어중 음절 추출해서 빈도표를 구하는 방식

result<-vector()
for (i in 1:length(single_data$entry)) {
  if (length(freq_data[[i]]) > 2) {
    for (j in 2:(length(freq_data[[i]])-1)){
      result<-c(result,freq_data[[i]][j])
    }
    
  }
  
}

mid_resulttable = table(result)
mid_resultprop = prop.table(mid_resulttable)
mid_syllable_output = cbind(mid_resulttable, mid_resultprop)
write.csv(cbind(mid_resulttable,mid_resultprop),"wordinternalsyllable.csv")

# }