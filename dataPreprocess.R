# <data 전처리>

# (1) 한국어 raw data를 불러오기 위해 read.csv() 함수를 사용하고 불러온 것을 raw_data라는 이름으로 저장합니다.  

raw_data = read.csv(file = file.choose(), header = TRUE)

# (raw_data = read.csv(file = file.choose(), header = T, sep = ",", quote=""): 이건 엑셀 내부 쉼표 때문에 오류 발생)

# (2) raw_data에 저장된 데이터 entry 즉, raw_data$entry에는 꼬리 번호가 있습니다. 이것을 제거하기 위해서는  gsub() 함수를 사용합니다.

raw_data$entry <- gsub("__[[:alnum:]]+", "", raw_data$entry) # 원 데이터에 이상한 기호 또는 한자 포함된 것 해결 안 됨
raw_data$entry <- gsub("\\(.+\\)", "", raw_data$entry)    # 괄호 제거 
raw_data$entry <- gsub(" ", "", raw_data$entry) # 띄어쓰기 제거 

# 꼬리 번호 제거는 다음 두 식을 연달아 돌려도 해결 가능합니다. 
# raw_data$entry <- gsub("__[[:digit:]]+", "", raw_data$entry)
# raw_data$entry <- gsub("__x[[:digit:]]+", "", raw_data$entry)

# (3) 이제 꼬리 번호가 제거된 데이터를 data라는 이름으로 저장합니다.

data = raw_data
data$entry

# (4) 꼬리 번호가 제거된 데이터를 write.csv()함수를 사용하여 R 밖읋 빼내어 csv 파일로 저장합니다.

write.csv(data, file=file.choose(), row.names=F)

# (5) data에 저장된 데이터 즉, data$entry에는 아직 동음이의어들이 있습니다. 이 동음이의어들을 제거하고자 한다면, 다음 식을 사용하기 바랍니다. 
# 만약 (8)번을 수행하여 품사별로 나누어 분석하고자 한다면, (8)번을 먼저 수행ㅘㄴ 후, 동음이의어를 제거하기 위해 (5)번을 수행하는 것이 
# 바람직하다. 

duplicated <- which(duplicated(data$entry))
if(length(duplicated)>0) single_data <- data[-duplicated,]

# (6) single_data$entry을 사용해서 위 식이 제대로 작동했는지 확인할 수 있습니다.

single_data$entry

# (파일 설명) single_data는 동음이의어가 제거된 단어와 그 관련 정보가 포함된 목록이고, 
# single_data$entry는 음절 단위로 묶인 동음이의어가 제거된 단어만 포함된 것입니다. 

# (7) 동음이의어가 제거된 데이터를 write.csv()함수를 사용하여 R 밖읋 빼내어 csv 파일로 저장합니다.

write.csv(single_data, file=file.choose(), row.names=F)

# (8) 꼬리 번호가 제거되고 동음이의어가 제거된 데이터에서 언하는 항목만 추출하시길 바라십니까? 
# 그렇다면 명사만을 제거한다고 가정하고 다음과 같이 해보시길 바랍니다.  

whatPOS<-"NNG"
NNG_data<- single_data[grep(whatPOS,single_data$POS),]

# (9) 명사 데이터 즉, NNG_data를 R 밖으로 빼내어 저장하려면, 역시 write.csv()함수를 사용합니다.

write.csv(NNG_data, file=file.choose(), row.names=F)