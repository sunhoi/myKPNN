# <한국어 데이터>
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

# (21) 이제 지금까지 작업으로 얻은 자료의 음운이웃 목록을 만듭니다. 
#기존 작업을 이어서 하는 경우에는 바로 x = as.character(phoneme_klat)을 이용해서 발음기호로 변환된 단어를 문자로 변환하고 
# 외부의 저장된 phoneme_klat을 사용할 경우에는 phoneme_klat = read.csv(file = file.choose(), header = TRUE)으로 R 내부로 불러 들인 후  
# x <- as.character(as.matrix(phoneme_klat))을 
# 사용하여 원자료의 데이터 유형인 데이터 프레임을 행렬(matrix)로 변환하여 다시 문자로 변환합니다. 
# 이러한 작업 이전에 병렬처리를 위한 R 패키지인 "doParallel"을 설치합니다.

if (!require(doParallel)) install.packages("doParallel")
library(doParallel)

# core 개수에 따라 작업을 나누라고 명령합니다.(이유 설명 요함)
# detectCores()는, 컴퓨터 CPU의 core 개수를 출력합니다. makeCluster()는 그 core의 개수만큼 할일을 clustering (분리) 합니다.
# 온전히 PNN작업만 할 경우, core 전부를 사용해서 병렬처리할 수 있으나, 
# 대부분의 경우, 백그라운드에서 백신을 돌리거나 아니면 웹브라우징, 문서작업 등등 다른 작업을 하는 경우가 많으므로 
# core전부를 사용하기보다는 detectCores()-1 를 통해 하나의 코어를 여분으로 남겨두는 것이 일반적입니다.
# 하지만 지금은 실습을 빠르게 하기 위해 core전부를 사용합니다.

cl <- makeCluster(detectCores())							
registerDoParallel(cl)

# 무엇을 unit으로 할지 지정합니다 (Klattese 혹은 자모 혹은 IPA...) 음운이웃을 따질 unit으로 표기된 표제어 목록을 object 'x' 로 지정합니다. 
# 예컨대, 아래 예시처럼, 앞서 1.3에서 생성한 object 'klat'을 x에 집어넣으면 
# klattese symbol상 unit distance = 1인 두 단어를 음운이웃으로 산출합니다.


# phoneme_klat = read.csv(file = file.choose(), header = TRUE)
# phoneme_klat
# class(phoneme_klat)
# x <- as.character(as.matrix(phoneme_klat))

x = as.character(phoneme_klat) 
x2 = as.character(phoneme_klat2)
x4 = as.character(phoneme_klat4)

# (22) 문자로 변환된 x를 확인합니다. 겹따옴표로 목록이 나오면 정상입니다.
x
x2
x4
head(x)

# (23) 음운 이웃을 만드는 코드입니다. 
# 결과값을 저장할 object인 neighbor_result를 미리 reserve해 놓습니다.

neighbor_result <- vector()

neighbor_result <- foreach (i=1:length(x), .combine='rbind', .packages="base") %dopar% {
  output <- vector()
  for (j in i:length(x)) {
    if(nchar(x[j]) > (nchar(x[i])-2) & nchar(x[j]) < (nchar(x[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
      if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
    }
  }
  output
}
alarm()

# Klattese2의 결과 

neighbor_result2 <- vector()

neighbor_result2 <- foreach (i=1:length(x2), .combine='rbind', .packages="base") %dopar% {
  output <- vector()
  for (j in i:length(x2)) {
    if(nchar(x2[j]) > (nchar(x2[i])-2) & nchar(x2[j]) < (nchar(x2[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
      if(adist(x2[i], x2[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
    }
  }
  output
}
alarm()

# Klattese4의 결과
neighbor_result4 <- vector()

neighbor_result4 <- foreach (i=1:length(x4), .combine='rbind', .packages="base") %dopar% {
  output <- vector()
  for (j in i:length(x4)) {
    if(nchar(x4[j]) > (nchar(x4[i])-2) & nchar(x4[j]) < (nchar(x4[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
      if(adist(x4[i], x4[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
    }
  }
  output
}
alarm()

# 함수 foreach와 adist를 통해 실제로 음운이옷 목록 만들기 
# 기본적으로 x에 존재하는 element의 개수의 조합 만큼 연산을 하므로, 연산속도는 element 개수에 민감합니다.
# 알고리즘적으로, 절대 음운이웃이 될수 없는 두 단어 (단어길이가 2 이상 차이나는 두 단어)를 연산에서 배제하면 연산속도가 빨라집니다.
# 마지막 줄의 alarm()은, 연산이 모두 끝났을 때 그 사실을 컴퓨터가 사용자에게 알려주기 위해 들어갑니다.
# 연산시간이 무척 길기 때문에, 연산이 얼마나 진행되었는지 퍼센티지로 보여주거나, 연산이 모두 끝났을 때 컴퓨터가 사용자에게 알려주는 명령어를 넣는 것이 관행입니다. 
# 기본기능만 구현하므로 진행상황를 출력하는 코드를 넣지는 않았습니다. 하지만 연산이 끝났을 때까지 다른 일을 할 수 있도록, alarm()을 넣었습니다.

# (24) 음운이웃 작동 결과인 neighbor_result를 확인하고, 필요시 외부에 저장합니다. 

head(neighbor_result)
neighbor_result
neighbor_result2
neighbor_result4

write.csv(neighbor_result, file=file.choose(), row.names=F)
write.csv(neighbor_result2, file=file.choose(), row.names=F)
write.csv(neighbor_result4, file=file.choose(), row.names=F)

# (25) 드디어 1차 목표인 음운이웃 네트워크 지표들을 R package 'igraph' 이용하여 산출합니다.
# package 'igraph' 설치하고, igraph network type 오브젝트 생성
# 앞서 KoNLP, doParallel등의 패키지를 불러온 것과 같은 방식으로 igraph package도 불러옵니다.

if (!require(igraph)) install.packages("igraph")
library(igraph)

# (26) 'net'이라는 이름으로 igraph object를 생성합니다. 
# igraph에 포함된 함수 graph_from_data_frame를 이용합니다. 
# 이 함수가 취하는 argument에는 여러가지가 있는데, 그중
# d: 연결되는 두 node에 대한 정보
# vertices: 각 node에 대한 정보: 각 어휘의 빈도, 층위, 등등등)
# directed: directed network인지 여부

single_data$id = 1:length(single_data$entry)

net =graph_from_data_frame(d=neighbor_result, vertices=single_data,directed=F)
net2 =graph_from_data_frame(d=neighbor_result2, vertices=single_data,directed=F)
net4 =graph_from_data_frame(d=neighbor_result4, vertices=single_data,directed=F)

# 생성된 네트워크인 'net'을 시각화할 수 있습니다.

plot(net, vertex.label=single_data$entry, vertex.shape="none", vertex.label.cex=0.8, edge.width=2, edge.arrow.size=0, margin=0)
plot(net2, vertex.label=single_data$entry, vertex.shape="none", vertex.label.cex=0.8, edge.width=2, edge.arrow.size=0, margin=0)
plot(net4, vertex.label=single_data$entry, vertex.shape="none", vertex.label.cex=0.8, edge.width=2, edge.arrow.size=0, margin=0)

# 발음기호로 이웃관계 시각화
# plot(net2, vertex.label=x2, vertex.shape="none", vertex.label.cex=0.8, edge.width=2, edge.arrow.size=0, margin=0)

# <대안>  edge_network라는 이름으로 igraph object를 생성합니다. 
# igraph에 포함된 함수 graph_from_data_frame를 이용합니다. 이 함수가 취하는 argument에는 여러가지가 있는데, 
# 그중 d는 연결되는 두 node에 대한 정보이고 directed는 directed network인지 여부이며, vertices는 어휘빈도, 층위 등 각 node에 대한 정보입니다.
# 지금은 어휘 정보를 고려하지 않으므로 vertices는 무시합니다.
# network를 시각화 하기 위해서는 음운이웃 직덩 결과인 음운이웃 리스트 neighbor_result를 
# graph_from_data_frame() 함수에 넣어 아래와 같이 igraph에 적합한 edge list를 만듭니다. 
 
# edge_network = graph_from_data_frame(d=neighbor_result, directed=F)

# edge_network에는 단어가 연결된 것이 기재되어 있는 것이 아니라, 
# 단어의 해당 번호가 연결되어 있습니다. 왜냐하면, neighbor_result에 단어의 해당 번호가 기재되어 있기 때문입니다.
# 따라서 이제 다시 번호에 해당하는 단어를 찾아 연결하여야 합니다. 

# (27) V(edge_network)$name)으로 연결 번호 목록을 찾은 뒤 이를 as.numeric() 함수로 numeric으로 변환하여, 
# vertex_number라고 칭합니다.

# vertex_number = as.numeric(V(edge_network)$name)
# class(vertex_number) # numeric으로 정상 변환되었나 확인

# (28) 그럼, 각 번호에 해당하는 단어를 추출하여 vertex_name에 저장합니다.   
# vertex_name = single_data$entry[vertex_number]
# vertex_name

# (29) 이제 생성된 네트워크인 edge_network을 시각화합니다. 
# vertex.label에는 음절 기반 한글 표기가 기재되어 있는 vertex_name가 들어가야 합니다. 

# plot(edge_network, layout=layout.fruchterman.reingold, vertex.label=vertex_name, vertex.shape="none", vertex.label.cex=0.8, edge.width=2, edge.arrow.size=1.2, margin=0)

# 위의 시각화는 단어 목록이 1000개만 되어도 시각화 효과가 떨어집니다. 
# 따라서 single_data$entry (단어 930개)로는 위의 시각화가 잘 작동하는지 알 길이 없습니다. 
# 정상 작동 여부를 시험하려면 단어 1000개를 가진 원자료를 200개만 잘라서 검증해 보는게 좋습니다.



# (30) Assortative Mixing by Degree 구해서 'amd'라는 이름의 vector로 저장합니다.

amd <- assortativity_degree(net)
amd2 <- assortativity_degree(net2)
amd4 <- assortativity_degree(net4)

# (31) Cluster의 개수를 구해서 no.cl라는 이름의 vector로 저장합니다.

no.cl <- components(net)$no
no.cl2 <- components(net2)$no
no.cl4 <- components(net4)$no

# (32) 전체 network의 node 개수를 구해서 size에, 
# largest cluster (giant cluster)에 속한 node의 개수를 구해서 gc에, 
# clustering coefficient 구해서 cc에, 
# 평균이웃개수 구해서 mean_neigh에, average shortest path length 구해서 aspl로 저장합니다.

size <- length(x)
size2 <- length(x2)
size4 <- length(x4)

gc <- components(net)$csize[which.max(components(net)$csize)]	# GC에 속한 node 개수를 구해서 'gc'라는 이름의 vector로 저장합니다.
gc2 <- components(net2)$csize[which.max(components(net2)$csize)]
gc4 <- components(net4)$csize[which.max(components(net4)$csize)]

cc <- transitivity(net, type = "global")			# Clustring coefficient 구해서 'cc'로 저장
cc2 <- transitivity(net2, type = "global")
cc4 <- transitivity(net4, type = "global")

mean_neigh <- mean(degree(net))					# 평균 이웃 개수를 구해서 'mean_neigh'로 저장
mean_neigh2 <- mean(degree(net2))	
mean_neigh4 <- mean(degree(net4))	

aspl <- mean_distance(net, directed=F)				# Average shortest path length 구해서 'aspl'로 저장
aspl2 <- mean_distance(net2, directed=F)
aspl4 <- mean_distance(net4, directed=F)


# (33) 마지막으로 네트워크 수치들을 cbind() 사용해서 합친 후, write.csv() 이용해서 외부파일로 저장합니다.

index_result <- cbind(size, gc, cc, mean_neigh, amd, aspl) # 정리해서 출력합니다.
index_result2 <- cbind(size2, gc2, cc2, mean_neigh2, amd2, aspl2)
index_result4 <- cbind(size4, gc4, cc4, mean_neigh4, amd4, aspl4)

print(index_result)
print(index_result2)
print(index_result4)

write.csv(net_result, file=file.choose(), row.names=F) 
write.csv(net_result2, file=file.choose(), row.names=F) 
write.csv(net_result4, file=file.choose(), row.names=F) 

# (34) 음운이웃 리스트를 pajek이 읽을 수 있는 형태로 전환하는 방법입니다.  
# 일단, x의 내용을 체크합니다. x는 발음 기호롤 변환된 문자 데이터이어야 합니다. 
x

# .net파일의 형식에 따라 가장 위에 "*Vertices (node개수)" 쓰고 이어서 붙입니다. 
pajek <- paste("*Vertices", length(x), sep=" ")		
pajek2 <- paste("*Vertices", length(x2), sep=" ")	
pajek4 <- paste("*Vertices", length(x4), sep=" ")	

pajek
pajek2
pajek4

# node의 이름들을 나열하는 for문입니다.

for (i in 1:length(x)){					
  pajek <- c(pajek, paste(i, phoneme_klat[i], sep=" "))
}

# Klattese2의 결과
for (i in 1:length(x2)){					
  pajek2 <- c(pajek2, paste(i, phoneme_klat2[i], sep=" "))
}

# Klattese4의 결과
for (i in 1:length(x4)){					
  pajek4 <- c(pajek4, paste(i, phoneme_klat4[i], sep=" "))
}

# 두 음운이웃 주소 쌍을 edge 형식으로 준비합니다.

pajeklist_result <- paste(neighbor_result[,1],neighbor_result[,2],sep=" ")	
pajeklist_result2 <- paste(neighbor_result2[,1],neighbor_result2[,2],sep=" ")
pajeklist_result4 <- paste(neighbor_result4[,1],neighbor_result4[,2],sep=" ")

# "*Edges"라고 쓴 다음, 뒤이어서 edge 목록들을 붙여넣습니다.

pajek <- c(pajek, "*Edges", pajeklist_result)
pajek2 <- c(pajek2, "*Edges", pajeklist_result2)			
pajek4 <- c(pajek4, "*Edges", pajeklist_result4)

# 정상 작동 여부를 확인합니다.
pajek
pajek2
pajek4
head(pajek)

# (35) 최종 결과물을 외부파일로 출력합니다. 이 때 write.csv()가 아니라 write()로 하여야 정상 작동하고, 
# 파일 확장자는 .net이어야 합니다.

write(pajek, file=file.choose())	
write(pajek2, file=file.choose())
write(pajek4, file=file.choose())

# 외부에 저장한 .net을 pajek에 구현함으로써 pajek 작동이 가능합니다.

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
single_data_klat2 <- cbind(single_data_klat2,phoneme_syllable_klat2)
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

# 병렬처리를 위한 환경설정
# R 패키지인 "doParallel"을 설치합니다

if (!require(doParallel)) install.packages("doParallel")
library(doParallel)

# 코어 수에 따라 일을 분담합니다.
# dectectCores()는, 컴퓨터 CPU의 core 개수를 출력합니다. makeCluster()는 그 core의 개수만큼 할일을 clustering (분리) 합니다.
# 온전히 PNN작업만 할 경우, core 전부를 사용해서 병렬처리할 수 있으나, 
# 대부분의 경우, 백그라운드에서 백신을 돌리거나 아니면 웹브라우징, 문서작업 등등 다른 작업을 하는 경우가 많으므로 core전부를 사용하기보다는 detectCores()-1 를 통해 하나의 코어를 여분으로 남겨두는 것이 일반적입니다.
# 하지만 지금은 실습을 빠르게 하기 위해 core전부를 사용합니다.

cl <- makeCluster(detectCores())							
registerDoParallel(cl)

# 음운이웃 목록 만드는 함수 genPNN을 만듭니다. 
# 이 함수는 for-loop의 병렬 버전인 foreach와, 두 string의 edit distance를 계산하는 adist를 이용해서 음운이웃 목록을 만듭니다.
# 기본적으로 x에 존재하는 element의 개수의 조합 만큼 연산을 하므로, 연산속도는 element 개수에 민감합니다.
# 알고리즘적으로, 연산의 횟수를 줄일수록 전체 연산속도가 빨라집니다.

# (45) 우리는 음절 기반 음운이웃을 두 가지 유형으로 나누어 생산합 것입니다. 
# 하나는 교체/첨가/삭제에 의한 음절 기반 음운이웃이고, 다른 하나는 교체에 의한 음절 기반 음운이웃입니다.  
# 먼저, 교체/첨가/삭제에 의한 음절 기반 음운이웃을 계산할 경우 단어 길이가 2 이상 차이나는 두 단어는 
# 절대 음운이웃이 될 수 없습니다.
# 한편, 교체에 의한 음운이웃만 계산할 경우 단어 길이가 다른 두 단어는 절대 음운이웃이 될 수 없습니다.
# 첨가/삭제의 포함 여부를 parameter로 지정할 수 있도록 합니다.
# 연산시간이 무척 길기 때문에, 연산이 얼마나 진행 되었는지 퍼센티지로 보여주거나, 
# 연산이 모두 끝났을 때 컴퓨터가 사용자에게 알려주는 명령어를 넣는 것이 관행입니다. 
# 기본기능만 구현하므로 진행상황를 출력하는 코드를 넣지는 않았습니다. 
# 하지만 연산이 끝났을 때까지 다른 일을 할 수 있도록, alarm()을 넣습니다.
# genPNN은 최소 2개의 argument를 가지는 함수입니다. argument 1은 앞서 r object 'x'로 정의한, (음소단위/음절단위) 단어목록입니다. 
# 그리고 argument 2는 음운이웃 계산에 첨가/삭제에 의한 음운이웃 관계를 포함할지 여부를 지정합니다. 
# argument 2의 default 값은 T, 즉 기본적으로 음운이웃 계산에는 첨가/삭제를 고려하도록 합니다.

genPNN <- function (x, deletion = T) {
  syllable_neighbor_result <- vector
  syllable_neighbor_result <- foreach (i=1:length(x), .combine='rbind', .packages="base") %dopar% {
    output <- vector()
    if (deletion == T){                                                     # include PN by deletion / insertion
      for (j in i:length(x)) {
        if(nchar(x[j]) > (nchar(x[i])-2) & nchar(x[j]) < (nchar(x[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
          if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
        }
      }
    } else {                                                          # only consider PN by substitution
      for (j in i:length(x)) {
        if(nchar(x[j]) == nchar(x[i])) {                                # 음소 개수가 같을 경우에만
          if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
        }
      }
    }
    output
  }
  alarm()
  return(syllable_neighbor_result)
}

# vector  -> vector()


genPNN <- function (x, deletion = T) {
  syllable_neighbor_result <- vector()
  syllable_neighbor_result <- foreach (i=1:length(x), .combine='rbind', .packages="base") %dopar% {
    output <- vector()
    if (deletion == T){                                                     # include PN by deletion / insertion
      for (j in i:length(x)) {
        if(nchar(x[j]) > (nchar(x[i])-2) & nchar(x[j]) < (nchar(x[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
          if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
        }
      }
    } else {                                                          # only consider PN by substitution
      for (j in i:length(x)) {
        if(nchar(x[j]) == nchar(x[i])) {                                # 음소 개수가 같을 경우에만
          if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
        }
      }
    }
    output
  }
  alarm()
  return(syllable_neighbor_result)
}

# (46) 이제 이용해서 음운이웃 목록을 만들어봅시다.
# 먼저, 교체/첨가/삭제로 인해 형성되는 음절 기반 음운이웃을 syllable_neighbor_result1으로 저장합니다. 

syllable_neighbor_result2 <- genPNN(syllable_result2, deletion = T)
write.csv(syllable_neighbor_result1, file=file.choose(), row.names=F)

# 다음으로, 단지 교체로 인해 형성되는 음절 기반 음운이웃을 syllable_neighbor_result2로 저장합니다

syllable_neighbor_result2 <- genPNN(syllable_result, deletion = F)
write.csv(syllable_neighbor_result2, file=file.choose(), row.names=F)

# 교체/첨가/삭제로 인해 형성되는 음소 기반 음운이웃을 phoneme_neighbor_result1로 저장합니다

# phoneme_neighbor_result1 <- genPNN(phoneme_result, deletion = T)
# write.csv(phoneme_neighbor_result1, file=file.choose(), row.names=F)

# 교체로 인해 형성되는 음소 기반 음운이웃을 phoneme_neighbor_result2로 저장합니다. 

# phoneme_neighbor_result2 <- genPNN(phoneme_result, deletion = F)
# write.csv(phoneme_neighbor_result2, file=file.choose(), row.names=F)


# (47)  R package 'igraph' 이용하여 네트워크 시각화

# package 'igraph' 설치하고, igraph network type 오브젝트 생성
# 앞서 KoNLP, doParallel등의 패키지를 불러온 것과 같은 방식으로 igraph package도 불러옵니다.

if (!require(igraph)) install.packages("igraph")
library(igraph)

# 'net'이라는 이름으로 igraph object를 생성합니다. 
# igraph에 포함된 함수 graph_from_data_frame를 이용합니다. 
# 이 함수가 취하는 argument에는 여러가지가 있는데, 그중
# d: 연결되는 두 node에 대한 정보
# vertices: 각 node에 대한 정보: 각 어휘의 빈도, 층위, 등등등)
# directed: directed network인지 여부

single_data$id = 1:length(single_data$entry)
net = graph_from_data_frame(d=neighbor_result, vertices=single_data,directed=F)
# (각종 neighbor_result, 예컨데, )
# 생성된 네트워크인 'net'을 시각화할 수 있습니다.phoneme_neighbor_result1과 같은 것이 d=에 들어갑니다.)

plot(net, vertex.label=single_data$entry, vertex.shape="none", vertex.label.cex=0.8, edge.width=2, edge.arrow.size=0, margin=0)

# (47) 각종 네트워크 지표 산출

# Assortative Mixing by Degree 구해서 'amd'라는 이름의 vector로 저장합니다.

amd <- assortativity_degree(net)

# net이 가지고 있는 Cluster의 개수를 구해서 no.cl로 저장합니다.

no.cl <- components(net)$no

# 전체 network의 node 개수를 구해서 size에, largest cluster (giant cluster)에 속한 node의 개수를 구해서 gc에,
# clustering coefficient 구해서 cc에, 평균이웃개수 구해서 mean_neigh에, average shortest path length 구해서 aspl로 저장합니다.

size <- length(x)
gc <- components(net)$csize[which.max(components(net)$csize)]	# GC에 속한 node 개수를 구해서 'gc'라는 이름의 vector로 저장합니다.
cc <- transitivity(net, type = "global")			# Clustring coefficient 구해서 'cc'로 저장
mean_neigh <- mean(degree(net))					# 평균 이웃 개수를 구해서 'mean_neigh'로 저장
aspl <- mean_distance(net, directed=F)				# Average shortest path length 구해서 'aspl'로 저장

# 마지막으로 네트워크 수치들을 cbind합니다. write.csv 이용해서 외부파일로 출력할 수도 있습니다.

net_result <- cbind(size, gc, cc, mean_neigh, amd, aspl) # 정리해서 출력합니다.
print(net_result)
# write.csv(net_result) 

# (48) 다음은 node별 지표들 산출하는 방법입니다. 즉, 각 node의 이웃개수, 각 node 이웃이 가지는 numeric attribute의 평균 등입니다.
# 한 node가 가지는 이웃 개수를 구하는 igraph 함수는 neighbors()입니다.
# 특정 node의 attribute는 igraph함수 vertex_attr() 로 구합니다.
# 이를 이용해서, 각 단어에 대해, 이웃의 개수(no_neighbors)와 이웃의 빈도값들을 평균낸 값(mean_neighbor_freq)을 구하고, 
# 그것을 input data.frame인 r object 'data'에 첨가하여 새로운 r object 'output'을 만듭니다.

no_neighbors <- vector()
mean_neighbor_freq <- vector()

for (i in 1:nrow(single_data)){
  neighbor_list <- as.list(neighbors(net,i))
  no_neighbors[i] <- length(neighbor_list)
  
  if(no_neighbors[i]!=0) {
    afreq_of_neighbors <- lapply(neighbor_list, vertex_attr, graph=net, name="afreq")
    mean_neighbor_freq[i] <- mean(unlist(afreq_of_neighbors))
  } else {mean_neighbor_freq[i] <- NA}
}

node_output <- cbind(single_data,no_neighbors,mean_neighbor_freq)


# <6월 5일부터 시작>

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


# (50) 음운작용들을 적용해서 output form을 도출하고 그것들을 기준으로 어두 어중 어말에서 음절 형태의 분포를 정리하기 
# (input: Korean PNN from namsling.tistory.com/9 , 한국어 음운작용 criteria csv파일들)
# Korean PNN.RData에서 시작함. 
# 아래 코드는 object 'sino','native','foreign' 각각을, word.list 중 각각 한자어, 고유어, 외래어를 분리한 오브젝트로 만들어 놓기
# native<-word.list[which(word.list$type=="native"),]
# sino<-word.list[which(word.list$type=="sino"),]
# foreign<-word.list[which(word.list$type=="foreign"),]
# total = single_data
  
# 각종 음운작용 적용 위한 자료들을 criteria 라는 폴더 안에 csv 형태로 저장함.
# 규칙적용하는 부분에 있어서 match 함수 사용해서 개선할 여지가 있음. 
# 또한 '적용할 규칙' '적용할 렉시콘' 두 인수를 받는 함수 "규칙적용"을 만들면 코드가 간결해질 것임.
# 마지막으로 KoNLP 패키지 상의 문제로 HangulAutomata가 불완전함. 
# 개발자에게 오류보고했으며 차기 패키지 업데이트에서 오류 해결될 것으로 예상
# 구체적으로, coda에 허용되지 않는 자음 출현 시 error남. 예외처리 적용하여 돌아가는 데에는 문제가 없지만 
# 결과물을 확인하여 수작업할 필요 있음.

library(KoNLP)

setwd(choose.dir())		## 음운작용 목록인 criteria 라는 폴더를 지정할 것
criteria_DoubleCoda = read.table("double_coda.csv",sep = ",",header=TRUE)
Klattese = read.table("klattese.csv",sep = ",",header=TRUE)
jamo<-vector()
cv<-vector()

# !!!분석대상 층위 여기에 반드시 입력!!! #
input_data <- sino # input_data에는 분석대상의 층위가 들어가야 함. 즉, 다음 셋중 하나 ① input_data <- native ② input_data <- sino ③ input_data <- foreign
# !!!분석대상 층위 여기에 반드시 입력!!! #
# input_data = single_data

input_data<- as.character(input_data$entry)
AllNum<-length(input_data)

# 겹받침 처리(ㄺ->ㄹㄱ), coda ㄷ/ㅌ의 경우 구개음화에 관련되므로 x/X로 표시.
# 그리고 초성 음가 없는 'ㅇ' 제거, 마지막으로 linearization하는 과정

for (i in 1:AllNum) {
  letter<-convertHangulStringToJamos(input_data[i]) #자음과 모음으로 분리하여 letter라는 vector에 넣음 (예: "고구마"라는 단어가 입력되면 ->"ㄱㅗ" "ㄱㅜ" "ㅁㅏ")
  for (j in 1:length(letter)) {
    DC<-match(substr(letter[j],3,3),criteria_DoubleCoda$double)
    if (is.na(DC)==FALSE){substr(letter[j],3,4)<-as.character(criteria_DoubleCoda$separated[DC])} #겹받침을 둘로 나눔 (eg. "ㄳ" -> "ㄱㅅ")
    if (substr(letter[j],3,3) == "ㄷ") {substr(letter[j],3,3)<- "x"}
    if (substr(letter[j],3,3) == "ㅌ") {substr(letter[j],3,3)<- "X"}
    if (substr(letter[j],1,1) == "ㅇ") {letter[j]<-substr(letter[j],2,4)} #음가가 없는 onset 위치 'ㅇ'을 제거
  }
  
  jamo[i]<-paste(letter, collapse="") #글자경계 없애고linearizaion 하는부분
}

# 구개음화 처리하기
jamo<-gsub("xㅣ","ㅈㅣ",jamo)
jamo<-gsub("Xㅣ","ㅊㅣ",jamo)
jamo<-gsub("x","ㄷ",jamo)
jamo<-gsub("X","ㅌ",jamo)

#아래 부분은 aspiration 해결 (eg. 북한 -> 부칸)
criteria_Aspiration<-read.table("aspiration.csv",sep = ",",header=TRUE)
for (k in 1:AllNum) {
  if(grepl("ㅎ",jamo[k])){
    for (l in 1:nrow(criteria_Aspiration)){
      if(grepl(criteria_Aspiration$from[l],jamo[k])){
        jamo[k]<-sub(criteria_Aspiration$from[l],criteria_Aspiration$to[l],jamo[k])
      }
    }
  }
}
#끝
#아래의 for문은 CV표시
for (k in 1:AllNum) {
  phoneme<-unlist(strsplit(jamo[k],split=""))
  for (l in 1:length(phoneme)){if(is.na(match(phoneme[l],Klattese$C))==TRUE){phoneme[l]="V"}else{phoneme[l]="C"}}
  cv[k]<-paste(phoneme,collapse="")
}
#이상, 규칙적용 이전까지 구현 완료
#아래 부분은 자음 3개가 연쇄되었을 때 해결 (eg. 닭도->ㄷㅏㄱㄷㅗ)
for (k in 1:AllNum) {
  CCC_location<-unlist(gregexpr("VCCC",cv[k]))
  for (l in CCC_location){
    CCC_part<-substr(jamo[k],l+1,l+2)
    for (m in 1:nrow(criteria_DoubleCoda)){
      if(grepl(criteria_DoubleCoda$separated[m],CCC_part)){
        jamo[k]<-sub(CCC_part,criteria_DoubleCoda$to[m],jamo[k])
        cv[k]<-sub("CCC","CC",cv[k])
      }
    }
  }
}
# 이상 CCC ->CC 해결
# 아래 부분은 단어 끝에 나오는 자음연쇄(겹받침)의 음가를, 마치 뒤에 자음이 이어지는 것처럼 정해줌
for (k in 1:AllNum) {
  if(grepl("CC$",cv[k])){
    for (l in 1:nrow(criteria_DoubleCoda)){
      if(grepl(paste(criteria_DoubleCoda$separated[l],"$",sep=""),jamo[k])){
        jamo[k]<-sub(criteria_DoubleCoda$separated[l],criteria_DoubleCoda$to[l],jamo[k])
        cv[k]<-sub("CC$","C",cv[k])
        
      }
    }
  }
}
# 완료
#아래 부분은 음운동화 적용
criteria_Assimilation<-read.table("assimilation.csv",sep = ",",header=TRUE)
for (k in 1:AllNum) {
  for (l in 1:nrow(criteria_Assimilation)){
    if(grepl(criteria_Assimilation$from[l],jamo[k])){
      jamo[k]<-sub(criteria_Assimilation$from[l],criteria_Assimilation$to[l],jamo[k])
    }
  }
}
# 음운동화 해결
# 표준발음법 제23항(예외없는 경음화) 적용
criteria_Tensification<-read.table("tensification.csv",sep = ",",header=TRUE)
for (k in 1:AllNum) {
  for (l in 1:nrow(criteria_Tensification)){
    if(grepl(criteria_Tensification$from[l],jamo[k])){
      jamo[k]<-sub(criteria_Tensification$from[l],criteria_Tensification$to[l],jamo[k])
    }
  }
}
# 표준발음법 제23항 적용 완료
# 여기서부터는 음절말 장애음 중화
neutral<-read.table("neutralization.csv",sep = ",",header=TRUE)
for (k in 1:AllNum) {
  phoneme<-unlist(strsplit(jamo[k],split=""))
  for (l in 1:length(phoneme)){
    if(is.na(match(phoneme[l],neutral$from))==FALSE){
      if(l==length(phoneme)|unlist(strsplit(cv[k],split=""))[l+1]=="C"){phoneme[l]<-as.character(neutral$to[match(phoneme[l],neutral$from)])}
    }
    
    jamo[k]<-paste(phoneme,collapse="")
  }
}
# 끝

# 최종 결과물을 다시 한글로 모아쓰기
# ㅇ+V 연쇄를 ㅇㅇ+V으로 만든다.
for (k in 1:AllNum) {
  if (grepl("ㅇ",jamo[k])){
    phoneme<-unlist(strsplit(jamo[k],split=""))
    splitcv<-unlist(strsplit(cv[k],split=""))
    if(length(phoneme)>2){
      for (l in 2:(length(phoneme)-1)) {
        if(phoneme[l]=="ㅇ"&splitcv[l+1]=="V") {
          phoneme[l]<-"ㅇㅇ"
          splitcv[l+1]<-"CV" 
        }
      }
      jamo[k]<-paste(phoneme,collapse="")
      cv[k]<-paste(splitcv,collapse="")
    }
  }
}
# 어두 V로 시작하거나, VV 연쇄가 있으면 중간에 음가없는 ㅇ을 삽입하기
for (k in 1:AllNum) {
  if(grepl("^V",cv[k])){
    jamo[k]<-paste0("ㅇ",jamo[k])
    cv[k]<-paste0("C",cv[k])
  }
  while(grepl("VV",cv[k])){
    phoneme<-unlist(strsplit(jamo[k],split=""))
    splitcv<-unlist(strsplit(cv[k],split=""))
    for (p in 2:length(phoneme)) {
      if (splitcv[p]=="V") {
        if(splitcv[p-1]=="V"){
          splitcv[p]<-"CV"
          phoneme[p]<-paste0("ㅇ",phoneme[p])
        }
      }
    }
    jamo[k]<-paste(phoneme,collapse="")
    cv[k]<-paste(splitcv,collapse="")
  }
}
#끝

# 그 결과를 바탕으로 automata로 모아쓰기
output<-vector()
for (i in 1:AllNum) {
  tryCatch(output[i]<-HangulAutomata(jamo[i]),
           error=function(e) {output[i]<-paste(jamo[i], collapse="")}
  )
}

## !!!! 여기서 반드시 정지한 다음 output을 확인해야 함. 에러가 난 부분을 수작업으로 처리해야 함. "오빠", "아빠" 등 고유어에서 에러 많이 나옴. !!!! ##

## 이 결과를 가지고 어두 어중 어말 구분 (R Code 11 이용)
# 어두
output<-strsplit(output,split="")
firstsyllable<-unlist(lapply(output, `[[`, 1))
first_table<-table(firstsyllable)
first_prop<-prop.table(first_table)
word_initial<-cbind(first_table,first_prop)

# 어말
lastsyllable<-unlist(sapply(output, tail, 1))
last_table<-table(lastsyllable)
last_prop<-prop.table(last_table)
word_final<-cbind(last_table,last_prop)

# 어중
midsyllable<-vector()
for (i in 1:AllNum) {
  if (length(output[[i]]) > 2) {
    for (j in 2:(length(output[[i]])-1)){
      midsyllable<-c(midsyllable,output[[i]][j])
    }
    
  }
}

mid_table <- table(midsyllable)
mid_prop <- prop.table(mid_table)
word_internal <- cbind(mid_table,mid_prop)

# 어두 어중 어말 단위로 저장함
setwd(choose.dir()) # 저장할 경로
write.csv(word_initial,"word_initial.csv")
write.csv(word_internal,"word_internal.csv")
write.csv(word_final,"word_final.csv")

### 13. 음절 목록을 입력받아 각 음절을 음소 단위로 분리
## septophonemes는 word_initial.csv, word_internal.csv, word_final.csv 이렇게 3개의 csv파일이 들어간 폴더를 입력받아, 각각을 음소단위로 분리한 다음 1word_initial.csv 1word_internal.csv 1word_final.csv 파일로 출력함.
septophonemes<-function(){
  library(KoNLP)
  setwd(choose.dir())
  file.list <- list.files(pattern=".csv")
  for (j in file.list) {
    imsi <- read.csv(file=j, header=T)
    imsi2 <- as.character(imsi[,1])
    result <- c("onset","nucleus","coda")
    for (i in 1:length(imsi2)){
      phonemic<-unlist(strsplit(convertHangulStringToJamos(imsi2[i]),split=""))
      if(phonemic[1]=="ㅇ") phonemic[1]<-""
      if(is.na(phonemic[3])) phonemic[3]<-""
      result<-rbind(result,phonemic)
    }
    result <- result[-1,]
    colnames(result) <- c("onset","nucleus","coda")
    write.csv(cbind(imsi,result),file=paste0("1",j))
  }
}


### 14. 음절 나열하고 위치(어두 어중 어말)에 따라 중복되는지 출력하기
setwd(choose.dir())  # word_initial.csv, word_internal.csv, 그리고 word_final.csv가 저장된 폴더를 지정할 것
initial <- read.csv(file="word_initial.csv",header=T)
internal <- read.csv(file="word_internal.csv",header=T)
final <- read.csv(file="word_final.csv",header=T)

initial <- as.character(initial[,1])
internal <- as.character(internal[,1])
final <- as.character(final[,1])

total<-c(initial,internal,final)
total<-total[!duplicated(total)]

result <- c("initial","internal","final")
for (i in 1:length(total)) {
  syllable<-vector()
  if(length(grep(total[i],initial)!=0)){
    syllable[1]<-total[i]
  } else {syllable[1]<-""}
  if(length(grep(total[i],internal)!=0)){
    syllable[2]<-total[i]
  } else {syllable[2]<-""}
  if(length(grep(total[i],final)!=0)){
    syllable[3]<-total[i]
  } else {syllable[3]<-""}
  result<-rbind(result,syllable)
  
}
write.csv(result,file=file.choose())		# 결과물 출력하기

### 15. 한국어 기저 음절단위 PNN (input = 1번코드의 결과물, Klattese 변환표 (자모음 표시를 위해서).. // 이때 "기저"의 의미는 재음절화 적용, 다른 음운규칙은 적용 X )
## 0단계: 사전작업. 필요한 파일, 즉 한국어 단어목록과 Klattese 변환표를 불러오기. 그리고 필요한 패키지들 불러오기
data <- read.csv(file=file.choose(),header=T)									# 1번코드의 결과로 출력된 csv파일을 읽어옵니다.
jamo <- as.character(data$result)												# 그중 두번째 column인 result는 자모분리된 결과
klat <- as.character(data$klat)

Klattese<-read.table(file=file.choose(), sep = ",", header=TRUE)				# 한글자모 - Klattese 변환표인 "klattese.csv" 를 읽어와야 함

if (!require(KoNLP)) install.packages("KoNLP")				# 컴퓨터에 KoNLP가 설치되어 있는지 점검하기, 없으면 install.packages()를 써서 설치합니다
if (!require(doParallel)) install.packages("doParallel")	# 컴퓨터에 doParallel이 설치되어 있는지 점검하기, 없으면 install.packages()를 써서 설치합니다
library(doParallel)
library(KoNLP)

AllNum <- nrow(data) 									# 전체 단어개수는 많이 쓰게 될 상수이므로 Allnum이라고 지정해줌.

## 1단계: 각 자모에 대해 그것이 자음인지 모음인지 표시하기. 그래야 모음과 모음이 연쇄될 때, 그리고 coda 'ㅇ' + V 연쇄될 때 그 사이에 ㅇ을 넣어줄 수 있음.
cv <- vector()											# cv 구분 저장될 vector로서 'cv' 선언합니다.
for (k in 1:AllNum) {
  phoneme<-unlist(strsplit(jamo[k],split=""))
  for (l in 1:length(phoneme)){if(is.na(match(phoneme[l],Klattese$C))==TRUE){phoneme[l]="V"}else{phoneme[l]="C"}}
  cv[k]<-paste(phoneme,collapse="")
}


## 2단계: 자모분리한 결과를 함수 'HangulAutomata()'를 이용해서 음절단위로 조립
# ㅇ+V 연쇄를 ㅇㅇ+V으로 만든다.
for (k in 1:AllNum) {
  if (grepl("ㅇ",jamo[k])){
    phoneme<-unlist(strsplit(jamo[k],split=""))
    splitcv<-unlist(strsplit(cv[k],split=""))
    if(length(phoneme)>2){
      for (l in 2:(length(phoneme)-1)) {
        if(phoneme[l]=="ㅇ"&splitcv[l+1]=="V") {
          phoneme[l]<-"ㅇㅇ"
          splitcv[l+1]<-"CV" 
        }
      }
      jamo[k]<-paste(phoneme,collapse="")
      cv[k]<-paste(splitcv,collapse="")
    }
  }
}

# 어두 V로 시작하거나, VV 연쇄가 있으면 중간에 음가없는 ㅇ을 삽입하기
for (k in 1:AllNum) {
  if(grepl("^V",cv[k])){
    jamo[k]<-paste0("ㅇ",jamo[k])
    cv[k]<-paste0("C",cv[k])
  }
  while(grepl("VV",cv[k])){
    phoneme<-unlist(strsplit(jamo[k],split=""))
    splitcv<-unlist(strsplit(cv[k],split=""))
    for (p in 2:length(phoneme)) {
      if (splitcv[p]=="V") {
        if(splitcv[p-1]=="V"){
          splitcv[p]<-"CV"
          phoneme[p]<-paste0("ㅇ",phoneme[p])
        }
      }
    }
    jamo[k]<-paste(phoneme,collapse="")
    cv[k]<-paste(splitcv,collapse="")
  }
}

# 필수적인 ㅇ이 포함된 상태를 기초로 함수 HangulAutomata()를 사용
output<-vector()										# 자모조립된 결과가 들어갈 벡터로 output
for (i in 1:length(jamo)) {								# jamo 개수만큼 반복문을 돌리기
  tryCatch(output[i]<-HangulAutomata(jamo[i]),			# HangulAutomata가 불완전한 상태이기 때문에 오류처리
           error=function(e) {output[i]<-paste(jamo[i], collapse="")}
  )
}

## !!!! 여기서 반드시 정지한 다음 output을 확인하여 NA (에러가 난 부분)을 수작업으로 넣어주어야 함. 2018년 8월 4일 현재, KoNLP 개발자가 문제해결을 완료한 것으로 보이지만 아직 패키지에 반영이 되지 않은 상태임 !!!! ##

## 4단계: 음절을 기준으로 하여 음운이웃 목록 만들기 (4번 R-code 활용)
cl <- makeCluster(detectCores())							# dectectCores()는, 컴퓨터 CPU의 core 개수를 출력합니다. makeCluster()는 그 core의 개수만큼 할일을 clustering (분리) 합니다.
registerDoParallel(cl)										# 이 clustering을 아래에 쓰일 foreach라는 함수에서 사용하려면, registerDoParallel()을 통해 등록해주어야 합니다.
result <- vector()											# 3단계의 결과값이 저장될 벡터 result를 미리 지정해줍니다.

result <- foreach (i=1:length(output), .combine='rbind', .packages="base") %dopar% {
  intermediate <- vector()
  for (j in i:length(output)) {
    if(nchar(output[j]) > (nchar(output[i])-2) & nchar(output[j]) < (nchar(output[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
      if(adist(output[i], output[j])==1) intermediate <- rbind(intermediate,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
    }
  }
  intermediate
}
alarm()
x <- output 													# 6번코드가 뒤이어서 실행될 때 오류가 나지 않도록 하기 위한 장치
## 이 음운이웃 목록을 기초로, 5번 코드 혹은 6번 코드를 이용해서 네트워크 분석을 할 수 있음.



### 16. 기저형/표면형, 층위별, 음소/음절 PNN 리스트 구축하기 (input = Korean PNN from http://namsling.tistory.com/9 , 음운작용 목록 csv파일들 // 이때 "기저"의 의미는 재음절화까지만 적용, "표면"의 의미는 재음절화 이후 각종 음운작용 적용)
## (Korean PNN.RData 상에서 작업이 이루어진다고 가정)
library(KoNLP)

native<-word.list[which(word.list$type=="native"),]
sino<-word.list[which(word.list$type=="sino"),]
foreign<-word.list[which(word.list$type=="foreign"),]
total <- word.list

genPNN<-function(surface=NULL, stratum=NULL, unit=NULL){		
  # 함수 genPNN은 argument 3개를 입력받음. 
  # surface = 표면형을 가지고 PNN 구축할 것인지 여부. T/F 값. default 값은 F
  # stratum = 어떤 층위를 가지고 PNN 구축할 것인지? "native", "sino", "foreign", "total." default 값은 "total"
  # unit = 음운이웃의 기준을 음소로 할 것인지 아니면 음절로 할 것인지? "syllable"을 넣어주면 음절을 기준으로 하지만, 아무것도 넣지 않거나 "phoneme"이라고 넣으면 음소를 기준으로 PNN을 구축함.
  
  if(is.null(surface)) surface=F
  if(is.null(stratum)) stratum="total"
  if(is.null(unit)) unit = "phoneme"
  if(!unit=="syllable"&&!unit=="phoneme") unit <- "phoneme"
  
  question <- readline(paste0("Generating a PNN of the ", stratum, " stratum, where neighbors differ by a ", unit, " (surface = ", surface,"). Correct??  Y/N   ")) # 입력한 조건들이 맞는지 사용자에게 확인을 받음.
  if (!tolower(question)=="y") stop("please start over")
  setwd(choose.dir())																						# 음운작용 목록인 criteria 라는 폴더를 지정할 것
  criteria_DoubleCoda<-read.table("double_coda.csv",sep = ",",header=TRUE)								# double coda 리스트.
  Klattese<-read.table("klattese.csv",sep = ",",header=TRUE)												# Klattese 리스트
  
  input_data <- get(stratum)																				# genPNN의 argument로 입력받은 stratum 값을 기준으로 대상 데이터를 추려냄.
  input_data <- as.character(input_data$entry)
  AllNum <- length(input_data)
  jamo<-vector()
  cv<-vector()
  
  # 아래 코드는 음운규칙 적용 여부와 무관한 작용들을 공통적으로 처리해줌 즉,
  # 1. 겹받침 처리(ㄺ->ㄹㄱ), coda ㄷ/ㅌ의 경우 구개음화에 관련되므로 x/X로 표시.
  # 2. 그리고 초성 음가 없는 'ㅇ' 제거, 마지막으로 linearization하는 과정
  
  for (i in 1:AllNum) {
    letter<-convertHangulStringToJamos(input_data[i]) #자음과 모음으로 분리하여 letter라는 vector에 넣음 (예: "고구마"라는 단어가 입력되면 ->"ㄱㅗ" "ㄱㅜ" "ㅁㅏ")
    for (j in 1:length(letter)) {
      DC<-match(substr(letter[j],3,3),criteria_DoubleCoda$double)
      if (is.na(DC)==FALSE){substr(letter[j],3,4)<-as.character(criteria_DoubleCoda$separated[DC])} #겹받침을 둘로 나눔 (eg. "ㄳ" -> "ㄱㅅ")
      if (substr(letter[j],3,3) == "ㄷ") {substr(letter[j],3,3)<- "x"}						#구개음화의 대상이 되는 종성 ㄷ을 따로 마킹함
      if (substr(letter[j],3,3) == "ㅌ") {substr(letter[j],3,3)<- "X"}						#구개음화의 대상이 되는 종성 ㅌ을 따로 마킹함
      if (substr(letter[j],1,1) == "ㅇ") {letter[j]<-substr(letter[j],2,4)} #음가가 없는 onset 위치 'ㅇ'을 제거
    }
    
    jamo[i]<-paste(letter, collapse="") #글자경계 없애고linearizaion 하는부분
  }
  
  for (k in 1:AllNum) {
    phoneme<-unlist(strsplit(jamo[k],split=""))
    for (l in 1:length(phoneme)){if(is.na(match(phoneme[l],Klattese$C))==TRUE){phoneme[l]="V"}else{phoneme[l]="C"}}
    cv[k]<-paste(phoneme,collapse="")
  }
  
  # 이하, if절 내의 음운작용은 genPNN의 argument 중 surface 값이 T로 주어졌을 때만 적용.
  if(surface) {			
    
    # 구개음화
    jamo <- gsub("xㅣ","ㅈㅣ",jamo)
    jamo <- gsub("Xㅣ","ㅊㅣ",jamo)
    jamo <- gsub("x","ㄷ",jamo)
    jamo <- gsub("X","ㅌ",jamo)
    
    #아래 부분은 aspiration 해결 (eg. 북한 -> 부칸)
    criteria_Aspiration<-read.table("aspiration.csv",sep = ",",header=TRUE)
    for (k in 1:AllNum) {
      if(grepl("ㅎ",jamo[k])){
        for (l in 1:nrow(criteria_Aspiration)){
          if(grepl(criteria_Aspiration$from[l],jamo[k])){
            jamo[k]<-sub(criteria_Aspiration$from[l],criteria_Aspiration$to[l],jamo[k])
          }
        }
      }
    }
    #끝
    #아래 부분은 자음 3개가 연쇄되었을 때 해결 (eg. 닭장->ㄷㅏㄱㅈㅏㅇ)
    for (k in 1:AllNum) {
      CCC_location<-unlist(gregexpr("VCCC",cv[k]))
      for (l in CCC_location){
        CCC_part<-substr(jamo[k],l+1,l+2)
        for (m in 1:nrow(criteria_DoubleCoda)){
          if(grepl(criteria_DoubleCoda$separated[m],CCC_part)){
            jamo[k]<-sub(CCC_part,criteria_DoubleCoda$to[m],jamo[k])
            cv[k]<-sub("CCC","CC",cv[k])
          }
        }
      }
    }
    # 이상 CCC ->CC 해결
    # 아래 부분은 단어 끝에 나오는 자음연쇄(겹받침)의 음가를, 마치 뒤에 자음이 이어지는 것처럼 정해줌
    for (k in 1:AllNum) {
      if(grepl("CC$",cv[k])){
        for (l in 1:nrow(criteria_DoubleCoda)){
          if(grepl(paste(criteria_DoubleCoda$separated[l],"$",sep=""),jamo[k])){
            jamo[k]<-sub(criteria_DoubleCoda$separated[l],criteria_DoubleCoda$to[l],jamo[k])
            cv[k]<-sub("CC$","C",cv[k])
            
          }
        }
      }
    }
    # 완료
    #아래 부분은 음운동화 적용
    criteria_Assimilation<-read.table("assimilation.csv",sep = ",",header=TRUE)
    for (k in 1:AllNum) {
      for (l in 1:nrow(criteria_Assimilation)){
        if(grepl(criteria_Assimilation$from[l],jamo[k])){
          jamo[k]<-sub(criteria_Assimilation$from[l],criteria_Assimilation$to[l],jamo[k])
        }
      }
    }
    # 음운동화 해결
    # 표준발음법 제23항(예외없는 경음화) 적용
    criteria_Tensification<-read.table("tensification.csv",sep = ",",header=TRUE)
    for (k in 1:AllNum) {
      for (l in 1:nrow(criteria_Tensification)){
        if(grepl(criteria_Tensification$from[l],jamo[k])){
          jamo[k]<-sub(criteria_Tensification$from[l],criteria_Tensification$to[l],jamo[k])
        }
      }
    }
    # 표준발음법 제23항 적용 완료
    # 여기서부터는 음절말 장애음 중화
    neutral<-read.table("neutralization.csv",sep = ",",header=TRUE)
    for (k in 1:AllNum) {
      phoneme<-unlist(strsplit(jamo[k],split=""))
      for (l in 1:length(phoneme)){
        if(is.na(match(phoneme[l],neutral$from))==FALSE){
          if(l==length(phoneme)|unlist(strsplit(cv[k],split=""))[l+1]=="C"){phoneme[l]<-as.character(neutral$to[match(phoneme[l],neutral$from)])}
        }
        
        jamo[k]<-paste(phoneme,collapse="")
      }
    }
    # 끝
  } # genPNN argument 'surface'가 F일 경우에 적용되는 if절의 끝.
  
  jamo <- gsub("x","ㄷ",jamo)	# 만약 규칙적용 안할시 구개음화 적용안되었으므로, 앞서 구개음화 대비하여 ㄷ/ㅌ를 x/X로 바꿨던 것을 다시 ㄷ/ㅌ로 환원시키기
  jamo <- gsub("X","ㅌ",jamo)
  
  output <- jamo
  
  # 이하, 한글로 모아쓰기는 genPNN의 argument 중 unit 값이 "syllable"로 주어졌을 때만 적용함. 한글로 모아써서 output에 덮어씀.
  if(unit=="syllable") {			
    
    # 최종 결과물을 다시 한글로 모아쓰기
    # ㅇ+V 연쇄를 ㅇㅇ+V으로 만든다.
    for (k in 1:AllNum) {
      if (grepl("ㅇ",jamo[k])){
        phoneme<-unlist(strsplit(jamo[k],split=""))
        splitcv<-unlist(strsplit(cv[k],split=""))
        if(length(phoneme)>2){
          for (l in 2:(length(phoneme)-1)) {
            if(phoneme[l]=="ㅇ"&splitcv[l+1]=="V") {
              phoneme[l]<-"ㅇㅇ"
              splitcv[l+1]<-"CV" 
            }
          }
          jamo[k]<-paste(phoneme,collapse="")
          cv[k]<-paste(splitcv,collapse="")
        }
      }
    }
    # 어두 V로 시작하거나, VV 연쇄가 있으면 중간에 음가없는 ㅇ을 삽입하기
    for (k in 1:AllNum) {
      if(grepl("^V",cv[k])){
        jamo[k]<-paste0("ㅇ",jamo[k])
        cv[k]<-paste0("C",cv[k])
      }
      while(grepl("VV",cv[k])){
        phoneme<-unlist(strsplit(jamo[k],split=""))
        splitcv<-unlist(strsplit(cv[k],split=""))
        for (p in 2:length(phoneme)) {
          if (splitcv[p]=="V") {
            if(splitcv[p-1]=="V"){
              splitcv[p]<-"CV"
              phoneme[p]<-paste0("ㅇ",phoneme[p])
            }
          }
        }
        jamo[k]<-paste(phoneme,collapse="")
        cv[k]<-paste(splitcv,collapse="")
      }
    }
    #끝
    
    # 그 결과를 바탕으로 automata로 모아쓰기
    output<-vector()
    for (i in 1:AllNum) {
      tryCatch(output[i]<-HangulAutomata(jamo[i]),
               error=function(e) {output[i]<-paste(jamo[i], collapse="")}
      )
    }
    
    error.list <- which(is.na(output))				# KoNLP의 함수 HangulAutomata가 여전히 오류를 내고 있으므로, 그러한 경우 수작업을 수행해야 함. error 난 케이스들의 address를 모아서 vector 'error.list' 만듦.
    for (i in error.list) {							
      output[i] <- readline(paste0("(", which(error.list==i)," / ", length(error.list),")Please manually complete the Hangul syllables for ", jamo[i],"?   "))		# 각 error.list 에 대해 사용자가 직접 수작업으로 입력할 수 있도록 해줌.
    }
  }
  
  ## 마지막: character vector인 output에서 1개 string 단위(음소 단위라면 자모, 음절 단위라면 글자)를 기준으로 PNN 구축하기
  if (!require(doParallel)) install.packages("doParallel")	# 컴퓨터에 doParallel이 설치되어 있는지 점검하기, 없으면 install.packages()를 써서 설치합니다
  library(doParallel)											# 패키지 doParallel을 실행합니다.
  cl <- makeCluster(detectCores())							# dectectCores()는, 컴퓨터 CPU의 core 개수를 출력합니다. makeCluster()는 그 core의 개수만큼 할일을 clustering (분리) 합니다.
  registerDoParallel(cl)										# 이 clustering을 아래에 쓰일 foreach라는 함수에서 사용하려면, registerDoParallel()을 통해 등록해주어야 합니다.
  x <- as.character(output)
  result <- vector()											# 결과값이 저장될 벡터 result를 미리 지정해줍니다.
  result <- foreach (i=1:length(x), .combine='rbind', .packages="base") %dopar% {
    output <- vector()
    for (j in i:length(x)) {
      if(nchar(x[j]) > (nchar(x[i])-2) & nchar(x[j]) < (nchar(x[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
        if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
      }
    }
    output
  }
  alarm()
  
  return(result)
}
## 이 결과는 PNN 리스트이므로, klattese 기호, 혹은 표제어의 원 형태 등과 함께 5번코드 혹은 6번코드를 이용해 네트워크 분석을 할 수 있음.


### 17. 기저형 상에서 자모분리한 후 초성 ㅇ 제거하고 음절경계 $ 포함해서 나열하기. (input = Korean PNN from http://namsling.tistory.com/9)
## (Korean PNN.RData 상에서 작업이 이루어진다고 가정)
# 1단계. 자모분리하기
library(KoNLP)
data <- as.character(word.list$entry)					# object 'word.list' 중 표제어는 entry라는 column에 factor type으로 들어있다. 따라서 그 부분만 추출해서 data로 저장한다. 이때 character type으로 바꾼다.
jamo <- vector()
cv <- vector()
for (i in 1:length(data)) {
  syllable_cv <- vector()
  syllable <- convertHangulStringToJamos(data[i])		# 벡터 'data'의 i번째 element를 자모분리(convertHangulStringToJamos)하고 그 결과를 벡터 'syllable'에 덮어씁니다.
  for (j in 1:length(syllable)) {						# 반복문 안에서 또 반복문을 돌릴 건데 j는 1부터 'syllable'의 element 총 개수까지 하나씩 올라갑니다.
    phonemic <- unlist(strsplit(syllable[j], split=""))	# 'syllable'의 j번째 element를 각 자모단위로 분리해서 새로운 vector 'phonemic'에 넣습니다.
    if(phonemic[1] == "ㅇ") {phonemic <- phonemic[-1]}		# 첫번째 자모(즉, 초성)가 'ㅇ'이면, 그것을 제거합니다.
    cv_int <- unlist(lapply(phonemic, is.moeum))
    cv_int <- ifelse(cv_int,"V","C")
    syllable_cv[j] <- paste(cv_int, collapse="")
    syllable_cv[j] <- paste0(syllable_cv[j], "$")
    syllable[j] <- paste(phonemic, collapse="")		# 'phonemic'을 결합해서 다시 음절단위로 만듭니다. 그러나 초성의 ㅇ은 제거된 상태입니다. 
    syllable[j] <- paste0(syllable[j],"$")			# 음절단위로 결합된 끝에 syllable boundary "$"를 넣어줍니다.
    
  }
  jamo[i] <- paste(syllable, collapse="")				# 그 결과를 jamo로 저장합니다.
  cv[i] <- paste(syllable_cv,collapse="")
}

# 2단계. 결과 출력하기
stratum<-as.character(word.list$type)					# 표제어 정보와 더불어 고유어/한자어/외래어 층위 정보가 있으면 좋을 것이기 때문에 그것도 stratum으로 따로 뽑아냄.
output<-cbind(data, stratum, jamo,cv)
write.csv(output, file=file.choose(), row.names = F)

## 이렇게 출력된 결과값을 엑셀 등에서 열어서 'cv' colum 상의 C$V 연쇄가 있으면 이때의 C가 재음절화되는 자음임을 알 수 있음.