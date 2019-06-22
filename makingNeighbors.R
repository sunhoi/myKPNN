# <이웃관계 목록 만들기>

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