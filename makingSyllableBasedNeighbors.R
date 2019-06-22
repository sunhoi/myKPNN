# <음절 기반 음운이웃 만드는 코드>

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

syllable_neighbor_result1 <- genPNN(syllable_result, deletion = T)
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