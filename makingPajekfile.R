# <Pajek 파일 만들기>

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