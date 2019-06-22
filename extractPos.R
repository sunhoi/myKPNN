# <품사 추출>
# (8) 꼬리 번호가 제거되고 동음이의어가 제거된 데이터에서 언하는 항목만 추출하시길 바라십니까? 
# 그렇다면 명사만을 제거한다고 가정하고 다음과 같이 해보시길 바랍니다.  

whatPOS<-"NNG"
NNG_data<- single_data[grep(whatPOS,single_data$POS),]

# (9) 명사 데이터 즉, NNG_data를 R 밖으로 빼내어 저장하려면, 역시 write.csv()함수를 사용합니다.

write.csv(NNG_data, file=file.choose(), row.names=F)