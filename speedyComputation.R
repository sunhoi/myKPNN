# <계산 속도 증진>

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