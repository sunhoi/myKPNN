# <각 단어의 지표>

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