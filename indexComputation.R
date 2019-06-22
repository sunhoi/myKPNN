# <이웃 지표 계산>

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
