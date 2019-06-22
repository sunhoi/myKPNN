# <이웃관계 시각화 과정>

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