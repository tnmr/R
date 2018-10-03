# ネットワーク構築・可視化 ####

# データの読み込み ####
library(readr)
# ファイルナンバー（下の数字を変える）
file_num = 3
votes <- c(1, 0, 0, 6, 25)
rank <- c(12, 17, 17, 4, 1)
file_net <- sprintf("~/Documents/R/workspace/study_data/adjacent_matrix%s.csv", as.character(file_num))
com_net <- read.table(file_net, sep=",", header=T, row.names=1)
file_info <- sprintf("~/Documents/R/workspace/study_data/com_info%s.csv", as.character(file_num))
com_info <- read.table(file_info, sep=",", row.names=1, fileEncoding="CP932")

# 平均情報量の算出関数 ####
Entropy <- function(x){
  ent <- sum(-x*log2(x), na.rm=T)
  max_ent <- log2(length(x))
  score <- ent / max_ent
  output <- c(ent, max_ent, score)
  output
}

# 発言情報整理 ####
user_levels <- levels(com_info[,1])
user <- match(com_info[,1], user_levels)   # 発言ユーザ
frame_color <- rainbow(length(user_levels))[user]
user_hist = c()
for(x in 1:length(user_levels)){
  user_hist <- append(user_hist, sum(user_levels[x]==com_info[,1])/nrow(com_info))
}
user_ent <- c(Entropy(user_hist))
user_stat <- rbind(user_levels, user_hist)
user_color = c()
color_level = 8
for(x in 1:length(user)){
  user_color <- append(user_color, heat.colors(color_level)[color_level-(color_level*user_hist[user[x]])])
}
com_len <- nchar(as.character(com_info[,4]))
com_norm <- com_len / max(com_len)

# 距離データの作成 ####
#r = 20
#rd = c()
#for(x in 1:nrow(com_net)){
#  for(y in 1:ncol(com_net)){
#    if(com_net[x, y] == 1){
#      rd <- append(rd, abs(info[x, 3] - info[y, 3]) + r)
#    } else {
#      rd <- append(rd, 4320000)
#    }
#  }
#}
#mat_rd <- matrix(c(rd), nrow=nrow(com_net), ncol=ncol(com_net))
#dis <- as.dist(mat_rd)

# 隣接（距離）行列の作成　####
r = 20
v = c()
for(x in 1:nrow(com_net)){
  for(y in 1:ncol(com_net)){
    v <- append(v, com_net[x, y])   # 0,1代入
    #if(com_net[x, y]==1){
    #  v <- append(v, abs(com_info[x, 3] - com_info[y, 3]) + r)
    #} else {
    #  v <- append(v, 0)
    #}
  }
}
g_mat <- matrix(c(v), ncol=ncol(com_net), nrow=nrow(com_net), byrow=T)

# エッジリストの作成 ####
#v = c()
#for(x in 1:nrow(com_net)){
#  for(y in 1:ncol(com_net)){
#    if(com_net[x, y] == 1){
#      v <- append(v, x)
#      v <- append(v, y)
#    }
#  }
#}
#ed <- matrix(c(v), ncol=2, byrow=T)

# ネットワークの描画 ####
library(igraph)
g <- graph.adjacency(g_mat, weighted=T)
#g <- graph.edgelist(ed)
set.seed(1)
plot(g, vertex.size=8, edge.arrow.size=0.2, vertex.color="lightblue", vertex.label=V(g)$name, layout=layout.fruchterman.reingold)
#set.seed(1)
#plot(g,  vertex.size=8, edge.arrow.size=0.2, vertex.color="lightblue", layout=layout.reingold.tilford)
V(g)$color <- user
set.seed(1)
plot(g, vertex.size=8, edge.arrow.size=0.2, vertex.label=V(g)$name, layout=layout.fruchterman.reingold)

V(g)$color <- user_color
set.seed(1)
plot(g, vertex.size=6+12*com_norm, edge.arrow.size=0.2, vertex.label=V(g)$name, vertex.frame.color=frame_color, layout=layout.fruchterman.reingold)

#max_w <- max(E(g)$weight)
set.seed(1)
#plot(g, vertex.size=8, edge.arrow.size=0.2, vertex.color="lightblue", edge.label=E(g)$weight, edge.label.cex=0.6, edge.color=rgb(E(g)$weight/max_w, 1, E(g)$weight/max_w), layout=layout.fruchterman.reingold)
#plot(g, vertex.size=8, edge.arrow.size=0.2, vertex.color="lightblue", edge.label=E(g)$weight, edge.label.cex=0.6, edge.color=gray(E(g)$weight/max_w), layout=layout.fruchterman.reingold)
plot(g, vertex.size=8, edge.arrow.size=0.2, vertex.color="lightblue", edge.label=E(g)$weight, edge.label.cex=0.6, edge.color=gray(E(g)$weight/86400), layout=layout.fruchterman.reingold)

#set.seed(1)
#plot(g, vertex.size=E(g)$weight/max_w*100, edge.arrow.size=0.2, vertex.color="lightblue", layout=layout.fruchterman.reingold)

# ユーザー発言頻度の影響と中心性の可視化 ####
# 次数中心性
set.seed(1)
plot(g, vertex.size=6+1.2*degree(g),  edge.arrow.size=0.2, vertex.label=V(g)$name, vertex.frame.color=frame_color, layout=layout.fruchterman.reingold)
# 媒介中心性
set.seed(1)
plot(g, vertex.size=6+0.2*betweenness(g),  edge.arrow.size=0.2, vertex.label=V(g)$name, vertex.frame.color=frame_color, layout=layout.fruchterman.reingold)
