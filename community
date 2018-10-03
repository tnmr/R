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



# コミュニティ分類 ####

# 連結成分ごとにコミュニティ分類 ####
cls <- clusters(g, "weak")
V(g)$color <- cls$membership
set.seed(1)
plot(g, vertex.size=8, edge.arrow.size=0.2, layout=layout.fruchterman.reingold)

# 話題ごとの発言者数
for(i in 1:cls$no){
  memb_part = c()
  for(j in 1:vcount(g)){
    if(cls$membership[j] == i){
      memb_part <- append(memb_part, user[j])
    }
  }
  part_hist =c()
  memb_uni <- unique(memb_part)
  for(k in 1:length(memb_uni)){
    part_hist <- append(part_hist, sum(memb_part==memb_uni[k])/length(memb_part))
  }
  write.table(part_hist, file="~/Documents/R/workspace/study_data/part_hist.csv", sep=",", append=T, col.names=memb_uni, fileEncoding="CP932")
  memb_part <- c()
}

# 連結成分の分析 ####
dcg_index = c()
for(i in 1:length(dcg)){
  set.seed(1)
  plot(dcg[[i]], vertex.size=8, edge.arrow.size=0.2, vertex.label=V(g)$name, layout=layout.fruchterman.reingold)
  dcg_index <- append(dcg_index, Indexs(dcg[[i]]))
  dcg_deg <- degree.distribution(dcg[[i]])
  dcg_spr <- c(dcg_deg[3:length(dcg_deg)]) / sum(dcg_deg[3:length(dcg_deg)])
  if(length(dcg_deg) >= 3){
    hist_part <- barplot(dcg_spr, names.arg=c(2:(length(dcg_deg)-1)), xlab="degree", ylab="frequency", ylim=(0:1), main="Degree Proportion of Network as a Part", border=F)
    dcg_total <- cumsum(dcg_spr)
    lines(x=hist_part, y=dcg_total)
  }
}
dcg_mat <- matrix(dcg_index, nrow=length(dcg), byrow=T)
dcg_stat <- rbind(Statistics(dcg_mat[ ,1]), Statistics(dcg_mat[ ,2]), Statistics(dcg_mat[ ,3]), Statistics(dcg_mat[ ,4]), Statistics(dcg_mat[ ,5]), Statistics(dcg_mat[ ,6]), Statistics(dcg_mat[ ,7]), Statistics(dcg_mat[ ,8]), Statistics(dcg_mat[ ,9]))
part_indexs <- c("ノード数", "連結ノード数", "エッジ数",  "グラフの高さ", "連結ノード数に対する高さ", "密度", "推移性", "平均最短経路長", "スモールワールド性")
write.table(dcg_stat, file=file_output, append=T, sep=",", row.names=part_indexs, col.names=stat_name, fileEncoding="CP932")

# 学習データ作成 ####
neighbors <- get.edgelist(g)
for(i in 1:nrow(neighbors)){
  parent <- user[neighbors[i,1]]
  child <- user[neighbors[i,2]]
  
}

# 部分グラフごとに会話パターン抽出 ####
dcg <- decompose.graph(g, min.vertices=2)
deg <- degree(g)
deg_in <- degree(g, mode="in")
deg_out <- degree(g, mode="out")

# 発言の時間差に基づくデンドログラム作成 ####
cl <- hclust(dis, method="ward.D")
plot(cl, hang=-1)
rect.hclust(cl, h=4320000)
cut <- cutree(cl, h=4320000)
V(g)$color <- cut
set.seed(1)
plot(g, vertex.size=8, edge.arrow.size=0.2, layout=layout.fruchterman.reingold)

# 辺の媒介中心性算出 ####
#g.cent.betweenness <- centralization.betweenness(g, directed=T)
g.edge.betweenness <- edge.betweenness(g, directed=T)
set.seed(1)
plot(g, vertex.size=8, edge.arrow.size=0.2, vertex.color="lightblue", layout=layout.fruchterman.reingold, edge.label=g.edge.betweenness)

# 辺の媒介中心性に基づくコミュニティ検出 ####
eb <- edge.betweenness.community(g)
#fg5 <- fastgreedy.community(g)           # undirected graphs only
#le5 <- leading.eigenvector.community(g)  # undirected graphs only
#ml5 <- multilevel.community(g)           # undirected graphs only
#sp5 <- spinglass.community(g)            # connected graph only
#op5 <- optimal.community(g)
dend <- as.dendrogram(eb)
plot(dend)
V(g)$color <- eb$membership
dendPlot(eb)
set.seed(1)
plot(g, vertex.size=8, edge.arrow.size=0.2, layout=layout.fruchterman.reingold)

# 複数所属のコミュニティ検出 ####
library(linkcomm)
lc <- getLinkCommunities(ed)
plot(lc, type='graph')
