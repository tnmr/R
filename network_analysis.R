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



# ネットワーク分析 ####

# 最短経路長（グラフの高さ）関数 ####
PathLength <- function(x){
  link_len <- path.length.hist(x)
  height <- length(link_len$res)
  height
}

# ネットワーク指標の関数 ####
Indexs <- function(x){
  v <- vcount(x)                              # ノード数
  v_link <- length(degree(x)[degree(x)!=0])   # 連結ノード数
  e <- ecount(x)                              # エッジ数
  len <- PathLength(x)                        # グラフの高さ
  rate_len <- len / v_link                    # 連結ノード数に対する高さ
  #den <- graph.density(x)   # 密度
  den <- e / sum(1:v-1)                       # 密度
  tran <- transitivity(x, isolates="zero")    # 推移性
  #assort <- assortativity.degree(x)          # 連結する2ノード間の次数の相関係数
  len_mean <- average.path.length(x)          # 平均最短経路長
  small <- len_mean / v_link                  # スモールワールド性
  output <- c(v, v_link, e, len, rate_len, den, tran, len_mean, small)
  output
}

# 全体のネットワーク指標算出 ####
dcg <- decompose.graph(g, min.vertices=2)
g_index <- c(votes[file_num], rank[file_num], Indexs(g), length(dcg), length(dcg) / vcount(g))
whole_indexs <- c("得票数", "順位", "ノード数", "連結ノード数", "エッジ数",  "グラフの高さ", "連結ノード数に対する高さ", "密度", "推移性", "平均最短経路長", "スモールワールド性", "部分グラフ数", "ノード数に対する部分グラフの数")
file_output <- sprintf("~/Documents/R/workspace/study_data/output%s.csv", as.character(file_num))
write.table(g_index, file=file_output, sep=",", row.names=whole_indexs, col.names=F, fileEncoding="CP932")
write.table(user_stat, file=file_output, append=T, sep=",", row.names=F, col.names=F, fileEncoding="CP932")
write.table(user_ent, file=file_output, append=T, sep=",", row.names=c("平均情報量", "最大値", "得点"), col.names=F, fileEncoding="CP932")

# 次数分布描画（リンク数0はなし） ####
library(e1071)
# 入出合計リンク数(1~)
deg_link <- c(degree.distribution(g)[2:length(degree.distribution(g))]) / sum(degree.distribution(g)[2:length(degree.distribution(g))])
hist_whole <- barplot(deg_link, names.arg=c(1:(length(degree.distribution(g))-1)), space=0, xlab="degree", ylab="frequency", ylim=(0:1), main="All Degree Proportion of Network as a Whole", col="#ADD8E67F", border=F)
total_spr <- cumsum(deg_link)
lines(x=hist_whole, y=total_spr, col="blue")
# 入リンク数(1~)
in_link <- c(degree.distribution(g, mode="in")[2:length(degree.distribution(g, mode="in"))]) / sum(degree.distribution(g, mode="in")[2:length(degree.distribution(g, mode="in"))])
hist_whole <- barplot(in_link, names.arg=c(1:(length(degree.distribution(g, mode="in"))-1)), space=0, xlab="degree", ylab="frequency", ylim=(0:1), main="In Degree Proportion of Network as a Whole", col="#FF63477F", border=F)
total_spr <- cumsum(in_link)
lines(x=hist_whole, y=total_spr, col="red")
# 出リンク数(1~)
out_link <- c(degree.distribution(g, mode="out")[2:length(degree.distribution(g, mode="out"))]) / sum(degree.distribution(g, mode="out")[2:length(degree.distribution(g, mode="out"))])
hist_whole <- barplot(out_link, names.arg=c(1:(length(degree.distribution(g, mode="out"))-1)), space=0, xlab="degree", ylab="frequency", ylim=(0:1), main="Out Degree Proportion of Network as a Whole", col="#90EE907F", border=F)
total_spr <- cumsum(out_link)
lines(x=hist_whole, y=total_spr, col="green")

# 入出合計リンク数(1~)
hist_row <- c("平均情報量", "最大値", "得点", "歪度", "尖度")
deg_skew <- skewness(deg_link)     # 歪度
deg_kurt <- kurtosis(deg_link)     # 尖度
write.table(deg_link, file=file_output, append=T, sep=",", row.names=as.character(c(1:(length(degree.distribution(g))-1))), col.names=c("入出リンクの次数分布(1~)"), fileEncoding="CP932")
spr_stat <- c(Entropy(deg_link), deg_skew, deg_kurt)
write.table(spr_stat, file=file_output, append=T, sep=",", row.names=hist_row, col.names=F, fileEncoding="CP932")
# 入リンク数(1~)
in_skew <- skewness(in_link)     # 歪度
in_kurt <- kurtosis(in_link)     # 尖度
write.table(in_link, file=file_output, append=T, sep=",", row.names=as.character(c(1:(length(degree.distribution(g, mode="in"))-1))), col.names=c("入リンクの次数分布(1~)"), fileEncoding="CP932")
in_spr_stat <- c(Entropy(in_link), in_skew, in_kurt)
write.table(in_spr_stat, file=file_output, append=T, sep=",", row.names=hist_row, col.names=F, fileEncoding="CP932")
# 出リンク数(1~)
out_skew <- skewness(out_link)     # 歪度
out_kurt <- kurtosis(out_link)     # 尖度
write.table(out_link, file=file_output, append=T, sep=",", row.names=as.character(c(1:(length(degree.distribution(g, mode="out"))-1))), col.names=c("出リンクの次数分布(1~)"), fileEncoding="CP932")
out_spr_stat <- c(Entropy(out_link), out_skew, out_kurt)
write.table(out_spr_stat, file=file_output, append=T, sep=",", row.names=hist_row, col.names=F, fileEncoding="CP932")

# べき分布と仮定したときの推定される指数γ
#spr <- degree(g)[degree(g)>0]
#exp <- 1+vcount(g)/sum(log(spr/min(spr)))
#pro_exp = c()
#for(k in 1:(length(degree.distribution(g))-1)){
#  pro_exp <- append(pro_exp, k^(-exp))
#}
#lines(x=hist_whole, y=pro_exp, col="blue")
#write.table(exp, file=file_output, append=T, sep=",", row.names="指数γ", col.names=F, fileEncoding="CP932")

# 最大連結成分の抽出（ノードの数） ####
#g_long <- delete.vertices(g, subset(V(g), cls$membership!=which(cls$csize==max(cls$csize))[[1]]))
#max_len <- max(cls$csize)
#set.seed(1)
#plot(g_long, vertex.size=8, edge.arrow.size=0.2, vertex.color="lightgreen", vertex.label=V(g)$name, layout=layout.fruchterman.reingold)

# 分散の関数 ####
Variance <- function(x){
  m <- mean(x)
  denominator <- length(x)
  deviation <- x - m
  square <- deviation^2
  variance <- sum(square) / denominator
  variance
}

# 統計データ関数 ####
Statistics <- function(x){
  max <- max(x)                    # 最大
  min <- min(x)                    # 最小
  mean <- mean(x)                  # 平均
  median <- median(x)              # 中央値
  variance <- Variance(x)          # 分散
  deviation <- sqrt(Variance(x))   # 標準偏差
  output <- c(max, min, mean, median, variance, deviation)
  output
}

# 中心性指標 ####
stat_name <- c("最大", "最小", "平均", "中央値", "分散", "標準偏差")
centrality_name <- c("次数中心性", "近接中心性", "媒介中心性", "固有ベクトル中心性", "ページランク")
centrality_stat <- rbind(Statistics(degree(g)), Statistics(closeness(g)), Statistics(betweenness(g)), Statistics(evcent(g)$vector), Statistics(page.rank(g)$vector))
write.table(centrality_stat, file=file_output, append=T, sep=",", row.names=centrality_name, col.names=stat_name, fileEncoding="CP932")

# ハブになっているノードの確認 ####
hub = c()
for(i in 1:vcount(g)){
  if(degree(g)[i] >= 3){
    hub <- append(hub, c(user[i], user_hist[user[i]], com_len[i]))
  }
}
hub_mat <- matrix(hub, ncol=3, byrow=T)
