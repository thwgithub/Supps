########################################################################
#重置根目录：setwd('E:/学习文件盘/论文写作/第八篇文章')
#获取根目录：getwd()
#数据读取,如果是Excel要保存为csv格式
#除此之外，这些包也能读取，RODBC、xlsx、openxlsx、gdata、readxl
data=read.table('data19.csv',sep=',',header=F,
col.names=c('province','X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13',
'X14','X15','X16','X17','X18','X19','X20'))
financial_data=data.frame(x1=data[2],x2=data[3],x3=data[4],x4=data[5],
x5=data[6],x6=data[7],x7=data[8],x8=data[9],x9=data[10],x10=data[11],x11=data[12],
x12=data[13],x13=data[14],x14=data[15],x15=data[16],x16=data[17],x17=data[18],
x18=data[19],x19=data[20],x20=data[21],
row.names = c("北京", "天津", "河北", "山西", "内蒙古", "辽宁", "吉林", 
                   "黑龙江", "上海", "江苏", "浙江", "安徽", "福建", "江西", 
                   "山东",  "河南", "湖北", "湖南", "广东", "海南","广西",  
                   "重庆", "四川", "贵州", "云南", "西藏", "陕西", "甘肃", 
                   "青海", "宁夏", "新疆"))
financial_pca=princomp(financial_data, cor=T)#pca
results=summary(financial_pca, loadings=TRUE)
biplot(financial_pca)#主成分散点图，双标图
####综合得分计算及排名
library(mvstats)#仅能安装R4.0以前的版本，只能本地安装
financial_rank=princomp.rank(financial_pca,m=4)
#按照综合得分排序
financial_Rank=financial_rank[order(-financial_rank[,6]),]
FRRound3=round(financial_Rank,3)#保留有效数字
#将R中的数据写成excel文件
library(writexl)
FR=as.data.frame(FRRound3)
name=as.data.frame(rownames(FRRound3))
write_xlsx(name,'province19.xlsx')
write_xlsx(FR,'resultRank19.xlsx')

####聚类验证#####
###########kmeans算法##################
km <- kmeans(scale(financial_data), 4, nstart = 20000)#标准化，无需提前计算距离
km   #查看结果
sort(km$cluster)#根据聚类标号排序

library(factoextra)
fviz_cluster(km, data=scale(financial_data))#kmeans算法结果的可视化

####DCFS变量筛选算法
library(FNN)
library(infotheo)
library(energy)
library(mlbench)#DNA
############################################################
#DCFS算法
############################################################
DCFS0=function(c,w,delta){
library(energy)
DR=vector()
for(i in 1:ncol(w))
{
DR[i]=dcor(c,w[,i])
}
list=rev(order(DR))
S=rev(order(DR)[which(DR[order(DR)]>delta)])#将大于delta的特征初始化
List=list[-match(S,list)]
for(i in 1:length(List))
{
S_group=c(S,List[i])
if(dcor(c,w[,S_group])>dcor(c,w[,S])) S=S_group
else S=S
}
return(S)
}
c=financial_data[,1];w=financial_data[,-1]
DR=vector()
for(i in 1:ncol(w))
{
DR[i]=dcor(c,w[,i])
}
delta=mean(DR)+3*var(DR)
S_DCFS=DCFS0(c,w,delta)
sort_SD=sort(S_DCFS)
new_data=financial_data[,sort_SD]

####变量选择后的主成分分析
financial_pca=princomp(new_data, cor=T)
summary(financial_pca, loadings=TRUE)
library(mvstats)
financial_rank=princomp.rank(financial_pca,m=2)
#按照综合得分排序
financial_Rank=financial_rank[order(-financial_rank[,4]),]

####聚类验证#####

###########kmeans算法##################
km <- kmeans(scale(new_data), 4, nstart = 20000)#标准化，无需提前计算距离
km   #查看结果
sort(km$cluster)#根据聚类标号排序
library(factoextra)
fviz_cluster(km, data=scale(new_data))#kmeans算法结果的可视化








