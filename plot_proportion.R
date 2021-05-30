plot_proportion<-function(res){
res=res[!is.na(res$drugset),]
a=vector()
for(i in 1:nrow(res)){
  if(length(strsplit(res$drugset[i],split=" ")[[1]])>1){
    for(j in 1:length(strsplit(res$drugset[i],split=" ")[[1]])){
       a=append(a,strsplit(res$drugset[i],split=" ")[[1]][j])
}
  }
  if(length(strsplit(res$drugset[i],split=" ")[[1]])==1){
    a=append(a,strsplit(res$drugset[i],split=" ")[[1]][1])
}
}
uniq_a=unique(a)
uniq_r=unique(res$race)
uniq_s=unique(res$sex)
uniq_age=unique(res$age)


prop_drg=vector()
prop_s=vector()
prop_r=vector()
prop_age=vector()
for(i in 1:length(uniq_a)){
  cnt=0
  for(j in 1:nrow(res)){
    if (regexpr(uniq_a[i],res$drugset[j])[[1]]==1){
    cnt=cnt+1
  }
  }
  prop_drg[i]=cnt/nrow(res)
}

for(i in 1:length(uniq_s)){
  cnt=0
  for(j in 1:nrow(res)){
    if (regexpr(uniq_s[i],res$sex[j])[[1]]==1){
      cnt=cnt+1
    }
  }
  prop_s[i]=cnt/nrow(res)
}  
  
for(i in 1:length(uniq_r)){
  cnt=0
  for(j in 1:nrow(res)){
    if (regexpr(uniq_r[i],res$race[j])[[1]]==1){
      cnt=cnt+1
    }
  }
  prop_r[i]=cnt/nrow(res)
}  

for(i in 1:length(uniq_age)){
  cnt=0
  for(j in 1:nrow(res)){
    if (regexpr(uniq_age[i],res$age[j])[[1]]==1){
      cnt=cnt+1
    }
  }
  prop_age[i]=cnt/nrow(res)
}  

m_tab=matrix(0,(length(uniq_a)+length(uniq_s)+length(uniq_r)+length(uniq_age)),3)


    
m_tab[,1]=append(append(append(rep(colnames(res)[2],length(uniq_a)),rep(colnames(res)[4],length(uniq_r))),rep(colnames(res)[5],length(uniq_s))), rep(colnames(res)[6],length(uniq_age)))
m_tab[,2]=append(append(append(uniq_a,uniq_r),uniq_s),uniq_age)
m_tab[,3]=append(append(append(prop_drg,prop_r),prop_s),prop_age)
#uniq_g=unique(res$group)
# l=1
# m_tab=matrix(0,length(uniq_g)*length(uniq_a),3)
# for(i in 1:length(uniq_g)){
#   b=res[which(res$group==uniq_g[i]),]
#   
#   for(j in 1:length(uniq_a)){
#     cnt=0
#     for(k in 1:nrow(b)){
#     if (regexpr(uniq_a[j],b$drugset[k])[[1]]==1){
#       cnt=cnt+1
#     }
#     }
#     
#     m_tab[l,1]=uniq_g[i]
#     m_tab[l,2]=uniq_a[j]
#     m_tab[l,3]=cnt
#     l=l+1  
#   }
# }

colnames(m_tab)<-c('V1','V2','V3')
m_tab=as.data.frame(m_tab)
m_tab$V3=round(as.numeric(m_tab$V3)*100,2)
return(m_tab)
}