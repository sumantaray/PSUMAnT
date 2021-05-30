stat_tab<-function(result_map_notna,p,drg,side,node){
  
  options(warn=-1)
  tr=as(result_map_notna[[p]][,1:16], "transactions")
  conf_ji=array()
  conf_ij=array()
  lift_ij=array()
  sp_ij=array()
  tr_mat=1*as(items(tr),"matrix")
  k=1
  i=which(colnames(tr_mat)==drg)
  for(j in 1:16){
    sp_i= sum(tr_mat[,i]*result_map_notna[[p]][,20])/sum(result_map_notna[[p]][,20]) #sum(tr_mat[,i])/nrow(tr_mat)
    sp_j= sum(tr_mat[,j]*result_map_notna[[p]][,20])/sum(result_map_notna[[p]][,20])
    sp_ij[k]=round(sum(result_map_notna[[p]][which(apply(tr_mat[,c(i,j)],1,all)==1),20])/sum(result_map_notna[[p]][,20]),4)
    conf_ij[k]=round(sp_ij[k]/sp_i,4)
    conf_ji[k]=round(sp_ij[k]/sp_j,4)
    lift_ij[k]=round(conf_ij[k]/sp_j,4)
    k=k+1
  }
  
  df=data.frame(drg,node,sp_ij,lift_ij, conf_ij,conf_ji)
  
  df=df[setdiff(1:16,i),]
  
  if(side=="Right"){
    df$delta_conf=df[,5]-df[,6]
  }
  
  if(side=="Left"){
    df$delta_conf=df[,6]-df[,5]
  }
  
  colnames(df)<-c('Drug (D)','Drug (D*)', 'Supp_W (D->D*)','Lift_W', 'Conf_W (D->D*)','Conf_W (D*->D)','Delta_Conf_w')
  
  
  return(df)
}
