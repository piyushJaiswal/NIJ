getNeighbourHoodHistory <- function(df,grids_coord,radius,vars, agg.by, gridnum){

  pts = as.matrix(grids_coord[grid==gridnum,2:3,with=F])
  refs = as.matrix(grids_coord[grid!=gridnum,2:3,with=F])
  distances <- distGeo(refs,pts)/1000
  neighbours <- grids_coord[which(distances<=radius),grid]
  
  df2 <- subset(df, grid %in% neighbours)
  df2 <- df2[,lapply(.SD,function(x) mean(x)), .SDcols = vars, by=agg.by]
  if(nrow(df2)==0)
    next()
  colnames(df2)[colnames(df2)!=agg.by] = paste0("neighbourhood_",colnames(df2)[colnames(df2)!=agg.by])
  df2[,grid:=gridnum]
    
  return (df2)
}

