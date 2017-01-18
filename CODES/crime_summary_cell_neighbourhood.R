library(data.table)
library(geosphere)
library(parallel)

load("../DERIVED/grids_info.Rdata")
load("../DERIVED/DATA/NIJ2012-2015_grid#_added.Rdata")
source("lib.R")

panel <- subset(panel, grid!=0)
panel[,occ_date:=as.Date(occ_date)]
setorder(panel,"occ_date","grid")

# Create a master panel of all grids
panel_master <- grids[,1,with=F]
setorder(panel_master,"grid")

# Get annual crime numbers
panel[,occ_year:=year(occ_date)]
panel[,count:=1]
annual <- dcast(panel, grid ~ occ_year + CATEGORY, value.var = "count", fun.aggregate = sum)
panel_master <- merge(panel_master, annual, by="grid", all.x=T)

# Get quarterly crime numbers (ALTERNATIVE: by both quarter and year)
panel[,occ_month:= month(occ_date)]
panel[,occ_month2:=occ_month+1]; panel[occ_month2==13, occ_month2:=1]
panel[,occ_quarter:=cut(occ_month2,breaks=c(1,4,7,10,12), include.lowest=T, right=F, labels=F)]
quarter <- dcast(panel, grid ~ occ_quarter + CATEGORY, value.var = "count", fun.aggregate = sum)
panel_master <- merge(panel_master, quarter, by="grid", all.x=T)

panel_master[is.na(panel_master)] = 0

# Get the neighbourhood annual and quarterly history
Sys.time()
cl <- makeCluster(getOption("cl.cores", 3))
clusterExport(cl, c("panel_master","grids","getNeighbourHoodHistory"))
p <- parLapply(cl = cl, X=panel_master$grid,fun = function(x){
  library(data.table)
  library(geosphere)
  neighbourhood_hist_grid <- getNeighbourHoodHistory(panel_master,grids_coord = grids[,1:3,with=F], 
                                                     radius=1, vars=colnames(panel_master)[2:33], 
                                                     agg.by = NULL, gridnum=x)
  if(nrow(neighbourhood_hist_grid)==0)
    return(NULL)
  else{
    return(neighbourhood_hist_grid)
  }
})
Sys.time()
stopCluster(cl=cl)
neighbourhood_hist_allgrids <- rbindlist(p)
save(neighbourhood_hist_allgrids, file="../DERIVED/NEIGHBOURHOOD/neighbourhood1km_crimehist_all_cells.Rdata")

panel_master <- merge(panel_master, neighbourhood_hist_allgrids, by="grid", all.x=T)

indx <- grep("BURGLARY",colnames(panel_master))
panel1 <- subset(panel_master, select=c("grid", colnames(panel_master)[indx]))
panel1[,category:="BURGLARY"]
colnames(panel1) <- gsub("_BURGLARY","",colnames(panel1))
indx <- grep("MOTOR VEHICLE THEFT",colnames(panel_master))
panel2 <- subset(panel_master, select=c("grid", colnames(panel_master)[indx]))
panel2[,category:="MOTOR VEHICLE THEFT"]
colnames(panel2) <- gsub("_MOTOR VEHICLE THEFT","",colnames(panel2))
indx <- grep("OTHER",colnames(panel_master))
panel3 <- subset(panel_master, select=c("grid", colnames(panel_master)[indx]))
panel3[,category:="OTHER"]
colnames(panel3) <- gsub("_OTHER","",colnames(panel3))
indx <- grep("STREET CRIMES",colnames(panel_master))
panel4 <- subset(panel_master, select=c("grid", colnames(panel_master)[indx]))
panel4[,category:="STREET CRIMES"]
colnames(panel4) <- gsub("_STREET CRIMES","",colnames(panel4))

panel_master <- rbind(panel1, panel2, panel3, panel4)
panel_master[,all_years:=get("2012")+get("2013")+get("2014")+get("2015")]
save(panel_master, file="../DERIVED/PANELS/panel_master_all_12_15.Rdata.Rdata")








