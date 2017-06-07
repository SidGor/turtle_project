###########################Asset Recording#####################################
library(dplyr)

if(nrow(sta_contract_dt) != 0){
  
  
  
  
  a_cont  <- sta_contract_dt
  a_close <- vector()           #存储收盘价
  
  
  for (j in 1:length(product_ids)) {
    
    a_close[j] <- cdt[[6 + (j-1) * 15]][ptr]  
    
  }
  
  cls_status <- data.table(product_name = product_ids,
                           vm           = vm,
                           close        = a_close)
  
  a_cont     <- as.data.table(left_join(a_cont, cls_status, by = "product_name")) 
  
  
  asset_out <- data.table(
    date = cdt[ptr,date], 
    cash = cash,
    fee  = fee,
    holding_value = sum(holding * vm * a_close), 
    pos_profit = sum(a_cont$direction * (a_cont$close - a_cont$enter_price) * a_cont$no_contract * a_cont$vm),
    pos_dir = sum(position),
    closed_profit = closed_profit
    
  )
  
  
  asset_list <- list.append(asset_list, asset_out)
  
}else{ 
  
  a_close <- vector()           #存储收盘价
  
  for (j in 1:length(product_ids)) {
    
    a_close[j] <- cdt[[6 + (j-1) * 15]][ptr]  
    
  }
  
  asset_out <- data.table(
    date = cdt[ptr,date], 
    cash = cash,
    fee  = fee,
    holding_value = sum(holding*a_close), 
    pos_profit = "no holding",
    pos_dir = sum(position),
    closed_profit = closed_profit
  )
  
  
  
}#end of if nrow(sta_contract_dt == 0)






###########################Asset Recording#####################################



#######################some more data##########################################

asset_dt[, net_profit := closed_profit + pos_profit - fee]
asset_dt[, profit := c(0,diff(net_profit))] 
asset_dt[, cum_profit := cumsum(profit)]
asset_dt[, cummax_cum_profit := cummax(cum_profit)]
asset_dt[, drawdown := cum_profit - cummax_cum_profit]
###############################################################################