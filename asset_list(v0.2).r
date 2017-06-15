#####################Asset Chart Update########################################

a_close <- vector() #存储收盘价

for (j in 1:length(product_ids)) {
  
  a_close[j] <- cdt[[ptr,6+(j-1)*15]]  
  
}

a_close[na.fill(is.na(a_close),1)] = last_a_close[na.fill(is.na(a_close),1)]

position = na.fill(holding/units,0) #更新一下position

if(nrow(sta_contract_dt) != 0){
  
  a_cont  <- sta_contract_dt
  
  
  cls_status <- data.table(product_name = product_ids,
                           vm           = vm,
                           close        = a_close)
  
  a_cont     <- as.data.table(left_join(a_cont, cls_status, by = "product_name")) 
  
  
  asset_out <- data.table(
    date = cdt[ptr,date], 
    fee  = fee,
    holding_value = sum(holding * vm * a_close, na.rm = TRUE), 
    pos_profit = sum(a_cont$direction * (a_cont$close - a_cont$enter_price) * a_cont$no_contract * a_cont$vm),
    pos_dir = sum(position),
    closed_profit = closed_profit
  )
  
  
  
}else{ 
  
  
  if(length(asset_list) == 0){
    pos_profit = 0
  }else{
    pos_profit = asset_list[[length(asset_list)]][.N,pos_profit]
  } 
  
  
  
  asset_out <- data.table(
    date = cdt[ptr,date], 
    fee  = fee,
    holding_value = sum(na.omit(holding*a_close)), #the omit should only be used when ptr = 56 that no previous price
    pos_profit = pos_profit,
    pos_dir = sum(position),
    closed_profit = closed_profit
  )
  
  
}#end of if nrow(sta_contract_dt == 0)

asset_list <- list.append(asset_list, asset_out)

last_a_close[na.fill(!is.na(a_close),0)] = a_close[!is.na(a_close)]

###########################增加持仓记录模块####################################



position_rec <- as.data.table(t(c(as.character(cdt[ptr,date]),round(position,2))))
holding_rec  <- as.data.table(t(c(as.character(cdt[ptr,date]),round(holding,2))))

position_list <- list.append(position_list, position_rec)
holding_list <- list.append(holding_list, holding_rec)


#####################End of Asset Chart Update#################################


######################Asset Data Management#########################################

trades_dt <- list.stack(trades, data.table = TRUE)

trades_dt[, net_profit := profit - commision]  

asset_dt  <- list.stack(asset_list, data.table = TRUE)

asset_dt[, net_profit := closed_profit + pos_profit - fee]
asset_dt[, daily_profit := c(0,diff(net_profit))] 
asset_dt[, port_value := account + net_profit]

hist_position <- list.stack(position_list, data.table = TRUE)
hist_holding  <- list.stack(holding_list, data.table = TRUE)

colnames(hist_position) = colnames(hist_holding) = c("date",product_ids)
###########################Asset Recording#####################################


