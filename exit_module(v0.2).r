############################  Exit Strategy  ##################################
library(dplyr)
##########################Test Environment#####################################
#backing up the original data
#
#测试日期20161011

save_holding = holding
save_position = position
save_cash = cash
#10月11日，ptr
ptr = 86   #假设走到86行
save_fee = fee
save_trades = trades
#############################################################################


#####################Exit Strategy#############################################



temp_cdt <- cdt[ptr]                   #需要改回ptr

l_contracts <- sta_contract_dt[direction == 1]  #存储多单
s_contracts <- sta_contract_dt[direction == -1] #存储空单

#读取20日高低价数据
#


#提取高低收矩阵

e_highs <- vector()           #存储最高价
e_lows <- vector()            #存储最低价
e_20highs <- vector()           #存储突破价
e_20lows <- vector()            #存储突破价
e_opens <- vector()           #存储开盘价

for (j in 1:length(product_ids)) {
  
  e_highs[j] <- temp_cdt[[4 + (j-1) * 15]]  
  e_lows[j]  <- temp_cdt[[5 + (j-1) * 15]]
  e_20highs[j] <- temp_cdt[[13 + (j-1) * 15]]  
  e_20lows[j]  <- temp_cdt[[14 + (j-1) * 15]]
  e_opens[j] <- temp_cdt[[3 + (j-1) * 15]]
}

#集成每个产品当日的相关数据
information <- data.table(product_name = product_ids,
                          opens   = e_opens,
                          high    = e_highs,
                          low     = e_lows,
                          highs20 = e_20highs,
                          lows20  = e_20lows
)

#集成表格以便判断是否止损。

if(nrow(l_contracts) != 0) {
  
  l_contracts <- as.data.table(left_join(l_contracts, information, by = "product_name"))
  l_exit <- l_contracts[low < lows20,]
  
}else{
  
  l_exit = data.table()
  
}


if(nrow(s_contracts) != 0) {
  s_contracts <- as.data.table(left_join(s_contracts, information, by = "product_name"))
  s_exit <- s_contracts[high > highs20,]
}else{
  s_exit = data.table()
}



#记录每笔交易信息-平空仓
if(nrow(s_exit) == 0){
  
}else{
  
  for (j in 1:nrow(s_exit)){
    
    product_match = match(s_exit[j,product_name],product_ids)
    
    trade_id    = s_exit[j, trade_id]
    enter_date  = s_exit[j,enter_date]
    enter_price = s_exit[j,enter_price]
    leave_date  = cdt[ptr,date]
    leave_price = s_exit[j,highs20] + slippage[product_match] 
    ori_direction = s_exit[j,direction]
    commision   = leave_price * vm[product_match] + enter_price * vm[product_match]
    profit      = (enter_price - leave_price) * vm[product_match] * s_exit[j,no_contract] 
    closed_profit = closed_profit + profit
    fee         = fee + leave_price * vm[product_match] * s_exit[j,no_contract] * fee.rate[product_match] + fee_rate_fix[product_match]
    
    cash        = cash - leave_price * vm[product_match] * s_exit[j,no_contract] - leave_price * vm[product_match] * s_exit[j,no_contract] * fee.rate[product_match] - fee_rate_fix[product_match]
    position[product_match] = position[product_match] +1 #adjust position
    holding[product_match] = holding[product_match] + s_exit[j,no_contract] #空仓应该反加回去
    
    
    trade_out_e  <- data.table(
      trade_id   = trade_id,
      item       = product_ids[product_match],
      enter_date = enter_date,
      enter_price = enter_price,
      leave_date = leave_date,
      leave_price = leave_price,
      long_short = ori_direction,
      commision = commision,
      profit    = profit,
      contracts = s_exit[j,no_contract]
    )
    trades <- list.append(trades, trade_out_e)
  }
  # 删除standing_contract里面的数据
  
  d_trades <- list.map(trades, trade_id)
  
  for (l in 1:length(d_trades)){
    
    standing_contract[d_trades[[l]]] = NULL
    
  }
  # 更新sta_contract_dt
  sta_contract_dt <-  list.stack(standing_contract, data.table = TRUE)   #use data.frame for easy tracking
  
  
}#end of judging nrow == 0



#记录每笔交易信息-平多仓
if(nrow(l_exit) == 0){
  
}else{
  
  for (j in 1:nrow(l_exit)){
    
    
    product_match = match(l_exit[j,product_name],product_ids)
    
    trade_id    = l_exit[j, trade_id]
    enter_date  = l_exit[j, enter_date]
    enter_price = l_exit[j, enter_price]
    leave_date  = cdt[ptr, date]
    leave_price = l_exit[j, lows20] - slippage[product_match]   #go against the trade 
    ori_direction = l_exit[j, direction]
    commision   = leave_price * vm[product_match] + enter_price * vm[product_match]
    profit      = (leave_price - enter_price) * vm[product_match] * l_exit[j,no_contract] 
    closed_profit = closed_profit + profit
    fee         = fee + leave_price * vm[product_match] * l_exit[j,no_contract] * fee.rate[product_match] + fee_rate_fix[product_match]
    
    cash        = cash + leave_price * vm[product_match] * l_exit[j,no_contract] - leave_price * vm[product_match] * l_exit[j,no_contract] * fee.rate[product_match] - fee_rate_fix[product_match]
    position[product_match] = position[product_match] - 1 #adjust position
    holding[product_match] = holding[product_match] - l_exit[j,no_contract] #空仓应该反加回去,多仓应该反减
    
    
    trade_out_e  <- data.table(
      trade_id   = trade_id,
      item       = product_ids[product_match],
      enter_date = enter_date,
      enter_price = enter_price,
      leave_date = leave_date,
      leave_price = leave_price,
      long_short = ori_direction,
      commision = commision,
      profit    = profit,
      contracts = l_exit[j,no_contract]
    )
    trades <- list.append(trades, trade_out_e)
  }
  # 删除standing_contract里面的数据
  
  d_trades <- list.map(trades, trade_id)
  
  for (l in 1:length(d_trades)){
    
    standing_contract[d_trades[[l]]] = NULL
    
  }
  # 更新sta_contract_dt
  sta_contract_dt <-  list.stack(standing_contract, data.table = TRUE)   #use data.frame for easy tracking
  
  
}#end of judging nrow == 0



#####################End of Exit Strategy######################################