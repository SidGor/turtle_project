######close position###########################################################
library(dplyr)  #使用left_join

####################Testing Environment#####################################
#backing up the original data
#可以重新处理一下enter_price和cut_price, 空仓已经记录为-1L，乘以-1可以使大小反
#向，也就是说可以与多仓公用一个大小比较的算法。加一列现价。

#测试日期20161011

save_holding = holding
save_position = position
save_cash = cash
  #10月11日，ptr
ptr = 56   #假设走到86行
save_fee = fee
save_trades = trades
#############################################################################



save_sta_dt <- sta_contract_dt         #需要改回全集
temp_cdt <- cdt[ptr]                   #需要改回ptr


l_contracts <- save_sta_dt[direction == 1]  #存储多单
s_contracts <- save_sta_dt[direction == -1] #存储空单


#提取高低收矩阵


c_highs <- vector()           #存储最高价
c_lows <- vector()            #存储最低价
c_opens <- vector()           #存储开盘价

for (j in 1:length(product_ids)) {
  
  c_highs[j] <- temp_cdt[[4 + (j-1) * 15]]  
  c_lows[j]  <- temp_cdt[[5 + (j-1) * 15]]
  c_opens[j] <- temp_cdt[[3 + (j-1) * 15]]
  
}

#集成每个产品当日的相关数据
information <- data.table(product_name = product_ids,
                          highs = c_highs,
                          lows  = c_lows,
                          opens = c_opens
)

#集成表格以便判断是否止损。

if(nrow(l_contracts) != 0) {
  
  l_contracts <- as.data.table(left_join(l_contracts, information, by = "product_name"))
  l_close <- l_contracts[cut_point > lows,]
  
}else{
  
  l_close = data.table()
  
}

if(nrow(s_contracts) != 0) {
  s_contracts <- as.data.table(left_join(s_contracts, information, by = "product_name"))
  s_close <- s_contracts[cut_point < highs,]
}else{
  s_close = data.table()
}


#记录每笔交易信息-平空仓
if(nrow(s_close) == 0){
  
}else{
  
  for (j in 1:nrow(s_close)){
    
    
    product_match = match(s_close[j,product_name],product_ids)
    
    trade_id    = s_close[j, trade_id]
    enter_date  = s_close[j,enter_date]
    enter_price = s_close[j,enter_price]
    leave_date  = cdt[ptr,date]
    leave_price = s_close[j,cut_point] + slippage[product_match] 
    ori_direction = s_close[j,direction]
    commision   = leave_price * vm[product_match] + enter_price * vm[product_match]
    profit      = (enter_price - leave_price) * vm[product_match] * s_close[j,no_contract] 
    closed_profit = closed_profit + profit
    fee         = fee + leave_price * vm[product_match] * s_close[j,no_contract] * fee.rate[product_match]
    
    cash        = cash - leave_price * vm[product_match] * s_close[j,no_contract] - leave_price * vm[product_match] * s_close[j,no_contract] * fee.rate[product_match]
    position[product_match] = position[product_match] +1 #adjust position
    holding[product_match] = holding[product_match] + s_close[j,no_contract] #空仓应该反加回去
    
    
    trade_out  <- data.table(
      trade_id   = trade_id,
      enter_date = enter_date,
      enter_price = enter_price,
      leave_date = leave_date,
      leave_price = leave_price,
      long_short = ori_direction,
      commision = commision,
      profit    = profit,
      contracts = s_close[j,no_contract]
    )
    trades <- list.append(trades, trade_out)
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
if(nrow(l_close) == 0){
  
}else{
  
  for (j in 1:nrow(l_close)){
    
    
    product_match = match(l_close[j,product_name],product_ids)
    
    trade_id    = l_close[j, trade_id]
    enter_date  = l_close[j, enter_date]
    enter_price = l_close[j, enter_price]
    leave_date  = cdt[ptr, date]
    leave_price = l_close[j, cut_point] - slippage[product_match]   #go against the trade 
    ori_direction = l_close[j, direction]
    commision   = leave_price * vm[product_match] + enter_price * vm[product_match]
    profit      = (leave_price - enter_price) * vm[product_match] * l_close[j,no_contract] 
    closed_profit = closed_profit + profit
    fee         = fee + leave_price * vm[product_match] * l_close[j,no_contract] * fee.rate[product_match]
    
    cash        = cash + leave_price * vm[product_match] * l_close[j,no_contract] - leave_price * vm[product_match] * l_close[j,no_contract] * fee.rate[product_match]
    position[product_match] = position[product_match] - 1 #adjust position
    holding[product_match] = holding[product_match] - l_close[j,no_contract] #空仓应该反加回去,多仓应该反减
    
    
    trade_out  <- data.table(
      trade_id   = trade_id,
      enter_date = enter_date,
      enter_price = enter_price,
      leave_date = leave_date,
      leave_price = leave_price,
      long_short = ori_direction,
      commision = commision,
      profit    = profit,
      contracts = l_close[j,no_contract]
    )
    trades <- list.append(trades, trade_out)
  }
  # 删除standing_contract里面的数据
  
  d_trades <- list.map(trades, trade_id)
  
  for (l in 1:length(d_trades)){
    
    standing_contract[d_trades[[l]]] = NULL
    
  }
  # 更新sta_contract_dt
  sta_contract_dt <-  list.stack(standing_contract, data.table = TRUE)   #use data.frame for easy tracking
  
  
}#end of judging nrow == 0



###############################################################################
