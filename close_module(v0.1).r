######close position###########################################################
library(dplyr)  #使用left_join

test_dt <- sta_contract_dt[1:8]

#可以重新处理一下enter_price和cut_price, 空仓已经记录为-1L，乘以-1可以使大小反
#向，也就是说可以与多仓公用一个大小比较的算法。加一列现价。

#测试日期20161011

temp_cdt <- cdt[86]  #10月11日，ptr


match("c",letters) #can use match to locate which price to extract



l_contracts <- test_dt[direction == 1]
s_contracts <- test_dt[direction == -1]

#提取高低收矩阵


c_highs <- vector()
c_lows <- vector()
c_opens <- vector()

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
l_contracts <- as.data.table(left_join(l_contracts, information, by = "product_name"))
s_contracts <- as.data.table(left_join(s_contracts, information, by = "product_name"))


l_close <- l_contracts[cut_point > lows,]
s_close <- s_contracts[cut_point < highs,]

#记录每笔交易信息
for (j in 1:nrow(s_close)){
  

  product_match = match(s_close[j,product_name],product_ids)
  
  enter_date  = s_close[j,enter_date]
  enter_price = s_close[j,enter_price]
  leave_date  = cdt[i,date]
  leave_price = s_close[j,cut_point] + slippage[product_match] 
  ori_direction = s_close[j,direction]
  commision   = leave_price * vm[product_match] + enter_price * vm[product_match]
  profit      = (enter_price - leave_price) * vm[product_match] * s_close[j,no_contract] 
  closed_profit = closed_profit + profit
  fee         = leave_price * vm[product_match] * s_close[j,no_contract] * fee.rate[product_match]
  
  cash        = cash + leave_price * vm[product_match] * s_close[j,no_contract]
  
  trade_out  <- list(
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

# 更新holding,position （请返回上一个loop写）

# 删除standing_contract里面的数据




###############################################################################