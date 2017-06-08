#####################Asset Monitor#############################################

#判定是否要调整操盘资金量的大小  
if (account < 0.9*shadow_account) {
  
  shadow_account = shadow_account*0.8
  
} else if (account >(1/0.9)*shadow_account & shadow_account < acc_origin) {
  
  shadow_account = 1.25*shadow_account
  
}



#计算Unit，并且判定调整  

NxDPPs <- rep(NA,length(product_ids))

for (j in 1:length(product_ids)){
  
  NxDPPs[j] <- as.numeric(cdt[[15*j-5]][ptr])  #根据相对位置提取N*DPP以计算Unit限制 
  
}

units <- 0.01*shadow_account/NxDPPs             #当前每个产品下的Unit
units <- floor(units)                           #向下取整
#上面是整个模型都会用到的数据，轻易勿动
############################真.Asset Monitor 开始线#########################################



###############################test.environment#############################################

#2015年12月2日整体持仓是-10，可以将时间调到那天

#根据holding重算position

#test 1
threshold_1 = 4     #单向判断，请记得改回12

position = holding/units

judgement = -1*(holding>=0) + 1 * (holding <0)   #这个可以用于判定计算应调整仓位时的正负方向,与持仓方向相反

test_1 = abs(position) - threshold_1

while(max(test_1) > 0){
  
  a_items_1 <- product_ids[(test_1 >0)] 
  
  a_close_1 <- sta_contract_dt[product_name %in% a_items_1,][.N,]
  
  pick_1  =  match(a_close_1$product_name,product_ids)
  
  holding[pick_1] = holding[pick_1] + judgement[pick_1] * a_close_1$no_contract #改合约数


#记账
trade_id    = a_close_1[, trade_id]
enter_date  = a_close_1[, enter_date]
enter_price = a_close_1[, enter_price]
leave_date  = cdt[ptr, date]
leave_price = cdt[[3 + (pick_1 - 1) * 15]][ptr] - slippage[pick_1]   #开盘就应该平掉了 
ori_direction = "test1"
commision   = leave_price * vm[pick_1] + enter_price * vm[pick_1]
profit      = -judgement[pick_1] * (leave_price - enter_price) * vm[pick_1] * a_close_1$no_contract 
closed_profit = closed_profit + profit
fee         = fee + leave_price * vm[pick_1] * a_close_1$no_contract * fee.rate[pick_1]

cash        = cash + judgement[pick_1] * leave_price * vm[pick_1] * a_close_1$no_contract - leave_price * vm[pick_1] * a_close_1$no_contract * fee.rate[pick_1]

trade_out_a  <- data.table(
  trade_id   = trade_id,
  item       = product_ids[pick_1],
  enter_date = enter_date,
  enter_price = enter_price,
  leave_date = leave_date,
  leave_price = leave_price,
  long_short = ori_direction,
  commision = commision,
  profit    = profit,
  contracts = a_close_1[,no_contract]
)
trades <- list.append(trades, trade_out_a)

standing_contract[[sta_contract_dt[product_name == product_ids[pick_1]][.N]$trade_id]] = NULL

sta_contract_dt <- list.stack(standing_contract, data.table = TRUE)

position = holding/units

test_1 = abs(position) - threshold_1

} #stop of while max(test_1) > 0


#test 2

threshold_2 = 6     #风险控制，请记得改回6

position = holding/units

judgement = -1*(holding>=0) + 1 * (holding <0)   #这个可以用于判定计算应调整仓位时的正负方向,与持仓方向相反

test_2 = abs(position %*% corr_mat[[1]]) - threshold_2
  
while(max(test_2) > 0){

test_2_adj = test_2*(test_2>0) * judgement

a_items_2 <- product_ids[test_2>0]  #哪些产品可以调整？

a_close_2 <- sta_contract_dt[product_name %in% a_items_2,][.N,]

pick_2  =  match(a_close_2$product_name,product_ids)


holding[pick_2] = holding[pick_2] + judgement[pick_2] * a_close_2$no_contract #改合约数

#记账
trade_id    = a_close_2[, trade_id]
enter_date  = a_close_2[, enter_date]
enter_price = a_close_2[, enter_price]
leave_date  = cdt[ptr, date]
leave_price = cdt[[3 + (pick_2 - 1) * 15]][ptr] - slippage[pick_2]   #开盘就应该平掉了 
ori_direction = "test2"
commision   = leave_price * vm[pick_2] + enter_price * vm[pick_2]
profit      = -judgement[pick_2] * (leave_price - enter_price) * vm[pick_2] * a_close_2$no_contract 
closed_profit = closed_profit + profit
fee         = fee + leave_price * vm[pick_2] * a_close_2$no_contract * fee.rate[pick_2]

cash        = cash + judgement[pick_2] * leave_price * vm[pick_2] * a_close_2$no_contract - leave_price * vm[pick_2] * a_close_2$no_contract * fee.rate[pick_2]

trade_out_a2  <- data.table(
  trade_id   = trade_id,
  item       = product_ids[pick_2],
  enter_date = enter_date,
  enter_price = enter_price,
  leave_date = leave_date,
  leave_price = leave_price,
  long_short = ori_direction,
  commision = commision,
  profit    = profit,
  contracts = a_close_2[,no_contract]
)
trades <- list.append(trades, trade_out_a2)

standing_contract[[sta_contract_dt[product_name == product_ids[pick_2]][.N]$trade_id]] = NULL

sta_contract_dt <- list.stack(standing_contract, data.table = TRUE)

position = holding/units

test_2 = abs(position %*% corr_mat[[1]]) - threshold_2

} #end of test 2


#test_3

threshold_3 = 10     #风险控制，请记得改回10

position = holding/units

judgement = -1*(holding>=0) + 1 * (holding <0)   #这个可以用于判定计算应调整仓位时的正负方向,与持仓方向相反

test_3 = abs(position %*% corr_mat[[2]]) - threshold_3

while(max(test_3) > 0){
  
  test_3_adj = test_3*(test_3>0) * judgement
  
  a_items_3 <- product_ids[test_3>0]  #哪些产品可以调整？
  
  a_close_3 <- sta_contract_dt[product_name %in% a_items_3,][.N,]
  
  pick_3  =  match(a_close_3$product_name,product_ids)
  
  holding[pick_3] = holding[pick_3] + judgement[pick_3] * a_close_3$no_contract #改合约数
  
  #记账
  trade_id    = a_close_3[, trade_id]
  enter_date  = a_close_3[, enter_date]
  enter_price = a_close_3[, enter_price]
  leave_date  = cdt[ptr, date]
  leave_price = cdt[[3 + (pick_3 - 1) * 15]][ptr] - slippage[pick_3]   #开盘就应该平掉了 
  ori_direction = "test3"
  commision   = leave_price * vm[pick_3] + enter_price * vm[pick_3]
  profit      = -judgement[pick_3] * (leave_price - enter_price) * vm[pick_3] * a_close_3$no_contract 
  closed_profit = closed_profit + profit
  fee         = fee + leave_price * vm[pick_3] * a_close_3$no_contract * fee.rate[pick_3]
  
  cash        = cash + judgement[pick_3] * leave_price * vm[pick_3] * a_close_3$no_contract - leave_price * vm[pick_3] * a_close_3$no_contract * fee.rate[pick_3]
  
  trade_out_a3  <- data.table(
    trade_id   = trade_id,
    item       = product_ids[pick_3],
    enter_date = enter_date,
    enter_price = enter_price,
    leave_date = leave_date,
    leave_price = leave_price,
    long_short = ori_direction,
    commision = commision,
    profit    = profit,
    contracts = a_close_3[,no_contract]
  )
  trades <- list.append(trades, trade_out_a3)
  
  standing_contract[[sta_contract_dt[product_name == product_ids[pick_3]][.N]$trade_id]] = NULL
  
  sta_contract_dt <- list.stack(standing_contract, data.table = TRUE)
  
  position = holding/units
  
  test_3 = abs(position %*% corr_mat[[2]]) - threshold_3
  
} #end of test_3

#test 4 

threshold_4 = 12     #单向判断，请记得改回12

position = holding/units

judgement = -1*(holding>=0) + 1 * (holding <0)   #这个可以用于判定计算应调整仓位时的正负方向,与持仓方向相反

test_4 = abs(sum(position)) - threshold_4

while(max(test_4) > 0){

a_items_4 <- product_ids[(position * sum(position)) >0] 



a_close_4 <- sta_contract_dt[product_name %in% a_items_4,][.N,]

pick_4  =  match(a_close_4$product_name,product_ids)

holding[pick_4] = holding[pick_4] + judgement[pick_4] * a_close_4$no_contract #改合约数

#记账
trade_id    = a_close_4[, trade_id]
enter_date  = a_close_4[, enter_date]
enter_price = a_close_4[, enter_price]
leave_date  = cdt[ptr, date]
leave_price = cdt[[3 + (pick_4 - 1) * 15]][ptr] - slippage[pick_4]   #开盘就应该平掉了 
ori_direction = "test4"
commision   = leave_price * vm[pick_4] + enter_price * vm[pick_4]
profit      = -judgement[pick_4] * (leave_price - enter_price) * vm[pick_4] * a_close_4$no_contract 
closed_profit = closed_profit + profit
fee         = fee + leave_price * vm[pick_4] * a_close_4$no_contract * fee.rate[pick_4]

cash        = cash + judgement[pick_4] * leave_price * vm[pick_4] * a_close_4$no_contract - leave_price * vm[pick_4] * a_close_4$no_contract * fee.rate[pick_4]

trade_out_a4  <- data.table(
  trade_id   = trade_id,
  item       = product_ids[pick_4],
  enter_date = enter_date,
  enter_price = enter_price,
  leave_date = leave_date,
  leave_price = leave_price,
  long_short = ori_direction,
  commision = commision,
  profit    = profit,
  contracts = a_close_4[,no_contract]
)
trades <- list.append(trades, trade_out_a4)

standing_contract[[sta_contract_dt[product_name == product_ids[pick_4]][.N]$trade_id]] = NULL

sta_contract_dt <- list.stack(standing_contract, data.table = TRUE)

position = holding/units

test_4 = abs(sum(position)) - threshold_4
}#end of test 4
#####################Asset Monitor#############################################