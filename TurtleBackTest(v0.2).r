rm(list=ls())
##########################Packages#############################################
library(quantex) #数据下载
library(rlist)   #一些好用的列表操作比如list.stack
library(data.table) 
library(dplyr)   #在清洁数据的时候以及平仓模块需要用到 left_join 以及%>%
library(zoo)     #要使用rollapplyr来算各种滚动数据

###############################################################################

#####################Input & Lists ############################################

product_ids          <- c("rb", "cu", "al")          ##########################
start_date           <- 20160101                     ##########  DATA  ########  
end_date             <- 20161231                     ########  LOADING  ####### 
frequency            <- "day"                        ##########################


vm                   <- c(products$rb$volume_multiple,     #提取合约单位
                          products$cu$volume_multiple,     
                          products$al$volume_multiple)      
                                                            
account              <- 100000000                          #初始账户资金

acc_origin           <- account   #当account大于这个数的时候就不用调整shadow
                                                           
shadow_account       <- account   #判定Unit大小的账户资金，最大为初始资金

cash                 <- account   #初始现金
                                                            
slippage             <- c(2*products$rb$price_tick,        #滑点读取   
                          2*products$cu$price_tick,         
                          2*products$al$price_tick)         
                                                             
fee.rate             <- c(products$rb$cost_ratio,        #手续费读取  
                          products$cu$cost_ratio,         
                          products$al$cost_ratio)          

fee                  <- 0

closed_profit        <- 0                                #累积完成利润
                                                             
system.selection     <- 2  #choose sys1 or sys2            #采用哪个交易系统        
                                                             
position             <- rep(0,length(product_ids))   #表现持仓单位数（/unit）

holding              <- rep(0,length(product_ids))   #表现持仓合约数

corr_mat             <- list(                       #两个判定风险的相关矩阵
                        clscorr = matrix(c(1,1,0,1,1,0,0,0,1),3,3,
                                    dimnames = list(product_ids, product_ids)
                                          ),
                        
                        lslcorr = matrix(c(1,0,0,0,1,0,0,0,1),3,3,
                                     dimnames = list(product_ids, product_ids)
))

close_sys             <- NULL  #后面可以用这个控制使用什么平仓规则

data                  <- list()          #存储价格数据
trades                <- list()          #交易记录
standing_contract     <- list()          #持仓记录
asset_sheet           <- list()          #资产记录


#非常有用的bar和pre方程，节省很多工作量：

bar <- function(w){   # eg. bar("close") returns close price of this bar
  cdt[[w]][ptr]
}
pre <- function(w, n=1){   # eg. pre("close") returns the previous close price
  if(n <= 0){
    stop("pre(): window should be greater than 1")
  } else {
    cdt[[w]][ptr - 1 - abs(n-1)]
  }
}

#####################End of "Input & Lists"####################################

####################Data Cleaning##############################################

#下载数据进行初步处理↓
for (i in 1:length(product_ids)) {
#下载数据↓
  data[[i]] <- query_dominant_future_bars(product_id = product_ids[i], 
                                        trading_day = start_date ~ end_date, 
                                        type = frequency)

  if (nrow(data[[i]]) < 22)  {
    stop(paste(product_ids[[i]],"doesn't contain over 21 rows for calculation"))
  }
#简化数据↓  
  data[[i]] <- na.omit(data[[i]][, .(date = trading_day, 
                        code = instrument_id, open, high, low, close, volume)])
  #跳过了所有有NA的数据，下面合并的时候可能会重新出现NA值（因为用了left_join）
  
#变更产品名
  data[[i]][,code := product_ids[i]]

#计算N*DPP
  data[[i]][, TR := pmax(high-low,
                    high-shift(close,1,type = "lag"),
                    shift(close,1,type = "lag")-low)]   #使用pmax()算true range
  
  s <- mean(data[[i]][2:21,TR])
  v <- rep(NA,nrow(data[[i]]))
  k <- data[[i]][,TR]
  v[1:20] = NA
  v[21] = s
  
  for (j in 1:(nrow(data[[i]])-21)){
    v[21+j] = (v[21+j-1]*19+k[21+j])/20
  }
  
  data[[i]][, ATR := shift(v,1,type="lag")]
  #↑除了计算ATR之外，还需要lag一个单位以避免未来数据
  #(这样做后面可以用NA直接跳过不用交易的日期，否则开仓算法容易报错)
  
  data[[i]][, NxDPP := vm[1]*ATR]   #这个就是待除的N*DPP，本来想合并三个公式，
                                    #但那样的可读性会极其差，放弃了。 
  
#添加 10日及20日收盘最高最低线（一共有4条）

  data[[i]][, max10high := shift(rollapplyr(high, width = 10, FUN = max, 
                                            fill = NA ), 1, type = "lag")]
  data[[i]][, min10low := shift(rollapplyr(low, width = 10, FUN = min, 
                                           fill = NA ), 1, type = "lag")]
  data[[i]][, max20high := shift(rollapplyr(high, width = 20, FUN = max, 
                                            fill = NA ), 1, type = "lag")]
  data[[i]][, min20low := shift(rollapplyr(low, width = 20, FUN = min, 
                                           fill = NA ), 1, type = "lag")]


#添加55日收盘价最高最低线(两条)（system2）
  
  data[[i]][, max55high := shift(rollapplyr(high, width = 55, FUN = max, 
                                            fill = NA ), 1, type = "lag")]
  data[[i]][, min55low := shift(rollapplyr(low, width = 55, FUN = min, 
                                           fill = NA ), 1, type = "lag")]
  
  
    
}#end of product data downloading loop

names(data) = product_ids            #命名


###################测试：假设al最后一行无数据，最后是可以形成表格的############
#                                                                             #
# data$al <- data$al[1:(.N)-1,]   #删掉一个数据的尾巴                         #
# data$cu <- data$cu[-21,]        #删掉一个数据的中间                         #
###############################################################################

data_bind <- data %>%           #通过reduce的形式来合并表格，缺失值会变成NA
              Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .)

data_dt <- as.data.table(data_bind)  #这就是可以输出到下一个环节的大表了

cdt <- copy(data_dt)

####################End of Data Cleaning#######################################

#####################Main Loop#################################################

for (ptr in 1:nrow(cdt)){ #start of main loop
  
#bug 129
  
#跳过前面N行
  
 if(is.na(cdt[ptr,max55high])) next


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
  
  
  
  #建立4个测试向量
  
  test1 <- rep(NA,length(product_ids))
  test2 <- rep(NA,length(product_ids))
  test3 <- rep(NA,length(product_ids))
  test4 <- rep(NA,length(product_ids))
  
  
  #根据Unit判定position，如果position 全为0则直接跳过，否则进行下列运算。
  
  if(all(position == 0)) {
    
    test1
    test2
    test3
    test4
    
  } else {}
  
  #subset需要调整的product
  
  #forloop j in selected products, 每个做一次平仓

  
#####################End of Asset Monitor######################################

####################Open Position##############################################


  #system2
  
  #generate long/short signal
  highs  <- vector()
  lows <- vector()
  highs_55 <- vector()
  lows_55 <- vector()
  ATRs  <- vector()
  
  sig_long  <- vector()
  sig_short <- vector()
  
  
  for (j in 1:length(product_ids)){     #extract the high,55high, low,55low
    
    highs  <- append(highs,cdt[[4+(j-1)*15]][ptr]) #vector that have high prices
    highs_55 <- append(highs_55,cdt[[15+(j-1)*15]][ptr]) #vector that have the upper channel
    lows <- append(lows,cdt[[5+(j-1)*15]][ptr])
    lows_55 <- append(lows_55,cdt[[16+(j-1)*15]][ptr])
    ATRs  <- append(ATRs,cdt[[9+(j-1)*15]][ptr]) #help to determin strength of signal
  }  
  
  #Then we will need a vector to see if channels been broke
  
  sig_long <- highs > highs_55  #These tell whether we need to long/short
  sig_short <- lows < lows_55   
  
  #But we need to go one step further, how many units to long/short?
  #Turtle rules add units for every 0.5 * N (ATR), so we need to make
  #it clear about how many Ns are the prices exceed signals
  
  unit_long <- floor((sig_long * highs - sig_long * highs_55)/(0.5*ATRs))  #make sure you don't include any negative number
  
  unit_long <- unit_long  + sig_long#0.5N的情况下应该一共进2个unit，所以整体要根据sig加1或者加0
  
  unit_long[unit_long > 4] = 4 #大于2个N不加仓
  
  unit_short <- floor((sig_short * lows_55 - sig_short * lows)/(0.5*ATRs))
  
  unit_short <- unit_short + sig_short
  
  unit_short[unit_short > 4] = 4 #大于2个N不继续加空仓    
  
  
  
  enter_date = NA     #中转日期
  product_name = NA     #产品类型
  direction = NA      #中转合约方向
  enter_price = NA    #中转入场价
  cut_point = NA      #中转止损价
  no_contract = NA    #中转合约数量
  
  #1.生成交易单
  #long_plan <- sig_long * units  #The aggregate plan of how many *contracts(not tons)* should be add
  
  #建立测试仓位用的向量,相当于缓存
  
  #
  
  for (j in 1:length(product_ids)){
    
    if (unit_long[j] == 0) next  #节省运算时间,跳过没有买入计划的产品
    t_position = copy(position) #在单日开多单的情况下必须重复读取实际的position，因为
    #t_position会在k-loop里面累加，影响到其他产品的测试结果
    for(k in 1:unit_long[j]) {
      
      t_position[j] = t_position[j] + 1
      
      #test 1: any direction ,single holding should be less than 4
      if (any(abs(t_position) > 4)) {
        #test 2: any direction, combination of strong corr assets should be less than 6
      }else if (any(abs(t_position %*% corr_mat$clscorr) > 6)){
        #test 3: any direction, combination of losely corr assets should be less than 10  
      }else if (any(abs(t_position %*% corr_mat$lslcorr) > 10)){
        #test 4: any direction, total holding should be less than 12  
      }else if (abs(sum(t_position)) > 12){
      }else {
        
        position[j] <- t_position[j]     #update the actual position 
        
        holding[j] <- holding[j] + units[j]   #update holdings
        
        enter_date <- cdt[[1]][ptr]       
        direction <- 1L                 # 1L long, -1L short
        enter_price <- cdt[[15 + (j-1) * 15]][ptr] + slippage[j]  #subset the channel price + slippage
        fee <- fee + enter_price * units[j] * vm[j] * fee.rate[j]          #update total fee
        cut <- enter_price - 2 * cdt[[9+(j-1)*15]][ptr]          #lost cutting point, 2N
        trade_id <- paste("|",direction,"|",enter_date,cdt[[2 + (j-1) * 15]][ptr],"00",k,sep = "")
        
        contract <- data.table(trade_id = trade_id,
                         enter_date = enter_date,                    #saving contract information
                         product_name   = cdt[[2 + (j-1) * 15]][ptr],
                         direction = direction,
                         enter_price = enter_price,
                         cut_point = cut,
                         no_contract = units[j]
        )
        
        standing_contract = list.append(standing_contract, contract)  #adding contract to current holding
        
        cash <- cash - enter_price * units[j] * vm[j] - enter_price * units[j] * vm[j] * fee.rate[j]    #update cash
        
      }
      
      
    }#end of k looping for open tests
    
  }#开多仓loop
  
  enter_date = NA     #中转日期
  product_name = NA     #产品类型
  direction = NA      #中转合约方向
  enter_price = NA    #中转入场价
  cut_point = NA      #中转止损价
  no_contract = NA    #中转合约数量
  
  #1.生成交易单
  #long_plan <- sig_long * units  #The aggregate plan of how many *contracts(not tons)* should be add
  
  #建立测试仓位用的向量,相当于缓存
  
  #
  
  for (j in 1:length(product_ids)){
    
    if (unit_short[j] == 0) next  #节省运算时间,跳过没有买入计划的产品
    t_position = copy(position) #在单日开多单的情况下必须重复读取实际的position，因为
    #t_position会在k-loop里面累加，影响到其他产品的测试结果
    for(k in 1:unit_short[j]) {
      
      t_position[j] = t_position[j] - 1
      
      #test 1: any direction ,single holding should be less than 4
      if (any(abs(t_position) > 4)) {
        #test 2: any direction, combination of strong corr assets should be less than 6
      }else if (any(abs(t_position %*% corr_mat$clscorr) > 6)){
        #test 3: any direction, combination of losely corr assets should be less than 10  
      }else if (any(abs(t_position %*% corr_mat$lslcorr) > 10)){
        #test 4: any direction, total holding should be less than 12  
      }else if (abs(sum(t_position)) > 12){
      }else {
        
        position[j] <- t_position[j]     #update the actual position 
        
        holding[j] <- holding[j] - units[j]   #update holdings
        
        enter_date <- cdt[[1]][ptr]       
        direction <- -1L                 # 1L long, -1L short
        enter_price <- cdt[[16 + (j-1) * 15]][ptr] - slippage[j]  #subset the channel price - slippage
        fee <- fee + enter_price * units[j] * vm[j] * fee.rate[j]          #update total fee
        cut <- enter_price + 2 * cdt[[9+(j-1)*15]][ptr]          #lost cutting point, 2N
        trade_id <- paste("|",direction,"|",enter_date,cdt[[2 + (j-1) * 15]][ptr],"00",k,sep = "")
        
        contract <- data.table(trade_id = trade_id,
                         enter_date = enter_date,                    #saving contract information
                         product_name   = cdt[[2 + (j-1) * 15]][ptr],
                         direction = direction,
                         enter_price = enter_price,
                         cut_point = cut,
                         no_contract = units[j]     
        )
        
        standing_contract = list.append(standing_contract, contract)  #adding contract to current holding
        
        
        cash <- cash + enter_price * units[j] * vm[j] - enter_price * units[j] * vm[j] * fee.rate[j]   #update cash
        
      }
      
      
    }#end of k looping for open tests
    
  }#开空仓loop

####################################数据处理##################################  
names(standing_contract) = list.map(standing_contract, trade_id)     #update trade id as name

sta_contract_dt <-  list.stack(standing_contract, data.table = TRUE)   #use data.frame for easy tracking


####################End of Open Position#######################################

#####################Close Position############################################





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


#####################End of Close Position#####################################

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
    fee         = fee + leave_price * vm[product_match] * s_exit[j,no_contract] * fee.rate[product_match]
    
    cash        = cash - leave_price * vm[product_match] * s_exit[j,no_contract] - leave_price * vm[product_match] * s_exit[j,no_contract] * fee.rate[product_match]
    position[product_match] = position[product_match] +1 #adjust position
    holding[product_match] = holding[product_match] + s_exit[j,no_contract] #空仓应该反加回去
    
    
    trade_out  <- data.table(
      trade_id   = trade_id,
      enter_date = enter_date,
      enter_price = enter_price,
      leave_date = leave_date,
      leave_price = leave_price,
      long_short = ori_direction,
      commision = commision,
      profit    = profit,
      contracts = s_exit[j,no_contract]
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
    fee         = fee + leave_price * vm[product_match] * l_exit[j,no_contract] * fee.rate[product_match]
    
    cash        = cash + leave_price * vm[product_match] * l_exit[j,no_contract] - leave_price * vm[product_match] * l_exit[j,no_contract] * fee.rate[product_match]
    position[product_match] = position[product_match] - 1 #adjust position
    holding[product_match] = holding[product_match] - l_exit[j,no_contract] #空仓应该反加回去,多仓应该反减
    
    
    trade_out  <- data.table(
      trade_id   = trade_id,
      enter_date = enter_date,
      enter_price = enter_price,
      leave_date = leave_date,
      leave_price = leave_price,
      long_short = ori_direction,
      commision = commision,
      profit    = profit,
      contracts = l_exit[j,no_contract]
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




















#############################End of Exit Strategy##############################



#####################End of Exit Strategy######################################

#####################Asset Chart Update########################################
#####################End of Asset Chart Update#################################

  
}# end of main loop
######################End of Main Loop#########################################

trades_dt <- list.stack(trades, data.table = TRUE)
