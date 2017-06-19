rm(list=ls())
##########################Packages#############################################
library(quantex) #数据下载
library(rlist)   #一些好用的列表操作比如list.stack
library(data.table) 
library(dplyr)   #在清洁数据的时候以及平仓模块需要用到 left_join 以及%>%
library(zoo)     #要使用rollapplyr来算各种滚动数据
library(ggplot2)
###############################################################################

#####################Input & Lists ############################################

product_ids          <- c("cu", "al", "zn", "pb", "au", "rb", "ag", "ru", "c",  "m",  "a",  
                          "y",  "p",  "jd", "l",  "j",  "jm", "i",  "SR", "CF", "TA", "OI", 
                          "FG", "RM"
)      


##########################
start_date           <- 20140101                     ##########  DATA  ########  
end_date             <- 20161230                     ########  LOADING  ####### 
frequency            <- "day"                        ##########################




#("cu","al","zn","pb","au","ag","rb","wr","hc","fu","bu",    可选用的产品
# "ru","c","m","a","c","y","p","fb","bb","jd","l","v","pp",
# "j","jm","i","WH","PM","SR","CF","TA","OI",
# "MA","FG","RM","TC")

# wr，PM,RS,JR,v,"WH","TC"的数据长度不足


#"cu" "al" "zn" "pb" "au" "rb" "ag" "ru" "c"  "m"  "a"  
#"y"  "p"  "jd" "l"  "j"  "jm" "i"  "SR" "CF" "TA" "OI" 
#"FG" "RM"
#   从14年1月1日到16年12月30日有完整数据的产品

#读取一手合约产品数量
vm                   <- as.vector(as.matrix(list.stack(products[product_ids], 
                                     data.table = TRUE)[,4]))    
                                                            
account              <- 100000000                          #初始账户资金

acc_origin           <- account   #当account大于这个数的时候就不用调整shadow
                                                           
shadow_account       <- account   #判定Unit大小的账户资金，最大为初始资金
                                  #当资产价值小于一定值时会进行调整。

cash                 <- account   #初始现金，后面会添加与先进有关功能

                                                            
slippage             <- as.vector(as.matrix(list.stack(products[product_ids], 
                                     data.table = TRUE)[,3]))  #读取滑点         
                                                             
fee.rate             <- as.vector(as.matrix((list.stack(products[product_ids], 
                                     data.table = TRUE)[,5]))) #浮动费率 

fee_rate_fix         <- as.vector(as.matrix((list.stack(products[product_ids], 
                                     data.table = TRUE)[,6]))) #固定费率 

last_a_close = rep(0,length(product_ids)) #在asset record环节记录上一期有效收盘价

fee                  <- 0                            #累计费率

closed_profit        <- 0                            #累积完成利润
                                                             
system.selection     <- 2  #choose sys1 or sys2      #采用哪个交易系统        
                                                             
position             <- rep(0,length(product_ids))   #持仓风险单位数（/unit）

holding              <- rep(0,length(product_ids))   #表现持仓合约数

corr_mat             <- list(                        #两个判定风险的相关系数矩阵
                        clscorr = diag(1,length(product_ids),length(product_ids)
                                          ),
                        
                        lslcorr = diag(1,length(product_ids),length(product_ids)
                        )
)

colnames(corr_mat$clscorr) = 
  rownames(corr_mat$clscorr) = 
  colnames(corr_mat$lslcorr) =
  rownames(corr_mat$lslcorr) = product_ids           #调整风险矩阵的名字

close_sys             <- NULL            #后面可以用这个控制使用什么平仓规则

data                  <- list()          #下载历史数据
trades                <- list()          #存储交易记录
standing_contract     <- list()          #存储持仓合约记录
asset_list            <- list()          #存储资产状态记录
position_list         <- list()          #存储风险持仓单位
holding_list          <- list()          #存储合约持仓单位

#####################End of "Input & Lists"####################################

####################Data Cleaning##############################################

#下载数据进行初步处理↓
for (i in 1:length(product_ids)) {
#下载数据↓
  data[[i]] <- query_dominant_future_bars(product_id = product_ids[i], 
                                        trading_day = start_date ~ end_date, 
                                        type = frequency)


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
  
  s <- mean(data[[i]][2:21,TR])         #ATR的第一个数是需要取TR平均值计算的
  v <- rep(NA,nrow(data[[i]]))
  k <- data[[i]][,TR]
  v[1:20] = NA
  v[21] = s
  
  for (j in 1:(nrow(data[[i]])-21)){
    v[21+j] = (v[21+j-1]*19+k[21+j])/20
  }
  
  data[[i]][, ATR := shift(v,1,type="lag")]
  #↑除了计算ATR之外，还lag了一个单位以避免未来数据
  #(这样做后面可以用NA直接跳过不用交易的日期，否则开仓算法容易报错)
  
  data[[i]][, NxDPP := vm[i]*ATR]   #这个就是待除的N*DPP，本来想合并三个公式，
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


#处于sys2判定55日通道突破的需要，这里要求数据至少要有56行否则会报错。
if (nrow(data[[i]]) < 56)  {
  stop(paste(product_ids[[i]],"doesn't contain over 56 rows for calculation"))
}else {  


names(data) = product_ids            #命名

data_bind <- data %>%           #通过reduce的形式来合并表格，缺失值会变成NA
              Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .)

data_dt <- as.data.table(data_bind)  #这就是可以输出到下一个环节的大表了

cdt <- copy(data_dt)

####################End of Data Cleaning#######################################

########################Correlation Matrix#####################################

close_list <- list()

for (i in 1:length(product_ids)) {
  
  close_list[[i]] <- data[[i]][,c(1,6)]
  names(close_list[i]) = product_ids[i]
  close_list[[i]][,DailyReturn := c(NA,diff(data[[i]][,close]))/data[[i]][,shift(close,1,type = "lag")]]
}

return_table <- close_list[[1]][,c(1,3)]

for (i in 2:length(product_ids)){
  
  return_table <- cbind(return_table, close_list[[i]][,3])
}

return_table <- return_table[2:.N,]

corr_mats <- cor(return_table[,2:ncol(return_table)])
rownames(corr_mats) = colnames(corr_mats) = product_ids

#corr_mat[[1]] <- (corr_mats > 0.6) * 1
#corr_mat[[2]] <- (corr_mats > 0.5) * 1

###############################################################################






#####################Main Loop#################################################

for (ptr in 1:nrow(cdt)){ #start of main loop #debug之后记得改回nrow(cdt) 


  
  if(is.na(cdt[ptr,max55high.x])) next    #跳过前面N行，一般以第一个产品为准


#####################Asset Monitor#############################################
  
  #判定是否要调整操盘资金量的大小  
  if (account < 0.9*shadow_account) {
    
    shadow_account = shadow_account*0.8
    
  } else if (account >(1/0.9)*shadow_account & shadow_account < acc_origin) {
            #当account的价值回涨，且小于起始数字的时候就会进行调整
            #这个功能尚未开放（目前没自动更新account）
    shadow_account = 1.25*shadow_account
    
  }
  
  
  
  #计算Unit，并且判定调整  
  
  NxDPPs <- rep(NA,length(product_ids))
  
  for (j in 1:length(product_ids)){
    
    NxDPPs[j] <- as.numeric(cdt[[ptr, 10 + (j-1)*15]])  #提取N*DPP以计算Unit限制 
    
  }
  
  units <- 0.01*shadow_account/NxDPPs             #turtle的风险暴露单位，
  units <- floor(units)                           #向下取整
#上面是整个模型都会用到的数据，轻易勿动 
###############################################################################  
  
  
  #test 1
  threshold_1 = 4     #同一个产品不能超过4个风险单位
  
  position = na.fill(holding/units,0) #如果units为NA的情况下，
                                      #就当做没有holding取0（反正也无法调仓）
  
  judgement = -1*(holding>=0) + 1 * (holding <0)   #用于判定计算应调整仓位时的
                                                   #正负方向,与持仓方向相反
  
  test_1 = abs(position) - threshold_1             #大于0则爆了风险
  
  while(max(na.exclude(test_1)) > 0){
    
    a_items_1 <- product_ids[(test_1 >0)]          #找出哪个产品的风险爆了）
    
    a_close_1 <- sta_contract_dt[product_name %in% a_items_1,][.N,] 
                                                #从现存合约里面将同类产品取出
    
    pick_1  =  match(a_close_1$product_name,product_ids) 
                                                #找出到底哪个产品需要反向平仓
    holding[pick_1] = holding[pick_1] + judgement[pick_1] * a_close_1$no_contract 
                                                         #改合约数
    
    
    #记账-更新trades里面的信息
    trade_id    = a_close_1[, trade_id]
    enter_date  = a_close_1[, enter_date]
    enter_price = a_close_1[, enter_price]
    leave_date  = cdt[ptr, date]
    leave_price = cdt[[3 + (pick_1 - 1) * 15]][ptr] - slippage[pick_1]#开仓价平仓 
    ori_direction = "test1"
    commision   = leave_price * vm[pick_1] + enter_price * vm[pick_1]
    profit      = -judgement[pick_1] * (leave_price - enter_price) * 
                  vm[pick_1] * a_close_1$no_contract 
    
    closed_profit = closed_profit + profit
    
    fee         = fee + leave_price * vm[pick_1] * a_close_1$no_contract * 
                  fee.rate[pick_1] + fee_rate_fix[pick_1]
    
    cash        = cash + judgement[pick_1] * leave_price * vm[pick_1] * 
                  a_close_1$no_contract - leave_price * vm[pick_1] *  
                  a_close_1$no_contract * fee.rate[pick_1] - fee_rate_fix[pick_1]
    
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
    trades <- list.append(trades, trade_out_a)   #为trades更新列
    
    standing_contract[[sta_contract_dt[product_name == 
                                  product_ids[pick_1]][.N]$trade_id]] = NULL
                                  #整列删除对应交易记录
    sta_contract_dt <- list.stack(standing_contract, data.table = TRUE)
                                  #重新更新持仓合约数的data.table
    position = na.fill(holding/units,0)
    
    test_1 = abs(position) - threshold_1
    
  } #stop of while max(test_1) > 0
  
  
  #test 2 - 与test 1基本相同
  
  threshold_2 = 6     #相关性风险控制，以unit为标准控制单位
  
  position = na.fill((holding/units),0)
  
  position[is.na(position)] = 0
  
  judgement = -1 * (holding>=0) + 1 * (holding <0)   
  
  test_2 = abs(position %*% corr_mat[[1]]) - threshold_2
  
  while(na.exclude(max(test_2)) > 0){
    
    test_2_adj = test_2 * (test_2 > 0) * judgement
    
    a_items_2 <- product_ids[as.logical(corr_mat[[1]][,test_2>0])] #哪些产品可以调整？  
    
    a_close_2 <- sta_contract_dt[product_name %in% a_items_2,][.N,]
    
    pick_2  =  match(a_close_2$product_name, product_ids)
    
    
    holding[pick_2] = holding[pick_2] + judgement[pick_2] * 
                      a_close_2$no_contract #改合约数
    
    #记账-与test1基本相同
    trade_id    = a_close_2[, trade_id]
    enter_date  = a_close_2[, enter_date]
    enter_price = a_close_2[, enter_price]
    leave_date  = cdt[ptr, date]
    leave_price = cdt[[3 + (pick_2 - 1) * 15]][ptr] - slippage[pick_2]    
    ori_direction = "test2"
    commision   = leave_price * vm[pick_2] + enter_price * vm[pick_2]
    profit      = -judgement[pick_2] * (leave_price - enter_price) * 
                  vm[pick_2] * a_close_2$no_contract 
    
    closed_profit = closed_profit + profit
    
    fee         = fee + leave_price * vm[pick_2] * a_close_2$no_contract * 
                  fee.rate[pick_2] + fee_rate_fix[pick_2]
    
    cash        = cash + judgement[pick_2] * leave_price * vm[pick_2] * 
                  a_close_2$no_contract - leave_price * vm[pick_2] * 
                  a_close_2$no_contract * 
                  fee.rate[pick_2] - fee_rate_fix[pick_2]
    
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
    
    standing_contract[[sta_contract_dt[product_name == 
                                  product_ids[pick_2]][.N]$trade_id]] = NULL
                                  #删除对应的交易
    
    sta_contract_dt <- list.stack(standing_contract, data.table = TRUE)
    
    position = na.fill(holding/units,0)
    
    test_2 = abs(position %*% corr_mat[[1]]) - threshold_2
    
  } #end of test 2
  
  
  #test_3
  
  threshold_3 = 10     #风险控制，请记得改回10
  
  position = na.fill(holding/units,0)
  
  position[is.na(position)] = 0
  
  judgement = -1*(holding>=0) + 1 * (holding <0)  
  
  test_3 = abs(position %*% corr_mat[[2]]) - threshold_3
  
  while(max(test_3) > 0){
    
    test_3_adj = test_3*(test_3>0) * judgement
    
    a_items_3 <- product_ids[as.logical(corr_mat[[2]][,test_3>0])]  #哪些产品可以调整？
    
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
    fee         = fee + leave_price * vm[pick_3] * a_close_3$no_contract * fee.rate[pick_3] + fee_rate_fix[pick_3]
    
    cash        = cash + judgement[pick_3] * leave_price * vm[pick_3] * a_close_3$no_contract - leave_price * vm[pick_3] * a_close_3$no_contract * fee.rate[pick_3] - fee_rate_fix[pick_3]
    
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
    
    position = na.fill(holding/units,0)
    
    test_3 = abs(position %*% corr_mat[[2]]) - threshold_3
    
  } #end of test_3
  
  #test 4 
  
  threshold_4 = 12     #单向判断，请记得改回12
  
  position_4 <- na.fill((holding/units),0)
  
  judgement = -1*(holding>=0) + 1 * (holding <0)   #这个可以用于判定计算应调整仓位时的正负方向,与持仓方向相反
  
  test_4 = abs(sum(na.omit(position_4))) - threshold_4
  
  while(max(test_4) > 0){
    
    a_items_4 <- product_ids[(na.fill(position,0) * sum(na.fill(position,0))) >0] 
    
    
    
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
    fee         = fee + leave_price * vm[pick_4] * a_close_4$no_contract * fee.rate[pick_4] + fee_rate_fix[pick_4]
    
    cash        = cash + judgement[pick_4] * leave_price * vm[pick_4] * a_close_4$no_contract - leave_price * vm[pick_4] * a_close_4$no_contract * fee.rate[pick_4] - fee_rate_fix[pick_4]
    
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
    
    position = na.fill(holding/units,0)
    
    test_4 = abs(sum(position)) - threshold_4
  }#end of test 4
  
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
    
    if (is.na(unit_long[j])){ next
    }else if (unit_long[j] == 0) {next }  #节省运算时间,跳过没有买入计划的产品
      t_position = copy(position) #在单日开多单的情况下必须重复读取实际的position，因为
      t_position[is.na(t_position)] = 0
      #t_position会在k-loop里面累加，影响到其他产品的测试结果
      for(k in 1:unit_long[j]) {
      
        t_position[j] = t_position[j] + 1
      
        #test 1: any direction ,single holding should be less than 4
        if (any(abs(na.exclude(t_position)) > 4)) {
        #test 2: any direction, combination of strong corr assets should be less than 6
        }else if (any(abs(as.vector(t_position) %*% corr_mat$clscorr) > 6)){
        #test 3: any direction, combination of losely corr assets should be less than 10  
        }else if (any(abs(c(t_position) %*% corr_mat$lslcorr) > 10)){
        #test 4: any direction, total holding should be less than 12  
        }else if (abs(sum(t_position)) > 12){
        }else {
        
        position[j] <- t_position[j]     #update the actual position 
        
        holding[j] <- holding[j] + units[j]   #update holdings
        
        enter_date <- cdt[[1]][ptr]       
        direction <- 1L                 # 1L long, -1L short
        enter_price <- max(cdt[[ptr,3 + (j - 1) * 15]], cdt[[ptr,15 + (j - 1) * 15]]) + slippage[j]  #subset the channel price + slippage
        fee <- fee + enter_price * units[j] * vm[j] * fee.rate[j] + fee_rate_fix[j]          #update total fee
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
        
        cash <- cash - enter_price * units[j] * vm[j] - enter_price * units[j] * vm[j] * fee.rate[j] - fee_rate_fix[j]    #update cash
        
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
    if (is.na(unit_short[j])){next}
    else if (unit_short[j] == 0) next  #节省运算时间,跳过没有买入计划的产品
    t_position = copy(position) #在单日开多单的情况下必须重复读取实际的position，因为
    t_position[is.na(t_position)] = 0
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
        enter_price <- min(cdt[[ptr , 3 + (j-1) * 15]], cdt[[ptr, 16 + (j-1) *15]]) - slippage[j]  #subset the channel price - slippage
        fee <- fee + enter_price * units[j] * vm[j] * fee.rate[j] + fee_rate_fix[j]          #update total fee
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
        
        
        cash <- cash + enter_price * units[j] * vm[j] - enter_price * units[j] * vm[j] * fee.rate[j] - fee_rate_fix[j]   #update cash
        
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
    leave_price = max(cdt[[ptr, 3 + (product_match - 1) * 15]],s_close[j,cut_point]) + slippage[product_match] 
    ori_direction = s_close[j,direction]
    commision   = leave_price * vm[product_match] + enter_price * vm[product_match]
    profit      = (enter_price - leave_price) * vm[product_match] * s_close[j,no_contract] 
    closed_profit = closed_profit + profit
    fee         = fee + leave_price * vm[product_match] * s_close[j,no_contract] * fee.rate[product_match] + fee_rate_fix[product_match]
    
    cash        = cash - leave_price * vm[product_match] * s_close[j,no_contract] - leave_price * vm[product_match] * s_close[j,no_contract] * fee.rate[product_match] - fee_rate_fix[product_match]
    position[product_match] = position[product_match] +1 #adjust position
    holding[product_match] = holding[product_match] + s_close[j,no_contract] #空仓应该反加回去
    
    
    trade_out  <- data.table(
      trade_id   = trade_id,
      item       = product_ids[product_match],
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
    leave_price = min(cdt[[ptr, 3 + (product_match - 1) * 15]], l_close[j, cut_point]) - slippage[product_match]   #go against the trade 
    ori_direction = l_close[j, direction]
    commision   = leave_price * vm[product_match] + enter_price * vm[product_match]
    profit      = (leave_price - enter_price) * vm[product_match] * l_close[j,no_contract] 
    closed_profit = closed_profit + profit
    fee         = fee + leave_price * vm[product_match] * l_close[j,no_contract] * fee.rate[product_match] + fee_rate_fix[product_match]
    
    cash        = cash + leave_price * vm[product_match] * l_close[j,no_contract] - leave_price * vm[product_match] * l_close[j,no_contract] * fee.rate[product_match] - fee_rate_fix[product_match]
    position[product_match] = position[product_match] - 1 #adjust position
    holding[product_match] = holding[product_match] - l_close[j,no_contract] #空仓应该反加回去,多仓应该反减
    
    
    trade_out  <- data.table(
      trade_id   = trade_id,
      item       = product_ids[product_match],
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
    leave_price = max(cdt[[ptr, 3 + (product_match - 1) * 15]], s_exit[j,highs20]) + slippage[product_match] 
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
    leave_price = min(cdt[[ptr, 3 + (product_match - 1) * 15]], l_exit[j, lows20]) - slippage[product_match]   #go against the trade 
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

  
}# end of main loop
######################End of Main Loop#########################################

trades_dt <- list.stack(trades, data.table = TRUE)

trades_dt[, net_profit := profit - commision]  

asset_dt  <- list.stack(asset_list, data.table = TRUE)

asset_dt[, net_profit := closed_profit + pos_profit - fee]
asset_dt[, daily_profit := c(0,diff(net_profit))] 
asset_dt[, port_value := account + net_profit]

hist_position <- list.stack(position_list, data.table = TRUE)
hist_holding  <- list.stack(holding_list, data.table = TRUE)

colnames(hist_position) = colnames(hist_holding) = c("date",product_ids)
###########################Performance##########################################


# 计算每日收益率：（不加杠杆，return = 每日利润/本金）
asset_dt[, return := daily_profit / (shadow_account)]
calc_annual_return <- function(x){ # 根据日度收益率计算年化收益率
  (1 + sum(x)) ^(250 / length(x)) - 1   #由于本金永远锁定在Account而非再投资，这里不能用prod而是用sum来简单加总
}
calc_sharpe_ratio <- function(x){ # 根据日度收益率计算 sharpe ratio
  calc_annual_return(x) / (sqrt(250) * sd(x))
}
# 年化收益率
annual_return <- calc_annual_return(asset_dt$return)
# 夏普比
sharpe_ratio <- calc_sharpe_ratio(asset_dt$return)
# 最大回撤
asset_dt[, cummax_value := cummax(na.omit(port_value))]
asset_dt[, drawdown := port_value - cummax_value]

maxdrawdown <- min(asset_dt$drawdown)
maxdrawdown_idx <- which.min(asset_dt$drawdown)
maxdrawdown_per <-
  asset_dt[maxdrawdown_idx, drawdown] /
  asset_dt[maxdrawdown_idx, cummax_value]
# 胜率
win_prob <- mean(trades_dt$profit > 0)
# 平均盈亏比
win_loss_ratio <-
  (sum(trades_dt[profit > 0, profit]) / trades_dt[profit > 0, .N]) /
  (sum(trades_dt[profit <= 0, -profit]) / trades_dt[profit <= 0, .N])
indicators <- sprintf("
                      年化收益率：%s
                      Sharpe_Ratio: %f
                      最大资金回撤：%f（百分比：%s）
                      胜率：%s
                      平均盈亏比：%f
                      ",
                      scales::percent(annual_return),
                      sharpe_ratio,
                      maxdrawdown, scales::percent(maxdrawdown_per),
                      scales::percent(win_prob),
                      win_loss_ratio
)
cat(indicators)


# 找出利润最大的一次交易
trades_dt[which.max(trades_dt$profit), .(enter_date, leave_date)]

# 资金曲线
plot(asset_dt$net_profit, type = "l")


hist(trades_dt$profit, breaks = 50)


par(mfrow = c(3, 1))
plot(asset_dt$net_profit, type = "l", main = "account") # 账户资金曲线
plot(asset_dt$return, type = "h", main = "daily return") # 日度收益率
plot(asset_dt$pos_dir, type = "h", main = "position") # 每日postion
par(mfrow = c(1,1))

} #end of if (nrow(data[[i]]) < 56)
