
#考虑过整体设限的风险控制方案，比如说将现有持仓的向量乘以相关性矩阵，然后就可以判断整个持仓有没有超过风险限制，
#但是这么做有一个问题，那就是判断出持仓风险之后无法有效地设定反向建仓的规则，同时，这种办法无法有效地识别“
#同向开仓不能超过12个Unit“的问题。

#一个新的思路是当判定出爆仓之后，从holding的记录上面逐条删除重新判定，也就是说一共有两套方程：
#1. 加仓的时候用循环判定加仓是在范围内的，一旦超出范围，撤销上一个加仓并且转向下一个（暂时不考虑优先加信号强的仓）。
#2. 每回合开始的资金控制，一旦判定不及格就以倒叙的形式减仓，那就是说“standing_contract”的顺序往回减并计入损失。


library(rlist) #list.append,list.stack用到了
#建仓判定：

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
        
        holding[j] <- position[j] * units[j]   #update holdings
        
        enter_date <- cdt[[1]][56]       
        direction <- 1L                 # 1L long, -1L short
        enter_price <- cdt[[15 + (j-1) * 15]][56] + slippage[j]  #subset the channel price + slippage
        fee <- fee + enter_price * vm[j] * fee.rate[j]          #update total fee
        cut <- enter_price - 2 * cdt[[9+(j-1)*15]][56]          #lost cutting point, 2N
        
        contract <- list(enter_date = enter_date,                    #saving contract information
                         product_name   = cdt[[2 + (j-1) * 15]][56],
                         direction = direction,
                         enter_price = enter_price,
                         cut_point = cut,
                         no_contract = long_plan[j]
        )
        
        standing_contract = list.append(standing_contract,contract)  #adding contract to current holding
        
        cash <- cash - enter_price - fee                         #update cash
        
    }
    
    
}#end of k looping for open tests
  
}#开仓loop

sta_contract_dt <-  list.stack(standing_contract, data.table = TRUE)   #use data.frame for easy tracking
