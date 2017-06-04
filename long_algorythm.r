
#考虑过整体设限的风险控制方案，比如说将现有持仓的向量乘以相关性矩阵，然后就可以判断整个持仓有没有超过风险限制，
#但是这么做有一个问题，那就是判断出持仓风险之后无法有效地设定反向建仓的规则，同时，这种办法无法有效地识别“
#同向开仓不能超过12个Unit“的问题。

#一个新的思路是当判定出爆仓之后，从holding的记录上面逐条删除重新判定，也就是说一共有两套方程：
#1. 加仓的时候用循环判定加仓是在范围内的，一旦超出范围，撤销上一个加仓并且转向下一个（暂时不考虑优先加信号强的仓）。
#2. 每回合开始的资金控制，一旦判定不及格就以倒叙的形式减仓，那就是说“standing_contract”的顺序往回减并计入损失。


#建仓判定：

#1.生成交易单
long_plan <- sig_long * units  #the aggregate plan of how many *contracts(not tons)* should be add

t_position = copy(position)#建立测试仓位用的向量

for (j in 1:length(product_ids)){

  if (long_plan[j] == 0) next  #节省运算时间
  
     t_position[j] = (t_position[j] + long_plan[j])/units[j]
  
  #test 1: any direction ,single holding should be less than 4
  if (any(abs(t_position) > 4)) {
  #test 2: any direction, combination of strong corr assets should be less than 6
  }else if (any(abs(t_position %*% corr_mat$clscorr) > 6)){
  #test 3: any direction, combination of losely corr assets should be less than 10  
  }else if (any(abs(t_position %*% corr_mat$lslcorr) > 10)){
  #test 4: any direction, total holding should be less than 12  
  }else if (abs(sum(t_position)) > 12){
    
  }else {
    
    position[j] <- t_position[j]
    
    holding[j] <- holding[j] + long_plan[j]
    
    date <- bar[date]
    direction <- 1L
    price <- bar[max55high] + slippage
    fee <- ?
    cut <- price - 2 * cdt[[9+(j-1)*15]]
    
  }

  }#开仓loop

