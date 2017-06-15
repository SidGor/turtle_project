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
