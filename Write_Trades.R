####################Write_Trades function####################

WriteTrades <- function(trade_id, 
                        item, 
                        enter_date, 
                        enter_price, 
                        leave_date,
                        leave_price,
                        ori_direction,
                        commision,
                        profit,
                        contracts
)
{  
  
  trade_out  <- data.table(
    trade_id   = trade_id,
    item       = item,
    enter_date = enter_date,
    enter_price = enter_price,
    leave_date = leave_date,
    leave_price = leave_price,
    long_short = ori_direction,
    commision = commision,
    profit    = profit,
    contracts = contracts
  )
  
  trades <<- list.append(trades, trade_out_a)
  
}

