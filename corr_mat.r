#cal correlation of products
library(ggplot2)
library(quatex)

product_ids          <- c("cu", "al", "zn", "pb", "au", "rb", "ag", "ru", "c",  "m",  "a",  
                          "y",  "p",  "jd", "l",  "j",  "jm", "i",  "SR", "CF", "TA", "OI", 
                          "FG", "RM"
)  
start_date           <- 20140101                     
end_date             <- 20161230                     
frequency            <- "day"                       



#Data Download#################################################################



for (i in 1:length(product_ids)) {
  #下载数据↓
  data[[i]] <- query_dominant_future_bars(product_id = product_ids[i], 
                                          trading_day = start_date ~ end_date, 
                                          type = frequency)
  
  data[[i]] <- na.omit(data[[i]][, .(date = trading_day, code = product_ids[i], close)])
  
  data[[i]][,return := c(NA,diff(data[[i]][,close]))/data[[i]][,shift(close,1,type = "lag")]]  
}
#################################################################

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

corr_mat <- cor(return_table[,2:ncol(return_table)])

rownames(corr_mat) = colnames(corr_mat) = product_ids

image(x=seq(dim(corr_mat)[1]), y=seq(dim(corr_mat)[1]),axes=F , z=corr_mat, 
      col=rev(heat.colors(20)), ylim = c(nrow(corr_mat)+0.5,0.5),
      xlab="", ylab="")
text(expand.grid(x=seq(dim(corr_mat)[2]), y=seq(dim(corr_mat)[2])), labels=round(c(corr_mat),2))

axis(3, at=seq(nrow(corr_mat)), labels = rownames(corr_mat), tick = F,las=1, lwd = 1.25,padj = 1.5, cex.axis=0.8)
axis(2, at=seq(ncol(corr_mat)), labels = colnames(corr_mat), tick = F,las=1, lwd = 0.25)




####or we can calculate "Beta" between stocks.
cov_mat <- cov(return_table[,2:ncol(return_table)])
