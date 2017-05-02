# given an xts object will fill in data from open.at time to close.at time
fill_in <- function(x, open.at = "070000", close.at = "140000", freq = "sec"){
        t_date <- as.Date(index(x[1]))
        x <- as.xts(x, index(x))
        start <- timeChar(open.at)
        end <- timeChar(close.at)
        indexTZ(x) <- "America/Chicago"
        start_insert <- as.POSIXct(paste(as.Date(index(x)[1]),
                        paste(start$hr, start$min, start$sec, sep = ":")))
        end_insert <- as.POSIXct(paste(as.Date(index(x)[1]),
                        paste(end$hr, end$min, end$sec, sep = ":")))
        newts <- xts( ,seq.POSIXt(start_insert, end_insert, freq))


        out <- na.locf(cbind(newts, x, all=TRUE), maxgap = Inf)

        return(out)

}


### given a string of time, "HHMMNN" or "HMMNN" will return the hour, minute, and second component
timeChar <- function(timeString){
        sec <- substr(timeString, nchar(timeString) - 1, nchar(timeString))
        min <- substr(timeString, nchar(timeString) - 3, nchar(timeString) - 2)
        hr <- substr(timeString,1,nchar(timeString)-4)

        list(hr = hr, min = min, sec = sec)
}


#given a trades dataframe, will return a [1x10] dataframe of trade states for that period
get_stats <- function(trade_df){
        trade_date <- as.character(as.Date(trade_df$Entry.DateTime[1]))
        num_trades <- length(unique(trade_df$Entry.DateTime))
        winners <- length(unique(trade_df$Entry.DateTime[trade_df$Profitable]))
        pnl <- round(sum(trade_df$Profit.Loss) / num_trades, 2)
        runup <- round(sum(trade_df$Runup) / num_trades, 2)
        drawdown <- round(sum(trade_df$Drawdown) / num_trades, 2)
        win_rate <- round(100*winners / num_trades, 2)
        profitable <- pnl >= 0
        trade_stats <- data.frame(Date = trade_date, Winners = winners, Trades = num_trades,
                                  Profit.Loss = pnl, Runup = runup, Drawdown = drawdown,
                                  Win.Rate = win_rate, Total.PnL = pnl*num_trades, Profitable = profitable)
        return(trade_stats)
}
