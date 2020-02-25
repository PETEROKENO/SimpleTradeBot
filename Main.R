# To separate entries in log files
print("################# Start Log Entry #################")

library(devtools)
install_github("DheerajAgarwal/rgdax", ref="dev")
library(mailR)
library(stringi)
library(curl)
library(xts)
library(TTR)

# API Keys
api_key <- "<<YOUR_API_KEY_HERE>>"
api_secret <- "<<YOUR_API_SECRET_HERE>>"
api_passphrase <- "<<YOUR_API_PASSPHRASE_HERE>>"

# Build Functions
curr_bal_usd <- function(x) {
  m <- rgdax::accounts(api.key = api_key, secret = api_secret, passphrase = api_passphrase)
  m <- subset(m$available, m$currency == 'USD')
  m
}

curr_bal_eth <- function(x) {
  n <- rgdax::accounts(api.key = api_key, secret = api_secret, passphrase = api_passphrase)
  n <- subset(n$available, n$currency == 'ETH')
  n
}

curr_rsi14_api <- function(x) {
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  rsi_gdax <- tail(TTR::RSI(df[,5],
                            n = 14),
                   n = 1)
  rsi_gdax
}
# v.2
rsi14_api_less_one <- function(x) {
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  rsi_gdax_less_one <- head(tail(TTR::RSI(df[,5],
                                          n = 14),
                                 n = 2), n = 1)
  rsi_gdax_less_one
}
rsi14_api_less_two <- function(x) {
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  rsi_gdax_less_two <- head(tail(TTR::RSI(df[,5],
                                          n = 14),
                                 n = 3), n = 1)
  rsi_gdax_less_two
}
rsi14_api_less_three <- function(x) {
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  rsi_gdax_less_three <- head(tail(TTR::RSI(df[,5],
                                          n = 14),
                                 n = 4), n = 1)
  rsi_gdax_less_three
}
rsi14_api_less_four <- function(x) {
  df <- rgdax::public_candles(product_id = "ETH-USD",
                              granularity = 900)
  rsi_gdax_less_four <- head(tail(TTR::RSI(df[,5],
                                          n = 14),
                                 n = 5), n = 1)
  rsi_gdax_less_four
}

bid <- function(x) {
  bid <- rgdax::public_orderbook(product_id = "ETH-USD", level = 1)
  bid <- bid$bids[1]
  bid
}
ask <- function(x) {
  ask <- rgdax::public_orderbook(product_id = "ETH-USD", level = 1)
  ask <- ask$asks[1]
  ask
}

usd_hold <- function(x) {
  rgdax::holds(currency = "USD", api.key = api_key, secret = api_secret, passphrase = api_passphrase)
}
eth_hold <- function(x) {
  holds <- rgdax::holds(currency = "ETH", api.key = api_key, secret = api_secret, passphrase = api_passphrase)
  holds
}
cancel_orders <- function(x) {
  cancel_orders <- rgdax::cancel_order(api.key = api_key, secret = api_secret, passphrase = api_passphrase)
  cancel_orders
}
buy_exe <- function(x) {
  # get order size in iterative manner
  order_size <- round(curr_bal_usd()/ask(),3)[1]-0.005
  # place initial order
  while(curr_bal_eth() == 0) {
    rgdax::add_order(product_id = "ETH-USD", api.key = api_key, secret = api_secret, passphrase = api_passphrase,
              type = "limit", price = bid(), side = "b", size = order_size)
    # sleep to see if order takes
    Sys.sleep(17)
    # check to see if ETH bal >= order amt
    if (curr_bal_eth() > 0){"buysuccess"}else{
      cancel_orders() # if curr_eth_bal not > 0, cancel order and start over
    }
  }
}
sell_exe <- function(x) {
  # place initial order
  while(curr_bal_eth() > 0) {
    rgdax::add_order(product_id = "ETH-USD", api.key = api_key, secret = api_secret, passphrase = api_passphrase,
              type = "limit", price = ask(), side = "s", size = curr_bal_eth())
    # sleep to see if order takes
    Sys.sleep(17)
    # check to see if ETH bal >= order amt
    if (curr_bal_eth() == 0){"buysuccess"}else{
      cancel_orders() # start over
    }
  }
}
# position <- (read.csv("/Users/slayter/RStudio/TradeBot/position.csv", header = TRUE))[1,2]

curr_rsi14_api <- curr_rsi14_api()
Sys.sleep(2)
rsi14_api_less_one <- rsi14_api_less_one()
Sys.sleep(2)
rsi14_api_less_two <- rsi14_api_less_two()
Sys.sleep(2)
rsi14_api_less_three <- rsi14_api_less_three()
Sys.sleep(2)
rsi14_api_less_four <- rsi14_api_less_four()
Sys.sleep(2)

order_price_tiered3 <- 99999
order_price_tiered5 <- 99999
order_price_tiered8 <- 99999

# Actual Trading Loop
if (curr_bal_usd() >= 20) { # if we have more than $20 start loop
  if (curr_rsi14_api >= 30 & # and current rsi >= 35
      rsi14_api_less_one <= 30 & # previous close RSI <= 35
      rsi14_api_less_two < 30 | rsi14_api_less_three < 30 | rsi14_api_less_four < 30) { # i-2, i-3, or i-4 RSI < 35
    
    # 1 Buy
    buy_exe()
    
    Sys.sleep(100)
    
    # 2 save buy price in csv
    position <- write.csv(bid(), file = "/Users/slayter/RStudio/TradeBot/position.csv")
    
    # 3 send email
    send.mail(from = "<<YOUR_EMAIL_HERE>>",
              to = c("<<YOUR_EMAIL_HERE>>"),
              subject = "GDAX ETH - Buy",
              body = paste("Your model says buy right now at price", bid()),
              smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "<<YOUR_EMAIL_HERE>>", passwd = "<<YOUR_PASSWORD_HERE>>", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    
    # 4 print for logs
    print("buy")
    Sys.sleep(3)
    
    # 5 enter tiered limit sell orders
    
    # Order 1: ake 1/3 profits at 1%
    order_size_tiered3 <- round(curr_bal_eth()/3,3)
    order_price_tiered3 <- round(bid() * 1.01,2)
    Sys.sleep(1)
    rgdax::add_order(product_id = "ETH-USD", api.key = api_key, secret = api_secret, passphrase = api_passphrase,
              type = "limit", price = order_price_tiered3, side = "s", size = order_size_tiered3)
    Sys.sleep(20)
    # Order 2: take 1/3 profits at 4% gain
    order_size_tiered5 <- round(curr_bal_eth()/2,3)
    order_price_tiered5 <- round(bid() * 1.04,2)
    Sys.sleep(1)
    rgdax::add_order(product_id = "ETH-USD", api.key = api_key, secret = api_secret, passphrase = api_passphrase,
              type = "limit", price = order_price_tiered5, side = "s", size = order_size_tiered5)
    Sys.sleep(20)
    # Order 3: take 1/3 profits at 7%
    order_size_tiered8 <- round(curr_bal_eth(),3)
    order_price_tiered8 <- round(bid() * 1.07,2)
    Sys.sleep(1)
    rgdax::add_order(product_id = "ETH-USD", api.key = api_key, secret = api_secret, passphrase = api_passphrase,
              type = "limit", price = order_price_tiered8, side = "s", size = order_size_tiered8)
  }else{"nobuy-badtime"}
}else{"nobuy-lowbal"}

Sys.time()

print("################# End Log Entry #################")
