#Jonas Schmitten 
#Introduction to Web Mining
#June 2021

# Packages ----------------------------------------------------------------

#packages
library(htmltab)
library(tibble)
library(httr)
library(ggplot2)
library(usdata)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(stargazer)
library(lubridate)
library(reshape2)
library(scales)




# Web Scraping  -----------------------------------------------------------

#retrieve senator data
url_senator = "https://www.quiverquant.com/sources/senatetrading"
Senator_Data = htmltab(doc = url_senator, which = '//*[@id="myTable"]')
head(Senator_Data)
#write.csv(Senator_Data, '/Users/jonasschmitten/Desktop/FS 2021/Introduction to Web Mining/Project/Senator_Data.csv', row.names = F )

#Convert to actual date format 
Senator_Data$Date = as.Date(Senator_Data$Date, "%m/%d/%y")

#retrieve house member data
url_house = "https://www.quiverquant.com/sources/housetrading"
House_Data = htmltab(doc = url_house, which = '//*[@id="myTable"]')
head(House_Data)
#write.csv(House_Data, '/Users/jonasschmitten/Desktop/FS 2021/Introduction to Web Mining/Project/House_Data.csv', row.names = F )

#retrieve list of all members of the house of representatives
url_house_members = 'https://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives'
house_members = htmltab(doc = url_house_members, which = '//*[@id="votingmembers"]')
house_members = house_members[,1:3]
row.names(house_members) = NULL

#retrieve list of all members of the senate 
url_senate_members = 'https://en.wikipedia.org/wiki/List_of_current_United_States_senators'
senate_members = htmltab(doc = url_senate_members, which = '//*[@id="senators"]')

#state abbreviations 
url_state_abbrv = 'https://abbreviations.yourdictionary.com/articles/state-abbrev.html'
state_abbrv = htmltab(doc = url_state_abbrv, which = '//*[@id="__layout"]/div/div/div[2]/div[1]/article/section[5]/div/table')





# Data Cleaning -----------------------------------------------------------

#Convert to actual date format 
House_Data$Date = as.Date(House_Data$Date, "%m/%d/%y")

#Order House Data according to Date
House_Data = House_Data[order(House_Data$Date),]
row.names(House_Data) = NULL

#Count number of purchases, sales, and exchanges
table(House_Data$`Purchase / Sale`)
#61 exchange, 4006 purchase, 4153 sale 

#Remove observations with exchange
House_Data = House_Data[!(House_Data$`Purchase / Sale`=="Exchange"),]

#Take highest amount and set it to numeric
#convert c("$1,001-$15,000", "$1,000,001-$5,000,000", "$50,001-$100,000", "$15,001-$50,000", "$100,001-250,000", "250,001-$1,000,000") to upper point 
for (i in 1:nrow(House_Data)){
  if (nchar(House_Data[i,"Amount"])>13){
    House_Data[i,"Amount"] = as.integer(gsub(",", "", substr(str_split(House_Data[i,"Amount"], "-",simplify = T)[2], 
                                                             2, nchar(str_split(House_Data[i,"Amount"], "-",simplify = T)[2]))))
  } }

#convert single value 
House_Data$Amount[House_Data$Amount == ">$5,000,000"] = as.integer(5000000)
House_Data$Amount[House_Data$Amount == ">$5,000,000"] = as.integer(5000000)

#take out decimal
House_Data$Amount = gsub(",", "", House_Data$Amount)

#Conver the remaining amounts
for (i in 1:nrow(House_Data)){
  if (substr(House_Data[i,"Amount"],1,1) == "$"){
    House_Data[i,"Amount"] = as.numeric(substr(House_Data[i,"Amount"], 2,nchar(House_Data[i,"Amount"])))
                             
  } }

#Change type of amount to numeric 
House_Data$Amount = sapply(House_Data$Amount, as.numeric)

#Create new state variable 
House_Data$State = substr(House_Data$Party, 1,2)

#Get state from abbreviation
House_Data$State = abbr2state(House_Data$State)
#Get full district name 
House_Data$District = paste(House_Data$State, substr(House_Data$Party,3,4))

#rename problematic stock names to be in line with Yahoo
House_Data$`Stock *`[House_Data$`Stock *`=='BRK.B'] = "BRK-B"

#Take 0 out of district names
House_Data$District = gsub("0", "", House_Data$District)

#getting rid of whitespace at the end of string
House_Data$District = str_trim(House_Data$District)
house_members$District = str_trim(house_members$District)

#replace states with only one representative 
house_members$District[which(grepl('at-large', house_members$District))]

house_members$District[which(grepl( "Alaska", house_members$District))] = 'Alaska'
house_members$District[which(grepl( "Delaware", house_members$District))] = 'Delaware'
house_members$District[which(grepl( "Montana", house_members$District))] = 'Montana'
house_members$District[which(grepl( "North Dakota", house_members$District))] = 'North Dakota'
house_members$District[which(grepl( "South Dakota", house_members$District))] = 'South Dakota'
house_members$District[which(grepl( "Vermont", house_members$District))] = 'Vermont'
house_members$District[which(grepl( "Wyoming", house_members$District))] = 'Wyoming'

#Merge Party from house_members into House_Data based on district 
House_Data$PoliticalParty = NA
row.names(House_Data) = NULL

for (j in 1:nrow(House_Data)){
  for (i in 1:nrow(house_members)){
    #data contains a non-ASCII space like "\u00A0"
    #try utf8::utf8_print(unique(house_members[3, "District"]), utf8 = FALSE)
    if (stringi::stri_trans_general(house_members[i, "District"], "latin-ascii") == House_Data[j,"District"]){
      House_Data[j, "PoliticalParty"] = house_members[i, "Party"] 
      #know which iteration at
      print(j)
      }}}





# Yahoo Finance Stock Prices ----------------------------------------------

#Get Yahoo Finance Prices 
get_stocks = function(company, from, to){
  tmp = tempfile()
  #change url depending on ticker input
  paste0('https://finance.yahoo.com/quote/', company, '/history?p=', company) %>% 
    httr::GET(handle = httr::handle('https://finance.yahoo.com/quote/')) %>%
    httr::content('text') -> raw_html
  
  strsplit(raw_html, "crumb\":\"")   %>%
    unlist                             %>%
    strsplit("\"")                     %>%
    lapply(`[`, 1)                     %>%
    unlist                             %>%
    table                              %>%
    sort                               %>%
    rev                                %>%
    names                              %>%
    `[`(2)                             %>% 
    paste0("https://query1.finance.yahoo.com/v7/finance/download/", company, 
           "?period1=",as.numeric(as.POSIXct((paste(from, "6:13:46 EST")))),"&period2=",
           as.numeric(as.POSIXct((paste(to, "23:13:46 EST")))),"&interval=1d&events=history&crumb=", .) %>%
           
    httr::GET(handle = httr::handle('https://finance.yahoo.com/quote/')) %>%
    httr::content("text", encoding = "UTF-8") %>%
    writeBin(tmp, useBytes = T)
  
  #avoid warnings 
  suppressWarnings(read.csv(tmp) %>% tibble::as_tibble())
}








# Trading Strategy --------------------------------------------------------

#return the 4 most BOUGHT stocks by month
#top_stocks = House_Data[House_Data$`Purchase / Sale` == 'Purchase',] %>% 
top_stocks = House_Data[House_Data$`Purchase / Sale` == 'Purchase' & House_Data$PoliticalParty == 'Republican',] %>%  
#top_stocks = House_Data[House_Data$`Purchase / Sale` == 'Purchase' & House_Data$PoliticalParty == 'Democratic',] %>%  
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month) %>%
  count(`Stock *`) %>%
  arrange(month, desc(n))%>%
  top_n(4) %>%
  #avoid ties with slice
  slice(1:4) %>%
  mutate(id = row_number())

top_stocks = add_column(top_stocks, Date = sort(rep(seq(from = as.Date('2020-01-1'), to=as.Date('2020-12-31'), by= 'month'),4)), .before =1)
top_stocks = top_stocks[,c(1,3:ncol(top_stocks))]
  
#arrange df by stocks
portfolio_stocks = top_stocks %>%
  group_by(Date, `Stock *`) %>%
  pivot_wider(id_cols = Date, names_from = id, values_from = `Stock *`, names_sep = "")
#portfolio_stocks = na.locf(portfolio_stocks)
portfolio_stocks = as.data.frame(portfolio_stocks)

#change DEACU to DKNG
portfolio_stocks[which(grepl('DEACU', portfolio_stocks)),2] = 'DKNG'

#change SNE to SONY
portfolio_stocks[5,3] = 'SONY'

#Create empty data frame to store prices
df = data.frame(matrix(ncol = 4, nrow = 0))
colnames(df) = c("symbol", "Date", "Open", "Close")

stock_price_list = vector("list", length = nrow(portfolio_stocks))

#pull stock data with function
for (row in 1:nrow(portfolio_stocks)) {
 for (col in 2:ncol(portfolio_stocks)){
   stock = get_stocks(portfolio_stocks[row,col], portfolio_stocks[row,'Date']+months(1)
                      , ceiling_date(portfolio_stocks[row,'Date']+months(1), 'month')-days(1) )[c('Date', 'Open', 'Close')]
   stock = add_column(stock, symbol = portfolio_stocks[row,col], .before =1)
   df = rbind(df,stock)
   
   #progress overview
   print(portfolio_stocks[row,col])
   
  stock_df = df %>%
  group_by(symbol) #%>%
  #filter(row_number() %in% c(1, n()))
 stock_price_list[[row]] = stock_df
 } 
}
rm(stock, df)

#add month column
stock_df = add_column(stock_df, Month = format(as.Date(stock_df$Date), "%Y-%m"), .after = 1)

#final data frame with opening and closing prices for stocks in each month
stock_df = stock_df %>% 
  group_by(symbol,Month) %>% 
  filter(row_number() %in% c(1, n()))

#create portfolio value column
stock_df$value = NA

#arrange df by date and ticker
stock_df = stock_df %>% 
  arrange(Date, symbol)

#write.csv(stock_df, '/Users/jonasschmitten/Desktop/FS 2021/Introduction to Web Mining/Project/stock_df.csv', row.names = F )

#starting capital of investment strategy
pf_value = 100000
diff_pf = 0

#avoid scientific notation
options(scipen=999)

#Loop to Calculate Portfolio Value iteratively
k = 0
#nrow(stock_df)
for (i in 1:nrow(stock_df)){
  k = k + 1 
  stock_df$value[i] = floor((1/(ncol(portfolio_stocks)-1) * pf_value)/stock_df$Open[i]) * stock_df$Open[i]
  if (k == 4) {
    diff_pf = pf_value - sum(stock_df$value[(i-3):i])
    #print(sum(stock_df$Value[(i-3):i]))
    #print(diff_pf)
  }
  if (k > 4 ) {
    stock_df$value[i] = floor((1/(ncol(portfolio_stocks)-1) * pf_value)/stock_df$Open[i-4]) * stock_df$Close[i]
    if (k == 8) {
      pf_value = sum(stock_df$value[(i-3):i]) + diff_pf
      #print(pf_value)
      k = 0 
    }}}



#calculate value of portfolio on a monthly basis
monthly_sum = stock_df %>%
  group_by(Date) %>%
  summarise(politician_portfolio = sum(value))

#Pull SPY as a benchmark against reddit portfolio
SPY = get_stocks('SPY', '2020-02-03', '2021-01-29')

SPY = SPY[SPY$Date %in% monthly_sum$Date,]
monthly_sum = head(monthly_sum, 315)
monthly_sum$SPY = SPY$Open*309.262409 #scale to value of reddit portfolio (100'000)
#monthly_sum = melt(monthly_sum, "Date")




# Data Visualisation ------------------------------------------------------

#Pie chart total distribution 
pie(c(length(house_members$Party[house_members$Party == 'Democratic'])/length(house_members$Party),length(house_members$Party[house_members$Party == 'Republican'])/length(house_members$Party))
    , labels = c('D', 'R'), col = c('blue', 'red'), init.angle=90)

#Pie chart trades
pie(c(length(House_Data$PoliticalParty[House_Data$PoliticalParty == 'Democratic'])/length(House_Data$PoliticalParty),length(House_Data$PoliticalParty
    [House_Data$PoliticalParty == 'Republican'])/length(House_Data$PoliticalParty)), labels = c('D', 'R'), col = c('blue', 'red'), init.angle=90)

#plot monthly value of reddit portfolio vs benchmark
#ggplot(monthly_sum) + geom_line(aes(x = as.Date(Date), y = value, color = variable)) + 
#  scale_y_continuous(labels = comma)

#adjust for rep, dem, total
ggplot() + 
  geom_line(data=monthly_sum, aes(x=as.Date(Date), y=SPY, color='SPY')) +
  geom_line(data=monthly_sum, aes(x=as.Date(Date), y=politician_portfolio, color='politician_portfolio')) + labs(color="Indices") +
  scale_color_manual(values = c(SPY="black",politician_portfolio="purple")) +
  xlab('') +
  ylab('Value')


# LaTeX Tables ------------------------------------------------------------

#4 most bought stocks per month table 
stargazer(portfolio_stocks, summary = F)















