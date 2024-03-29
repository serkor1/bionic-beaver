# script: Generate Transfer Costs
# objective: 
# date: 2022-08-04
# author: Serkan Korkmaz


# transfer cost; #####
transfer_price <- data.table(
  class = rep(
    'Indkomst',
    4
  ),
  subclass = c(
    'Fleksjob',
    'Førtidspension',
    'Midlertidig overførselsindkomst',
    'Selvforsørgende'
  ),
  price = c(
    3913,
    3700,
    2989,
    9549
  )
)









if (developper_mode) {
  
  
  fwrite(
    transfer_price,
    file = "sample/parameters/model1/transfer_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
  
  
  fwrite(
    transfer_price,
    file = "www/documentation/resources/transfer_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
} else {
  
  fwrite(
    transfer_price,
    file = "input/parameters/model1/transfer_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
  
  
  fwrite(
    transfer_price,
    file = "www/documentation/resources/transfer_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
}

