# script: Generate Transfer Costs
# objective: 
# date: 2022-08-04
# author: Serkan Korkmaz


# transfer cost; #####
transfer_price <- data.table(
  class = rep(
    'Overførsel',
    4
  ),
  subclass = c(
    'Fleksjob',
    'Førtidspension',
    'Midlertidig Overførselsindkomst',
    'Selvforsørgende'
  ),
  price = c(
    3913,
    3700,
    2989,
    9549
  )
)




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