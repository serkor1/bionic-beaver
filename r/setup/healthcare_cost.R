# script: Generate Cost Parameters
# objective: Hardcoded costs associated with health care services
# date: 2022-08-04
# author: Serkan Korkmaz



# Primary Sector; ####

primary_sector <- data.table(
  class = c(
    rep('Primær sektor',5)
    ),
  subclass = c(
    'Almen praksis',
    'Andet speciale',
    'Fysioterapi',
    'Psykiater',
    'Psykolog'
  ),
  price = c(
    140,
    162,
    304,
    830,
    596
  )
)


# psychiatric care; #####
psychiatric_care <- data.table(
  class = c(
    rep('Psykiatrien', 3)
  ),
  subclass = c(
    'Ambulant',
    'Indlæggelse',
    'Skadestue'
  ),
  price = c(
    1987,
    3970,
    1987
  )
)


# somatic care; #####
somatic_care <- data.table(
  class = c(
    rep('Somatikken', 3)
  ),
  subclass = c(
    'Ambulant',
    'Indlæggelse',
    'Skadestue'
  ),
  price = c(
    2891,
    12201,
    2288
  )
)



# Append; #####
healthcare_price <- rbind(
  primary_sector,
  psychiatric_care,
  somatic_care
)


if (developper_mode) {
  
  fwrite(
    healthcare_price,
    file = "sample/parameters/model1/healtcare_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
  
  fwrite(
    healthcare_price,
    file = "www/documentation/resources/healtcare_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
} else {
  
  fwrite(
    healthcare_price,
    file = "input/parameters/model1/healtcare_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
  
  fwrite(
    healthcare_price,
    file = "www/documentation/resources/healtcare_price.csv",
    sep = ";",
    row.names = FALSE
  )
  
}

