# This script contains
# the parameters for each input

outcomes <- list(
  Arbejdsmarked = c(
    "Skat af Indkomst" = "tax",
    "Midlertidig Indkomstoverførsel",
    "Førtidspension"
  ),
  `Sundhedsydelser - Primær Sektor` = c(
    "Almen Praksis"      = "Almen Praksis",
    "Psykolog"           = "Psykolog",
    "Psykiater"          = "Psykiater",
    "Fysioterapi"        = "Fysioterapi",
    "Andre Speciallæger" = "Anden Speciale"
  ),
  `Sundhedsydelser - Sekundær Sektor` = c(
    "Psykiatrisk Ambulant Forløb"    = "psy_Outpatient",
    "Psykiatrisk Skadestue"          = "psy_Emergency",
    "Psykiatrisk Sygehusindlæggelse" = "psy_Inpatient",
    "Somatisk Ambulant Behandling"   = "som_Outpatient",
    "Somatisk Skadestue"             = "som_Emergency",
    "Somatisk Sygehusindlæggelse"    = "som_Inpatient"
  )
  
  
) 



# 
# demographics <- list(
#   `Køn` = c(
#     "Mand",
#     "Kvinde"
#   ),
#   `Uddannelse` = c(
#     "Faglært",
#     "Ufaglært",
#     "Videregående Uddannelse"
#   ),
#   `Alder` = c(
#     "18-49 År" = "[18-49]",
#     "50-65 År" = "[50-65]",
#     "65+ År"   = "[65+]"
#   ),
#   `Arbejdsmarkedsstatus` = c(
#     "Aktiv på Arbejdsmarkedet",
#     "Inaktiv på Arbejdsmarkedet",
#     "Udenfor Arbejdsmarkedet"
#   )
# )


