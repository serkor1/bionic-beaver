[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://vive.shinyapps.io/BISundhed/)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
![Maintainer](https://img.shields.io/badge/Maintainer-Serkan_Korkmaz-blue)


### Beregner til Investeringer i Sundhed

Modellen er udviklet af `VIVE Sundhed` og `VIVE Effekt`. Modellen er finansieret af Lægemiddelindustriforeningen.


#### Forslag til forbedringer

Alle forslag skal uploades til `development`-branch, og skrives på engelsk. Forslag der indebærer optimeringer til
databehandling som ikke er skrevet i `data.table` afvises automatisk.

#### Fejl og mangler

Hvis der er fejl, og mangler i modellen kan du oprette en ticket her. 


### Download repository

```
git clone https://github.com/serkor1/bionic-beaver.git
```

**Bemærk:** For at køre modellen på windows skal du have [rtools](https://cran.r-project.org/bin/windows/Rtools/) installeret først.

```R
install.packages('devtools')
install.packages('renv')
```

Når du har installeret `devtools` og `renv`, kan du installere alle biblioteker ved at,

```R
renv::restore()
```



