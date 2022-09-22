# script: Basetable
# objective: Generate CSS for tables
# date: 2022-08-22
# author: Serkan Korkmaz

.model1_table <- function(data_list, alternate) {
  
  
  
  iterator <- 0
  
  map(
    data_list,
    .f = function(element) {
      
      if (is.null(element)) {
        
        return(NULL)
        
      }
      
      
      yaxis_text <- fcase(
        inherits(element, c("primary_care")),
        fifelse(
          alternate,
          no = "Gennemsnitlig besøg pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        ),
        inherits(element, c(
          "psychiatric_care", "somatic_care"
        )),
        fifelse(
          alternate,
          no = "Gennemsnitlig sengedage pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        )
        ,
        inherits(element, c("transfers")),
        fifelse(
          alternate,
          no = "Gennemsnitlig antal uger pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        ),
        default = fifelse(
          alternate,
          no = "Gennemsnitlig antal recepter pr. person",
          yes ="Gennemsnitlig omkostninger pr. person"
        )
      )
      
      
      
      element <- DT::datatable(
        element,
        
        rownames = FALSE,
        extensions = c("Buttons", "RowGroup"),
        caption = paste('Tabel:', yaxis_text),
        fillContainer = TRUE,
        #filter = 'none',
        
        options = list(
          ordering = FALSE,
          scrollX = TRUE,
          pageLength = 8,
          #dom = "Bfrtip",
          #dom = 'Btp',
          dom = 'tp',
          columnDefs =list(
            list(
              className = "dt-head-center dt-center", targets = 1:(ncol(element) - 1)
            )
          ),
          rowGroup = list(dataSrc = c(1)),
          #buttons = c("csv", "excel"),
          fixedHeader = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#5E81AC', 'color': '#fff'});",
            "}"
          )
        )
        
        
      )
      
      
      
      
      element <-  formatPercentage(
        element,
        columns = c('Effekt')
      )
      
      formatStyle(
        element,
        columns = c(3),
        color = 'black',
        backgroundColor = 'black',
        background = '#e0e0e0'
      ) %>% formatStyle(columns = 0,target='row', color = 'black') %>% formatStyle(
        columns = c('Effekt'),
        background = styleColorBar(
          c(0,1),
          color = 'rgb(94, 129, 172, 0.4)'
        )
      ) 
      
    }
  )
  
  
}
