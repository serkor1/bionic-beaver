# script: baselayer
# objective: create functions that generates baseplots
# for the data
# author: Serkan Korkmaz
# date: 2022-06-16


.layout_model1 <- function(plot_list) {
  map(plot_list,
      function(plot) {
        map(plot,
            function(plot) {
              # Generate Y axis Text; #####
              get_data <- plotly_data(plot)
              
              
              get_allocator <- str_to_sentence(str_remove(
                unique(get_data$allocator),
                "[:graph:]*[:blank:]*[:graph:]*_"
              ))
              
              yaxis_text <- fcase(
                inherits(get_data, c("primary_care")),
                "Gennemsnitlig besøg pr. person",
                inherits(get_data, c(
                  "psychiatric_care", "somatic_care"
                )),
                "Gennemsnitlig sengedage pr. person",
                inherits(get_data, c("transfers")),
                "Gennemsnitlig antal uger pr. person"
              )
              
              yaxis_text <- paste(yaxis_text, '\n',
                                  get_allocator)
              
              
              # Add Layout; #####
              plotly::layout(
                plot,
                
                yaxis = list(
                  showgrid = FALSE,
                  zeroline = TRUE,
                  title = yaxis_text
                ),
                
                xaxis = list(
                  showgrid = FALSE,
                  zeroline = FALSE,
                  range = list(-2, 5),
                  title = 'Tid'
                ),
                
                
                legend = list(title = "Gruppe",
                              orientation = 'h'),
                
                annotations = list(
                  text = "Intervention start",
                  yref = "paper",
                  x = 0,
                  y = 1.1,
                  showarrow = FALSE,
                  font = list(size = 14,
                              color = 'blue')
                ),
                
                shapes = list(
                  list(
                    type = "line",
                    line = list(color = 'blue',
                                dash  = "dot"),
                    y0 = 0,
                    y1 = 1,
                    yref = "paper",
                    # i.e. y as a proportion of visible region
                    x0 = 0,
                    x1 = 0,
                    layer = "above"
                  )
                )
                
              )
              
            })
        
        
        
      })
  
}





# Add layout;

plot_layout <- function(plot_list) {
  if (inherits(plot_list, 'model1')) {
    plot_list %>% .layout_model1()
  }
}
