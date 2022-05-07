# Grouped Plot; #####

.plot_data <- function(plot, effect = rep(50,5)) {
  
  # Function Information; ####
  #' @param data data.table that has been grinded for
  #' for plotting.
  #' 
  #' @param effect vector of numeric values that serves as
  #' intervention effect
  #' 
  #' @return list of plots
  
  data <- plot$data
  base_plot <- plot$plot
  
  
  # TODO: This part should be migrated to 
  # plot grinder!
  
  # Prepare Data; ####
  
  data[
    year > 0,
    effect := (effect/100),
    by = .(allocator)
  ]
  
  data[
    !is.na(effect),
    `:=`(
      cDifference   = difference * effect,
      # Was abs - but not used.
      cIntervention = max(Intervention - ((difference * effect)),0)
    ),
    by = c("allocator", "year")
  ]
  
  data[
    year == 0,
    `:=`(
      effect = 0,
      cIntervention = Intervention
    )
    
  ]
  
  
  
  
  # Return Value; ####
  
  plot_counter <- 0
  
  
  plot_list <- data %>%
    split(.$allocator) %>% 
    map(
      .f = function(data) {
        
        # Set Legend; ####
        plot_counter <<- plot_counter + 1
        
        legend <- fifelse(
          plot_counter > 1,
          yes = FALSE,
          no = TRUE
        )
        
        base_plot %>%
          .base_layout(data = data) %>% 
          # Add Intervention Group
          add_trace(
            data = data,
            x = ~year,
            y = ~Intervention,
            line = list(
              color = "steelblue"
            ),
            marker = list(
              color = "steelblue"
            ),
            type   = "scatter",
            mode   = "lines+markers",
            showlegend = legend,
            name = "Valgt Gruppe"
          ) %>% add_trace(
            data = data[!is.na(effect)],
            x = ~year,
            y = ~cIntervention,
            line = list(
              color = "steelblue",
              dash = "dot"
            ),
            marker = list(
              color = "steelblue"
            ),
            type   = "scatter",
            mode   = "lines+markers",
            showlegend = legend,
            name = "Kontrafaktisk Værdi"
          ) %>%

          # Add Control Group
          add_trace(
            data = data,
            x = ~year,
            y = ~Control, # Was COntrol
            line = list(
              color = "orange"
            ),
            marker = list(
              color = "orange"
            ),
            type   = "scatter",
            mode   = "lines+markers",
            showlegend = legend,
            name = "Sammenligningsgruppe"
          )
        
        
        
      }
    )
  
  
  return(plot_list)
  
  
}



.base_layout <- function(plot, data) {
  
  
  # Set Title;
  yaxis_text <- fcase(
    inherits(data, c("primary_care")), "Gennemsnitlig besøg pr. person",
    inherits(data, c("psychiatric_care", "somatic_care")), "Gennemsnitlig sengedage pr. person",
    inherits(data, c("transfers")), "Gennemsnitlig antal uger pr. person"
  )
  
  
  plot %>% layout(
    yaxis = list(
      showgrid = FALSE,
      title = paste(
        yaxis_text, "\n",
        str_to_sentence(str_remove(unique(data$allocator), "[:graph:]*[:blank:]*[:graph:]*_"))
      )
    ),
    
    xaxis = list(
      showgrid = FALSE,
      
      
      title = "Tid"
    )
    
  )
  
  
  
}





.base_plot <- function(data = NULL, difference = NULL) {
  
  
  # Generate Baseplot
  plot <- plot_ly(
    colors = "Paired",
    type   = "scatter",
    mode   = "lines+markers"
  ) %>% layout(
    paper_bgcolor = '#ffffff00',
    plot_bgcolor='#ffffff00',
    
    # Set X-Axis Range
    xaxis = list(
      range = list(-2,5),
      zeroline = FALSE,
      showgrid  = FALSE,
      linecolor = "white",
      title     = "Tid"
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      linecolor = "white"
    ),
    
    
    
    shapes = list(
      list(
        type = "line",
        line = list(
          color = "white",
          dash  = "dot"
        ),
        y0 = 0, y1 = 1,
        yref = "paper", # i.e. y as a proportion of visible region
        x0 = 0, x1 = 0,
        layer = "above"
      )
    ),
    
    annotations = list(
      text = "Intervention start",
      yref = "paper",
      x = 0,
      y = 1.1,
      showarrow = FALSE,
      font = list(
        size = 14,
        color = "white"
      )
    ),
    legend = list(
      title = "Gruppe",
      orientation = 'h'
    ),
    font = list(
      size = 14,
      color = "white"
    )
  )
  
  
  data <- data
  
  
  return(
    list(
      data = data,
      plot = plot
    )
  )
  
  
  
  
}


do_plot <- function(data, effect, difference = NULL) {
  
  
 
  
  plot_list <- .base_plot(
    data = data %>% setDT(),
    difference = difference
  )
  
  
  if (!is.null(data)) {
      
    plot_list <- .plot_data(
      plot = plot_list,
      effect = effect
      )

    plot_list %>% subplot(
      titleX = TRUE,
      titleY = TRUE
    )  %>% config(displayModeBar = FALSE)
  } else {
    
    plot_list$plot %>% subplot(
      titleX = TRUE,
      titleY = TRUE
    ) %>% config(displayModeBar = FALSE)
    
  }
  
  
  
  
  
  
  
  
  
  
}
