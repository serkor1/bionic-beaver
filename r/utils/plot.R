# Grouped Plot; #####

.plot_data <- function(
    plot,
    effect = rep(50,5),
    show_baseline = TRUE,
    color_intervention = "steelblue",
    color_control      = "orangee",
    color_background   = "white"
    ) {
  
  # Function Information; ####
  #' @param data data.table that has been grinded for
  #' for plotting.
  #' 
  #' @param effect vector of numeric values that serves as
  #' intervention effect
  #' 
  #' @return list of plots
  
  show_baseline <- fcase(
    isTRUE(show_baseline), TRUE,
    default = FALSE
  )
  
  data <- plot$data
  base_plot <- plot$plot
  
  
  # # TODO: This part should be migrated to 
  # # plot grinder!
  # 
  # # Prepare Data; ####
  # 
  # data[
  #   year > 0,
  #   effect := (effect/100),
  #   by = .(allocator)
  # ]
  # 
  # data[
  #   !is.na(effect),
  #   `:=`(
  #     cDifference   = difference * effect,
  #     # Was abs - but not used.
  #     cIntervention = max(Intervention - ((difference * effect)),0)
  #   ),
  #   by = c("allocator", "year")
  # ]
  # 
  # data[
  #   year == 0,
  #   `:=`(
  #     effect = 0,
  #     cIntervention = Intervention
  #   )
  #   
  # ]
  
  
  
  
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
        
        plot <- base_plot %>%
          .base_layout(data = data) %>% 
          # Add Intervention Group
          add_trace(
            data = data,
            x = ~year,
            y = ~intervention,
            line = list(
              color = color_intervention
            ),
            marker = list(
              color = color_intervention
            ),
            type   = "scatter",
            mode   = "lines+markers",
            showlegend = legend,
            name = "Valgt Gruppe"
          ) %>% add_trace(
            data = data,
            x = ~year,
            y = ~cintervention,
            line = list(
              color = color_intervention,
              dash = "dot"
            ),
            marker = list(
              color = color_intervention
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
            y = ~control, # Was COntrol
            line = list(
              color = color_control
            ),
            marker = list(
              color = color_control
            ),
            type   = "scatter",
            mode   = "lines+markers",
            showlegend = legend,
            name = "Sammenligningsgruppe"
          ) 
        
          if (show_baseline) {
            
            plot %>% # Add Control Group
              add_trace(
                data = data,
                x = ~year,
                y = ~population, # Was COntrol
                line = list(
                  color = color_background,
                  dash = "dot"
                ),
                marker = list(
                  color = color_background
                ),
                type   = "scatter",
                mode   = "lines+markers",
                showlegend = legend,
                name = "Befolkningen"
              )
          } else {
            
            plot
            
          }
        
        
        
        
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





.base_plot <- function(data = NULL, difference = NULL,color_background   = "white") {
  
  
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
      linecolor = color_background,
      title     = "Tid"
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      linecolor = color_background
    ),
    shapes = list(
      list(
        type = "line",
        line = list(
          color = color_background,
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
        color = color_background
      )
    ),
    legend = list(
      title = "Gruppe",
      orientation = 'h'
    ),
    font = list(
      size = 14,
      color = color_background
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


do_plot <- function(data, effect, difference = NULL, show_baseline = TRUE, color_intervention = "steelblue", color_background = "white", color_control = "orange") {
  
  
 
  
  plot_list <- .base_plot(
    data = data %>% setDT(),
    difference = difference,
    color_background = color_background
  )
  
  
  if (!is.null(data)) {
      
    plot_list <- .plot_data(
      plot = plot_list,
      effect = effect,
      show_baseline = show_baseline,
      color_intervention = color_intervention,
      color_control = color_control,
      color_background = color_background
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
