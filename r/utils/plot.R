# Grouped Plot; #####

.plot_difference <- function(data, effect = rep(50,5)) {
  
  # Function Information; ####
  #' @param data data.table that has been grinded for
  #' for plotting.
  #' 
  #' @param effect vector of numeric values that serves as
  #' intervention effect
  #' 
  #' @return list of plots
  
  # Prepare Data; ####
  
  # Step 1) 
  # Cast to Wide Format
  data <- data %>% dcast(
    year + allocator ~ allocation,
    value.var = "outcome"
  )
  
  # Step 2)
  # Add Effects
  data[
    year > 0,
    effect := (1 - effect/100)
    ,
    by = .(allocator)
  ]
  
  
  # Step 2)
  # Calculate Differences
  data[
    ,
    `:=`(
      outcome = sum(
        -Control,
        Intervention
      ),
      counterfactual = sum(
        -Control,
        Intervention * effect
      )
    )
    
    ,
    by = 1:nrow(data)
  ]
  
  data[
    year == 0,
    counterfactual := outcome
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
        
        plot_ly(
          data   = data,
          x      = ~year,
          y      = ~outcome,
          name   = "Forskel",
          type = "scatter",
          mode = "lines+markers",
          showlegend = legend,
          line = list(
            color = "steelblue"
          ),
          marker = list(
            color = "steelblue"
          )
        ) %>% add_trace(
          y = ~counterfactual,
          name = "Interventionseffekt",
          colors = "steelblue",
          type = "scatter",
          mode = "lines+markers",
          line = list(
            dash = "dot",
            color = "steelblue"
          ),
          marker = list(
            color = "steelblue"
          )
          
        )  %>% layout(
          paper_bgcolor = '#ffffff00',
          plot_bgcolor='#ffffff00',
          annotations = list(
            text = unique(data$allocator),
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            x = 0.5,
            y = 1,
            yanchor = "bottom",
            showarrow = FALSE
          ),
          yaxis = list(
            title = "Forskellen",
            showgrid = FALSE,
            range = c(~min(outcome)*3, ~max(abs(outcome)*3))
          ),
          xaxis = list(
            title = NULL,
            showgrid = FALSE
          ),
          legend = list(
            title = list("Gruppe"),
            orientation = 'h'
          ),
          font = list(
            size = 14,
            color = "white"
          )
        ) %>% layout(
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
            ),
            
            # Add Rectange
            list(
              type = "rect",
              fillcolor = "green",
              line = list(color = "green"),
              opacity = 0.1,
              yref = "y",
              xref = "x",
              y0 = 0,
              y1 = -1000,
              x0 = -2,
              x1 = 5,
              layer = "below"
            ),
            
            list(
              type = "rect",
              fillcolor = "red",
              line = list(color = "red"),
              opacity = 0.1,
              yref = "y",
              xref = "x",
              y0 = 0,
              y1 = 1000,
              x0 = -2,
              x1 = 5,
              layer = "below"
            )
          ),
          annotations = list(
            text = "Intervention Start",
            yref = "paper",
            x = 0,
            y = 1.1,
            showarrow = FALSE
          )
        ) 
        
        
      }
    )
  
  
  return(plot_list)
  
}








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
            name = "Intervention"
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
            name = "Kontrafaktisk Intervention"
          ) %>% 
          
          # Add Control Group
          add_trace(
            data = data,
            x = ~year,
            y = ~Control,
            line = list(
              color = "orange"
            ),
            marker = list(
              color = "orange"
            ),
            type   = "scatter",
            mode   = "lines+markers",
            showlegend = legend,
            name = "Kontrol"
          ) %>% .base_layout(legend = legend, data = data) 
        
        
        
      }
    )
  
  
  return(plot_list)
  
  
}



.base_layout <- function(plot, legend, data) {
  
  
  # Set Title;
  yaxis_text <- fcase(
    inherits(data, c("primary_care")), "Gennemsnitlig Besøg pr. Person",
    inherits(data, c("psychiatric_care", "somatic_care")), "Gennemsnitlig Sengedage pr. Person",
    inherits(data, c("transfers")), "Gennemsnitlig Antal Uger pr. Person"
  )
  
  
  plot %>% layout(
    yaxis = list(
      showgrid = FALSE,
      title = paste(
        yaxis_text, "\n",
        str_remove(unique(data$allocator), "[:graph:]*[:blank:]*[:graph:]*_")
      )
    ),
    
    xaxis = list(
      showgrid = FALSE,
      title = "Tid"
    )
    
  )
  
  
  
}





.base_plot <- function(data,difference) {
  
  
  # Set Title;
  yaxis_text <- fcase(
    inherits(data, c("primary_care")), "Gennemsnitlig Besøg pr. Person",
    inherits(data, c("psychiatric_care", "somatic_care")), "Gennemsnitlig Sengedage pr. Person",
    inherits(data, c("transfers")), "Gennemsnitlig Antal Uger pr. Person"
  )
  
  # yaxis_text <- fifelse(
  #   isTRUE(difference), yes = paste(yaxis_text, "(Forskellen)"), yaxis_text
  # )
  
  
  # Generate Baseplot
  plot <- plot_ly(
    colors = "Paired",
    type   = "scatter",
    mode   = "lines+markers"
  ) %>% layout(
    paper_bgcolor = '#ffffff00',
    plot_bgcolor='#ffffff00',
    
    
    
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
      text = "Intervention Start",
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





do_plot <- function(data, effect, difference = FALSE) {
  
  
  
  plot_list <- .base_plot(
    data = data,
    difference = difference
  )
  
  
  if (nrow(data) != 0) {
    if (difference) {
      
      plot_list <- data %>%
        .plot_difference(effect = effect)
      
      
    } else {
      
      plot_list <- .plot_data(plot = plot_list,effect = effect)
      
    }
    
    
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