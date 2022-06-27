# script: plot.r
# objective: Create Robust plotting for the 
# application
# author: Serkan Korkmaz
# date: 2022-06-12


.plot_bar <- function(plot) {
  
  
  #' function information
  #' 
  #' 
  #' @param plot a baseplot object. This includes
  #' a relevant data.table and an empty plot.
  
  # extract objects; ####
  data <- plot$data
  baseplot <- plot$plot
  
  
  
  baseplot %>% plot_ly(
    data = data,
    x = "Control",
    y = ~control,
    type = "bar"
  ) %>% add_trace(
    data = data,
    x = "Intervention",
    y = ~intervention
  )
  
  
  
  
  
  
}



# Grouped Plot; #####

.plot_data <- function(
    plot,
    effect = rep(50,5),
    show_baseline = TRUE,
    color_intervention = "steelblue",
    color_control      = "orange",
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
            x = ~x,
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
            x = ~x,
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
            x = ~x,
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
              x = ~x,
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
  
  #OBSOLETE
  
  if (inherits(data, "children")){
    
    plot <- plot_ly(
      colors = "Paired",
      type   = "bar"
    )
    
  } else {
    
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
    
    
  }
  
  
  
  
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
    data = data %>% 
      setDT(),
    difference = difference,
    color_background = color_background
  )
  
  
  if (!is.null(data)) {
    
    if (inherits(data, "children")) {
      
      
      plot_list <- .plot_bar(plot_list)
      
    } else {
      
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
      
    }
    
  } else {
    
    plot_list$plot %>% subplot(
      titleX = TRUE,
      titleY = TRUE
    ) %>% config(displayModeBar = FALSE)
    
  }
  
  
  plot_list$plot
  
  
  
  
  
  
  
  
  
}


plot_basedata <- function(
    plot,
    show_baseline = TRUE,
    color_intervention = "steelblue",
    color_control      = "orange",
    color_background   = "white") {
  
  
  #' function information
  #' 
  #' @param plot a list of plot and a data.table created by
  #' the baselayer
  #' 
  #' @param show_baseline logical. TRUE by default, and determines wether the
  #' population values should be plotted aswell.
  #' 
  #' @param color_* character of length one, that determines the colors
  #' of the plot elements
  
  
  
  
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
            x = ~x,
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
          )%>%
          
          # Add Control Group
          add_trace(
            data = data,
            x = ~x,
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
              x = ~x,
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


plot_baselayer <- function(data, color_background = "white") {
  
  #' function information
  #' 
  #' @param data.table data.table in wide format
  #' that has been prepared for plotting
  #' 
  #' @param color_background character of lenght one
  #' that determines the line color of the axes
  #' 
  #' @returns a list of length 2 with
  #' the plot and its data.
  
  
  
  if (inherits(data, "children")){
    
    plot <- plot_ly(
      colors = "Paired",
      type   = "bar"
    )
    
  } else {
    
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
    
    
  }
  
  
  
  
  data <- data
  
  
  return(
    list(
      data = data,
      plot = plot
    )
  )
  
  
}



plot_effect <- function(plot,
                        color_intervention = "steelblue",
                        color_control      = "orange",
                        color_background   = "white") {
  
  
  plot %>% map(
    .f = function(unique_plot) {
      
      unique_plot %>% add_trace(
        x = ~x,
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
        showlegend = TRUE,
        name = "Kontrafaktisk Værdi"
      ) 
      
    }
  )
  
  
}
