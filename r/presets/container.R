vive_picker <- function(id, title = "title", header = "header", multiple = FALSE, choices = NULL, max = NULL, selected = NULL,...) {
  
  
  
  pickerInput(
    inputId = id,
    label = NULL,
    selected = selected,
    choices = choices,
    options = pickerOptions(
      title = title,
      actionsBox = TRUE,
      ...,
      selectedTextFormat = "count > 1",
      maxOptions = fifelse(is.null(max), yes = length(unlist(choices)), 1),
      # TODO: Add text
      size = 10,
      header = header
    ),
    multiple = multiple,
    width = "100%"
  )
  
}




vive_infobox <- function(effect, data) {
  
  
  message("Creating VIVE Aggregated Information Box")
  message("----------------------------------------\n")
  
  # Function Information; ######
  #'@param data data.table

  
  
  output <- bs4Card(
    solidHeader = FALSE,
    id = "test",
    gradient = TRUE,
    collapsible = TRUE,
    background = "primary",
    width = 12,
    headerBorder = FALSE,
    title = NULL,
    fluidRow(

      column(
        width = 3,
        descriptionBlock(
          number = paste(mean(effect), "%"),
          numberColor = "lime",
          numberIcon = icon("caret-up"),
          header = HTML("&nbsp"),
          text = "Årlig Forventet Effekt",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      ),

      column(
        width = 3,
        descriptionBlock(
          number = data$outcome,
          numberColor = data$outcome_status,#ns("infobox_outcome"),#data$outcome_status,
          numberIcon = icon(data$outcome_icon),
          header = data$outcome_label,
          text = "Faktisk Forskel",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      ),

      column(
        width = 3,
        descriptionBlock(
          number = data$counterfactual,
          numberColor = data$counter_status,
          numberIcon = icon(data$counter_icon),
          header = data$counter_label,
          text = "Kontrafaktisk Forskel",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      ),

      column(
        width = 3,
        descriptionBlock(
          number = paste(data$percent_change, "%"),
          numberColor = "lime",
          numberIcon = icon("caret-up"),
          header = "Ændring",
          text = "Effekt",
          rightBorder = FALSE,
          marginBottom = FALSE
        )
      )
    )


  )
  
  
  
  message("Done!")
  
  
  return(
    output
  )
  
  
}





bar <- function(data, session = getDefaultReactiveDomain()) {
  
  
  
  # outcome_label <- fcase(
  #   inherits(data, "primary_care"), "Besøg",
  #   inherits(data, c("psychiatric_care", "somatic_care")), "Sengedage",
  #   inherits(data, c("transfers")), "Uger"
  # )
  
  outcome_label ="placeholder"
  
  # This works
  # data <- data %>% foo()
  
  
  bs4Card(
    solidHeader = FALSE,
    id = "test",
    gradient = TRUE,
    collapsible = TRUE,
    background = "primary",
    width = 12,
    headerBorder = FALSE,
    title = NULL,
    fluidRow(
      
      # Effect
      column(
        width = 3,
        descriptionBlock(
          number = sum(data$outcome) %>% round(),
          numberColor = "lime",
          numberIcon = icon("cog"),
          header = HTML("&nbsp"),
          text = "Årlig Forventet Effekt",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      ),
      
      # Actual Difference
      column(
        width = 3,
        descriptionBlock(
          number = sum(data$Control) %>% round(),
          numberColor = "lime",
          numberIcon = icon("cog"),
          header = outcome_label,
          text = "Faktisk Forskel",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      ),
      
      column(
        width = 3,
        descriptionBlock(
          number = sum(data$Intervention) %>% round(),
          numberColor = "lime",
          numberIcon = icon("cog"),
          header = outcome_label,
          text = "Kontrafaktisk Forskel",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      ),
      
      column(
        width = 3,
        descriptionBlock(
          number = sum(data$Intervention) %>% round(),
          numberColor = "lime",
          numberIcon = icon("cog"),
          header = "Ændring",
          text = "Effekt",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      )
  )
  )
    
    
  
  
  
  
}







