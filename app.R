library(shiny)
library(tidyverse)
library(lubridate)
#  a faire noté secabilité avec coche pour sécable sinon comprimé plein
#  a faire utilisation des autres med par dosage décroissant
`%||%` <- function(a, b) if (!is.null(a)) a else b
ui <- fluidPage(
  titlePanel("Décroissance Médicamenteuse"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("modalite", "Modalité de décroissance :", 
                  choices = c("Date de fin", "Pourcent", "Milligramme","Comprimé"),
                  selected = "Milligramme"),
      # Affichage dynamique du champ modalité
      uiOutput("modalite_input"),
      
      dateInput("date_debut", "Date de début :", value = Sys.Date()),
      
      numericInput("pas_temps", "Pas de temps (en jours) :", value = 1, min = 1),
      # Bouton pour ajouter un médicament
      actionButton("add_medicament", "Ajouter un médicament"),
      uiOutput("med_inputs")
      
    ),
    
    mainPanel(
      h4("Résumé de la saisie"),
      verbatimTextOutput("summary"),
      plotOutput("calendarPlot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(n = 1, meds = list(), doses = list())
  
  # Sauvegarde les valeurs entrées à chaque mise à jour
  observe({
    for (i in 1:rv$n) {
      rv$meds[[i]] <- isolate(input[[paste0("medicament", i)]])
      rv$doses[[i]] <- isolate(input[[paste0("dose", i)]])
      rv$nb_CP[[i]] <- isolate(input[[paste0("nb_CP", i)]])
    }
  })
  # Ajout d'un nouveau médicament lorsque le bouton est cliqué
  observeEvent(input$add_medicament, {
    rv$n <- rv$n + 1
  })
  
  # Ajoute un nouveau champ si le dernier est rempli
  # observe({
  #   req(input[[paste0("medicament", rv$n)]],
  #       input[[paste0("dose", rv$n)]], 
  #       input[[paste0("nb_CP", rv$n)]])
  #   last_med <- input[[paste0("medicament", rv$n)]]
  #   last_dose <- input[[paste0("dose", rv$n)]]
  #   last_nb_CP <- input[[paste0("nb_CP", rv$n)]]
  #   if (!is.null(last_med) && last_med != "" &&
  #       !is.null(last_dose) && !is.na(last_dose) && last_dose != "" &&
  #       !is.null(last_nb_CP) && !is.na(last_nb_CP) && last_nb_CP != "") {
  #     rv$n <- rv$n + 1
  #   }
  # })
  
  output$med_inputs <- renderUI({
    inputs <- lapply(1:rv$n, function(i) {
      med_val <- if (length(rv$meds) >= i) rv$meds[[i]] else ""
      dose_val <- if (length(rv$doses) >= i) rv$doses[[i]] else NA
      nbcp_val <- if (length(rv$nbcp_val) >= i) rv$nbcp_val[[i]] else 1
      
      fluidRow(
        column(6, textInput(paste0("medicament", i), paste("Médicament", i),
                            value = med_val)),
        column(6, numericInput(paste0("dose", i), paste("Dosage", i),
                               value = dose_val)),
        column(6, numericInput(paste0("nb_CP", i), paste("Nombre de comprimés", i),
                               value = nbcp_val, min = 1))
      )
    })
    do.call(tagList, inputs)
  })
  
  
  output$modalite_input <- renderUI({
    if (input$modalite == "Date de fin") {
      date_fin_default <- Sys.Date() + 30  # Date de fin = 30 jours après la date du jour
      return(dateInput("date_fin", "Date de fin :", value = date_fin_default))
    } else if (input$modalite %in% c("Milligramme", "Comprimé")) {
      return(numericInput("dose_specifique", "Dose (en milligrammes/comprimé) :", value = 0, min = 1))
    } else if (input$modalite == "Pourcent") {
      return(numericInput("dose_specifique_pourcent", "Dose (en pourcentage) :", value = 0, min = 0, max = 100))
    }
  })
  
  
data_reactive <- reactive({  
  med <- input[[paste0("medicament", 1)]]
  dose <- input[[paste0("dose", 1)]]
  nb_CP <- input[[paste0("nb_CP", 1)]]
  dose_journaliere <- dose*nb_CP
  nb_paliers <- ceiling(dose_journaliere / input$dose_specifique)
  # (dose*nb_CP/input$dose_specifique)*input$pas_temps
  dates_palier <- input$date_debut + (0:(nb_paliers - 1)) * input$pas_temps
  
  df <- tibble(
    date = seq(input$date_debut, by = "1 day", length.out = input$pas_temps * nb_paliers)
  ) %>%
    mutate(
      palier = findInterval(date, dates_palier),
      dose = pmax(dose_journaliere - palier * input$dose_specifique, 0),
      jour =  format(date, "%a") |> stringr::str_to_lower(),
      semaine = isoweek(date),
      mois = stringr::str_to_title(format(date, "%b"))
    )
  # Reordonner les jours pour lecture gauche-droite lun -> dim
  df$jour <- factor(df$jour, 
                    levels = c("lun.", "mar.", "mer.", "jeu.", "ven.", "sam.", "dim."))
  df$mois <- factor(df$mois, 
                    levels = c("Janv.", "Févr.", "Mars", "Avr.", "Mai", "Juin", "Juil.", 
                                        "Août", "Sept.", "Oct.", "Nov.", "Déc."))
  df
})


output$calendarPlot <- renderPlot({
  
  ggplot(data_reactive(), aes(x = jour, y = semaine, fill = dose)) +
    geom_tile(color = "white", size = 0.4) +
    scale_fill_gradient(low = "#fff5f0", high = "#de2d26") +
    scale_y_continuous(breaks = seq(1, max(df$semaine), 1)) + 
    scale_y_reverse() +
    facet_wrap(~ mois, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 14) +
    labs(title = "Calendrier de décroissance posologique",
         x = "Jour de la semaine", y = "Semaine ISO", fill = "Dose (mg)") +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.text = element_text(size = 16, face = "bold"),
      legend.position = "right"
    )
})

  
  
  
  
  
  
  
  output$summary <- renderPrint({
    meds <- list()
    for (i in 1:(rv$n)) {
      med <- input[[paste0("medicament", i)]]
      dose <- input[[paste0("dose", i)]]
      nb_CP <- input[[paste0("nb_CP", i)]]
      if (!is.null(med) && med != "" && !is.null(dose) && !is.na(dose) &&
                !is.null(nb_CP) && !is.na(nb_CP) && nb_CP != "") {
        meds[[i]] <- paste(med, ":", dose*nb_CP,"\ndecroissance en ", (dose*nb_CP/input$dose_specifique)*input$pas_temps," j")
      }
    }
    
    cat("Modalité :", input$modalite, "\n")
    cat("Date de début :", as.character(input$date_debut), "\n")
    if (input$modalite == "Date de fin") {
      cat("Date de fin :", as.character(input$date_fin), "\n")
    } else {
      cat("Dose spécifiée :", input$dose_specifique, "\n")
    }
    cat("Médicaments :\n")
    cat(paste(unlist(meds), collapse = "\n"))
  })
}

shinyApp(ui = ui, server = server)