# dashboard-economique-ci/app.R
# Dashboard Économique Côte d'Ivoire - Portfolio INSSEDS

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Interface utilisateur
ui <- fluidPage(
  
  # CSS simple pour un look professionnel
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, sans-serif;
        background-color: #f8f9fa;
      }
      .well {
        background-color: white;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }
      h3 {
        color: #2c3e50;
        border-bottom: 3px solid #3498db;
        padding-bottom: 10px;
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #2980b9;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #2980b9;
      }
      table {
        width: 100%;
        border-collapse: collapse;
      }
      th {
        background-color: #3498db;
        color: white;
        padding: 10px;
      }
      td {
        padding: 10px;
        border-bottom: 1px solid #ddd;
      }
      tr:hover {
        background-color: #f5f5f5;
      }
    "))
  ),
  
  # Titre principal
  titlePanel(
    div(
      h3("📊 Dashboard Économique - Côte d'Ivoire"),
      h5("Portfolio professionnel | Consultant Data Science & ML | INSSEDS"),
      style = "text-align: center; margin-bottom: 30px;"
    )
  ),
  
  # Layout principal
  sidebarLayout(
    
    # Sidebar avec contrôles
    sidebarPanel(
      width = 4,
      div(
        class = "well",
        h4("🎯 Sélection des données"),
        
        # Sélecteur d'indicateur
        selectInput("indicateur",
                   "Choisissez l'indicateur économique :",
                   choices = c(
                     "Croissance du PIB (%)" = "pib",
                     "Taux d'Inflation (%)" = "inflation",
                     "Dette Publique (% PIB)" = "dette",
                     "Balance Commerciale (Milliards FCFA)" = "balance"
                   ),
                   selected = "pib"),
        
        hr(),
        
        # Période
        sliderInput("periode",
                   "Période d'analyse :",
                   min = 2015,
                   max = 2024,
                   value = c(2015, 2024),
                   sep = ""),
        
        hr(),
        
        # Boutons d'action
        h4("💼 Contact professionnel"),
        p("Pour un diagnostic data personnalisé :"),
        actionButton("btn_diagnostic", "Demander un diagnostic", 
                    class = "btn-primary", 
                    style = "width: 100%; margin-bottom: 10px;"),
        actionButton("btn_contact", "Voir mes coordonnées",
                    style = "width: 100%;")
      )
    ),
    
    # Panneau principal
    mainPanel(
      width = 8,
      
      # Graphique principal
      div(
        class = "well",
        plotlyOutput("graphique_principal", height = "400px")
      ),
      
      # Statistiques récapitulatives
      div(
        class = "well",
        h4("📈 Statistiques récapitulatives"),
        verbatimTextOutput("statistiques")
      ),
      
      # Tableau de données
      div(
        class = "well",
        h4("📋 Données économiques"),
        DTOutput("tableau_donnees")
      ),
      
      # Section services
      div(
        class = "well",
        h4("🚀 Mes services professionnels"),
        HTML('
        <table>
          <tr>
            <th>Service</th>
            <th>Description</th>
            <th>Durée</th>
            <th>Investissement</th>
          </tr>
          <tr>
            <td><strong>Diagnostic Data</strong></td>
            <td>Analyse complète + recommandations</td>
            <td>5 jours</td>
            <td>150 000 FCFA</td>
          </tr>
          <tr>
            <td><strong>Dashboard</strong></td>
            <td>Tableau de bord interactif</td>
            <td>2 semaines</td>
            <td>À partir de 300 000 FCFA</td>
          </tr>
          <tr>
            <td><strong>Modèle ML</strong></td>
            <td>Prédiction/Classification</td>
            <td>3-4 semaines</td>
            <td>À partir de 600 000 FCFA</td>
          </tr>
        </table>
        ')
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Données économiques (simulées pour démonstration)
  donnees <- reactive({
    data.frame(
      annee = 2015:2024,
      pib = c(8.8, 7.7, 7.4, 6.8, 6.2, 7.4, 7.0, 6.7, 6.5, 6.8),
      inflation = c(1.2, 0.7, 0.8, 0.7, 2.4, 2.5, 4.2, 5.3, 5.0, 4.5),
      dette = c(35, 37, 39, 42, 45, 47, 49, 51, 53, 55),
      balance = c(-500, -450, -400, -350, -300, -250, -200, -150, -100, -50)
    ) %>%
      filter(annee >= input$periode[1], annee <= input$periode[2])
  })
  
  # Graphique principal
  output$graphique_principal <- renderPlotly({
    data <- donnees()
    
    # Configuration selon l'indicateur
    config <- switch(input$indicateur,
                    "pib" = list(
                      titre = "Évolution de la croissance du PIB",
                      y_lab = "Croissance PIB (%)",
                      couleur = "#2ecc71"
                    ),
                    "inflation" = list(
                      titre = "Évolution du taux d'inflation",
                      y_lab = "Inflation (%)",
                      couleur = "#e74c3c"
                    ),
                    "dette" = list(
                      titre = "Évolution de la dette publique",
                      y_lab = "Dette publique (% PIB)",
                      couleur = "#f39c12"
                    ),
                    "balance" = list(
                      titre = "Évolution de la balance commerciale",
                      y_lab = "Balance (Milliards FCFA)",
                      couleur = "#3498db"
                    ))
    
    # Création du graphique
    p <- ggplot(data, aes(x = annee, y = .data[[input$indicateur]])) +
      geom_line(color = config$couleur, size = 1.5, alpha = 0.8) +
      geom_point(color = config$couleur, size = 3) +
      geom_area(fill = config$couleur, alpha = 0.2) +
      labs(title = config$titre,
           x = "Année",
           y = config$y_lab) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Segoe UI", size = 12)
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  # Statistiques récapitulatives
  output$statistiques <- renderPrint({
    data <- donnees()
    valeur <- data[[input$indicateur]]
    
    cat("=== Statistiques descriptives ===\n\n")
    cat("Période :", min(data$annee), "-", max(data$annee), "\n")
    cat("Nombre d'années :", nrow(data), "\n")
    cat("Moyenne :", round(mean(valeur), 2), "\n")
    cat("Médiane :", round(median(valeur), 2), "\n")
    cat("Minimum :", round(min(valeur), 2), "\n")
    cat("Maximum :", round(max(valeur), 2), "\n")
    cat("Écart-type :", round(sd(valeur), 2), "\n")
    cat("\n")
    cat("Dernière valeur (", max(data$annee), ") :", 
        round(valeur[which.max(data$annee)], 2))
  })
  
  # Tableau de données
  output$tableau_donnees <- renderDT({
    data <- donnees()
    datatable(
      data,
      options = list(
        pageLength = 5,
        dom = 'tip',
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(columns = 2:5, digits = 2)
  })
  
  # Bouton diagnostic
  observeEvent(input$btn_diagnostic, {
    showModal(modalDialog(
      title = h4(icon("chart-line"), " Diagnostic Data & Décision"),
      div(
        h5("Je propose un diagnostic complet en 5 jours comprenant :"),
        tags$ul(
          tags$li("🎯 Analyse approfondie de vos données"),
          tags$li("📊 Création d'un dashboard synthèse"),
          tags$li("💡 3 recommandations stratégiques prioritaires"),
          tags$li("📋 Plan d'action détaillé")
        ),
        hr(),
        h5("Investissement : 150 000 FCFA"),
        h5("Livraison : 5 jours ouvrés"),
        hr(),
        h5("Pour démarrer :"),
        p("Envoyez un email à : ", strong("ukouassi33@gmail.com")),
        p("Ou contactez-moi sur WhatsApp : ", 
          a("Cliquez ici", href="https://wa.me/2250701678337", target="_blank"))
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  # Bouton contact
  observeEvent(input$btn_contact, {
    showModal(modalDialog(
      title = h4(icon("address-card"), " Coordonnées professionnelles"),
      div(
        h5("📧 Email : ukouassi33@gmail.com"),
        h5("📱 WhatsApp : ", 
           a("+225 07 01 67 83 37", href="https://wa.me/2250701678337", target="_blank")),
        h5("💼 LinkedIn : En cours de création (bientôt disponible)"),
        h5("⏱️ Disponibilité : Réponse sous 24h"),
        hr(),
        h5("📍 Zone d'intervention :"),
        p("• Abidjan et région"),
        p("• Travail à distance possible"),
        hr(),
        h5("🕐 Heures de contact :"),
        p("Lundi - Vendredi : 9h - 18h"),
        p("Samedi : 10h - 14h")
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
