# Load packages required by the Server
library("shiny")

# Load the biological functions
source(
  file = "app_functions.R",
  local = TRUE
)
# Define the Server
server <- function(input, output) {
  stored_dna <- reactiveVal(
    value = NULL
  )
  
  observeEvent(
    eventExpr = input$generate_sequence,
    handlerExpr = {
      base_probabilities <- c(
        input$prob_A,
        input$prob_T,
        input$prob_C,
        input$prob_G
      )
      
      validate(
        need(
          abs(sum(base_probabilities) - 1) < 1e-10,
          paste(
            "The probabilities for A, T, C, and G",
            "must sum to 1."
          )
        )
      )
      
      generated_dna <- gene_dna(
        length = input$n_bases,
        base_probs = base_probabilities
      )
      
      stored_dna(
        generated_dna
      )
    }
  )
  
  stored_rna <- reactive({
    req(
      stored_dna()
    )
    
    transcribe_dna(
      dna = stored_dna()
    )
  })
  
  stored_protein <- reactive({
    req(
      stored_rna()
    )
    
    translate_rna(
      rna = stored_rna()
    )
  })
  
  output$output_heading <- renderText({
    switch(
      EXPR = input$selected_output,
      dna = "Generated DNA sequence",
      rna = "Transcribed RNA sequence",
      protein = "Translated amino-acid sequence"
    )
  })
  
  output$selected_sequence <- renderText({
    validate(
      need(
        !is.null(stored_dna()),
        "Click \"Generate sequence\" to create a sequence."
      )
    )
    
    switch(
      EXPR = input$selected_output,
      dna = stored_dna(),
      rna = stored_rna(),
      protein = stored_protein()
    )
  })
}