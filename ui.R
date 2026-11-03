# Load packages required by the User Interface
library("shiny")
library("bslib")

# Define the User Interface
ui <- page_fluid(
  layout_columns(
    col_widths = 12,
    
    card(
      titlePanel(
        title = "Virtual Central Dogma"
      ),
      
      helpText(
        paste(
          "Generate a DNA sequence and choose whether to display",
          "the DNA, transcribed RNA, or translated protein."
        )
      ),
      
      style = paste(
        "background-color: #f0f0f0;",
        "padding: 15px;"
      )
    )
  ),
  
  layout_columns(
    col_widths = 12,
    
    card(
      card_header(
        "Virtual Gene Generator"
      ),
      
      sliderInput(
        inputId = "n_bases",
        label = "Number of bases:",
        min = 3,
        max = 60,
        value = 30,
        step = 3,
        width = "100%"
      ),
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        
        numericInput(
          inputId = "prob_A",
          label = "Probability of A",
          value = 0.25,
          min = 0,
          max = 1,
          step = 0.05
        ),
        
        numericInput(
          inputId = "prob_T",
          label = "Probability of T",
          value = 0.25,
          min = 0,
          max = 1,
          step = 0.05
        ),
        
        numericInput(
          inputId = "prob_C",
          label = "Probability of C",
          value = 0.25,
          min = 0,
          max = 1,
          step = 0.05
        ),
        
        numericInput(
          inputId = "prob_G",
          label = "Probability of G",
          value = 0.25,
          min = 0,
          max = 1,
          step = 0.05
        )
      ),
      
      helpText(
        "The four base probabilities must sum to 1."
      ),
      
      actionButton(
        inputId = "generate_sequence",
        label = "Generate sequence"
      )
    )
  ),
  
  layout_columns(
    col_widths = 12,
    
    card(
      card_header(
        "Select an output"
      ),
      
      radioButtons(
        inputId = "selected_output",
        label = "Choose which sequence to display:",
        choices = c(
          "DNA" = "dna",
          "RNA" = "rna",
          "Protein" = "protein"
        ),
        selected = "dna",
        inline = TRUE
      ),
      
      strong(
        textOutput(
          outputId = "output_heading"
        )
      ),
      
      verbatimTextOutput(
        outputId = "selected_sequence"
      )
    )
  )
)