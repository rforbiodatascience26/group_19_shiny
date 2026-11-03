# Generate a virtual DNA sequence
gene_dna <- function(
    length,
    base_probs = c(0.25, 0.25, 0.25, 0.25)
) {
  
  # Check that length is one positive whole number
  if (
    length(length) != 1 ||
    !is.numeric(length) ||
    is.na(length) ||
    length <= 0 ||
    length %% 1 != 0
  ) {
    stop("The DNA length must be one positive whole number.")
  }
  
  # Check that the DNA length is divisible by 3
  if (length %% 3 != 0) {
    stop("The DNA length must be divisible by 3.")
  }
  
  # Check that exactly four probabilities were supplied
  if (length(base_probs) != 4) {
    stop("You must provide four probabilities: one for A, T, C, and G.")
  }
  
  # Check that all probabilities are numbers
  if (!is.numeric(base_probs) || any(is.na(base_probs))) {
    stop("All base probabilities must be numeric values.")
  }
  
  # Check that all probabilities are between 0 and 1
  if (any(base_probs < 0) || any(base_probs > 1)) {
    stop("Each base probability must be between 0 and 1.")
  }
  
  # Check that the probabilities sum to approximately 1
  if (abs(sum(base_probs) - 1) >= 1e-10) {
    stop("The probabilities for A, T, C, and G must sum to 1.")
  }
  
  # Randomly sample DNA bases
  dna_vector <- sample(
    x = c("A", "T", "C", "G"),
    size = length,
    replace = TRUE,
    prob = base_probs
  )
  
  # Join the bases into one character string
  dna_string <- paste0(
    dna_vector,
    collapse = ""
  )
  
  return(dna_string)
}


# Transcribe DNA into RNA
transcribe_dna <- function(dna) {
  
  # Return an empty result if no DNA was supplied
  if (is.null(dna) || dna == "") {
    return("")
  }
  
  # Standardize the sequence
  dna <- toupper(dna)
  dna <- gsub("[[:space:]]", "", dna)
  
  # Check for invalid DNA characters
  if (grepl("[^ATCG]", dna)) {
    stop("The DNA sequence may only contain A, T, C, and G.")
  }
  
  # Replace thymine with uracil
  rna <- gsub(
    pattern = "T",
    replacement = "U",
    x = dna
  )
  
  return(rna)
}


# Translate RNA codons into an amino-acid sequence
translate_rna <- function(rna) {
  
  # Return an empty result if no RNA was supplied
  if (is.null(rna) || rna == "") {
    return("")
  }
  
  # Standardize the sequence
  rna <- toupper(rna)
  rna <- gsub("[[:space:]]", "", rna)
  
  # Check for invalid RNA characters
  if (grepl("[^AUCG]", rna)) {
    stop("The RNA sequence may only contain A, U, C, and G.")
  }
  
  # Translation requires complete groups of three bases
  if (nchar(rna) %% 3 != 0) {
    stop("The RNA sequence length must be divisible by 3.")
  }
  
  # Find the starting position of every codon
  firsts <- seq(
    from = 1,
    to = nchar(rna),
    by = 3
  )
  
  # Find the ending position of every codon
  lasts <- seq(
    from = 3,
    to = nchar(rna),
    by = 3
  )
  
  # Divide the RNA string into codons
  codons <- substring(
    text = rna,
    first = firsts,
    last = lasts
  )
  
  # RNA codon table
  codon_table <- c(
    "UUU" = "F", "UCU" = "S", "UAU" = "Y", "UGU" = "C",
    "UUC" = "F", "UCC" = "S", "UAC" = "Y", "UGC" = "C",
    "UUA" = "L", "UCA" = "S", "UAA" = "_", "UGA" = "_",
    "UUG" = "L", "UCG" = "S", "UAG" = "_", "UGG" = "W",
    
    "CUU" = "L", "CCU" = "P", "CAU" = "H", "CGU" = "R",
    "CUC" = "L", "CCC" = "P", "CAC" = "H", "CGC" = "R",
    "CUA" = "L", "CCA" = "P", "CAA" = "Q", "CGA" = "R",
    "CUG" = "L", "CCG" = "P", "CAG" = "Q", "CGG" = "R",
    
    "AUU" = "I", "ACU" = "T", "AAU" = "N", "AGU" = "S",
    "AUC" = "I", "ACC" = "T", "AAC" = "N", "AGC" = "S",
    "AUA" = "I", "ACA" = "T", "AAA" = "K", "AGA" = "R",
    "AUG" = "M", "ACG" = "T", "AAG" = "K", "AGG" = "R",
    
    "GUU" = "V", "GCU" = "A", "GAU" = "D", "GGU" = "G",
    "GUC" = "V", "GCC" = "A", "GAC" = "D", "GGC" = "G",
    "GUA" = "V", "GCA" = "A", "GAA" = "E", "GGA" = "G",
    "GUG" = "V", "GCG" = "A", "GAG" = "E", "GGG" = "G"
  )
  
  # Look up each codon and join the amino acids
  protein <- paste0(
    codon_table[codons],
    collapse = ""
  )
  
  return(protein)
}


# Count DNA bases
base_freqs <- function(dna) {
  
  # Return zero counts when no DNA is supplied
  if (is.null(dna) || dna == "") {
    return(
      data.frame(
        Base = factor(
          c("A", "C", "G", "T"),
          levels = c("A", "C", "G", "T")
        ),
        Frequency = c(0, 0, 0, 0)
      )
    )
  }
  
  # Standardize the sequence
  dna <- toupper(dna)
  dna <- gsub("[[:space:]]", "", dna)
  
  # Split the DNA sequence into individual bases
  dna_vector <- strsplit(
    x = dna,
    split = ""
  )[[1]]
  
  # Count every base, including bases with a zero count
  base_counts <- table(
    factor(
      dna_vector,
      levels = c("A", "C", "G", "T")
    )
  )
  
  data.frame(
    Base = names(base_counts),
    Frequency = as.numeric(base_counts)
  )
}