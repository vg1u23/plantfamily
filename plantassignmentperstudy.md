# 📦 Install required packages (only if not already installed)
install.packages("taxize")
install.packages("readxl")
install.packages("writexl")

# 📚 Load all packages
library(phyloseq)
library(ggplot2)
library(ALDEx2)
library(SummarizedExperiment)
library(Biobase)
library(devtools)
library(ComplexHeatmap)
library(BiocGenerics)
library(BiocManager)
library(metagenomeSeq)
library(Maaslin2)
library(edgeR)
library(lefser)
library(limma)
library(KEGGREST)
library(DESeq2)
library(dplyr)
library(taxize)
library(readxl)
library(writexl)

# 📂 Set working directory
setwd("C:/Users/vg1u23/OneDrive - University of Southampton/Desktop/PhD_Gschiel/Project/Project1_Bioinformatics/metadata_files/metadataperstudy")

# 📝 Define input file name
input_file <- "PRJNA1129450.xlsx"

# 📥 Read the metadata Excel file
metadata <- read_excel(input_file)

# 🧼 Clean species names (removes authors like "L.")
clean_species_name <- function(name) {
  sub("^([A-Za-z]+\\s+[a-z\\-]+).*", "\\1", name)
}

# 🌿 Function to get plant family using NCBI (with error handling)
get_family <- function(species_name) {
  species_name <- clean_species_name(species_name)
  
  classification_info <- tryCatch(
    classification(species_name, db = "ncbi"), 
    error = function(e) return(NA)  # Return NA if classification fails
  )
  
  if (!is.na(classification_info) && length(classification_info) > 0) {
    family <- classification_info[[1]] %>%
      filter(rank == "family") %>%
      pull(name)
    
    return(ifelse(length(family) > 0, family, NA))
  }
  return(NA)
}

# 🔁 Apply the function to the "plant_species" column
metadata$plant_family <- sapply(metadata$plant_species, get_family)

# 📁 Set output folder and create if it doesn't exist
output_folder <- "C:/Users/vg1u23/OneDrive - University of Southampton/Desktop/PhD_Gschiel/Project/Project1_Bioinformatics/metadata_files/metadataperstudy/Project_PlantFamily"
dir.create(output_folder, showWarnings = FALSE)

# 💾 Define output file path based on input file
output_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(input_file), "_with_family.xlsx"))

# 💽 Save the result to the output Excel file
write_xlsx(metadata, output_file)

# ✅ Done!
message("✅ Saved: ", output_file)
