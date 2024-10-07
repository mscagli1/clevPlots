# Set wd and load packages ------------------------------------------------
here::i_am("scripts/01_data-import.R")
library(tidyverse)
library(here)
library(ggplot2)

# Import data -------------------------------------------------------------
data_paths <- dir(here("data"), pattern = "*.csv", full.names = TRUE)  # Get absolute paths for data
file_names <- dir(here("data"), pattern = "*.csv", full.names = FALSE) |>  # Get data names without .csv
  str_remove(".csv")
names(data_paths) <- file_names  # Name paths with original data file names

data_list <- map(data_paths, read_csv)  # Read in data
data_list <- data_list |> map(~ filter(.x, !is.na(Name)))  # Remove any genes with no name (?)

# Slice dataset and plot data ---------------------------------------------
n_genes <- 25  # Number of genes to slice
top_n_list <- data_list |> map(~ slice_max(.x, TOM.similarity.measure, n = n_genes))  # Take top n genes by TOM score
#Additional details for plotting function
color_vec <- c(atf4 = "red2", cebpg = "dodgerblue2")
titles <- c("Atf4", "Cebpg")
plot_info <- list(top_n_list, file_names, color_vec, titles)  # Combine all vectors into list

# Plot function
plot_clev <- function(data, filename, mycolor, title) {
  new_filename <- paste0(filename, sep = ".", "top", n_genes, "plot.pdf")

  my_plot <- ggplot(data, aes(x = TOM.similarity.measure, y = reorder(Name, TOM.similarity.measure))) +
    geom_segment(aes(yend = Name), xend = 0, colour = "grey50") +
    geom_point(color = mycolor) +
    labs(title = title, subtitle = paste("Top", n_genes, "genes")) +
    xlab("TOM Score \n (WCGNA)") +
    ylab(NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          aspect.ratio = 4,
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )

  ggsave(filename = new_filename,  # Save plot
         device = "pdf",
         width = 6,
         height = 12,
         units = "cm",
         path = here("plots")
         )

  return(my_plot)  # Return plot for interactive viewing
}

# Generate plots
plots <- pmap(plot_info, plot_clev, .progress = TRUE)
