#' Barchart of sports injuries per tissue and pathology type
#' @description
#' Generate a bar chart to show the frequency of sports injuries per tissue and pathology type.
#'
#' @param data Sports injury data, with columns Tissue, Pathology.Type and Frequency
#' @param colourblind_friendly Boolean variable indicating whether to use a colourblind friendly colour palette
#' @param colourOption Option to choose which colourblind friendly palette from the package viridis, either "A", "B", "C", "E", "F", "G" or "H" (Default is "A")
#' @param tissue_view Option to only view tissue type frequency
#'
#' @return Coloured barchart of sports injury data at tissue and pathology type level
#' @export
#'
#' @examples
#' vis_barchart(injuryTissuePathTable)
#' vis_barchart(injuryTissuePathTable, tissue_view = TRUE)
#' vis_barchart(injuryTissuePathTable, tissue_view = TRUE, colourblind = TRUE)
#' vis_barchart(injuryTissuePathTable, tissue_view = TRUE, colourblind = TRUE, colourOption = "D")
#'
vis_barchart <- function(data, tissue_view = FALSE, colourblind_friendly = FALSE, colourOption = "A") {
  library(ggplot2)
  library(dplyr)
  library(viridis)

  if (tissue_view) {
    data_summary <- data %>%
      group_by(Tissue) %>%
      summarise(Frequency = sum(Frequency)) %>%
      mutate(Tissue = factor(Tissue, levels = c(setdiff(Tissue, "Non-specific"), "Non-specific")))

    barchart <- ggplot(data_summary, aes(x = Tissue, y = Frequency, fill = Tissue)) +
      geom_col() +
      labs(x = "Tissue Type", y = "Total Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      scale_y_continuous(breaks = seq(0, max(data_summary$Frequency), by = 10))

    if (colourblind_friendly) {
      barchart <- barchart + scale_fill_viridis_d(option = colourOption)
    }

    return(barchart)
  }

  data <- data %>%
    mutate(Tissue = factor(Tissue,
                           levels = c(unique(Tissue[Tissue != "Non-specific"]), "Non-specific"))) %>%
    arrange(Tissue) %>%
    mutate(Pathology.Type = factor(Pathology.Type, levels = unique(Pathology.Type)))

  barchart <- ggplot(data, aes(x = Pathology.Type, y = Frequency, fill = Tissue)) +
    geom_col() +
    labs(x = "Pathology Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom") +
    scale_y_continuous(breaks = seq(0, max(data$Frequency), by = 5))

  if(colourblind_friendly) {
    barchart <- barchart + scale_fill_viridis_d(option = colourOption)
  }

  return(barchart)
}
