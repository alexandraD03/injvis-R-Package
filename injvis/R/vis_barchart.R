#' Barchart of sports injuries per tissue and pathology type
#' @description
#' Generate a bar chart to show the frequency of sports injuries per tissue and pathology type.
#'
#' @param data Sports injury data, with columns Tissue, Pathology.Type and Frequency
#' @param colurblond description
#' @param colourOption description
#'
#' @return Coloured barchart of sports injury data at tissue and pathology type level
#' @export
#'
#' @examples
#' vis_barchart(injuryTissuePathTable)
#' vis_barchart(injuryTissuePathTable, colourblind = TRUE)
#' vis_barchart(injuryTissuePathTable, colourblind = TRUE, colourOption = "D")
#'
vis_barchart <- function(data, colourblind = FALSE, colourOption = "A") {
  library(ggplot2)
  library(dplyr)
  library(viridis)

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

  if(colourblind) {
    barchart <- barchart + scale_fill_viridis_d(option = colourOption)
  }

  barchart
}
