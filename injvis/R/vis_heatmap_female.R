#' Heatmap of the female human body
#'
#' @description
#' Generate a heatmap of sports injury data onto a female human body with different anatomical views.
#'
#' @param data Sports injury data, with columns Body.area, Body.region and Frequency
#' @param body_view Anatomical view of female body (either front, back or side)
#' @param low_colour Low colour for gradient colouring of heatmap
#' @param high_colour High colour for gradient colouring of heatmap
#' @param title Title of heatmap
#' @param body_region Boolean variable indicating whether to display the data by body region. Default is to display data by body area and is FALSE
#'
#'
#' @return Frequency heatmap of injury data on a female human body
#' @export
#'
#' @examples vis_heatmap_female(injuryDataTable, "front", "yellow", "red", "Heat Map of Front Body")
vis_heatmap_female <- function(data, body_view, low_colour, high_colour, title){
  library(grid)
  library(png)
  library(ggplot2)
  library(dplyr)
  library(ggforce)
  library(viridis)

  # In case data is not in desired order
  body_order <- c(
    "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
    "Chest", "Thoracic spine", "Lumbosacral", "Abdomen", "Hip/groin", "Thigh",
    "Knee", "Lower leg", "Ankle", "Foot", "Region unspecified")

  data <- data %>%
    mutate(Body.area = factor(Body.area, levels = body_order)) %>%
    arrange(Body.area)

  max_radius <- 0.08
  min_radius <- 0.02

  # For symmetric body parts
  symmetric_areas <- c("Hand", "Foot", "Wrist", "Ankle", "Shoulder", "Elbow",
                       "Knee", "Hip/groin", "Forearm", "Upper arm", "Thigh", "Lower leg")

  if(body_view == "front"){
    x <- c(0.4880855, 0.4880855, 0.2910387, 0.2739042, 0.2396352, 0.2182170, 0.1710971,
           0.1282609, 0.4838019, 0.4880855, 0.4880855, 0.4838019, 0.3338750, 0.3767112,
           0.4152639, 0.4152639, 0.4366820, 0.4238311, 0.1882316)

    y <- c(1.82496679, 1.62791995, 1.54653104, 1.39232046, 1.28951342, 1.19527362, 1.07533206,
           0.98109227, 1.50797839, 1.34948419, 1.10103383, 1.22954264, 1.07961569, 0.88256885,
           0.60413309, 0.40708625, 0.18862127, 0.09009785, 1.78213052)
    img_path <- "Body Images/body_female_front_background_removed.png"

  } else if (body_view == "back") {
    x <- c(0.5009364, 0.5009364, 0.3552931, 0.2781878, 0.2567697, 0.2139334, 0.1882316,
           0.1282609, 0.5009364, 0.5009364, 0.4966528, 0.4966528, 0.3338750, 0.3895621,
           0.4109803, 0.4238311, 0.4452493, 0.4109803, 0.2096498)

    y <- c(1.8421013, 1.6493381, 1.5679492, 1.4351567, 1.2809462, 1.1909900, 1.0753321,
           0.9639578, 1.5465310, 1.4265895, 1.1652882, 1.2937970, 1.1395865, 0.8825688,
           0.6341185, 0.4156535, 0.1929049, 0.1029487, 1.7864142)
    img_path <- "Body Images/body_female_back_background_removed.png"

  } else if (body_view == "side") {
    x <- c(0.4238311, 0.4495329, 0.4109803, 0.4709510, 0.4752347, 0.6080271, 0.6851324,
           0.7622377, 0.6165944, 0.3638604, 0.3809949, 0.6722815, 0.4623838, 0.5480563,
           0.5052200, 0.4452493, 0.4195475, 0.5352054, 0.1154100)

    y <- c(1.88493757, 1.67075622, 1.53796378, 1.40945497, 1.29379704, 1.18670637, 1.07533206,
           0.98109227, 1.45657487, 1.37518596, 1.19527362, 1.25096077, 0.98109227, 0.83544895,
           0.59556584, 0.38566811, 0.18005402, 0.09009785, 1.88)
    img_path <- "Body Images/body_female_side_background_removed.png"

  } else {
    print("Please choose an appropriate body view (either front, back or side)")
  }

  body_coords <- data.frame(Body.area = data$Body.area, x, y)

  # Merge frequencies with coordinates
  plot_data <- data %>%
    left_join(body_coords, by = "Body.area")

  if (body_view %in% c("front", "back")) {
    plot_data <- bind_rows(
      plot_data,
      filter(plot_data, Body.area %in% symmetric_areas) %>%
        mutate(x = 0.99 - x)
    )
  }

  plot_data <- plot_data %>%
    mutate(radius = pmax (sqrt(Frequency / max(Frequency)) * max_radius, min_radius))

  body_img <- readPNG(img_path)
  body_grob <- rasterGrob(body_img, width = unit(1, "npc"), height = unit(2, "npc"))

  ggplot() +
    annotation_custom(body_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    geom_circle(
      data = plot_data,
      aes(x0 = x, y0 = y - 0.5, r = radius, fill = Frequency),
      colour = NA, alpha = 0.7
    ) +
    scale_fill_viridis(option = "cividis", begin = 0.1, end = 0.9) +
    coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
    theme_void() +
    ggtitle(title) +
    theme(plot.title = element_text(size = 20))

}
