#' Heatmap of the female human body
#'
#' @description
#' Generate a heatmap of sports injury data onto a female human body with different anatomical views.
#'
#' @param data Sports injury data, with columns Body.area, Body.region and Frequency
#' @param body_view Anatomical view of female body (either front/anterior, back/posterior or side/lateral)
#' @param low_colour Low colour for gradient colouring of heatmap
#' @param high_colour High colour for gradient colouring of heatmap
#' @param title Title of heatmap
#' @param body_region Boolean variable indicating whether to display the data by body region. Default is to display data by body area and is FALSE
#' @param show_labels Boolean variable indicating whether to display labels and frequency count for each body area/region
#' @param include_unspecified Boolean variable indicating whether to include frequency data with unspecified body area/region
#' @param colourblind_friendly Boolean variable indicating whether to use a colourblind friendly colour palette for heatmap gradient
#' @param colourOption  Option to choose which colourblind friendly palette from the package viridis, either "A", "B", "C", "E", "F", "G" or "H" (Default is "E")
#'
#'
#' @return Frequency heatmap of injury data on a female human body
#' @export
#'
#' @examples vis_heatmap_female(injuryDataTable, "front", "yellow", "red", "Heat Map of Front Body", body_region = FALSE, show_labels = TRUE, include_unspecified = TRUE)
#' vis_heatmap_female(injuryDataTable, "back", "blue", "green", "Heat Map of Back Body", body_region = TRUE)
#' vis_heatmap_female(injuryDataTable, "side", "yellow", "red", "Heat Map of Side Body", include_unspecified = FALSE)
vis_heatmap_female <- function(data, body_view, low_colour, high_colour, title, body_region=FALSE, show_labels = FALSE, include_unspecified = TRUE, colourblind_friendly = FALSE, colourOption = "E"){
  library(grid)
  library(png)
  library(ggplot2)
  library(dplyr)
  library(ggforce)
  library(viridis)

  if(body_region==FALSE){
  # In case data is not in desired order
    body_order <- c(
      "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
      "Chest", "Thoracic spine", "Lumbosacral", "Abdomen", "Hip and groin", "Thigh",
      "Knee", "Lower leg", "Ankle", "Foot", "Unspecified")

    data <- data %>%
      mutate(Body.area = factor(Body.area, levels = body_order)) %>%
      arrange(Body.area)

    max_radius <- 0.04

    # For symmetric body parts
    symmetric_areas <- c("Hand", "Foot", "Wrist", "Ankle", "Shoulder", "Elbow",
                         "Knee", "Hip and groin", "Forearm", "Upper arm", "Thigh", "Lower leg")

    if(body_view == "front" || body_view == "anterior"){
      x <- c(0.4880855, 0.4880855, 0.2910387, 0.2739042, 0.2396352, 0.2182170, 0.1710971,
             0.1282609, 0.4838019, 0.4880855, 0.4880855, 0.4838019, 0.3338750, 0.3767112,
             0.4152639, 0.4152639, 0.4366820, 0.4238311, 0.1882316)

      y <- c(1.82496679, 1.62791995, 1.54653104, 1.39232046, 1.28951342, 1.19527362, 1.07533206,
             0.98109227, 1.50797839, 1.34948419, 1.10103383, 1.22954264, 1.07961569, 0.88256885,
             0.60413309, 0.40708625, 0.18862127, 0.09009785, 1.78213052)
      img_path <- "Body Images/body_female_front_background_removed.png"

    } else if (body_view == "back"|| body_view == "posterior") {
      x <- c(0.5009364, 0.5009364, 0.3552931, 0.2781878, 0.2567697, 0.2139334, 0.1882316,
             0.1282609, 0.5009364, 0.5009364, 0.4966528, 0.4966528, 0.3338750, 0.3895621,
             0.4109803, 0.4238311, 0.4452493, 0.4109803, 0.2096498)

      y <- c(1.8421013, 1.6493381, 1.5679492, 1.4351567, 1.2809462, 1.1909900, 1.0753321,
             0.9639578, 1.5465310, 1.4265895, 1.1652882, 1.2937970, 1.1395865, 0.8825688,
             0.6341185, 0.4156535, 0.1929049, 0.1029487, 1.7864142)
      img_path <- "Body Images/body_female_back_background_removed.png"

    } else if (body_view == "side" || body_view == "lateral") {
      # x <- c(0.4238311, 0.4495329, 0.4109803, 0.4709510, 0.4752347, 0.6080271, 0.6851324,
      #        0.7622377, 0.6165944, 0.3638604, 0.3809949, 0.6722815, 0.4623838, 0.5480563,
      #        0.5052200, 0.4452493, 0.4195475, 0.5352054, 0.1154100 + 0.02)
      #
      # y <- c(1.88493757, 1.67075622, 1.53796378, 1.40945497, 1.29379704, 1.18670637, 1.07533206,
      #        0.98109227, 1.45657487, 1.37518596, 1.19527362, 1.25096077, 0.98109227, 0.83544895,
      #        0.59556584, 0.38566811, 0.18005402, 0.09009785, 1.88+0.05)
      x <- c(0.4321545, 0.4396580, 0.3871336, 0.4621685, 0.5034376,
        0.5972313, 0.6722662, 0.7397976, 0.6159900, 0.3908853,
        0.4171475, 0.6422522, 0.4846789, 0.4884307, 0.4921824,
        0.4021405, 0.4171475, 0.5109411, 0.1057527)

      y <- c(1.86134830, 1.66250582, 1.55745696, 1.43740112, 1.29108306,
        1.17102722, 1.07348185, 0.99469521, 1.45240810, 1.36986971,
        1.19353769, 1.27607608, 0.97593648, 0.81836319, 0.58950675,
        0.38316077, 0.19557352, 0.09052466, 1.99265938)
      img_path <- "Body Images/body_female_side_background_removed.png"

    } else {
      print("Please choose an appropriate body view (either front/anterior, back/posterior or side/lateral)")
    }

    body_coords <- data.frame(Body.area = data$Body.area, x, y)

    # Merge frequencies with coordinates
    plot_data <- data %>%
      left_join(body_coords, by = "Body.area")

    plot_data_labels <- plot_data

    if (body_view %in% c("front", "anterior", "back", "posterior")) {
      plot_data <- bind_rows(
        plot_data,
        filter(plot_data, Body.area %in% symmetric_areas) %>%
          mutate(x = 0.99 - x)
      )
    }

    plot_data <- plot_data %>%
      mutate(radius = max_radius)

    if (!include_unspecified) {
      plot_data <- plot_data %>% filter(Body.area != "Unspecified")
    }

    body_img <- readPNG(img_path)
    body_grob <- rasterGrob(body_img, width = unit(1, "npc"), height = unit(2, "npc"))

    p<- ggplot() +
      annotation_custom(body_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      geom_circle(
        data = plot_data,
        aes(x0 = x, y0 = y - 0.5, r = radius, fill = Frequency),
        colour = NA, alpha = 0.7
      ) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void() +
      ggtitle(title) +
      theme(plot.title = element_text(size = 20))

    if(colourblind_friendly==TRUE){
      p <- p+ scale_fill_viridis(option = colourOption, begin = 0.1, end = 0.9)
    } else {
      p <- p+ scale_fill_gradient(low = low_colour, high = high_colour)
    }

    if (show_labels) {
      plot_data_labels <- plot_data_labels %>%
        mutate(
          label_side = ifelse(y > 0.9, "left", "right"),
          label_side = ifelse(Body.area == "Head", "right", label_side),
          label_side = ifelse(Body.area == "Thoracic spine", "right", label_side),
          label_side = ifelse(Body.area == "Chest", "right", label_side),
          label_side = ifelse(Body.area == "Abdomen", "right", label_side),
          label_side = ifelse(Body.area == "Lumbosacral", "right", label_side),
          label_side = ifelse(Body.area == "Hip and groin", "right", label_side),
          label_x = ifelse(label_side == "right", 0.95, 0.1),
          label_x = ifelse(Body.area == "Hand", 0.025, label_x),
          label_x = ifelse(Body.area == "Wrist", 0.025, label_x),
          label_y = y - 0.5,
          label_y = ifelse(Body.area == "Shoulder", 1.5893341 - 0.55, label_y),
          label_y = ifelse(Body.area == "Hip and groin", y - 0.6, label_y)
        )

      p <- p + geom_curve(
        data = plot_data_labels,
        aes(x = x, y = y - 0.5, xend = label_x, yend = label_y),
        curvature = 0.3,
        color = "black",
        linewidth = 0.3,
        arrow = arrow(length = unit(0.01, "npc"))
      )

      p <- p + geom_text(
        data = plot_data_labels,
        aes(x = label_x, y = label_y + 0.02, label = Body.area),
        hjust = ifelse(plot_data_labels$label_side == "right", 0, 1),
        fontface = "bold",
        size = 2.5
      )

      p <- p + geom_text(
        data = plot_data_labels,
        aes(x = label_x, y = label_y - 0.02, label = Frequency),
        hjust = ifelse(plot_data_labels$label_side == "right", 0, 1),
        size = 2
      )
    }
    p

  } else if (body_region == TRUE) {
    # In case data is not in desired order
    body_order <- c("Head and neck", "Upper limb", "Trunk",
                    "Lower limb", "Unspecified")

    data <- aggregate(Frequency ~ Body.region, data = data, sum) %>%
      arrange(Body.region)

    data <- data %>%
      mutate(Body.region = factor(Body.region, levels = body_order)) %>%
      arrange(Body.region)

    max_radius <- 0.07

    if(body_view == "front" || body_view== "anterior"){
      x <- c(0.4848212, 0.4848212, 0.4848212, 0.4848212, 0.1552725)
      y <- c(1.8132487, 1.5432018, 1.2685779, 0.8383338, 1.8727505)
      img_path <- "Body Images/body_female_front_background_removed.png"

    } else if (body_view == "back" || body_view == "posterior") {
      x <- c(0.4985524, 0.4939753, 0.4939753, 0.4939753, 0.1598496)
      y <- c(1.7766321, 1.5752413, 1.2868862, 0.8703733, 1.8681734)
      img_path <- "Body Images/body_female_back_background_removed.png"

    } else if (body_view == "side" || body_view == "lateral") {
      x <- c(0.4710900, 0.4436276, 0.4482047 + 0.02, 0.4848212, 0.1095018)
      y <- c(1.8269798, 1.4882770, 1.1861907, 0.7467925, 2.0146395 - 0.09)
      img_path <- "Body Images/body_female_side_background_removed.png"

    } else {
      print("Please choose an appropriate body view (either front/anterior, back/posterior or side/lateral)")
    }

    body_coords <- data.frame(Body.region = data$Body.region, x, y)

    # Merge frequencies with coordinates
    plot_data <- data %>%
      left_join(body_coords, by = "Body.region")

    plot_data_labels <- plot_data

    plot_data <- plot_data %>%
      mutate(radius = max_radius)

    if (!include_unspecified) {
      plot_data <- plot_data %>% filter(Body.region != "Unspecified")
    }

    body_img <- readPNG(img_path)
    body_grob <- rasterGrob(body_img, width = unit(1, "npc"), height = unit(2, "npc"))

    p <- ggplot() +
      annotation_custom(body_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      geom_circle(
        data = plot_data,
        aes(x0 = x, y0 = y - 0.5, r = radius, fill = Frequency),
        colour = NA, alpha = 0.7
      ) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void() +
      ggtitle(title)+
      theme(plot.title = element_text(size = 20))

    if(colourblind_friendly==TRUE){
      p <- p + scale_fill_viridis(option = colourOption, begin = 0.1, end = 0.9)
    } else {
      p <- p + scale_fill_gradient(low = low_colour, high = high_colour)
    }

    if (show_labels) {
      plot_data_labels <- plot_data_labels %>%
        mutate(
          label_side = ifelse(y > 0.9, "left", "right"),
          label_side = ifelse(Body.region == "Head and neck", "right", label_side),
          label_x = ifelse(label_side == "right", 0.95, 0.1),
          label_x = ifelse(Body.region == "Head and neck", 0.9, label_x),
          label_y = y - 0.5,
        )

      p <- p + geom_curve(
        data = plot_data_labels,
        aes(x = x, y = y - 0.5, xend = label_x, yend = label_y),
        curvature = 0.3,
        color = "black",
        linewidth = 0.3,
        arrow = arrow(length = unit(0.01, "npc"))
      )

      p <- p + geom_text(
        data = plot_data_labels,
        aes(x = label_x, y = label_y + 0.02, label = Body.region),
        hjust = ifelse(plot_data_labels$label_side == "right", 0, 1),
        fontface = "bold",
        size = 3.5
      )

      p <- p + geom_text(
        data = plot_data_labels,
        aes(x = label_x, y = label_y - 0.02, label = Frequency),
        hjust = ifelse(plot_data_labels$label_side == "right", 0, 1),
        size = 3
      )
    }
    p
  }
}
