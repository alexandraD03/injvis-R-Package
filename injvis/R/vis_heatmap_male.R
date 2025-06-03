#' Heatmap of the male human body
#'
#' @description
#' Generate a heatmap of sports injury data onto a male human body with different anatomical views.
#'
#' @param data Sports injury data, with columns Body.area, Body.region and Frequency
#' @param body_view Anatomical view of male body (either front/anterior, back/posterior or side/lateral)
#' @param low_colour Low colour for gradient colouring of heatmap
#' @param high_colour High colour for gradient colouring of heatmap
#' @param title Title of heatmap
#' @param body_region Boolean variable indicating whether to display the data by body region. Default is to display data by body area and is FALSE
#' @param show_labels Boolean variable indicating whether to display labels and frequency count for each body area/region
#' @param include_unspecified Boolean variable indicating whether to include frequency data with unspecified body area/region
#' @param colourblind_friendly Boolean variable indicating whether to use a colourblind friendly colour palette for heatmap gradient
#' @param colourOption  Option to choose which colourblind friendly palette from the package viridis, either "A", "B", "C", "E", "F", "G" or "H" (Default is "E")
#'
#' @return Frequency heatmap of injury data on a male human body
#' @export
#'
#' @examples vis_heatmap_male(injuryDataTable, "front", "yellow", "red", "Heat Map of Front Body", body_region = FALSE, show_labels = TRUE, include_unspecified = TRUE)
#' vis_heatmap_male(injuryDataTable, "back", "blue", "green", "Heat Map of Back Body", body_region = TRUE)
#' vis_heatmap_male(injuryDataTable, "side", "yellow", "red", "Heat Map of Side Body", include_unspecified = FALSE)
vis_heatmap_male <- function(data, body_view, low_colour, high_colour, title, body_region=FALSE, show_labels = FALSE, include_unspecified = TRUE, colourblind_friendly = FALSE, colourOption = "E"){
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
    #max_radius <- c(0.06, 0.04, 0.05, 0.04, 0.04, 0.04, 0.04, 0.04, 0.06,
           #    0.04, 0.04, 0.055, 0.045, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04)


    # For symmetric body parts
    symmetric_areas <- c("Hand", "Foot", "Wrist", "Ankle", "Shoulder", "Elbow",
                         "Knee", "Hip and groin", "Forearm", "Upper arm", "Thigh", "Lower leg")

    if(body_view == "front" || body_view == "anterior"){
      x <- c(0.4891802, 0.4891802, 0.3502001, 0.2807100, 0.2774010, 0.2376924, 0.1913657,
             0.1516571, 0.4891802, 0.4924892, 0.4891802, 0.4891802, 0.3634363, 0.3998358,
             0.4229992, 0.4229992, 0.4395444, 0.4130720, 0.1814385)

      y <- c(1.8209676, 1.6488970, 1.5893341, 1.3907911, 1.2815924, 1.1790119, 1.0598861,
             0.9539965, 1.5264621, 1.3146829+0.1, 1.0764314 + 0.01, 1.2319567, 1.0135594, 0.8183255,
             0.6528730, 0.4642571, 0.2557870, 0.1532064, 1.8242766)
      img_path <- "Body Images/body_male_front_background_removed.png"

    } else if (body_view == "back" || body_view == "posterior") {
      x <- c(0.4891802, 0.4891802, 0.3502001, 0.2807100, 0.2774010 - 0.02, 0.2376924 - 0.02, 0.1913657 - 0.02,
             0.1516571 - 0.03, 0.4891802, 0.4924892, 0.4891802, 0.4891802, 0.3634363, 0.3998358,
             0.4229992, 0.4229992, 0.4395444, 0.4130720, 0.1814385)

      y <- c(1.85, 1.72, 1.5893341, 1.3907911, 1.2815924, 1.1790119, 1.0598861,
             0.9539965, 1.5264621, 1.3146829, 1.0764314, 1.2319567, 1.0135594, 0.8183255,
             0.6528730, 0.4642571, 0.2557870, 0.1532064, 1.8242766)
      img_path <- "Body Images/body_male_back_background_removed.png"

    } else if (body_view == "side" || body_view == "lateral") {

      x <- c(0.45888956, 0.46621550, 0.37830424, 0.28673002, 0.16951501, 0.19881876, 0.19881876,
             0.18782986, 0.52482300, 0.38196721, 0.37464128, 0.56877863, 0.48453034, 0.43691175,
             0.41127097, 0.37830424, 0.37464128, 0.44790065, 0.07427782 + 0.1)

      y <- c( 1.8518703, 1.6723848, 1.5405179, 1.4159770, 1.3244028, 1.1815470, 1.0350282,
              0.9434540, 1.4745845, 1.3463806, 1.1449173, 1.2548063, 1.0386912, 0.8225760,
              0.5918090, 0.4233124, 0.2291750, 0.1339378, 1.8225665)
      img_path <- "Body Images/body_male_side_background_removed.png"

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
            label_y = y - 0.5,
            label_y = ifelse(Body.area == "Shoulder", 1.5893341 - 0.55, label_y),
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
          aes(x = label_x, y = label_y + 0.02, label = Body.area),  # or Body.region
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

  } else if (body_region == TRUE){
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
      x <- c(0.4868586, 0.4909954, 0.4868586, 0.4868586, 0.1600564)
      y <- c(1.7520395, 1.5907068, 1.2597678, 0.7344023, 1.7892701)
      img_path <- "Body Images/body_male_front_background_removed.png"

    } else if (body_view == "back" || body_view == "posterior") {
      x <- c(0.4951321, 0.4951321, 0.4951321, 0.4909954, 0.1848769)
      y <- c(1.7768599, 1.5782966, 1.2639046, 0.8336839, 1.8140906)
      img_path <- "Body Images/body_male_back_background_removed.png"

    } else if (body_view == "side" || body_view == "lateral") {
      x <- c(0.4785852, 0.4330811, 0.4909954, 0.4992689, 0.2096973)
      y <- c(1.7148089, 1.5203822, 1.2266739, 0.8378207, 1.8389110)
      img_path <- "Body Images/body_male_side_background_removed.png"

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
        # p <- p + geom_text(
        #   data = plot_data,
        #   aes(x = x, y = y - 0.6, label = paste0(Body.region, "\n", Frequency)),
        #   size = 3, vjust = -1, color = "black"
        # )

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
          aes(x = label_x, y = label_y + 0.02, label = Body.region),  # or Body.region
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

