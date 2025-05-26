#' Heatmap of the male human body
#'
#' @description
#' Generate a heatmap of sports injury data onto a male human body with different anatomical views.
#'
#' @param data Sports injury data, with columns Body.area, Body.region and Frequency
#' @param body_view Anatomical view of male body (either front, back or side)
#' @param low_colour Low colour for gradient colouring of heatmap
#' @param high_colour High colour for gradient colouring of heatmap
#' @param title Title of heatmap
#' @param body_region Boolean variable indicating whether to display the data by body region. Default is to display data by body area and is FALSE
#'
#' @return Frequency heatmap of injury data on a male human body
#' @export
#'
#' @examples vis_heatmap_male(injuryDataTable, "front", "yellow", "red", "Heat Map of Front Body")
vis_heatmap_male <- function(data, body_view, low_colour, high_colour, title, body_region=FALSE, show_labels = FALSE, include_unspecified = TRUE){
  library(grid)
  library(png)
  library(ggplot2)
  library(dplyr)
  library(ggforce)
  library(colorspace)
  library(paletteer)

  if(body_region==FALSE){
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
      x <- c(0.4891802, 0.4891802, 0.3502001, 0.2807100, 0.2774010, 0.2376924, 0.1913657,
             0.1516571, 0.4891802, 0.4924892, 0.4891802, 0.4891802, 0.3634363, 0.3998358,
             0.4229992, 0.4229992, 0.4395444, 0.4130720, 0.1814385)

      y <- c(1.8209676, 1.6488970, 1.5893341, 1.3907911, 1.2815924, 1.1790119, 1.0598861,
             0.9539965, 1.5264621, 1.3146829+0.1, 1.0764314, 1.2319567, 1.0135594, 0.8183255,
             0.6528730, 0.4642571, 0.2557870, 0.1532064, 1.8242766)
      img_path <- "Body Images/body_male_front_background_removed.png"

    } else if (body_view == "back") {
      x <- c(0.4891802, 0.4891802, 0.3502001, 0.2807100, 0.2774010, 0.2376924, 0.1913657,
             0.1516571, 0.4891802, 0.4924892, 0.4891802, 0.4891802, 0.3634363, 0.3998358,
             0.4229992, 0.4229992, 0.4395444, 0.4130720, 0.1814385)

      y <- c(1.85, 1.72, 1.5893341, 1.3907911, 1.2815924, 1.1790119, 1.0598861,
             0.9539965, 1.5264621, 1.3146829, 1.0764314, 1.2319567, 1.0135594, 0.8183255,
             0.6528730, 0.4642571, 0.2557870, 0.1532064, 1.8242766)
      img_path <- "Body Images/body_male_back_background_removed.png"

    } else if (body_view == "side") {

      x <- c(0.45888956, 0.46621550, 0.37830424, 0.28673002, 0.16951501, 0.19881876, 0.19881876,
             0.18782986, 0.52482300, 0.38196721, 0.37464128, 0.56877863, 0.48453034, 0.43691175,
             0.41127097, 0.37830424, 0.37464128, 0.44790065, 0.07427782)

      y <- c( 1.8518703, 1.6723848, 1.5405179, 1.4159770, 1.3244028, 1.1815470, 1.0350282,
              0.9434540, 1.4745845, 1.3463806, 1.1449173, 1.2548063, 1.0386912, 0.8225760,
              0.5918090, 0.4233124, 0.2291750, 0.1339378, 1.8225665)
      img_path <- "Body Images/body_male_side_background_removed.png"

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

    if (!include_unspecified) {
      plot_data <- plot_data %>% filter(Body.area != "Region unspecified")
    }

    body_img <- readPNG(img_path)
    body_grob <- rasterGrob(body_img, width = unit(1, "npc"), height = unit(2, "npc"))

    ggplot() +
      annotation_custom(body_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      geom_circle(
        data = plot_data,
        aes(x0 = x, y0 = y - 0.5, r = radius, fill = Frequency),
        colour = NA, alpha = 0.7
      ) +
     # scale_color_brewer(palette = colour_palette) +
      scale_fill_gradient(low = low_colour, high = high_colour) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void() +
      ggtitle(title) +
      if (show_labels) {
        geom_text(
          data = plot_data,
          aes(x = x, y = y - 0.6, label = paste0(Body.area, "\n", Frequency)),
          size = 3, vjust = -1, color = "black"
        )
      }

  } else if (body_region == TRUE){
    # In case data is not in desired order
    body_order <- c("Head and neck", "Upper limb", "Trunk",
                    "Lower limb", "Unspecified")

    #data <- #data %>%
     # mutate(Body.region = factor(Body.region, levels = body_order)) %>%
    data <- aggregate(Frequency ~ Body.region, data = data, sum) %>%
      arrange(Body.region)
    #%>%
    #  arrange(Body.region)

    max_radius <- 0.08
    min_radius <- 0.02

    # For symmetric body parts
    # symmetric_areas <- c("Head and neck", "Upper limb", "Trunk",
    #                      "Lower limb", "Unspecified")

    if(body_view == "front"){
      x <- c(0.4868586, 0.4909954, 0.4868586, 0.4868586, 0.1600564)
      y <- c(1.7520395, 1.5907068, 1.2597678, 0.7344023, 1.7892701)
      img_path <- "Body Images/body_male_front_background_removed.png"

    } else if (body_view == "back") {
      x <- c(0.4951321, 0.4951321, 0.4951321, 0.4909954, 0.1848769)
      y <- c(1.7768599, 1.5782966, 1.2639046, 0.8336839, 1.8140906)
      img_path <- "Body Images/body_male_back_background_removed.png"

    } else if (body_view == "side") {
      x <- c(0.4785852, 0.4330811, 0.4909954, 0.4992689, 0.2096973)
      y <- c(1.7148089, 1.5203822, 1.2266739, 0.8378207, 1.8389110)
      img_path <- "Body Images/body_male_side_background_removed.png"

    } else {
      print("Please choose an appropriate body view (either front, back or side)")
    }

    body_coords <- data.frame(Body.region = data$Body.region, x, y)

    # Merge frequencies with coordinates
    plot_data <- data %>%
      left_join(body_coords, by = "Body.region")
#
#     if (body_view %in% c("front", "back")) {
#       plot_data <- bind_rows(
#         plot_data,
#         filter(plot_data, Body.region %in% symmetric_areas) %>%
#           mutate(x = 0.99 - x)
#       )
#     }

    plot_data <- plot_data %>%
      mutate(radius = pmax (sqrt(Frequency / max(Frequency)) * max_radius, min_radius))

    if (!include_unspecified) {
      plot_data <- plot_data %>% filter(Body.region != "Unspecified")
    }

    body_img <- readPNG(img_path)
    body_grob <- rasterGrob(body_img, width = unit(1, "npc"), height = unit(2, "npc"))

    ggplot() +
      annotation_custom(body_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      geom_circle(
        data = plot_data,
        aes(x0 = x, y0 = y - 0.5, r = radius, fill = Frequency),
        colour = NA, alpha = 0.7
      ) +
      # scale_color_brewer(palette = colour_palette) +
      scale_fill_gradient(low = low_colour, high = high_colour) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void() +
      ggtitle(title) +
      if (show_labels) {
        geom_text(
          data = plot_data,
          aes(x = x, y = y - 0.5, label = Body.region),
          size = 3, vjust = -1, color = "black"
        )
      }
  }
}
