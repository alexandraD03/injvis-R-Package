vis_heatmap_male <- function(data, body_view, low_colour, high_colour){
                             #legend){
  library(grid)
  library(png)
  library(ggplot2)
  library(dplyr)
  # library(recolorize)
  # library(gridExtra)
  # library(OpenImageR)
  # library(magick)
  # library(RColorBrewer)
  # library(data.table)

  # In case data is not in desired order
  body_order <- c(
    "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
    "Chest", "Thoracic spine", "Lumbosacral", "Abdomen", "Hip/groin", "Thigh",
    "Knee", "Lower leg", "Ankle", "Foot", "Region unspecified"
  )

  data <- data %>%
    mutate(Body.area = factor(Body.area, levels = body_order)) %>%
    arrange(Body.area)

  if(body_view == "front"){
    x <- c(0.4891802, 0.4891802, 0.3502001, 0.2807100, 0.2774010, 0.2376924, 0.1913657,
      0.1516571, 0.4891802, 0.4924892, 0.4891802, 0.4891802, 0.3634363, 0.3998358,
      0.4229992, 0.4229992, 0.4395444, 0.4130720, 0.1814385)

    y <- c(1.8209676, 1.6488970, 1.5893341, 1.3907911, 1.2815924, 1.1790119, 1.0598861,
      0.9539965, 1.5264621, 1.3146829, 1.0764314, 1.2319567, 1.0135594, 0.8183255,
      0.6528730, 0.4642571, 0.2557870, 0.1532064, 1.8242766)

    body_coords <- data.frame(Body.area = data$Body.area, x, y
                              #size()
      )

    # Merge frequencies with coordinates
    plot_data <- data %>%
      left_join(body_coords, by = "Body.area")

    # For symmetric body parts
    symmetric_areas <- c("Hand", "Foot", "Wrist", "Ankle", "Shoulder", "Elbow",
                         "Knee", "Hip/groin", "Forearm", "Upper arm", "Thigh", "Lower leg")

    left_side <- plot_data %>%
      filter(Body.area %in% symmetric_areas)

    right_side <- left_side %>%
      mutate(x = 1 - x)

    plot_data_doubled <- bind_rows(plot_data, right_side)

    # Load body image
    body <- readPNG("Body Images/body_male_front_background_removed.png")
    g <- rasterGrob(body, width=unit(1,"npc"), height=unit(2,"npc"))

    # Plot
    ggplot(plot_data_doubled, aes(x, y-0.5)) +
      annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1) +
      geom_point(aes(size = Frequency, color = Frequency), alpha = 0.7) +
      scale_color_gradient(low = low_colour, high = high_colour) +
      scale_size(range = c(5, 15)) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void()

  } else if (body_view == "back") {
    x <- c(0.4891802, 0.4891802, 0.3502001, 0.2807100, 0.2774010, 0.2376924, 0.1913657,
           0.1516571, 0.4891802, 0.4924892, 0.4891802, 0.4891802, 0.3634363, 0.3998358,
           0.4229992, 0.4229992, 0.4395444, 0.4130720, 0.1814385)

    y <- c(1.8209676, 1.6488970, 1.5893341, 1.3907911, 1.2815924, 1.1790119, 1.0598861,
           0.9539965, 1.5264621, 1.3146829, 1.0764314, 1.2319567, 1.0135594, 0.8183255,
           0.6528730, 0.4642571, 0.2557870, 0.1532064, 1.8242766)

    body_coords <- data.frame(Body.area = data$Body.area, x, y
                              #size()
    )

    # Merge frequencies with coordinates
    plot_data <- data %>%
      left_join(body_coords, by = "Body.area")

    # For symmetric body parts
    symmetric_areas <- c("Hand", "Foot", "Wrist", "Ankle", "Shoulder", "Elbow",
                         "Knee", "Hip/groin", "Forearm", "Upper arm", "Thigh", "Lower leg")

    left_side <- plot_data %>%
      filter(Body.area %in% symmetric_areas)

    right_side <- left_side %>%
      mutate(x = 1 - x)

    plot_data_doubled <- bind_rows(plot_data, right_side)

    # Load body image
    body <- readPNG("Body Images/body_male_back_background_removed.png")
    g <- rasterGrob(body, width=unit(1,"npc"), height=unit(2,"npc"))

    # Plot
    ggplot(plot_data_doubled, aes(x, y-0.5)) +
      annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1) +
      geom_point(aes(size = Frequency, color = Frequency), alpha = 0.7) +
      scale_color_gradient(low = low_colour, high = high_colour) +
      scale_size(range = c(5, 15)) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void()

  } else if (body_view == "side") {
    body <- image_read("Body Images/body_male_side.png")

  } else {
    print("Please choose an appropriate body view (either front, back or side)")
  }
}
