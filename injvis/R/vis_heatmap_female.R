vis_heatmap_female <- function(data, body_view, low_colour, high_colour, title){
  library(grid)
  library(png)
  library(ggplot2)
  library(dplyr)

  # In case data is not in desired order
  body_order <- c(
    "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
    "Chest", "Thoracic spine", "Lumbosacral", "Abdomen", "Hip/groin", "Thigh",
    "Knee", "Lower leg", "Ankle", "Foot", "Region unspecified")

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
      mutate(x = 0.99 - x)

    plot_data_doubled <- bind_rows(plot_data, right_side)

    # Load body image
    body <- readPNG("Body Images/body_female_front_background_removed.png")
    g <- rasterGrob(body, width=unit(1,"npc"), height=unit(2,"npc"))

    # Plot
    ggplot(plot_data_doubled, aes(x, y-0.5)) +
      annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1) +
      geom_point(aes(size = Frequency, color = Frequency), alpha = 0.7) +
      scale_color_gradient(low = low_colour, high = high_colour) +
      scale_size(range = c(5, 15)) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void() +
      ggtitle(title)

  } else if (body_view == "back") {
    x <- c(0.4891802, 0.4891802, 0.3502001, 0.2807100, 0.2774010, 0.2376924, 0.1913657,
           0.1516571, 0.4891802, 0.4924892, 0.4891802, 0.4891802, 0.3634363, 0.3998358,
           0.4229992, 0.4229992, 0.4395444, 0.4130720, 0.1814385)

    y <- c(1.85, 1.72, 1.5893341, 1.3907911, 1.2815924, 1.1790119, 1.0598861,
           0.9539965, 1.5264621, 1.3146829, 1.0764314, 1.2319567, 1.0135594, 0.8183255,
           0.6528730, 0.4642571, 0.2557870, 0.1532064, 1.8242766)

    body_coords <- data.frame(Body.area = data$Body.area, x, y)

    # Merge frequencies with coordinates
    plot_data <- data %>%
      left_join(body_coords, by = "Body.area")

    # For symmetric body parts
    symmetric_areas <- c("Hand", "Foot", "Wrist", "Ankle", "Shoulder", "Elbow",
                         "Knee", "Hip/groin", "Forearm", "Upper arm", "Thigh", "Lower leg")

    left_side <- plot_data %>%
      filter(Body.area %in% symmetric_areas)

    right_side <- left_side %>%
      mutate(x = 0.99 - x)

    plot_data_doubled <- bind_rows(plot_data, right_side)

    # Load body image
    body <- readPNG("Body Images/body_female_back_background_removed.png")
    g <- rasterGrob(body, width=unit(1,"npc"), height=unit(2,"npc"))

    # Plot
    ggplot(plot_data_doubled, aes(x, y-0.5)) +
      annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1) +
      geom_point(aes(size = Frequency, color = Frequency), alpha = 0.7) +
      scale_color_gradient(low = low_colour, high = high_colour) +
      scale_size(range = c(5, 15)) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void() +
      ggtitle(title)

  } else if (body_view == "side") {

    x <- c(0.45888956, 0.46621550, 0.37830424, 0.28673002, 0.16951501, 0.19881876, 0.19881876,
           0.18782986, 0.52482300, 0.38196721, 0.37464128, 0.56877863, 0.48453034, 0.43691175,
           0.41127097, 0.37830424, 0.37464128, 0.44790065, 0.07427782)

    y <- c( 1.8518703, 1.6723848, 1.5405179, 1.4159770, 1.3244028, 1.1815470, 1.0350282,
            0.9434540, 1.4745845, 1.3463806, 1.1449173, 1.2548063, 1.0386912, 0.8225760,
            0.5918090, 0.4233124, 0.2291750, 0.1339378, 1.8225665)

    body_coords <- data.frame(Body.area = data$Body.area, x, y)

    # Merge frequencies with coordinates
    plot_data <- data %>%
      left_join(body_coords, by = "Body.area")

    body <- readPNG("Body Images/body_female_side_background_removed.png")
    g <- rasterGrob(body, width=unit(1,"npc"), height=unit(2,"npc"))

    # Plot
    ggplot(plot_data, aes(x, y-0.5)) +
      annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1) +
      geom_point(aes(size = Frequency, color = Frequency), alpha = 0.7) +
      scale_color_gradient(low = low_colour, high = high_colour) +
      scale_size(range = c(5, 15)) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void() +
      ggtitle(title)

  } else {
    print("Please choose an appropriate body view (either front, back or side)")
  }
}
