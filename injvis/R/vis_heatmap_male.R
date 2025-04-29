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

    # Body area coordinates on the image (normalised between 0-1)
    body_coords <- data.frame(
      Body.area = data$Body.area,
      x = c(0.4, 0.5, 0.3, 0.6, 0.5, 0.5, 0.3, 0.6, 0.5, 0.5, 0.3, 0.6, 0.5, 0.5, 0.3, 0.6, 0.3, 0.3, 0.3),
      y = c(1.2, 0.7, 0.7, 0.4, 0.5, 0.5, 0.3, 0.6, 0.5, 0.5, 0.3, 0.6, 0.5, 0.5, 0.3, 0.6, 0.3, 0.3, 0.3)
      #size = c()
    )

    # Merge frequencies with coordinates
    plot_data <- merge(body_coords, data, by = "Body.area")

    # Load human body image
    body <- readPNG("Body Images/body_male_front_background_removed.png")
    #  readPNG("human_body_outline.png")  # Replace with your image path
    g <- rasterGrob(body, width=unit(1,"npc"), height=unit(2,"npc"))

    # Plot
    ggplot(plot_data, aes(x, y)) +
      annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1) +
      geom_point(aes(size = Frequency, color = Frequency), alpha = 0.7) +
      scale_color_gradient(low = low_colour, high = high_colour) +
      scale_size(range = c(5, 15)) +
      coord_fixed(xlim = c(-0.1, 1.1), ylim = c(-0.4, 1.4)) +
      theme_void()

  } else if (body_view == "back") {
    body <- image_read("Body Images/body_male_back.png")

  } else if (body_view == "side") {
    body <- image_read("Body Images/body_male_side.png")

  } else {
    print("Please choose an appropriate body view (either front, back or side)")
  }
}
