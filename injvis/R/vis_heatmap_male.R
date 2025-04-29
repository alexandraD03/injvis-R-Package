vis_heatmap_male <- function(data, body_view, colour_palette, legend){
  library(grid)
  library(ggplot2)
  library(recolorize)
  library(gridExtra)
  library(OpenImageR)
  library(magick)
  library(RColorBrewer)

  if(body_view == "front"){
    #body <- image_read("/Users/alexandradooley/Desktop/Uni/Year 4/COMP3850/COMP3850-Group-15-R-Package/injvis/Body Images/body_male_front.png")
    body <- image_read("Body Images/body_male_front.png")
    #plot(body)

    #overlay <- image_blank(width = 20,
                     #      height = 20,
                     #      color = "#FF000080") # Red with 50% opacity

    # Composite the overlay on top
   # img_colored <- image_composite(body, overlay, operator = "atop")

    # Plot the result
    #plot(img_colored)

    #heat.colors(4, rev = FALSE))

    # # load image
    # body <- readImage(img_path, resize = NULL, rotate = NULL)
    # body <- load.image(img_path)
    # img <- readJPEG("/injvis/Body Images/body_male_front.jpg")

    # Get image size
    info <- image_info(body)
    img_width <- info$width
    img_height <- info$height

    # Create blank transparent layer
    heatmap_layer <- image_blank(width = img_width, height = img_height, color = "none")

    # Define a color palette (blue → red)
   # palette_func <- colorRampPalette(c("blue", "green", "yellow", "red"))
      #heat.colors(4, rev = FALSE)
      #colorRampPalette(c("blue", "green", "yellow", "red"))
    palette_colours <- colour_palette
      # heat.colors(100, rev = FALSE)
      #palette_func(100)

    body_regions <- data.frame(
      region = c("Head and neck", "Upper limb", "Trunk", "Lower limb", "Unspecified"),
      x_start = c(0.25, 0.0, 0.3, 0.33, 0),
      x_end   = c(0.75, 1, 0.7, 0.7, 0.05),
      y_start = c(0.95, 0.4, 0.2, 0, 0),
      y_end   = c(1, 0.8, 0.8, 0.6, 0.9),
      frequency = data$frequency
      #  c(0.8, 0.5, 0.5, 0.7, 0.6, 0.6)
    )

    body_areas <- data.frame(
      region = c("Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
                 "Chest", "Thoracic spine", "Lumbosacral", "Abdomen", "Hip/groin",
                 "Thigh", "Knee", "Lower leg", "Ankle", "Foot", "Region unspecified"),
      # x_start = c(0.4, 0.0, 0.6, 0.4, 0.4, 0.5),
      # x_end   = c(0.6, 0.4, 1.0, 0.6, 0.5, 0.6),
      # y_start = c(0.2, 0.2, 0.2, 0.4, 0.6, 0.6),
      # y_end   = c(0.4, 0.5, 0.5, 0.6, 1.0, 1.0),
      # heat_value = c(0.8, 0.5, 0.5, 0.7, 0.6, 0.6)
    )

    body_regions$norm_freq <- regions$frequency / sum(regions$frequency)
    #<- (regions$frequency - min(regions$frequency)) /
    #  (max(regions$frequency) - min(regions$frequency))
  #  regions$frequency / sum(regions$frequency)


    # Now for each region, create a colored transparent rectangle
    for (i in 1:nrow(body_regions)) {
      r <- body_regions[i,]

      # Calculate pixel positions
      x_start_px <- round(r$x_start * img_width)
      x_end_px <- round(r$x_end * img_width)
      y_start_px <- round(r$y_start * img_height)
      y_end_px <- round(r$y_end * img_height)

      width_px <- x_end_px - x_start_px
      height_px <- y_end_px - y_start_px

      # Color based on heat value
      color_index <- round(r$norm_freq * 99) + 1
      color <- adjustcolor(palette_colors[color_index], alpha.f = 0.5)

      # Create the rectangle
      rect <- image_blank(width = width_px, height = height_px, color = col2hex(color))

      # Composite rectangle into the heatmap layer
      heatmap_layer <- image_composite(
        heatmap_layer, rect,
        offset = paste0("+", x_start_px, "+", y_start_px)
      )
    }

    heatmap_layer_blurred <- image_blur(heatmap_layer, radius = 5, sigma = 3)
    final_img <- image_composite(body_img, heatmap_layer_blurred, operator = "atop")

    # Composite heatmap onto body
  #  final_img <- image_composite(body_img, heatmap_layer, operator = "atop")

    plot(final_img)

  } else if (body_view == "back") {
    body <- image_read("Body Images/body_male_back.png")

  } else if (body_view == "side") {
    body <- image_read("Body Images/body_male_side.png")

  } else {
    print("Please choose an appropriate body view (either front, back or side")
  }
}
