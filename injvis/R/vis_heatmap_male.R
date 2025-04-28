vis_heatmap_male <- function(data, body_view, colour_palette, legend){
  library(grid)
  library(ggplot2)
  library(recolorize)
  library(gridExtra)
  library(OpenImageR)
  library(magick)

  if(body_view == "front"){
    body <- image_read("/Users/alexandradooley/Desktop/Uni/Year 4/COMP3850/COMP3850-Group-15-R-Package/injvis/Body Images/body_male_front.jpg")

    # # load image
    # body <- readImage(img_path, resize = NULL, rotate = NULL)
    # body <- load.image(img_path)
    # img <- readJPEG("/injvis/Body Images/body_male_front.jpg")

  } else if (body_view == "back") {
    body <- image_read("/Users/alexandradooley/Desktop/Uni/Year 4/COMP3850/COMP3850-Group-15-R-Package/injvis/Body Images/body_male_back.jpg")

  } else if (body_view == "side") {
    body <- image_read("/Users/alexandradooley/Desktop/Uni/Year 4/COMP3850/COMP3850-Group-15-R-Package/injvis/Body Images/body_male_side.jpg")

  } else {
    print("Please choose an appropriate body view (either front, back or side")
  }
}
