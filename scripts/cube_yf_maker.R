
# Sources:
# https://mycarta.wordpress.com/2013/03/06/perceptual-rainbow-palette-the-goodies/
# https://mycarta.wordpress.com/color-palettes/
  
# aim : load csv file with rgb values,
# then use rgb() to convert to hex values
# then save 

vals <- read.csv("support/palettes/0-255/cubeYF_0-255.csv", header = F)
palette <- rgb(vals[,1], vals[,2], vals[,3], maxColorValue = 255)  
write.csv(x = palette, file = "cumulative_fertility_app/data/cube_yf_palette.csv")
