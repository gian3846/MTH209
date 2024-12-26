library(magick)
img <- image_read("http://polytopes.net/Tora_color.png")
plot(img)
img180 <- image_rotate(img,180)    ## rotate by 180
plot(img180)
img45 <-image_rotate(img,45)       ## rotate by 45
plot(img45)
img90 <- image_rotate(img, 90)     ## rotate by 90
plot(img90)
mirrorimg <- image_flop(img)       ## make the mirror image for the image
plot(mirrorimg)
