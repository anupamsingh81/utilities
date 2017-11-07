
# Animate

library(magick)

background <- image_read("/home/anupam/abc.png")
# And bring in a logo
image_info(background)
logo_raw <- image_read("/home/anupam/bum.gif") 


frames <- lapply(logo_raw, function(frame) {
  image_composite(background, frame, offset = "+250+420") # remeber +70+320 for travolta with normal saved ggplot
})

# more you increase second numberu in offset (+x+y) , gif image goes down towards x axis., more you increase x more to right

animation <- image_animate(image_join(frames))


image_write(animation, "/home/anupam/bumpeez.gif")


