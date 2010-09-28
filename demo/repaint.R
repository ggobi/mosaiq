
## too much repainting and focus

scene <- qscene()
root <- qlayer(scene)

points <-
    qlayer(root,
           paintFun = function(item, painter, exposed) {
               mosaiq.points(runif(10), runif(10),
                             painter = painter)
           },
           row = 1L, col = 0L,
           cache = TRUE,
           limits = qrect(c(0, 1), c(0, 1)))


points2 <-
    qlayer(root,
           paintFun = function(item, painter, exposed) {
               mosaiq.points(runif(10), runif(10),
                             painter = painter, col = "orange")
           },
           row = 0L, col = 0L,
           cache = TRUE,
           limits = qrect(c(0, 1), c(0, 1)))

(v <- qplotView(scene, opengl = FALSE))

timerHandler <- function()
{
    x <- points #v$scene()
    a <- x$hasFocus()
    x$clearFocus()
    ## x$update()
    v$update()
    b <- x$hasFocus()
    x$setFocus()
    ## x$update()
    v$update()
    print(c(a, b))
}

timer <- ## 1-second timer
    qtimer(1000, handler = timerHandler)


timer$start()
timer$stop()



qtpaint:::qupdate.QGraphicsView
qtpaint:::qupdate.QGraphicsScene

v$update()
v$viewport()$repaint()

scene$clearFocus()
v$scene()$update()
scene$setFocus()
v$scene()$update()




