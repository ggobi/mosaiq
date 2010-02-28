
## As a first step in resurrecting mosaiq, try out a basic layout
## example using the new smoke-based system

## QT example

library(qtbase)

scene <- Qt$QGraphicsScene()
textEdit <- scene$addWidget(Qt$QTextEdit())
pushButton <- scene$addWidget(Qt$QPushButton())
## textEdit <- Qt$QTextEdit() # crashes later
## pushButton <- Qt$QPushButton()

layout <- Qt$QGraphicsGridLayout()
layout$addItem(textEdit, 0, 0)
layout$addItem(pushButton, 0, 1)

form <- Qt$QGraphicsWidget()
form$setLayout(layout)
scene$addItem(form)

view <- Qt$QGraphicsView()
view$setScene(scene)
view





sceneWithPoints <- function()
{
    myscene <- Qt$QGraphicsScene()

    myview <- Qt$QGraphicsView(myscene)
    

        str(list(i = i, x = x[i], y = y[i], stroke = stroke, fill = fill))
        scene$addEllipse(x[i], y[i], 10, 10)

myview
}

plotwidget <- Qt$QGraphicsWidget()
plotlayout <- Qt$QGraphicsGridLayout()
plotwidget$setLayout(plotlayout)

strips <-
    lapply(month.name[1:4],
           function(s) {
               foo <- Qt$QGraphicsProxyWidget()
               foo$setWidget(Qt$QLabel(s))
               foo
           })
           
plotlayout$addItem(strips[[1]], 0, 0)
plotlayout$addItem(strips[[2]], 2, 0)
plotlayout$addItem(strips[[3]], 0, 2)
plotlayout$addItem(strips[[4]], 2, 2)


layout$addItem(pushButton, 0, 1)

figurescene <- Qt$QGraphicsScene()
figurescene$addItem(plotwidget)

figureview <- Qt$QGraphicsView()
figureview$setScene(figurescene)
figureview





