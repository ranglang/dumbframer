scroll = new ScrollComponent
    size: Screen.size
    scrollHorizontal: false
部落 = new Layer
    height: 1573
    width: -750
    image: "images/部落.jpg"
    parent:scroll.content
加下滑界面 = new Layer
    parent: scroll
    y: 58
    width: 753
    height: 1207
    image: "images/+.png"
# 	opacity: 0

加btn = new Layer
    parent: scroll.content
    x: 640
    y: 58
    width: 72
    height: 72
    image: "images/加.png"

 加btn.on Events.Click, ->
    加下滑界面.stateCycle("show","hide")

 加下滑界面.visible=false

加下滑界面.states =
    show:
# 		visible: true
        y: 58
        opacity: 1
    hide:
# 		visible: false
        y: -702
        opacity: 0
加下滑界面.animationOptions =
    curve: "spring(250,25,0)"