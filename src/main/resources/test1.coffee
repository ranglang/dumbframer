scroll = new ScrollComponent
    size: Screen.size
    scrollHorizontal: false
部落 = new Layer
    height: 1573
    width: 750
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
    加下滑界面.stateCycle("sdd","dd")
# 	.visible = ! 加下滑界面.visible

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
搜索btn = new Layer
    parent: scroll.content
    x: 552
    y: 58
    width: 72
    height: 72
    image: "images/加.png"
部落标题= new Layer
    parent: scroll.content
    html: "部落"
    x: 334
    y: 84
    backgroundColor: "rgba(123,123,123,0)"
    opacity: 1.00
    height: 52
    width: 122
    style:
        fontSize: "42px"
        color:"black"

部落动态1 = new Layer
    parent: scroll.content
    y: 159
    width: 750
    height: 604
    opacity: 1.00
    backgroundColor: "rgba(255,255,255,1)"
    shadowSpread: 1
    shadowColor: "rgba(0,0,0,1)"
    shadowY: 2
    shadowX: 2
    shadowBlur: 6
用户头像 = new Layer
    parent: 部落动态1
    width: 106
    height: 110
    borderRadius: 100
    backgroundColor: "rgba(255,128,0,1)"
    opacity: 1.00
    x: 13
    y: 6

任务名称= new Layer
    parent: 部落动态1
    html: "我们一起去看世界"
    x: 136
    y: 31
    backgroundColor: "rgba(123,123,123,0)"
    opacity: 1.00
    height: 35
    width: 296
    style:
        fontSize: "32px"
        color:"black"
用户名称= new Layer
    parent: 部落动态1
    html: "南宫踏萱"
    x: 136
    y: 78
    backgroundColor: "rgba(123,123,123,0)"
    opacity: 1.00
    height: 35
    width: 296
    style:
        fontSize: "24px"
        color:"black"
任务内容= new Layer
    parent: 部落动态1
    html: "我们一起去看世界，这是我第一次去北海享受风景，不喜欢不要喷我，我来分享我的生活"
    x: 13
    y: 134
    backgroundColor: "rgba(123,123,123,0)"
    opacity: 1.00
    height: 90
    width: 737
    style:
        fontSize: "36px"
        color:"black"
        lineHeight: "64px"
部落图标 = new Layer
    parent: 部落动态1
    x: 652
    y: 9
    height: 58
    width: 60
    image: "images/30创建部落.png"
图片展示1= new Layer
    parent: 部落动态1
    y: 271
    x: 11
    height: 238
    width: 238
    image: "images/57204df85c3a4.jpeg"
图片展示2= new Layer
    parent: 部落动态1
    y: 271
    x: 258
    height: 238
    width: 238
    image: "images/57204df85c3a4.jpeg"
图片展示3= new Layer
    parent: 部落动态1
    y: 271
    x: 502
    height: 238
    width: 238
    image: "images/57204df85c3a4.jpeg"
评论 = new Layer
    parent: 部落动态1
    y: 527
    x: 84
    height: 54
    width: 50
    image: "images/消息.png"
评论数= new Layer
    parent: 部落动态1
    html: "20"
    style:
        fontSize: "30px"
        color:"black"
    y: 539
    x: 141
    width: 38
    height: 30
手赞 = new Layer
    parent: 部落动态1
    y: 527
    x: 325
    height: 54
    width: 50
    image: "images/赞.png"
赞数= new Layer
    parent: 部落动态1
    html: "20"
    y: 543
    x: 389
    height: 30
    width: 30
    style:
        fontSize: "30px"
        color:"black"
更多 = new Layer
    parent: 部落动态1
    y: 527
    x: 592
    height: 54
    width: 50
    image: "images/更多.png"
分隔1= new Layer
    x: 251
    y: 682
    width: 1
    height: 70
    backgroundColor: "rgba(153,102,51,1)"
分隔2= new Layer
    x: 498
    y: 682
    width:1
    height: 70
    backgroundColor: "rgba(153,102,51,1)"
底部tab= new Layer
    parent: scroll.content
    y: 1373
    width: 750
任务tab底 = new Layer
    parent:底部tab
    y: 35
    x: 74
    width: 116
    height: 169
    backgroundColor: "rgba(30,135,255,1)"
    style:
        borderWidth: "20px"
        borderTopLeftRadius: "50px"
        borderTopRightRadius: "50px"
        borderStyle: "solid"
任务tab上圆 = new Layer
    parent:底部tab
    y: 53
    x: 87
    width: 90
    height: 90
    borderRadius: 100
    backgroundColor: "rgba(255,255,255,1)"
任务tab图标 = new Layer
    parent:底部tab
    y: 62
    x: 101
    width: 70
    height: 70
    image: "images/刀剑斧.png"
任务文字 = new Layer
    parent:底部tab
    html: "任务"
    height: 42
    y: 158
    width: 78
    x: 101
    style:
        fontSize: "30px"
        color:"white"
