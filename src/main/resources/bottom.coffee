
form = new Layer
# 	parent: collect
	y: 78
	width: 782
	height: 502
	backgroundColor: "rgba(123,123,123,0)"
	x: 79

headerImage = new Layer
	parent: form
	height: 42
	image: "images/购买流程.png"
	width: 780
	y: 0
	x: 0

imageClose = new Layer
	height: 23
	parent: form
	image: "images/关闭按钮（点击前）.png"
	width: 23
	x: 745
	y: 11



mainContent = new Layer
	parent: form
	y: 41
	width: 781
	height: 420
	x: 5
	backgroundColor: "rgba(123,123,123,0)"



itemBg = new Layer
	parent: mainContent
	height: 170
	width: 780
	backgroundColor: "rgba(230,230,230,1)"



itemContent = new Layer
	parent: itemBg
	y: 40
	height: 91
	width: 667
	x: 58
	backgroundColor: "rgba(123,123,123,0)"


itemImage = new Layer
	parent: itemContent
	height: 72
	x: 15
	width: 96
	y: 7
	backgroundColor: "rgba(16,200,235,1)"


titleTcl = new Layer
	html: "TCl"
	parent: itemContent
	height: 21
	x: 132
	y: 7
	backgroundColor: "rgba(123,123,123,0)"
	style:
		fontSize: "14px"
		color: "#4D4D4D"
		lineHeight: "21px"
		textAlign: "left"


titlecColor = new Layer
	html: "颜色: 红色"
	parent: itemContent
	height: 21
	x: 136
	y: 32
	backgroundColor: "rgba(123,123,123,0)"
	width: 74
	style:
		fontSize: "14px"
		color: "#4D4D4D"
		lineHeight: "21px"
		textAlign: "left"


titleCount = new Layer
	html: "数量: 1"
	parent: itemContent
	height: 21
	x: 334
	y: 32
	backgroundColor: "rgba(123,123,123,0)"
	width: 74
	style:
		fontSize: "14px"
		color: "#4D4D4D"
		lineHeight: "21px"
		textAlign: "left"


titlePriceBNote = new Layer
	html: "无最低消费"
	parent: itemContent
	height: 21
	x: 132
	y: 58
	backgroundColor: "rgba(123,123,123,0)"
	width: 74
	style:
		fontSize: "14px"
		color: "#4D4D4D"
		lineHeight: "21px"
		textAlign: "left"
		color: "#F15A24"


titlePrice = new Layer
	html: "合约价: 2000元"
	parent: itemContent
	height: 21
	x: 546
	y: 32
	backgroundColor: "rgba(123,123,123,0)"
	width: 102
	style:
		fontSize: "14px"
		color: "#4D4D4D"
		lineHeight: "21px"
		textAlign: "left"




items = new Layer
	parent: mainContent
	height: 93
	image: "images/items.png"
	width: 108
	y: 260
	x: 46

textArea = new Layer
	parent: mainContent
	x: 169
	y: 231
	height: 142
	width: 542
	borderWidth: 2
	borderColor: "rgba(230,230,230,1)"
	backgroundColor: "rgba(83,83,83,0)"

reason = new Layer
	x: 184
	y: 303
	height: 26
	width: 418
	html: "很抱歉带给您不好的体验，请写下退货原因，以便于我们改进！"
	backgroundColor: "rgba(123,123,123,0)"
	style:
		fontSize: "14px"
		color: "#999999"

fotter = new Layer
	parent: form
	x: 5
	y: Align.bottom -0
	width: 780
	height: 42
	backgroundColor: "#E6E6E6"

btnContinue = new Layer
	html: "提交申请"
	parent: fotter
	height: 30
	width: 94
	x: Align.center
	y: Align.center
	backgroundColor: "rgba(16,200,235,1)"
	style:
		fontSize: "14px"
		textAlign: "center"

