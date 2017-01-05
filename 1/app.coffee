# Project Info
# This info is presented in a widget when you share.
# http://framerjs.com/docs/#info.info

Framer.Info =
	title: ""
	author: "tian"
	twitter: ""
	description: ""


# Variables
pageCount = 8
gutter = 60

# Create PageComponent
pageScroller = new PageComponent
	x: Align.center
	y: Align.center
	point: Align.center
	width: 1920
	height: 1080
	scrollVertical: false
	clip: false

layerA = new Layer
	size: pageScroller.size


_1 = new Layer
	height: 1080
	image: "images/画板 1.png"
	width: 1920
	parent: layerA

pageScroller.addPage(layerA, 'right')
