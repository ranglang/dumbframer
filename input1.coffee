
layerA = new Layer
	x: 313
	y: 228
	backgroundColor: "rgba(128,0,128,1)"

a =1

layerC = new Layer
	x: 313
	y: 228
	parent: layerA
	backgroundColor: "rgba(128,0,128,1)"

layerB = new Layer
	x: 313
	y: 228
	parent: layerC
	backgroundColor: "rgba(128,0,128,1)"

layerD = new Layer
	x: 313
	y: 228
	parent: layerB
	backgroundColor: "rgba(128,0,128,1)"
