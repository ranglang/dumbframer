##  What is DumbFramer?

Note: Work in progress

This tool reads coffeeScript files from [FramerJS](https://github.com/koenbok/Framer).
and convert it into files of html add css . In the near future, a web application will be
avaible for works designed by framer. I think i will be more efficient for implement the work
into Angular like projects.

```
imageLayer = new Layer({width:128, height:128, image:"images/icon.png"})
imageLayer.center()
imageLayer.states = {
    second: {y:100, scale:0.6, rotationZ:100},
    third:  {y:300, scale:1.3},
    fourth: {y:200, scale:0.9, rotationZ:200}
}
```

## Usage

```
    $ sbt 'run'
```


## License

DumbFramer is distributed under the
[Scala License](http://www.scala-lang.org/license.html).
=======
