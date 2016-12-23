# Tool for generat clean code for design of [framerjs studio](https://framerjs.com/)

The project is a backend for [bump.red](http://bump.red/),
it receive a file or a zipped file , add parse the html file.

## See Test for  example





## Usage

Start services with sbt:

```
$ sbt
> ~re-start
```

With the service up, you can start sending HTTP requests:

```
$ curl http://localhost:9000/ip/8.8.8.8
{
  "city": "Mountain View",
  "query": "8.8.8.8",
  "country": "United States",
  "lon": -122.0881,
  "lat": 37.3845
}
```

## Author & license

If you have any questions regarding this project contact:

rang <caedman2011@gmail.com>

For licensing info see LICENSE file in project's root directory.
