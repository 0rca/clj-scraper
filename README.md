# clj-scraper

A web-scraper for personal enjoyment and experiments with core/async.
Supports two websites for your scraping pleasure.

## Requirements

1. [Leiningen](https://leiningen.org)
2. JDK >= 1.6

## Building

```sh
$ lein uberjar
```

## Usage

```sh
java -jar target/scraper-0.3.1-standalone.jar
```

## Options

    -c, --cache [dir]           cache files directory
    -o, --output [dir]          downloaded images directory
    -w, --workers [num]         number of download workers
    -d, --debug                 display debug info
    -s, --source [ngo|vrotmne]  handle of website to scrape
    -S, --skip [num]            skip first num posts of LJ
    -L, --list-only             save image urls, but don't download
    -x, --exit-on-exist         exit the process if downloaded file exists
    -h, --help                  print this help

## Examples

```sh
$ java jar target/scraper-0.3.1-standalone.jar -w 20 -s ngo
```

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
