## Simple web crawler

### Examples

    $ cat samples/sites.txt

    http://stackoverflow.com
    http://github.com
    http://redis.io


    $ lein run samples/sites.txt 2
      ......

### Run specs

    $ lein test

### TODO

  * Add support of relative URLs
  * Do not process repeating links
