

# jiffy

[![Linux Build Status](https://travis-ci.org/richfitz/jiffy.svg?branch=master)](https://travis-ci.org/richfitz/jiffy)

[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/jiffy?svg=true)](https://ci.appveyor.com/project/richfitz/jiffy)

It's pronounced "yiffy".

![dance!](http://media4.giphy.com/media/tZU2GT1fL08xi/giphy.gif)

Interface to [giphy](http://giphy.com) via their [API](https://github.com/Giphy/GiphyAPI).

Interface to 'Giphy'

## API key

Giphy requires use of a key to use their API.  For now, this package uses the public API key which may be revoked at some future time.  If you have a proper API key, set the environment variable `GIPHY_API_KEY` and it will be used instead.

## Installation


```r
devtools::install_github("richfitz/jiffy")
```
