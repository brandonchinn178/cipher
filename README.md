# cipher

Decrypt any substitution cipher in English.

## Usage

```bash
$ stack build
$ stack exec -- cipher <<< 'Rdeq eq tl alipynras qalralia'
...
This is an encrypted sentence
...
```

## Examples

```bash
$ stack exec -- cipher --no-limit <<< "fhlrd be rdh hlev zlur, rdh dluf zlur be xbmbgo lgf wgcybgo vcj acjxf sh ec kjad kcuh rdhg vcj'uh ybxxbgo rc sh."
```

## Todo

* Remove duplicate decryptions
* Run profiling to investigate slowdown (the example above runs in 3m)
* Rank more common words higher than rarer words (e.g. `be` over `fe`)
