# cipher

Decrypt any substitution cipher in English.

## Usage

```bash
$ stack build
$ stack exec -- decrypt <<< 'Qbco co s oijqijui qbsq roio ijunylqckj'
...
This is a sentence that uses encryption
...
```

## Examples

```bash
$ stack exec -- decrypt -d words.txt --no-limit <<< "fhlrd be rdh hlev zlur, rdh dluf zlur be xbmbgo lgf wgcybgo vcj acjxf sh ec kjad kcuh rdhg vcj'uh ybxxbgo rc sh."

$ cat examples/secret1.txt | stack exec decrypt
```

## Todo

* Quick exit if a word has no CipherMap
* Run profiling to investigate slowdown (the example above runs in 3m)
