# cipher

Decrypt any substitution cipher in English.

## Usage

```bash
$ stack build
$ stack exec -- decrypt <<< 'Qbco co s oijqijui qbsq roio ijunylqckj'
This is a sentence that uses encryption
```

## Examples

```bash
$ stack exec -- decrypt <<< "fhlrd be rdh hlev zlur, rdh dluf zlur be xbmbgo lgf wgcybgo vcj acjxf sh ec kjad kcuh rdhg vcj'uh ybxxbgo rc sh."

$ stack exec -- decrypt -f examples/secret1.txt

$ stack exec -- decrypt -f examples/secret2.txt -d Words --limit 10
```
