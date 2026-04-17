## Variables: 1 or 2, discrete or continuous
## To visualise: any geom

## Description: if x & y swapped, conceptually equivalent
## Equivalent according to following modes?
## strict: false
## structural: false
## visual: false
## conceptual: true

## Examples
## aes(x = a) vs aes(y = a)
## aes(x = a, y = b) vs aes(x = b, y = a)


## Description: if x & y swapped + coord_flip(), visually equivalent
## Equivalent according to following modes?
## strict: true
## structural: true
## visual: true
## conceptual: true

## Examples
## aes(x = a) + coord_flip() vs aes(y = a)
## aes(x = a, y = b) + coord_flip() vs aes(x = b, y = a)
