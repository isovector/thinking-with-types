## Impredicative Types {.rev2}

### Polytypes

Types come in two varieties---*monotypes* and *polytypes.* Monotypes are those
types that are entirely concrete, and polytypes are any types which contain a
`forall` quantifier. Thus `Int -> Bool` is a monotype, but `forall a. a ->
String` is a polytype.

hello

```{ghci=code/ImpredicativeTypes.hs}
interleaved
```

world

```{ghci=code/ImpredicativeTypes.hs}
serialized
```

