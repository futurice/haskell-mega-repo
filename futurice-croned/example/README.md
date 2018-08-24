# futurice-croned example

## build

```
cat croned-server.xz | xz --decompress > croned-example
docker build -t futurice-croned-example .
```

## Test

```
docker run -p 8000:8000 -e MYKEY=myvar --rm futurice-croned-example
```

---

## Notes

```
cp $(cabal-plan list-bin croned-server) .
strip croned-server
cat croned-server |  xz -9 > croned-server.xz
```
