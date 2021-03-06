# Santorini Game AI

## Unit Testing

```
$ stack test
```


## Running Program

```
$ stack build
$ stack exec santorini-player
```

```
$ stack build
$ scripts/play.sh 1
```

## Profiling

```
$ stack build --profile
$ source ./scripts/environ.sh
$ ./scripts/wrapper.sh < ./scripts/test_input.txt
```

- Check `santorini-player.prof`.


## Tournament

```
$ source ./scripts/environ.sh
$ $tn --timeout 10 ./scripts/wrapper_to.sh $pr > ./data/xxxx.txt 2> ./data/xxxx.err
$ $tn --timeout 10 --all-cards ./scripts/wrapper_to.sh $pr > ./data/xxxx.txt 2> ./data/xxxx.err
```
