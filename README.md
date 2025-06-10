# Haskell Servant with Persistent and Postgres

## Setup notes

**Run postgres in docker**

```
docker compose up -d
```

**Build and Run program in docker container to avoid issues on a Windows machine.**

```
docker build -t haskell-postgres .
docker compose -f docker-compose-dev.yml up -d
docker exec -it haskel-dev bash
stack build
stack exec haskel-servant-rest-api-exe
```

stack build && stack exec haskel-servant-rest-api-exe

## Haskel Notes

**Funktor Applicative Monad**

Functor is a type class that defines a mapping function over a type constructor. It allows you to apply a function to the value inside a context (like Maybe, List, etc.).
Applicative is a type class that extends Functor. It allows you to apply functions that are also in a context to values in a context. It provides the ability to lift functions into the context.
Monad is a type class that extends Applicative. It allows for chaining operations together, where the output of one operation can be used as the input to the next. It provides a way to handle computations that involve side effects or context.
