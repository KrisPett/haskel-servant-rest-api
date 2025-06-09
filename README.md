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
