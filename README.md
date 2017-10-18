# Pollen

## Dev DB Setup

```
$ createdb pollen_dev

$ psql -U postgres
> CREATE ROLE pollen;
> ALTER ROLE pollen with LOGIN;
> ALTER ROLE pollen with SUPERUSER;
```

## Test notification endpoint

```
$ curl -H "Content-Type: application/json" -X POST -d '{"deviceToken":"foo"}' http://localhost:8080/notifications
```
