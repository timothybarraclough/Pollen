# Pollen

## Dev DB Setup

```
$ createdb pollen_dev

$ psql -U postgres
> CREATE ROLE pollen;
> ALTER ROLE pollen with LOGIN;
> ALTER ROLE pollen with SUPERUSER;
```
