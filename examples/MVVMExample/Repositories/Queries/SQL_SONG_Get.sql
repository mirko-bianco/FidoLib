select
  rowid as id,
  title
from
  songs
where
  rowid = :id