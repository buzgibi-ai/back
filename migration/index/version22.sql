alter table auth.jwt add column uuid_hash bigint;
update auth.jwt set uuid_hash = random() * 100000 :: bigint;
alter table auth.jwt add constraint jwt__uuid_hash_uq unique (uuid_hash);
alter table auth.jwt alter column uuid_hash set not null;