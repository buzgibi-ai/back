create schema if not exists storage;
create table storage.file (
    id bigserial primary key,
    title text not null,
    mime text not null,
    hash text not null,
    created timestamptz not null default now(),
    modified timestamptz,
    bucket text not null default 'default',
    deleted timestamptz,
    is_deleted bool not null default false,
    exts json,
    constraint "file__hash_unique" unique ("hash"));