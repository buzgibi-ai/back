create schema if not exists auth;
create table auth.user (
    id bigserial primary key,
    email text not null,
    pass text not null,
    created timestamptz not null default now(),
    modified timestamptz,
    constraint email__uk unique (email));

create table auth.jwt (
    user_id bigserial not null,
    jwt text not null,
    created timestamptz not null default now(),
    is_valid boolean not null default true,
    constraint auth_user__fk foreign key (user_id)
    references auth.user(id));