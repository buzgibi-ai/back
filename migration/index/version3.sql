create schema if not exists customer;
create table customer.profile (
    id bigserial primary key ,
    user_id bigserial not null,
    name text null,
    surname text null,
    phone text null,
    created timestamptz not null default now(),
    constraint profile__user_id_fk foreign key (user_id) references auth.user(id),
    constraint profile__user_id_unique unique (user_id));

create table customer.survey (
    id bigserial primary key,
    user_id bigserial not null,
    survey text not null,
    created timestamptz not null default now(),
    processed timestamptz,
    survey_status text not null,
    latitude float8 not null,
    longitude float8 not null,
    category text not null,
    survey_type text not null,
    constraint survey__user_id_fk foreign key (user_id) references customer.profile(id));

create table customer.survey_files (
    survey_id bigserial not null,
    report_id bigint null,
    phones_id bigserial not null,
    constraint survey_files__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint survey_files__phones_id_fk foreign key (phones_id) references storage.file(id),
    constraint survey_files__survey_report_phones_uq unique (survey_id, phones_id));

create schema if not exists foreign_api;
create table foreign_api.bark (
    id bigserial primary key,
    bark_ident text not null,
    bark_status text not null,
    req jsonb not null,
    created timestamptz not null default now(), 
    modified timestamptz,
    constraint bark__bark_ident_uq unique (bark_ident));

create table customer.survey_bark (
    survey_id bigserial not null,
    voice_id bigint,
    bark_id bigserial not null,
    constraint survey_bark__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint survey_bark__bark_id_fk foreign key (bark_id) references foreign_api.bark(id),
    constraint survey_bark__survey_bark_uq unique (survey_id, bark_id));

create table customer.survey_phones (
    id serial primary key,
    survey_id bigserial not null,
    phone text not null,
    constraint survey_phones__survey_id_fk foreign key (survey_id) references customer.survey(id));