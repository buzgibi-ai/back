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

create table customer.survey_report (
    survey_id bigserial not null,
    report_id bigserial,
    constraint survey_report__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint survey_report__report_id_fk foreign key (report_id) references storage.file(id),
    constraint survey_report__survey_report unique (survey_id, report_id));

create schema if not exists foreign_api;
create table foreign_api.bark (
    id bigserial primary key,
    survey_id bigserial not null,
    bark_ident text not null,
    bark_status text not null,
    req jsonb not null,
    created timestamptz not null default now(), 
    modified timestamptz,
    constraint bark__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint bark__bark_ident_unique unique (bark_ident));

create table customer.survey_bark (
    survey_id bigserial not null,
    voice_id bigserial,
    bark_id bigserial not null,
    constraint survey_bark__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint survey_bark__bark_id_fk foreign key (bark_id) references foreign_api.bark(id),
    constraint survey_bark__survey_bark unique (survey_id, bark_id));

create table customer.survey_phones (
    survey_id bigserial not null,
    phone text not null,
    constraint survey_phones__survey_id_fk foreign key (survey_id) references customer.survey(id));

-- fill user profile with existing users from auth.user
insert into customer.profile
(user_id)
select id from auth.user;