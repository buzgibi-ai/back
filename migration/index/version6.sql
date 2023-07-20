alter table customer.survey_phones add column id serial primary key;

create table foreign_api.telnyx (
    id bigserial primary key,
    telnyx_ident text not null,
    application_name text not null,
    created timestamptz not null default now(),
    constraint telnyx__telnyx_ident_unique unique (id, telnyx_ident));

create table customer.phone_telnyx (
    voice_id bigint,
    telnyx_id bigserial not null,
    phone_id bigserial not null,
    constraint phone_telnyx__phone_id_fk foreign key (phone_id) references customer.survey_phones(id),
    constraint phone_telnyx__telnyx_id_fk foreign key (telnyx_id) references foreign_api.telnyx(id),
    constraint phone_telnyx__phone_telnyx unique (phone_id, telnyx_id));