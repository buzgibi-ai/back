create table foreign_api.telnyx_app (
    id bigserial primary key,
    survey_id bigserial not null,
    telnyx_ident text not null,
    application_name text not null,
    created timestamptz not null default now(),
    constraint telnyx_app__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint telnyx_app__telnyx_ident_uq unique (telnyx_ident));

create table customer.call_telnyx_app (
    voice_id bigint,
    phone_id bigserial not null,
    invalid text,
    call_leg_id text,
    call_control_id text,
    call_status text not null,
    call_hangup_cause text,
    constraint phone_telnyx_app__phone_id_fk foreign key (phone_id) references customer.survey_phones(id),
    constraint phone_telnyx__call_leg_id_uq unique (call_leg_id));