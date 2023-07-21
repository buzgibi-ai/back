alter table foreign_api.telnyx rename to telnyx_app;
alter table customer.phone_telnyx rename to phone_telnyx_app;

create table foreign_api.telnyx_app_call (
    id bigserial primary key,
    telnyx_app_id bigserial not null,   
    record_type text not null,
    call_session_id text not null,
    call_leg_id text not null,
    call_control_id text not null,
    is_alive boolean not null,
    constraint telnyx_app_call__telnyx_app_id_fk foreign key (telnyx_app_id) references foreign_api.telnyx_app(id));

create table foreign_api.telnyx_app_call_phone (
    telnyx_app_call_id bigserial not null,
    call_from text not null,
    call_to text not null,
    call_hangup_cause text,
    recording_urls jsonb,
    call_status text,
    constraint telnyx_app_call_phone__telnyx_app_call_id_fk foreign key (telnyx_app_call_id) references foreign_api.telnyx_app_call(id),
    constraint telnyx_app_call_phone__telnyx_app_call_id__call_to__call_from unique (telnyx_app_call_id, call_from, call_to));