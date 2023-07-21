alter table foreign_api.telnyx rename to telnyx_app;
alter table customer.phone_telnyx rename to phone_telnyx_app;

create table customer.telnyx_app_call (
    telnyx_app_id bigserial not null,   
    record_type text not null,
    call_session_id text not null,
    call_leg_id text not null,
    call_control_id text not null,
    is_alive boolean not null,
    constraint telnyx_app_call__telnyx_app_id_fk foreign key (telnyx_app_id) references foreign_api.telnyx_app(id));
