create table auth.email_confirmation_link (
    id bigserial primary key,
    link text not null,
    valid_until timestamptz not null,
    user_id bigserial not null,
    constraint auth_email_confirmation_link__user_id__fk foreign key (user_id) references auth.user(id));

alter table auth.user add column is_email_confirmed boolean not null default false;