create table auth.password_reset_link (
    id bigserial primary key,
    link text not null,
    valid_until timestamptz not null,
    user_id bigserial not null,
    created_at timestamptz not null default now(),
    is_expended boolean,
    constraint auth_email_confirmation_link__user_id__fk foreign key (user_id) references auth.user(id));
