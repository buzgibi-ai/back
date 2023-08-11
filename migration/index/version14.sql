create table public.notification (
    id bigserial primary key,
    text text not null,
    level text not null,
    created_at timestamptz not null default now());


create table customer.user_notification (
    user_id bigserial not null,
    notification_id bigserial not null,
    type jsonb not null,
    constraint user_notification__user_id_fk foreign key (user_id) references customer.profile(id),
    constraint user_notification__notification_id_fk foreign key (notification_id) references public.notification(id));