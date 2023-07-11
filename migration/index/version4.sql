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

create table customer.enquiry (
    id bigserial primary key,
    user_id bigserial not null,
    enquiry text not null,
    created timestamptz not null default now(),
    processed timestamptz,
    enquiry_status text not null,
    latitude double not null,
    longtitude double not null,
    constraint enquiry__user_id_fk foreign key (user_id) references customer.profile(id));

create table customer.enquiry_file (
    enquiry_id bigserial not null,
    report_id bigserial not null,
    voice_id bigserial not null,
    constraint enquiry_file__enquiry_id_fk foreign key (enquiry_id) references customer.enquiry(id),
    constraint enquiry_file__report_id_fk foreign key (report_id) references storage.file(id), 
    constraint enquiry_file__voice_id_fk foreign key (voice_id) references storage.file(id) );

-- fill user profile with existing users from auth.user
insert into customer.profile
(user_id)
select id from auth.user;