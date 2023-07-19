create table customer.voice_share_link (
    bark_id bigserial not null,
    share_link_url text not null,
    expires_at timestamptz not null,
    constraint voice_share_link__bark_id_fk foreign key (bark_id) references foreign_api.bark(id));