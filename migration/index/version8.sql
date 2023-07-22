create table customer.phone_openai (
    transcription_id bigserial not null,
    phone_id bigserial not null,
    constraint phone_openai__phone_id_fk foreign key (phone_id) references customer.survey_phones(id),
    constraint phone_openai__transcription_id_fk foreign key (transcription_id) references storage.file(transcription_id),
    constraint phone_openai__phone_transcription unique (phone_id, transcription_id));