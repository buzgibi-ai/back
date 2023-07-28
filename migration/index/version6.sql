create table customer.phone_transcription (
    transcription text not null,
    survey_id bigserial not null,
    phone_id bigserial not null,
    constraint phone_openai__phone_id_fk foreign key (phone_id) references customer.survey_phones(id),
    constraint phone_openai__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint phone_openai__phone_survey_uq unique (phone_id, survey_id));