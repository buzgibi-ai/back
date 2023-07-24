-- sa stands for sentiment analysis
create table customer.phone_sentiment_analysis (
    result text not null,
    survey_id bigserial not null,
    phone_id bigserial not null,
    constraint phone_phone_sentiment_analysis__phone_id_fk foreign key (phone_id) references customer.survey_phones(id),
    constraint phone_phone_sentiment_analysis__survey_id_fk foreign key (survey_id) references customer.survey(id),
    constraint phone_phone_sentiment_analysis__phone_survey unique (phone_id, survey_id));