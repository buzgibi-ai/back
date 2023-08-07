create table customer.survey_draft (
    id bigserial primary key,
    survey text not null,
    survey_id bigserial not null,
    created timestamptz not null default now(),
    constraint survey_draft__survey_id_fk foreign key (survey_id) references customer.survey(id));

insert into customer.survey_draft
(survey, survey_id)
select survey, id from customer.survey;

alter table customer.survey_bark add column survey_draft_id bigint;

update customer.survey_bark
set survey_draft_id = id
from customer.survey_draft as sd
where survey_bark.survey_id = sd.survey_id;

alter table customer.survey_bark add constraint survey_bark__survey_draft_id_fk foreign key (survey_draft_id) references customer.survey_draft(id);

alter table customer.survey_bark drop column survey_id;
alter table customer.survey drop column survey;

update customer.survey
set survey_status = 'submit'
where survey_status = 'received';