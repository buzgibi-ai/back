alter table customer.survey_phones add column is_valid_number bool;

update customer.survey_phones
set is_valid_number = true
where id in(select id from customer.survey_phones);

alter table customer.survey_phones alter column is_valid_number set not null;