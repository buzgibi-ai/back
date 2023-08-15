create or replace function send_report_to_server()
returns trigger as
$$
declare
  result jsonb;
begin
    select
        json_build_object(
        'survey', tmp.id,
        'report', tmp.report_id,
        'status', tmp.survey_status)
    into result 
    from (
      select 
        s.id, f.id as report_id, s.survey_status
      from customer.survey as s
      inner join customer.survey_files as sf
      on s.id = sf.survey_id
      inner join storage.file as f
      on sf.report_id = f.id
      where s.id = new.id and s.survey_status = 'processed') as tmp;
  perform pg_notify('report', coalesce(result :: text, 'null' :: text));
  return new;
end;
$$ language 'plpgsql';

create or replace trigger survey_report_delivery 
after update on customer.survey
for each row execute procedure send_report_to_server();