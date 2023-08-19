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
      from auth.user as u
      inner join customer.profile as p
      on u.id = p.user_id  
      inner join customer.survey as s
      on p.id = s.user_id
      left join customer.survey_files as sf
      on s.id = sf.survey_id
      left join storage.file as f
      on sf.report_id = f.id
      where s.id = new.id 
      and (
        s.survey_status = 'processed' or 
        regexp_like(s.survey_status, 'insufficient_funds:'))
    ) as tmp;
  perform 
    pg_notify('report' || '_' || u.id, coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';