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
      inner join customer.survey_files as sf
      on s.id = sf.survey_id
      inner join storage.file as f
      on sf.report_id = f.id
      where s.id = new.id and s.survey_status = 'processed') as tmp;
  perform 
    pg_notify('report' || '_' || u.id, coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger survey_report_delivery 
after update on customer.survey
for each row execute procedure send_report_to_server();


create or replace function send_voice_to_server()
returns trigger as
$$
declare
  result jsonb;
begin
    select 
      json_build_object(
        'survey', tmp.id,
        'voice', tmp.voice_id)
    into result 
    from (
      select 
        s.id, NEW.voice_id
      from customer.survey as s
      inner join customer.survey_draft as sd
      on s.id = sd.survey_id
      inner join customer.survey_bark as sb
      on sd.id = sb.survey_draft_id
      where sb.voice_id = NEW.voice_id) as tmp;
  perform 
    pg_notify('voice' || '_' || u.id, coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger bark_voice_delivery 
after update on customer.survey_bark
for each row execute procedure send_voice_to_server();