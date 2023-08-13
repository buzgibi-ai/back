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
  perform pg_notify('voice', result :: text);
  return new;
end;
$$ language 'plpgsql';

create or replace trigger bark_voice_delivery 
after update on customer.survey_bark
for each row execute procedure send_voice_to_server();