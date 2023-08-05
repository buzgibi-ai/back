alter table customer.phone_transcription add column error text;
alter table customer.phone_transcription alter column transcription drop not null;