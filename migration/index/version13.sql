create table public.phone_transcription_result (
    phone text not null,
    transcription text not null,
    result text not null,
    created_at timestamptz not null default now());