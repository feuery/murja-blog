CREATE TABLE blog.media (
       id uuid DEFAULT gen_random_uuid(),
       name text NOT NULL,
       data bytea NOT null)
