CREATE SCHEMA blog;

CREATE TABLE blog.users (
       Username VARCHAR(100) NOT NULL PRIMARY KEY,
       Password CHAR(128) NOT NULL, 	--SHA-512
       Nickname VARCHAR(1000) NOT NULL DEFAULT '',
       Img_location VARCHAR(2000));
