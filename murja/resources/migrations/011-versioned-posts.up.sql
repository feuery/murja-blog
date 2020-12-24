CREATE TABLE blog.Post_History (
       ID SERIAL,
       Title VARCHAR(1000) NOT NULL DEFAULT 'Untitled',
       Content TEXT NOT NULL DEFAULT '',
       creator_id INT NOT NULL,
       tags JSONB NOT NULL DEFAULT '[]'::jsonb,
       created_at TIMESTAMP NOT NULL,
       version INT NOT NULL DEFAULT 1,
       PRIMARY KEY(ID, version),
       FOREIGN KEY(creator_id) REFERENCES blog.users(ID)
              ON UPDATE CASCADE
	      ON DELETE CASCADE,
       FOREIGN KEY(ID) REFERENCES blog.Post(ID)
              ON UPDATE CASCADE
	      ON DELETE CASCADE);
