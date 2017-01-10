CREATE TABLE IF NOT EXISTS blog.UserGroup (
       ID SERIAL,
       Name VARCHAR NOT NULL UNIQUE,
       Description VARCHAR(2000) NOT NULL DEFAULT '',
       Img_location VARCHAR(2000) NOT NULL DEFAULT '',
       PRIMARY KEY (ID));

INSERT INTO blog.UserGroup(Name, Description) VALUES ('Admins', 'Group for admins');
INSERT INTO blog.UserGroup(Name, Description) VALUES ('Users', 'Group for ordinary mortals');

CREATE TABLE IF NOT EXISTS blog.GroupMapping (
       UserID INT NOT NULL,
       GroupID INT NOT NULL,
       PrimaryGroup BOOL NOT NULL DEFAULT FALSE, -- I don't think it's possible to enforce only one PrimaryGroup per user in the DML?
       PRIMARY KEY(UserID, GroupID),
       FOREIGN KEY(UserID) REFERENCES blog.Users(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE,
       FOREIGN KEY(GroupID) REFERENCES blog.UserGroup(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE);
