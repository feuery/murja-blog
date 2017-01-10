-- This is quickly spiralling to an endless abyss of shit
CREATE TABLE blog.Permission (
       ID SERIAL,
       action varchar(2000),
       PRIMARY KEY (ID));

INSERT INTO blog.Permission(action) VALUES('create-page');
INSERT INTO blog.Permission(action) VALUES('create-post');
INSERT INTO blog.Permission(action) VALUES('create-comment');
       
CREATE TABLE blog.GroupPermissions (
       PermissionID INT,
       GroupID INT,

       PRIMARY KEY(PermissionID, GroupID),
       FOREIGN KEY(PermissionID) REFERENCES blog.Permission(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE,
       FOREIGN KEY(GroupID) REFERENCES blog.UserGroup(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE);

-- The default sql accent PGSQL speaks is a bit braindead and I can't query the UserGroup table to get the exact id of admin group
-- 1 == admins' id
INSERT INTO blog.GroupPermissions VALUES(1, 1);
INSERT INTO blog.GroupPermissions VALUES(2, 1);
INSERT INTO blog.GroupPermissions VALUES(3, 1);

-- 2 == users' id
INSERT INTO blog.GroupPermissions VALUES(3, 2);
