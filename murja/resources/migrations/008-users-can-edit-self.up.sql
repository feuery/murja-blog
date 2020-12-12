-- Of course anyone can edit themselves
-- Which means these permissions are kind of unnecessary atm
INSERT INTO blog.Permission(ID, action) VALUES(9, 'delete-user');
INSERT INTO blog.Permission(ID, action) VALUES(10, 'edit-user');
INSERT INTO blog.Permission(ID, action) VALUES(11, 'edit-self');

INSERT INTO blog.GroupPermissions VALUES(9, 1);
INSERT INTO blog.GroupPermissions VALUES(10, 1);
INSERT INTO blog.GroupPermissions VALUES(11, 1);

INSERT INTO blog.GroupPermissions VALUES(11, 2);
