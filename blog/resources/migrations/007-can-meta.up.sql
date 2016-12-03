INSERT INTO blog.Permission(ID, action) VALUES(4, 'delete-post');
INSERT INTO blog.Permission(ID, action) VALUES(5, 'edit-post');

INSERT INTO blog.GroupPermissions VALUES(4, 1);
INSERT INTO blog.GroupPermissions VALUES(5, 1);
