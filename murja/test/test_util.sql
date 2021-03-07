-- :name test-user-exists? :? :1

select exists (select * from blog.users where username = 'test-user');

-- :name insert-test-user! :!
insert into blog.users (username, password, nickname, img_location)
values (:username, :password, :nickname, :img_location);

-- :name test-user-id :? :1
select id from blog.users where username = 'test-user';

-- :name delete-test-posts :!
delete from blog.post
where tags ?? 'test-generated'

-- :name delete-test-users :!
delete from blog.Users
where username like 'test%';
