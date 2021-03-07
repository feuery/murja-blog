-- :name count-users :? :1
SELECT COUNT(*) AS Count FROM blog.Users

-- :name update-user* :!
update blog.Users
set Nickname = :nickname,
    Img_location = :img_location,
    password = :password
where ID = :_id

-- :name insert-user* :!
insert into blog.Users (username, nickname, img_location, password)
values (:username, :nickname, :img_location, :password)

-- :name get-user :? :1
select * from blog.Users where username = :username

-- :name insert-groupmapping :!
insert into blog.groupmapping (userid, groupid, primarygroup)
values (:userid, :groupid, :primarygroup)
