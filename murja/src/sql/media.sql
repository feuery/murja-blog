-- :name insert-media :<!
insert into blog.media (name, data) values (:name, :data) returning id

-- :name get-media :? :1
select name, data from blog.media where id = :id::uuid

-- :name list-pictures* :?
select id, name from blog.media;

-- :name delete-picture* :!
delete from blog.media where id = :id;

-- :name select-referencing-posts* :?
select * from blog.media_post_pairing where media_id = :id;
