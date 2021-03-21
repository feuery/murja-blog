-- :name insert-media :<!
insert into blog.media (name, data) values (:name, :data) returning id

-- :name get-media :? :1
select name, data from blog.media where id = :id::uuid
