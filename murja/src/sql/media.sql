-- :name insert-media :<!
insert into blog.media (name, data) values (:name, :data) returning id
