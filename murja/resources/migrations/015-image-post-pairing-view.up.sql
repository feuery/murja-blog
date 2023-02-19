create or replace view blog.media_post_pairing as 
select post.id as post_id,
       post.title as post_title,
       media.id as media_id,
       media.name as media_name
from blog.post post
join blog.media media
     on post.content ilike '%'||media.id||'%';
