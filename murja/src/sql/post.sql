-- :name post-comments*

SELECT c.ID, c.parent_comment_id, 
c.Content,
c.created_at,
u.Username, u.Nickname, u.Img_location
FROM blog.Comment c
JOIN blog.Users u ON u.ID = c.creator_id
WHERE c.parent_post_id = :parent-post-id
ORDER BY c.created_at


-- :name get-titles-by-year*
SELECT p.Title,
       EXTRACT(MONTH FROM p.created_at) AS "Month",
       EXTRACT(YEAR FROM p.created_at) AS "Year",
       p.id as "Id",
       p.Tags
FROM blog.Post p
WHERE :show-hidden OR (NOT p.tags ?? 'unlisted' AND NOT p.tags ?? 'hidden')
ORDER BY p.created_at DESC

-- :name post-versions*
SELECT version 
FROM blog.Post_History 
WHERE ID = :post-id AND NOT tags ?? 'hidden' 
ORDER BY version ASC

-- :name next-post-id :? :1
SELECT p.ID 
FROM blog.Post p
WHERE p.ID < :post-id AND NOT p.tags ?? 'hidden'
ORDER BY p.ID DESC
LIMIT 1

-- :name prev-post-id :? :1
SELECT p.ID 
FROM blog.Post p
WHERE p.ID > :post-id AND NOT p.tags ?? 'hidden'
ORDER BY p.ID ASC
LIMIT 1

-- :name get-by-id* :? :1
SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS "amount-of-comments"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.ID = :post-id AND (NOT p.tags ?? 'hidden' OR (p.tags ?? 'hidden' AND :show-hidden))
GROUP BY p.ID, u.ID

-- :name get-versioned-by-id* :? :1
SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, p.version, COUNT(c.ID) AS "amount-of-comments"
FROM blog.Post_History p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.ID = :post-id AND p.version = :version-id AND not tags ?? 'hidden'
GROUP BY p.ID, u.ID, p.title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, p.version


-- :name get-all*
SELECT p.id, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS "amount-of-comments"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
--~ (when (contains? params :limit) "LIMIT :limit") 

-- :name get-page*
SELECT p.ID, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS "amount-of-comments"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE ((NOT p.tags ?? 'unlisted') OR :show-hidden)
  AND ((NOT p.tags ?? 'hidden') OR :show-hidden)
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT :page-size
OFFSET :page-id

-- :name landing-page-ids*
SELECT id
FROM blog.Post 
WHERE tags ?? 'landing-page' AND NOT tags ?? 'hidden'

-- :name get-posts-tags* :? :1
SELECT tags FROM blog.Post WHERE id = :post-id

-- :name update-tags* :!
update blog.post
set tags = :new-tags
where id = :post-id

-- :name insert-post :!
insert into blog.post (title, content, creator_id, tags)
values (:title, :content, :creator-id, :tags);

-- :name update-post :!
update blog.post
set title = :title,
    content = :content,
    tags = :tags
where id = :id

-- :name delete-post :!
delete blog.post
where id = :id
--~ (when (contains? params :version) "AND version = :version")

-- :name delete-comment :!
delete blog.comment
where id = :id

-- :name insert-comment :!
insert into blog.comment (parent_post_id, parent_comment_id, content, creator_id)
values (:parent-post-id, :parent-comment-id, :content, :creator-id)

-- :name get-landing-page* :? :1
SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS \"amount-of-comments\"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID

-- :name landing-page-title :? :1
SELECT p.Title, p.Id
FROM blog.Post p
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden'
